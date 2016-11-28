{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE UndecidableInstances       #-}
module GHCJS.HPlay.View(
    Widget(..)
  -- * Running it
  , module Transient.Move.Utils
  , runBody
  , addHeader
  , render
  , runWidget'
  , addSData
  -- * Widget Combinators and Modifiers
  , (<<)
  , (<<<)
  , (<!)
  , (<++)
  , (++>)
  , validate
  , wcallback
  -- * Basic Widgets
  , option
  , wprint
  , getString
  , inputString
  , getInteger
  , inputInteger
  , getInt
  , inputInt
  , inputFloat
  , inputDouble
  , getPassword
  , inputPassword
  , setRadio
  , setRadioActive
  , getRadio
  , setCheckBox
  , getCheckBoxes
  , getTextBox
  , getMultilineText
  , textArea
  , getBool
  , getSelect
  , setOption
  , setSelectedOption
  , wlabel
  , resetButton
  , inputReset
  , submitButton
  , inputSubmit
  , wbutton
  , wlink
  , noWidget
  , wraw
  , rawHtml
  , isEmpty
  -- * Events
  , BrowserEvent(..)
  -- * Out of Flow Updates
  , UpdateMethod(..)
  , at
  -- * Reactive and Events
  , IsEvent(..)
  , EventData(..)
  , EvData(..)
  , resetEventData
  , getEventData
  , setEventData
  , raiseEvent
  , fire
  , wake
  , pass
  , continueIf
  -- * Low-level and Internals
  , ElemID
  , FormInput(..)
  , getNextId
  , genNewId
  , continuePerch
  , getParam
  , getCont
  , runCont
  , elemById
  , withElem
  , getProp
  , setProp
  , alert
  , fromJSString
  , toJSString
  , getValue
  -- * Re-exported
  , module Control.Applicative
  , module GHCJS.Perch
  -- remove
  ,CheckBoxes(..)
)  where


import           Transient.Internals     hiding ((!>),input, option)
import           Transient.Logged
import           Transient.Move.Utils


import           Control.Monad.State
import qualified Data.Map                as M

import           Control.Applicative
import           Control.Concurrent
import           Data.Dynamic

import           Data.Maybe
import           Data.Monoid
import           Data.Typeable
import           Prelude                 hiding (id, span)
import           System.IO.Unsafe
import           Unsafe.Coerce


#ifdef ghcjs_HOST_OS
import           GHCJS.Foreign
import           GHCJS.Foreign.Callback
import           GHCJS.Marshal
import           GHCJS.Perch             hiding (JsEvent (..), eventName,
                                          option)
import           GHCJS.Types
import           Transient.Move          hiding (pack)

import           Data.JSString           as JS hiding (empty, span, strip)
#else
import           GHCJS.Perch             hiding (JSVal, JsEvent (..), eventName,
                                          option)
import           Transient.Move          hiding (JSString, pack)
#endif

#ifndef ghcjs_HOST_OS
type JSString = String
#endif

---- | if invoked from the browser, run A computation in the web server and return to the browser
--atServer :: Loggable a => Cloud a -> Cloud a
--atServer proc= do
--     server <- onAll getSData <|> error "server not set, use 'setData serverNode'"
--     runAt server  proc

toJSString :: (Show a, Typeable a) => a -> JSString
toJSString x =
  if typeOf x == typeOf (undefined :: String )
  then pack $ unsafeCoerce x
  else pack $ show x

fromJSString :: (Typeable a,Read a) => JSString -> a
fromJSString s = x
   where
     x | typeOf x == typeOf (undefined :: JSString) =
         unsafeCoerce x            --  !> "unsafecoerce"
       | typeOf x == typeOf (undefined :: String) =
         unsafeCoerce $ pack $ unsafeCoerce x            -- !!> "packcoerce"
       | otherwise = read $ unpack s            -- !> "readunpack"

getValue :: MonadIO m => Elem -> m (Maybe String)
getName :: MonadIO m => Elem -> m (Maybe String)
#ifdef ghcjs_HOST_OS
getValue e = liftIO $ do
   s <- getValueDOM e
   fromJSVal s -- return $ JS.unpack s

getName e = liftIO $ do
   s <- getNameDOM e
   fromJSVal s
#else
getValue = undefined
getName = undefined
#endif

elemById :: MonadIO m  => JSString -> m (Maybe Elem)
#ifdef ghcjs_HOST_OS
elemById id= liftIO $ do
   re <- elemByIdDOM id
   fromJSVal re
#else
elemById _= return Nothing
#endif

withElem :: ElemID -> (Elem -> IO a) -> IO a
withElem id f= do
  me <- elemById id
  case me of
     Nothing -> error ("withElem: not found"++ fromJSString id)
     Just e -> f e

atElem :: ElemID -> Perch -> Perch
atElem id f = Perch $ \ _ -> do
  me <- elemById id
  case me of
     Nothing -> error ("withElem: not found"++ fromJSString id)
     Just e -> build f e

data NeedForm= HasForm | HasElems  | NoElems deriving Show


type ElemID= JSString
newtype Widget a=  Widget{ norender :: TransIO a} deriving(Monad,MonadIO,Alternative,MonadPlus)

instance   Functor Widget where
  fmap f mx=   Widget. Transient $ fmap (fmap f) . runTrans $ norender mx

--instance Alternative Widget where
--   empty= Widget empty
--   (Widget x) <|> (Widget y)=  Widget $ Transient $ do
--            rx <- runTrans x
--            ry <- runTrans y
--            return $ rx <|> ry


instance Applicative Widget where
  pure= return

  Widget (Transient x) <*> Widget (Transient y) = Widget . Transient $ do
      mx <- x --  !> "mx"
      my <- y --  !> "my"
      return $ mx <*> my



instance Monoid a => Monoid (Widget a) where
  mempty= return mempty
  mappend x y= (<>) <$> x <*> y

instance AdditionalOperators Widget where

    (<**)  x y= Widget $  norender x <**  norender y

    (<***) x y= Widget $  norender x <*** norender y

    (**>)  x y= Widget $  norender x **>  norender y



runView :: Widget a -> StateIO (Maybe a)
runView  = runTrans . norender

-- | It is a callback in the view monad. The rendering of the second parameter substitutes the rendering
-- of the first paramenter when the latter validates without afecting the rendering of other widgets.
-- This allow the simultaneous execution of different dynamic behaviours in different page locations
-- at the same page.
wcallback
  ::  Widget a -> (a ->Widget b) -> Widget b

wcallback x f= Widget $ Transient $ do
   nid <-  genNewId
   runView $ do
             r <-  at nid Insert x
             at nid Insert $ f r






{-
instance Monoid view => MonadTrans (View view) where
  lift f = Transient $  (lift  f) >>= \x ->  returnFormElm mempty $ Just x
-}

type Name= JSString
type Type= JSString
type Value= JSString
type Checked= Bool
type OnClick1= Maybe JSString


-- | Minimal interface for defining the basic form and link elements. The core of MFlow is agnostic
-- about the rendering package used. Every formatting (either HTML or not) used with MFlow must have an
-- instance of this class.
-- See "MFlow.Forms.Blaze.Html for the instance for blaze-html" "MFlow.Forms.XHtml" for the instance
-- for @Text.XHtml@ and MFlow.Forms.HSP for the instance for Haskell Server Pages.
class (Monoid view,Typeable view)   => FormInput view where
    fromStr :: JSString -> view
    fromStrNoEncode :: String -> view
    ftag :: JSString -> view  -> view
    inred   :: view -> view
    flink ::  JSString -> view -> view
    flink1:: JSString -> view
    flink1 verb = flink verb (fromStr verb)
    finput :: Name -> Type -> Value -> Checked -> OnClick1 -> view
    ftextarea :: JSString -> JSString -> view
    fselect :: JSString -> view -> view
    foption :: JSString -> view -> Bool -> view
    foption1 :: JSString -> Bool -> view
    foption1   val msel= foption val (fromStr val) msel
    formAction  :: JSString -> JSString -> view -> view
    attrs :: view -> Attribs -> view

type Attribs= [(JSString, JSString)]


data ParamResult v a= NoParam | NotValidated String v | Validated a deriving (Read, Show)

valToMaybe (Validated x)= Just x
valToMaybe _= Nothing

isValidated (Validated x)= True
isValidated _= False

fromValidated (Validated x)= x
fromValidated NoParam= error "fromValidated : NoParam"
fromValidated (NotValidated s err)= error $ "fromValidated: NotValidated "++ s

getParam1 :: ( Typeable a, Read a, Show a)
          => JSString ->  StateIO (ParamResult Perch a)
getParam1 par = do
   me <- elemById par                        -- !> ("looking for " ++ show par)
   case me of
     Nothing -> return  NoParam
     Just e ->  do
       v <- getValue e                       -- !!> ("exist" ++ show par)
       readParam v                           -- !!> ("getParam for "++ show v)


type Params= Attribs



readParam :: (Typeable a, Read a, Show a)=> Maybe String -> StateIO (ParamResult Perch a)
readParam Nothing = return NoParam
readParam (Just x1) = r
 where
 r= maybeRead x1

 getType ::  m (ParamResult v a) -> a
 getType= undefined
 x= getType r

 maybeRead str= do
   let typeofx = typeOf x
   if typeofx == typeOf  ( undefined :: String)   then
           return . Validated $ unsafeCoerce str            -- !!> ("maybread string " ++ str)
    else case reads $ str  of          --            -- !!> ("read " ++ str) of
              [(x,"")] ->  return $ Validated x            -- !!> ("readsprec" ++ show x)
              _ -> do
                   let err= inred . fromStr $ toJSString $ "can't read \"" ++ str ++ "\" as type " ++  show (typeOf x)
                   return $ NotValidated str err

-- | Validates a form or widget result against a validating procedure
--
-- @getOdd= getInt Nothing `validate` (\x -> return $ if mod x 2==0 then  Nothing else Just "only odd numbers, please")@
validate
  :: Widget a
     -> (a -> StateIO  (Maybe Perch))
     -> Widget a
validate  w val=  do
   idn <- Widget $ Transient $ Just <$> genNewId
   rawHtml $ span ! id idn $ noHtml
   x <-  w
   Widget $ Transient $ do
          me <- val x
          case me of
             Just str -> do
                  liftIO $ withElem idn $ build $ clear >> inred  str
                  return Nothing
             Nothing  -> do
                  liftIO $ withElem idn $ build clear
                  return $ Just x




-- | Generate a new string. Useful for creating tag identifiers and other attributes.
--
-- if the page is refreshed, the identifiers generated are the same.

#ifdef ghcjs_HOST_OS
genNewId ::  StateIO  JSString
genNewId=  do
      Prefix pre <- getData `onNothing` return (Prefix "")
      n <- genId
      return  $ pre <> (toJSString $ {-'p':  (show $ Prelude.length log)++-} ('n':show n))
--      return  $  (toJSString $ {-'p':  (show $ Prelude.length log)++-} ('n':show n))

getPrev ::  StateIO  JSString
getPrev= do
      n' <- getPrevId
      let n= n'-1
      Prefix pre <- getData `onNothing` return (Prefix "")

      return  $ pre <> (toJSString $ {-'p':  (show $ Prelude.length log)++-} ('n':show n))
#else
genNewId ::  StateIO  JSString
genNewId= return $ pack ""

getPrev ::  StateIO  JSString
getPrev= return $ pack ""
#endif

--addPrefix= Transient $ do
--   n <- genId
--   Prefix s <- getData `onNothing` return ( Prefix "")
--   setData $ Prefix (toJSString( 's': show n)<> s)
--   return $ Just ()


-- | get the next ideitifier that will be created by genNewId
getNextId :: MonadState EventF  m  =>  m JSString
getNextId=  do
      n <- gets mfSequence

      return $ toJSString $ 'p':show n


-- | Display a text box and return a non empty String
getString  ::  Maybe String -> Widget String
getString = getTextBox
--     `validate`
--     \s -> if Prelude.null s then return (Just $ fromStr "")
--                    else return Nothing

inputString  :: Maybe String -> Widget String
inputString= getString

-- | Display a text box and return an Integer (if the value entered is not an Integer, fails the validation)
getInteger :: Maybe Integer -> Widget  Integer
getInteger =  getTextBox

inputInteger ::  Maybe Integer -> Widget  Integer
inputInteger= getInteger

-- | Display a text box and return a Int (if the value entered is not an Int, fails the validation)
getInt :: Maybe Int -> Widget Int
getInt =  getTextBox

inputInt :: Maybe Int -> Widget Int
inputInt =  getInt

inputFloat :: Maybe Float -> Widget Float
inputFloat =  getTextBox

inputDouble :: Maybe Double -> Widget Double
inputDouble =  getTextBox

-- | Display a password box
getPassword :: Widget String
getPassword = getParam Nothing "password" Nothing

inputPassword ::   Widget String
inputPassword= getPassword

newtype Radio a= Radio a deriving Monoid



-- | Implement a radio button
-- the parameter is the name of the radio group
setRadio :: (Typeable a, Eq a, Show a) =>
            a ->  Widget  (Radio a)
setRadio v = Widget $ Transient $ do
  RadioId n <- getData `onNothing` error "setRadio out of getRadio"
  id <- genNewId
  st <- get
--  setData HasElems       -- only for MFlow
  me <- liftIO $ elemById id
  checked <-  case me  of
       Nothing -> return ""
       Just e  -> liftIO $ getProp e "checked"
  let strs= if  checked=="true" then Just v else Nothing
--  let mn= if null strs then False else True
      ret= fmap  Radio  strs
      str = if typeOf v == typeOf(undefined :: String)
                   then unsafeCoerce v else show v

  addSData
      ( finput id "radio" (toJSString str) ( isJust strs ) Nothing `attrs` [("name",n)] :: Perch)

  return ret

setRadioActive :: (Typeable a, Eq a, Show a) =>
                    a -> Widget (Radio a)
setRadioActive rs = setRadio rs `raiseEvent` OnClick

data RadioId= RadioId JSString deriving Typeable

-- | encloses a set of Radio boxes. Return the option selected
getRadio
  :: Monoid a => [Widget (Radio a)] -> Widget a
getRadio ws = Widget $ Transient $ do
   id <- genNewId
   setData $ RadioId id
   fs <- mapM runView  ws
   let mx = mconcat fs
   delData $ RadioId id
   return $ fmap (\(Radio r) -> r) mx


data CheckBoxes a= CheckBoxes [a] deriving Show

instance Monoid (CheckBoxes a) where
  mappend (CheckBoxes xs) (CheckBoxes ys)= CheckBoxes $ xs ++ ys
  mempty= CheckBoxes []


-- | Display a text box and return the value entered if it is readable( Otherwise, fail the validation)
setCheckBox :: (Typeable a , Show a) =>
                Bool -> a -> Widget  (CheckBoxes a)
setCheckBox checked' v= Widget . Transient $ do
  n  <- genNewId
  st <- get
--  setData HasElems
  me <- liftIO $ elemById n


  let showv= toJSString (if typeOf v == typeOf (undefined :: String)
                             then unsafeCoerce v
                             else show v)

  addSData $  ( finput n "checkbox" showv  checked' Nothing :: Perch)

  case me of
       Nothing -> return Nothing
       Just e -> do
            checked <- liftIO $ getProp e "checked"
            return . Just . CheckBoxes $ if  checked=="true"  then [v] else []


getCheckBoxes ::  Show a => Widget  (CheckBoxes a) ->  Widget  [a]
getCheckBoxes w = Widget $ Transient $ do
   mrs <- runView w
   case mrs of
     Nothing -> return Nothing
     Just(CheckBoxes rs ) ->   return $ Just rs

whidden :: (Read a, Show a, Typeable a) => a -> Widget a
whidden x= res where
 res= Widget . Transient $ do
      n <- genNewId
      let showx= case cast x of
                  Just x' -> x'
                  Nothing -> show x
      r <- getParam1 n  `asTypeOf` typef res
      addSData (finput n "hidden" (toJSString showx) False Nothing :: Perch)
      return (valToMaybe r)
      where
      typef :: Widget a -> StateIO (ParamResult Perch a)
      typef = undefined




getTextBox
  :: (Typeable a,
      Show a,
      Read a) =>
     Maybe a ->  Widget a
getTextBox ms  = getParam Nothing "text" ms


getParam
  :: (Typeable a,
      Show a,
      Read a) =>
      Maybe JSString -> JSString -> Maybe a -> Widget  a
getParam look type1 mvalue= Widget . Transient $ getParamS look type1 mvalue

getParamS look type1 mvalue= do
    tolook <- case look of
       Nothing  -> genNewId
       Just n -> return n

    let nvalue x =  case x of
          Nothing -> mempty
          Just v  ->
              if (typeOf v== typeOf (undefined :: String)) then  pack (unsafeCoerce v)
              else if typeOf v== typeOf (undefined :: JSString) then unsafeCoerce v
              else toJSString $ show v             -- !!> "show"

    setData HasElems
    r <- getParam1 tolook

    case r of
       Validated x        -> do addSData (finput tolook type1 (nvalue $ Just x) False Nothing :: Perch) ; return $ Just x            -- !!> "validated"
       NotValidated s err -> do addSData (finput tolook type1  (toJSString s) False Nothing <> err :: Perch); return Nothing
       NoParam            -> do setData WasParallel;addSData (finput tolook type1 (nvalue mvalue) False Nothing :: Perch); return  Nothing




-- | Display a multiline text box and return its content
getMultilineText :: JSString
                 -> Widget String
getMultilineText nvalue =  res where
 res= Widget. Transient $ do
    tolook <- genNewId
    r <- getParam1 tolook  `asTypeOf` typef res
    case r of
       Validated x        -> do addSData (ftextarea tolook  $ toJSString x :: Perch); return $ Just x
       NotValidated s err -> do addSData (ftextarea tolook   (toJSString s) :: Perch); return  Nothing
       NoParam            -> do setData WasParallel;addSData (ftextarea tolook  nvalue :: Perch); return  Nothing
    where
    typef :: Widget String -> StateIO (ParamResult Perch String)
    typef = undefined

-- | A synonim of getMultilineText
textArea ::  JSString ->Widget String
textArea= getMultilineText



getBool :: Bool -> String -> String -> Widget Bool
getBool mv truestr falsestr= do
   r <- getSelect $   setOption truestr (fromStr $ toJSString truestr)  <! (if mv then [("selected","true")] else [])
                  <|> setOption falsestr(fromStr $ toJSString falsestr) <! if not mv then [("selected","true")] else []
   if  r == truestr  then return True else return False



-- | Display a dropdown box with the options in the first parameter is optionally selected
-- . It returns the selected option.
getSelect :: (Typeable a, Read a,Show a) =>
      Widget (MFOption a) ->  Widget  a
getSelect opts = res where
  res= Widget . Transient $ do
    tolook <- genNewId
    st <- get
--    setData HasElems
    r <- getParam1 tolook `asTypeOf` typef res
--    setData $ fmap MFOption $ valToMaybe r
    runView $ fselect tolook <<< opts
--
    return $ valToMaybe r

    where
    typef :: Widget a -> StateIO (ParamResult Perch a)
    typef = undefined


newtype MFOption a = MFOption a deriving (Typeable, Monoid)



-- | Set the option for getSelect. Options are concatenated with `<|>`
setOption
  :: (Show a, Eq a, Typeable a) =>
     a -> Perch -> Widget (MFOption a)
setOption n v = setOption1 n v False


-- | Set the selected option for getSelect. Options are concatenated with `<|>`
setSelectedOption
  :: (Show a, Eq a, Typeable a) =>
     a -> Perch -> Widget (MFOption a)
setSelectedOption n v= setOption1 n v True


setOption1 :: (Typeable a, Eq a, Show a) =>
      a -> Perch -> Bool ->  Widget  (MFOption a)
setOption1 nam  val check= Widget . Transient $ do
    let n = if typeOf nam == typeOf(undefined :: String)
                   then unsafeCoerce nam
                   else show nam

    addSData (foption (toJSString n) val check)

    return  Nothing -- (Just $ MFOption nam)


wlabel:: Perch -> Widget a -> Widget a
wlabel str w = Widget . Transient $ do
   id <- getNextId
   runView $ (ftag "label" str `attrs` [("for",id)] :: Perch) ++> w



-- passive reset button.
resetButton :: JSString -> Widget ()
resetButton label= Widget . Transient $ do
   addSData  (finput  "reset" "reset" label False Nothing :: Perch)
   return $ Just ()

inputReset :: JSString -> Widget ()
inputReset= resetButton

-- passive submit button. Submit a form, but it is not trigger any event.
-- Unless you attach it with `raiseEvent`
submitButton ::  (Read a, Show a, Typeable a) => a -> Widget a
submitButton label=  getParam Nothing "submit" $ Just label


inputSubmit ::  (Read a, Show a, Typeable a) => a -> Widget a
inputSubmit= submitButton

-- | active button. When clicked, return the first parameter
wbutton :: a -> JSString -> Widget a
wbutton x label= Widget $ Transient $ do
     idn <- genNewId
     runView $ do
        input  ! atr "type" "submit" ! id   idn ! atr "value" label `pass` OnClick
        return x
      `continuePerch`  idn


-- | when creating a complex widget with many tags, this call indentifies which tag will receive the attributes of the (!) operator.
continuePerch :: Widget a -> ElemID -> Widget a
continuePerch w eid=   c <<< w
      where
      c f =Perch $ \e' ->  do
         build f e'
         elemid eid

      elemid id= elemById id >>=  return . fromJust

--      child  e = do
--             jsval <- firstChild e
--             fromJSValUnchecked jsval


-- | Present a link. Return the first parameter when clicked
wlink :: (Show a, Typeable a) => a -> Perch -> Widget a
wlink x v=  do
    (a ! href ( toJSString $ "#/"++ show1 x)   $ v)  `pass` OnClick

    return x            -- !!> "PASS"

   where
   show1 x | typeOf x== typeOf (undefined :: String) = unsafeCoerce x
           | otherwise= show x



-- | show something enclosed in the <pre> tag, so ASCII formatting chars are honored
wprint :: ToElem a => a -> Widget ()
wprint = wraw . pre

-- | Enclose Widgets within some formating.
-- @view@ is intended to be instantiated to a particular format
--
-- NOTE: It has a infix priority : @infixr 5@ less than the one of @++>@ and @<++@ of the operators, so use parentheses when appropriate,
-- unless the we want to enclose all the widgets in the right side.
-- Most of the type errors in the DSL are due to the low priority of this operator.
--

(<<<) :: (Perch -> Perch)
         -> Widget a
         -> Widget a
(<<<) v form= Widget . Transient $ do
  rest <- getData `onNothing` return noHtml
  delData rest
  mx <- runView form
  f <- getData `onNothing` return noHtml
  setData $ rest <> v f
  return mx


infixr 5 <<<

-- | A parameter application with lower priority than ($) and direct function application
(<<) :: (Perch -> Perch) -> Perch -> Perch
(<<) tag content= tag $ toElem content

infixr 7 <<


-- | Append formatting code to a widget
--
-- @ getString "hi" <++ H1 << "hi there"@
--
-- It has a infix prority: @infixr 6@ higuer that '<<<' and most other operators
(<++) :: Widget a
      -> Perch
      -> Widget a
(<++) form v= Widget . Transient $ do
              mx <-  runView  form
              addSData v
              return mx

infixr 6  ++>
infixr 6 <++
-- | Prepend formatting code to a widget
--
-- @bold << "enter name" ++> getString Nothing @
--
-- It has a infix prority: @infixr 6@ higher that '<<<' and most other operators
(++>) :: Perch -> Widget a -> Widget a
html ++> w =
  Widget . Transient $ do
      addSData html
      runView w




-- | Add attributes to the topmost tag of a widget

--  it has a fixity @infix 8@
infixl 8 <!
widget <! attribs=  Widget . Transient $ do
      rest <- getData `onNothing` return mempty
      delData rest
      mx <- runView widget
      fs <- getData `onNothing` return mempty
      setData  $ rest <> (fs `attrs` attribs :: Perch)
      return mx


instance  Attributable (Widget a) where
 (!) widget atrib = Widget $ Transient $ do   -- widget <! [atrib]
              rest <- getData `onNothing` return (mempty:: Perch)
              delData rest
              mx <- runView widget
              fs <- getData `onNothing` return (mempty :: Perch)
              setData  $ do rest ; (child $ mspan fs) ! atrib :: Perch
              return mx
     where
     child render = Perch $ \e -> do
             e'    <- build render e
             jsval <- firstChild e'
             fromJSValUnchecked jsval

mspan cont=  Perch $ \e -> do
        n <- liftIO $ getName e
--        alert $ toJSString $ show n
        if n == Just "EVENT"
           then build cont e
           else build (nelem "event" `child` cont) e

-- | Empty widget that does not validate. May be used as \"empty boxes\" inside larger widgets.
--
-- It returns a non valid value.
noWidget  :: Widget a
noWidget= Control.Applicative.empty

-- | Render raw view formatting. It is useful for displaying information.
wraw ::  Perch -> Widget ()
wraw x= Widget $ addSData x >> return () -- x ++> return ()

-- |  wraw synonym
rawHtml= wraw

-- | True if the widget has no valid input
isEmpty :: Widget a -> Widget Bool
isEmpty w= Widget $ Transient $ do
  mv <- runView w
  return $ Just $ isNothing mv


-------------------------
instance   FormInput Perch  where
    fromStr = toElem
    fromStrNoEncode  = toElem
    ftag n v =  nelem n `child` v

    attrs tag  [] = tag
    attrs tag (nv:attribs) = attrs (attr tag nv) attribs

    inred msg=  ftag "b" msg `attrs` [("style","color:red")]

    finput n t v f c=
       let
        tag= ftag "input" mempty `attrs` [("type",  t), ("id",  n), ("value",  v)]
        tag1= if f then tag `attrs` [("checked", "")] else tag
       in case c of Just s -> tag1 `attrs` [("onclick", s)] ; _ -> tag1

    ftextarea nam text=
        ftag "textarea" mempty `attrs` [("id",  nam)] `child` text


    fselect nam list = ftag "select" mempty `attrs` [("id", nam)] `child` list
    foption  name v msel=
      let tag=  ftag "option" mempty `attrs` [("value", name)]  `child`  v
      in if msel then tag `attrs` [("selected", "")] else tag


    formAction action method1 form = ftag "form" mempty `attrs` [("acceptCharset", "UTF-8")
                                                         ,( "action", action)
                                                         ,("method",  method1)]
                                                         `child` form


    flink  v str = ftag "a" mempty `attrs` [("href",  v)] `child` str


---------------------------
data EvData =  NoData | Click Int (Int, Int) | Mouse (Int, Int) | MouseOut | Key Int deriving (Show,Eq,Typeable)




resetEventData :: Widget ()
resetEventData= Widget . Transient $ do
    setData $ EventData "Onload" $ toDyn NoData
    return $ Just ()            -- !!> "RESETEVENTDATA"


getEventData ::  Widget EventData
getEventData =  Widget getSData <|> return  (EventData "Onload" $ toDyn NoData) -- (error "getEventData: event type not expected")

setEventData ::   EventData -> Widget ()
setEventData =  Widget . setData


class IsEvent a where
   eventName :: a -> JSString
   buildHandler :: Elem -> a  ->(EventData -> IO()) -> IO()



data BrowserEvent= OnLoad | OnUnload | OnChange | OnFocus | OnMouseMove | OnMouseOver |
 OnMouseOut | OnClick | OnDblClick | OnMouseDown | OnMouseUp | OnBlur |
 OnKeyPress | OnKeyUp | OnKeyDown deriving Show

data EventData= EventData{ evName :: JSString, evData :: Dynamic} deriving (Show,Typeable)

--data OnLoad= OnLoad
instance  IsEvent  BrowserEvent  where
--  data EData _= EventData{ evName :: JSString, evData :: EvData} deriving (Show,Typeable)
  eventName e =
#ifdef ghcjs_HOST_OS
    JS.toLower $ JS.drop 2 (toJSString $ show e) -- const "load"
#else
    ""
#endif
  buildHandler elem e io =
    case e of
     OnLoad -> do
      cb <- syncCallback1 ContinueAsync (const $ setDat elem (io
                                           (EventData (eventName e) $ toDyn NoData)) )
      js_addEventListener elem (eventName e) cb

--data OnUnload = OnUnLoad
--instance  IsEvent  OnUnload   where
--  eventName= const "unload"
--  buildHandler elem e io = do
     OnUnload -> do
      cb <- syncCallback1 ContinueAsync (const $ setDat elem  $ io
                                           (EventData (eventName e) $ toDyn NoData) )
      js_addEventListener elem (eventName e) cb
--data OnChange= OnChange
--instance  IsEvent  OnChange   where
--  eventName= const "onchange"
--  buildHandler elem e io = do
     OnChange -> do
      cb <- syncCallback1 ContinueAsync (const $ setDat elem $ io
                                           (EventData (eventName e) $ toDyn NoData) )
      js_addEventListener elem (eventName e) cb

--data OnFocus= OnFocus
--instance  IsEvent  OnFocus   where
--  eventName= const "focus"
--  buildHandler elem e io = do
     OnFocus -> do
      cb <- syncCallback1 ContinueAsync (const $ setDat elem $ io
                                           (EventData (eventName e) $ toDyn NoData) )
      js_addEventListener elem (eventName e) cb

--data OnBlur= OnBlur
--instance  IsEvent  OnBlur   where
--  eventName= const "blur"
--  buildHandler elem e io = do
     OnBlur -> do
       cb <- syncCallback1 ContinueAsync (const $ setDat elem $ io
                                           (EventData (eventName e)$ toDyn NoData) )
       js_addEventListener elem (eventName e) cb

--data OnMouseMove= OnMouseMove Int Int
--instance  IsEvent  OnMouseMove  where
--  eventName= const "mousemove"
--  buildHandler elem e io= do
     OnMouseMove -> do
       cb <- syncCallback1 ContinueAsync
               (\r -> do
                 (x,y) <-fromJSValUnchecked r
                 stopPropagation r
                 setDat elem $ io $  EventData (eventName e) $  toDyn $ Mouse(x,y))
       js_addEventListener elem (eventName e) cb

--data OnMouseOver= OnMouseOver
--instance  IsEvent  OnMouseOver  where
--  eventName= const "mouseover"
--  buildHandler elem e io= do
     OnMouseOver -> do
       cb <- syncCallback1 ContinueAsync
                (\r -> do
                 (x,y) <-fromJSValUnchecked r
                 stopPropagation r
                 setDat elem $ io $ EventData (nevent e) $ toDyn $  Mouse(x,y))
       js_addEventListener elem (eventName e) cb

--data OnMouseOut= OnMouseOut
--instance  IsEvent  OnMouseOut   where
--  eventName= const "mouseout"
--  buildHandler elem e io = do
     OnMouseOut -> do
      cb <- syncCallback1 ContinueAsync (const $ setDat elem $ io
                                           (EventData (nevent e) $ toDyn $  NoData) )
      js_addEventListener elem (eventName e) cb

--data OnClick= OnClick
--
--instance  IsEvent  OnClick      where
--  eventName= const "click"
--  buildHandler elem e io= do
     OnClick -> do
      cb <- syncCallback1 ContinueAsync  $ \r -> do
          (i,x,y)<- fromJSValUnchecked r
          stopPropagation r
          setDat elem $ io $   EventData (nevent e) $ toDyn $  Click i (x,y)
      js_addEventListener elem (eventName e) cb

--data OnDblClick= OnDblClick
--instance  IsEvent  OnDblClick   where
--  eventName= const "dblclick"
--  buildHandler elem e io= do
     OnDblClick -> do
      cb <- syncCallback1 ContinueAsync  $ \r -> do
          (i,x,y)<- fromJSValUnchecked r
          stopPropagation r
          setDat elem $ io $   EventData (nevent e) $ toDyn $  Click i (x,y)
      js_addEventListener elem (eventName e) cb

--
--data OnMouseDown= OnMouseDown
--instance  IsEvent  OnMouseDown  where
--  eventName= const "mousedowm"
--  buildHandler elem e io= do
     OnMouseDown -> do
      cb <- syncCallback1 ContinueAsync $ \r -> do
          (i,x,y)<- fromJSValUnchecked r
          stopPropagation r
          setDat elem  $ io $   EventData (nevent e) $ toDyn $  Click i (x,y)
      js_addEventListener elem (eventName e) cb


--data OnMouseUp= OnMouseUp
--instance  IsEvent  OnMouseUp    where
--  eventName= const "mouseup"
--  buildHandler elem e io= do
     OnMouseUp -> do
      cb <- syncCallback1 ContinueAsync $ \r -> do
          (i,x,y)<- fromJSValUnchecked r
          stopPropagation r
          setDat elem $ io $   EventData (nevent e) $ toDyn $  Click i (x,y)
      js_addEventListener elem (eventName e) cb


--data OnKeyPress= OnKeyPress
--instance  IsEvent  OnKeyPress  where
--  eventName= const "keypress"
--  buildHandler elem e io = do
     OnKeyPress -> do
      cb <- syncCallback1 ContinueAsync $ \r -> do
            i <-  fromJSValUnchecked r
            stopPropagation r
            setDat elem  $ io $  EventData (nevent e) $ toDyn $  Key i
      js_addEventListener elem (eventName e) cb

--data OnKeyUp= OnKeyUp
--instance  IsEvent OnKeyUp    where
--  eventName= const "keyup"
--  buildHandler elem e io = do
     OnKeyUp -> do
      cb <- syncCallback1 ContinueAsync $ \r -> do
            i <-  fromJSValUnchecked r
            stopPropagation r
            setDat elem  $ io $ EventData (nevent e) $ toDyn $  Key i
      js_addEventListener elem (eventName e) cb

--data OnKeyDown= OnKeyDown
--instance  IsEvent  OnKeyDown   where
--  eventName= const "keydown"
--  buildHandler elem e io = do
     OnKeyDown -> do
      cb <- syncCallback1 ContinueAsync $ \r -> do
            i <-  fromJSValUnchecked r
            stopPropagation r
            setDat elem $ io $  EventData (nevent e) $ toDyn $ Key i
      js_addEventListener elem (eventName e) cb

   where


   nevent =  eventName

   setDat ::  Elem -> IO()  -> IO ()
   setDat elem action  = do
         action            -- !!> "begin action"
         return ()            -- !!> "end action"


addSData :: (MonadState EventF m,Typeable a ,Monoid a) => a -> m ()
addSData y=  do
  x <- getData `onNothing` return  mempty
  setData (x <> y)


newtype IdLine= IdLine JSString deriving(Read,Show)
data Repeat= Repeat | RepH JSString deriving (Eq, Read, Show)





-- | triggers the event that happens in a widget. The effects are the following:
--
-- 1)The event reexecutes the monadic sentence where the widget is, (with no re-rendering)
--
-- 2) with the result of this reevaluaution of 1), the rest of the monadic computation is executed
--
-- 3) update the DOM tree with the rendering generated by the reevaluation of 2).
--
-- As usual, If one step of the monadic computation return `empty` (`stop`), the reevaluation finish
-- So the effect of an event can be restricted as much as you may need.
--
-- The part of the monadic expression that is before the event is not evaluated and his rendering is untouched.
-- (but, at any moment, you can choose the element to be updated in the page using `at`)

-- to store the identifier number of the form elements to be set for that event

newtype IDNUM = IDNUM Int deriving Show

raiseEvent ::  IsEvent event  => Widget a -> event -> Widget a
#ifdef ghcjs_HOST_OS
raiseEvent w event = Widget . Transient $ do
       cont <- get
       let iohandler :: EventData -> IO ()
           iohandler eventdata =do
                runStateT (setData eventdata >> runCont' cont) cont   -- !!> "runCont INIT"
                return ()                                           -- !!> "runCont finished"
       runView $ addEvent event iohandler <<< w
--   return r
   where
   runCont' cont= do

     mn <- getData

     when (isJust mn) $ let IDNUM n = fromJust mn in modify $  \s -> s{mfSequence=  n}

     setData Repeat                -- !> "INITCLOSURE"
     mr <- runClosure cont
     return ()
     case mr of
         Nothing -> return Nothing
         Just r -> runContinuation cont r -- !> "continue"

       -- create an element and add any event handler to it.
   addEvent :: IsEvent a =>  a -> (EventData -> IO()) -> Perch -> Perch
   addEvent event iohandler be= Perch $ \e -> do
            e' <- build (mspan be) e
            buildHandler e' event iohandler
            return e


#else
raiseEvent w _ = w
#endif

#ifdef ghcjs_HOST_OS
foreign import javascript unsafe
  "$1.stopPropagation()"
  stopPropagation :: JSVal -> IO ()
#else
stopPropagation= undefined
#endif



-- | A shorter synonym for `raiseEvent`
fire ::   IsEvent event => Widget a -> event -> Widget a
fire = raiseEvent

-- | A shorter and smoother synonym for `raiseEvent`
wake ::   IsEvent event => Widget a -> event -> Widget a
wake = raiseEvent


-- | pass trough only if the event is fired in this DOM element.
-- Otherwise, if the code is executing from a previous event, the computation will stop
pass :: IsEvent event => Perch -> event -> Widget EventData
pass v event= do
        resetEventData
        wraw v `wake` event              -- !!> "WAKE"
        e@(EventData typ _) <- getEventData              -- !!> "GETEVENTDATA"
        continueIf (eventName event== typ) e             -- !!> show(eventName event== typ)

-- | return empty and the monadic computation stop if the condition is false.
-- If true, return the second parameter.
continueIf :: Bool -> a -> Widget a
continueIf b x  = guard b >> return x





runWidgetId' ::  Widget b -> ElemID  -> TransIO b
runWidgetId' ac id1=  Transient  runWidget1
 where
 runWidget1 = do

   me <- liftIO $ elemById id1                       --   !> ("RUNWIDGETID", id1)
   case me of
     Just e ->  do

          r <- runView $ runWidget' ac e       -- !!> show ("found",id1)
          return r             -- !!> show ( "END RUNWIDGETID", id1)


     Nothing -> -- runTrans ac !!> ( "ID NOT FOUND " ++ show id) -- runTrans ac
         do
            body <- liftIO  getBody                   --  !> ( "ID NOT FOUND " ++ show id1)
            liftIO $ build (span ! id id1 $ noHtml) body

            runWidget1
--            runTrans $ runWidget'  (  id1 <<<  ac) body



-- | run the widget as the content of a DOM element
-- the new rendering is added to the element
runWidget :: Widget b -> Elem  -> IO (Maybe b)
runWidget action e = do
     (mx, s) <- runTransient . norender $ runWidget' action e
     return mx


runWidget' :: Widget b -> Elem   -> Widget b
runWidget' action e  = Widget $ Transient $ do
--      liftIO $ clearChildren e    -- !> "clear 0"
      mx <- runView action                          -- !> "runVidget'"
      render <- getData `onNothing` (return  noHtml)

      liftIO $ build render e

      delData render
      return mx


-- | add a header in the <header> tag
addHeader :: Perch -> IO ()
addHeader format= do
    head <- getHead
    build format head
    return ()


-- | run the widget as the body of the HTML. It adds the rendering to the body of the document.
runBody :: Widget a -> IO (Maybe a)
runBody w= do
  body <- getBody
  runWidget  w body




-- #ifdef ghcjs_HOST_OS
---- | use this instead of `Transient.Move.runCloud'` when running in the browser
--runCloudIO :: Cloud a -> IO ()
--runCloudIO (Cloud mx)=  runTransient  mx >> return ()
-- #endif
--
--teleport= do
--    copyData $ Prefix ""
--    copyData $ IdLine ""
--    copyData $ Repeat
--    copyCounter
--    TL.teleport
--    where
--    copyCounter= do
--      r <- local $ gets mfSequence
--      onAll $ modify $ \s -> s{mfSequence= r}
--

-- | executes the computation and  add the effect of "hanging" the generated rendering from the one generated by the
-- previous `render` sentence, or from the body of the document, if there isn't any. If an event happens within
-- the `render` parameter, it deletes the rendering of all subsequent ones.
-- so that the sucessive sequence of `render` in the code will reconstruct them again.
-- However the rendering of elements combined with `<|>` or `<>` or `<*>`  are independent.
-- This allows for full dynamic and composable client-side Web apps.
render :: Widget a -> TransIO a
#ifdef ghcjs_HOST_OS
render  mx =  do
       id1 <- Transient $ do
                 me <- getData              -- !> "RENDER"
                 case me of
                     Just (IdLine id1) -> return $ Just id1
                     Nothing ->  Just <$> genNewId
       id2 <- Transient $ Just <$> genNewId

       n <- gets mfSequence
       setData $ IDNUM n

       setData $ IdLine id1
       runWidgetId' (mx' id2 <++ (span ! id id2 $ noHtml)) id1



  where
  mx' id2= Widget $ do
     r <- norender mx                                    --    !> "mx"
     addPrefix
     (setData $ IdLine id2)          --   !>  ("set",id2)

     do
           re <- getSData        -- succed if is the result of an event
           case re   of                                           --    !>  "event" of
             Repeat -> do
              me <- liftIO $ elemById id2
              case me of
                 Just e ->   (liftIO $ clearChildren e)          --      !> show ("clear1",id2)
                 Nothing -> return ()
--              setData $ RepH id2
              delData Repeat
              delData noHtml
             RepH  idx -> do
--               me <- liftIO $ elemById idx
--               case me of
--                 Just e ->   (liftIO $ clearChildren e)         --       !> show ("clear2",idx)
--                 Nothing -> return ()
               delData Repeat                                    --     !> "deldata Repeat"
           return r
        <|> return r                                             --     !> "NO DEL"


#else
render (Widget x)= x
#endif


    --   st@(EventF eff e x (fs) d n  r applic  ch rc bs)  <- get

    --   let cont=  EventF eff e x fs  d n  r applic  ch rc bs
    --   put cont
    --   liftIO $ print ("length1",Prelude.length fs)


-- | use this instead of `Transient.Base.option` when runing in the browser
option :: (Typeable b, Show b) =>  b -> String -> Widget b
option x v=  wlink x (toElem v)<++ " "


--foreign import javascript unsafe "document.body" getBody :: IO Elem



data UpdateMethod= Append | Prepend | Insert deriving Show



-- | Run the widget as the content of the element with the given id. The content can
-- be appended, prepended to the previous content or it can be the only content depending on the
-- update method.
at ::  JSString -> UpdateMethod -> Widget a -> Widget  a
at id method w= set <<< w
 where
 set :: Perch -> Perch
 set render = liftIO $ case method of
     Insert -> do
             forElems_ id $ clear >> render
             return ()
     Append -> do
             forElems_ id render
             return ()
     Prepend -> do
            forElems_ id $ Perch $ \e -> do
             jsval <- getChildren e
             es <- fromJSValUncheckedListOf jsval
             case es of
                       [] -> build render e >> return e
                       e':es -> do
                             span <- newElem "span"
                             addChildBefore span e e'
                             build render span
                             return e

#ifdef ghcjs_HOST_OS

foreign import javascript unsafe  "$1[$2].toString()" getProp :: Elem -> JSString -> IO JSString



foreign import javascript unsafe  "$1[$2] = $3" setProp :: Elem -> JSString -> JSString -> IO ()

foreign import javascript unsafe  "alert($1)" alert ::  JSString -> IO ()



foreign import javascript unsafe  "document.getElementById($1)" elemByIdDOM  :: JSString -> IO JSVal

foreign import javascript unsafe  "$1.value" getValueDOM :: Elem -> IO JSVal
foreign import javascript unsafe  "$1.tagName" getNameDOM :: Elem -> IO JSVal
#else
unpack= undefined
getProp :: Elem -> JSString -> IO JSString
getProp = undefined
setProp :: Elem -> JSString -> JSString -> IO ()
setProp = undefined
alert ::  JSString -> IO ()
alert= undefined
data Callback a= Callback a
data ContinueAsync=ContinueAsync
syncCallback1= undefined
fromJSValUnchecked= undefined
fromJSValUncheckedListOf= undefined
#endif

#ifdef ghcjs_HOST_OS
foreign import javascript unsafe
  "$1.addEventListener($2, $3,false);"
  js_addEventListener :: Elem -> JSString -> Callback (JSVal -> IO ()) -> IO ()
#else
js_addEventListener= undefined
#endif


#ifdef ghcjs_HOST_OS
foreign import javascript unsafe "document.head" getHead :: IO Elem
#else
getHead= undefined
#endif

#ifdef ghcjs_HOST_OS
foreign import javascript unsafe "$1.childNodes" getChildren :: Elem -> IO JSVal
foreign import javascript unsafe "$1.firstChild" firstChild :: Elem -> IO JSVal
foreign import javascript unsafe "$2.insertBefore($1, $3)" addChildBefore :: Elem -> Elem -> Elem -> IO()
#else

type JSVal = ()
getChildren :: Elem -> IO JSVal
getChildren= undefined
firstChild :: Elem -> IO JSVal
firstChild= undefined
addChildBefore :: Elem -> Elem -> Elem -> IO()
addChildBefore= undefined
#endif
