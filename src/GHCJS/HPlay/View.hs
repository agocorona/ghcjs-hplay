-----------------------------------------------------------------------------
--
-- Module      :  View
-- Copyright   :
-- License     :  BSD3
--
-- Maintainer  :  agocorona@gmail.com
-- Stability   :  experimental
-- Portability :
--
-- | The haste-hplayground framework.  <http://github.com/agocorona/hplayground>
--
-----------------------------------------------------------------------------

{-# LANGUAGE  FlexibleContexts, FlexibleInstances, ForeignFunctionInterface, OverloadedStrings
    ,DeriveDataTypeable, UndecidableInstances, ExistentialQuantification,TypeFamilies, StandaloneDeriving
    #-}
module GHCJS.HPlay.View(
Widget,
-- * re-exported
module Control.Applicative,

-- * widget combinators and modifiers

(<+>), (**>), (<**), validate
,firstOf, manyOf, allOf
,(<<<),(<<),(<++),(++>),(<!)
,wcallback

-- * basic widgets
,wprint
,getString,inputString, getInteger,inputInteger,
getInt, inputInt,inputFloat, inputDouble,getPassword,inputPassword,
setRadio,setRadioActive,getRadio
,setCheckBox, getCheckBoxes
,getTextBox, getMultilineText,textArea,getBool
,getSelect,setOption,setSelectedOption, wlabel,
resetButton,inputReset, submitButton,
inputSubmit, wbutton, wlink, noWidget, stop,wraw, isEmpty

-- * out of flow updates
,at, UpdateMethod(..)

-- * Session data storage
,getSessionData,getSData,setSessionData,setSData
,delSessionData,delSData

-- * reactive and events
,resetEventData,getEventData, getMEventData, setIOEventData, setEventData, IsEvent(..), EventData(..),EvData(..)
,raiseEvent, fire, wake, react, pass
,continueIf, wtimeout, Event(..)

-- * running it
,runWidget,runWidgetId, runBody, addHeader,static , dynamic

-- * Perch is reexported
,module GHCJS.Perch

-- * communications
,ajax,Method(..)

-- * low level and internals
,getNextId,genNewId, continuePerch
,getParam, getCont,runCont
,FormInput(..)
,View(..),FormElm(..),EventF(..), MFlowState(..)

)  where
import Control.Applicative
import Data.Monoid
import Control.Monad.State
import Control.Monad.IO.Class
import Data.Typeable

import Unsafe.Coerce
import Data.Maybe
import Unsafe.Coerce
import System.IO.Unsafe
import Control.Concurrent.MVar
import Data.IORef
import qualified Data.Map as M
import Control.Monad.Trans.Maybe
import Prelude hiding(id,span)
import GHCJS.Perch
import GHCJS.Types
import GHCJS.Marshal
import GHCJS.Foreign hiding (typeOf,getProp)
import Data.Dynamic

--import Debug.Trace
--(!>)= flip trace

foreign import javascript unsafe  "%1[%2].toString()" getPropDOM :: Elem -> JSString -> IO (JSRef JSString)

getProp :: MonadIO m => Elem -> JSString -> m String
getProp e s= liftIO $ do
  rs <- getPropDOM e s
  ms <- fromJSRef rs
  return $ case ms of
     Nothing -> ""
     Just s  -> fromJSString s

instance FromJSRef a => FromJSRef (Maybe a) where
 fromJSRef r =
   if eqRef r jsNull || eqRef r jsUndefined
    then return $ Just Nothing
    else do
        mx <- fromJSRef $ castRef r
        return $ case mx of
            Just x -> Just (Just x)
            Nothing -> Nothing

foreign import javascript unsafe  "document.getElementById($1)" elemByIdDOM  :: JSString -> IO (JSRef Elem)

foreign import javascript unsafe  "$1.value" getValueDOM :: Elem -> IO JSString

instance FromJSString (Maybe String) where
   fromJSString s= if isUndefined s then Nothing else Just $ fromJSString s

getValue :: MonadIO m => JSType a => Elem -> m (Maybe a)
getValue e= liftIO $  do
   s <- getValueDOM e
   return $ fromJSString s

elemById :: MonadIO m  => JSString -> m (Maybe Elem)
elemById id= liftIO $ do
   re <- elemByIdDOM id
   fromJSRef re

withElem :: ElemID -> (Elem -> IO a) -> IO a
withElem id f= do
  me <- elemById id
  case me of
     Nothing -> error ("withElem: not found"++ fromJSString id)
     Just e -> f e


data NeedForm= HasForm | HasElems  | NoElems deriving Show
type SData= ()

type ElemID= JSString

data EventF= forall b c.EventF (IO(Maybe b))  (b -> IO (Maybe c))

data MFlowState= MFlowState { mfPrefix :: String
                            , mfSequence :: Int
                            , needForm :: NeedForm
                            , process :: EventF
                            , fixed :: Bool
                            , lastEvent :: Dynamic
                            , mfData :: M.Map TypeRep SData}

type Widget a=  View Perch IO a
type WState view m = StateT MFlowState m
data FormElm view a = FormElm view (Maybe a)
newtype View v m a = View { runView :: WState v m (FormElm v a)}

mFlowState0= MFlowState "" 0 NoElems  (EventF (return Nothing)
                        (const (return Nothing)) ) False
                        (toDyn $toDyn $ EventData "OnLoad" NoData)
                        M.empty

noid= error "noId error"

instance Functor (FormElm view ) where
  fmap f (FormElm form x)= FormElm form (fmap f x)

instance (Monoid view) => Monoid (FormElm view a) where
  mempty= FormElm mempty Nothing
  mappend (FormElm f1 x1) (FormElm f2 x2)= FormElm (f1 <> f2) (x1 <|> x2)

instance  (Monad m,Functor m) => Functor (View view m) where
  fmap f x= View $   fmap (fmap f) $ runView x


instance (Monoid view,Functor m, Monad m) => Applicative (View view m) where
  pure a  = View  .  return . FormElm mempty $ Just a
  View f <*> View g= View $
                   f >>= \(FormElm form1 k) ->
                   g >>= \(FormElm form2 x) ->
                   return $ FormElm (form1 `mappend` form2) (k <*> x)

instance (Monoid view, Functor m, Monad m) => Alternative (View view m) where
  empty= View $ return $ FormElm mempty Nothing
  View f <|> View g= View $ do
                   FormElm form1 x <- f
                   FormElm form2 y <- g
                   return $ FormElm (form1 <> form2) (x <|> y)



strip st x= View $ do
    st' <- get
    put st'{mfSequence= mfSequence st}
    FormElm f mx <- runView x
    put st'
    return $ FormElm mempty mx

setEventCont :: Widget a -> (a -> Widget b)  -> ElemID -> StateT MFlowState IO EventF
setEventCont x f  id= do
   st <- get
   let conf = process st
   case conf of
     EventF  _ fs  -> do
       let idx=  runWidgetId (strip st x) "noid"
       put st{process= EventF idx ( \x ->   runWidgetId ( f x) id `bind` unsafeCoerce fs)  }
   return conf
   where
   at id w= View $ do
      FormElm render mx <- (runView w)
      return $ FormElm  (set  render)  mx
      where
      set render= liftIO $ do
         me <- elemById id
         case me of
          Nothing -> return ()
          Just e ->  do
                     clearChildren e
                     build render e
                     return ()


resetEventCont cont= modify $ \s -> s {process= cont}

instance Monad (View Perch IO) where
    x >>= f = View $ do
       fix <- gets fixed
       id1 <- genNewId 
       contold <- setEventCont x  f   id1
       FormElm form1 mk <- runView x
       resetEventCont contold
       case mk of
         Just k  -> do
            FormElm form2 mk <- runView $ f k
            return $ FormElm (form1 <> maybeSpan fix id1 form2) mk
         Nothing ->
            return $ FormElm  (form1 <> maybeSpan fix id1 noHtml)  Nothing
       where
       maybeSpan True id1 form= form
       maybeSpan False id1 form= span ! id id1 $ form



    return = View .  return . FormElm  mempty . Just
    fail msg= View . return $ FormElm (inred $ fromStr $ toJSString msg) Nothing


-- | To produce updates, each line of html produced by a "do" sequence in the Widget monad is included
-- within a 'span' tag. When the line is reexecuted after a event, the span is updated with the new
-- rendering.
--
-- static tell to the rendering that this widget does not change, so the extra 'span' tag for each
-- line in the sequence and the rewriting is not necessary. Thus the size of the HTML and the
-- performance is improved.

static w= View $ do
   st <- get
   let was = fixed st
   put st{fixed=True}
   r <- runView $ w
   modify $ \st -> st{fixed= was}
   return r

-- override static locally to permit dynamic effects inside a static widget. It is useful
-- when a monadic Widget computation -which perform no changes in rendering- has a to do some update:
--
-- > launchMissiles= static $ do
-- >    t <- armLauncher
-- >    c <- fixTarget t
-- >    f <- fire c
-- >    dynamic $ displayUpdate t c f
-- >    return ()

dynamic w= View $ do
   st <- get
   let was = fixed st
   put st{fixed= False}
   r <- runView $ w
   modify $ \st -> st{fixed= was}
   return r

instance (FormInput v,Monad (View v m), Monad m, Functor m, Monoid a) => Monoid (View v m a) where
  mappend x y = mappend <$> x <*> y  -- beware that both operands must validate to generate a sum
  mempty= return mempty


-- | It is a callback in the view monad. The rendering of the second parameter substitutes the rendering
-- of the first paramenter when the latter validates without afecting the rendering of other widgets.
-- This allow the simultaneous execution of different dynamic behaviours in different page locations
-- at the same page.
wcallback
  ::  Widget a -> (a ->Widget b) -> Widget b

wcallback x f= View $ do
   nid <-  genNewId 
   FormElm form mx <- runView $ do
             r <-  at nid Insert x
             at nid Insert $ f r

   return $ FormElm ((GHCJS.Perch.span ! atr "id" nid $ noHtml) <> form) mx



identified id w= View $ do
     let span= nelem "span" `attr` ("id", id)
     FormElm f mx <- runView w
     return $ FormElm (span `child` f) mx


instance  (FormInput view,Monad m,Monad (View view m)) => MonadState (View view m) where
  type StateType (View view m)= MFlowState
  get = View $  get >>=  return . FormElm mempty . Just
  put st = View $  put st >>=  return . FormElm mempty . Just


instance (FormInput view,Monad (View view m),MonadIO m) => MonadIO (View view m) where
    liftIO io=   let x= liftIO io in x `seq` lift x


----- some combinators ----

-- | Join two widgets in the same page
-- the resulting widget, when `ask`ed with it, return a 2 tuple of their validation results
-- if both return Noting, the widget return @Nothing@ (invalid).
--
-- it has a low infix priority: @infixr 2@
--
--  > r <- ask  widget1 <+>  widget2
--  > case r of (Just x, Nothing) -> ..
(<+>) , mix ::  (Monad m, FormInput view)
      => View view m a
      -> View view m b
      -> View view m (Maybe a, Maybe b)
mix digest1 digest2= View $ do
  FormElm f1 mx' <- runView  digest1
  s1 <- get
  FormElm f2 my' <- runView  digest2
  s2 <- get
  return $ FormElm (f1 <> f2)
         $ case (mx',my') of
              (Nothing, Nothing) -> Nothing
              other              -> Just other

infixr 2 <+>

(<+>)  = mix



-- | The first elem result (even if it is not validated) is discarded, and the secod is returned
-- . This contrast with the applicative operator '*>' which fails the whole validation if
-- the validation of the first elem fails.
--
-- However, the first element is displayed, as happens in the case of '*>' .
--
-- Here @w\'s@ are widgets and @r\'s@ are returned values
--
--   @(w1 <* w2)@  will return @Just r1@ only if w1 and w2 are validated
--
--   @(w1 <** w2)@ will return @Just r1@ even if w2 is not validated
--
--  it has a low infix priority: @infixr 1@

(**>) :: (Functor m, Monad m, FormInput view)
      => View view m a -> View view m b -> View view m b

(**>) f g = View $ do
   FormElm form1 k <- runView $ valid f
   FormElm form2 x <- runView g
   return $ FormElm (form1 <> form2) (k *> x)



valid form= View $ do
   FormElm form mx <- runView form
   return $ FormElm form $ Just undefined

infixr 1  **>  ,  <**

-- | The second elem result (even if it is not validated) is discarded, and the first is returned
-- . This contrast with the applicative operator '*>' which fails the whole validation if
-- the validation of the second elem fails.
-- The second element is displayed however, as in the case of '<*'.
-- see the `<**` examples
--
--  it has a low infix priority: @infixr 1@
(<**) :: (Functor m, Monad m, FormInput view) =>
     View view m a -> View view m b -> View view m a
-- (<**) form1 form2 =  form1 <* valid form2
(<**) f g = View $ do
   FormElm form1 k <- runView f
   s1 <- get
   FormElm form2 x <- runView $ valid g
   s2 <- get


   return $ FormElm (form1 <> form2) (k <* x)





instance Monoid view => MonadTrans (View view) where
  lift f = View $  (lift  f) >>= \x ->  return $ FormElm mempty $ Just x


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
fromValidated NoParam= error $ "fromValidated : NoParam"
fromValidated (NotValidated s err)= error $ "fromValidated: NotValidated "++ s

getParam1 :: (MonadIO m, MonadState  m, Typeable a, Read a, FormInput v)
          => JSString ->  m (ParamResult v a)
getParam1 par = do
   me <- elemById par
   case me of
     Nothing -> return  NoParam
     Just e ->  do
       mv <- getValue e
       case mv of
         Nothing -> return NoParam
         Just v -> do
           readParam v


type Params= Attribs



readParam :: (Monad m, MonadState  m, Typeable a, Read a, FormInput v)
           => String -> m (ParamResult v a)
readParam x1 = r
 where
 r= maybeRead x1

 getType ::  m (ParamResult v a) -> a
 getType= undefined
 x= getType r
 maybeRead str= do
   let typeofx = typeOf x
   if typeofx == typeOf  ( undefined :: String)   then
           return . Validated $ unsafeCoerce str
    else case readsPrec 0 $ str of
              [(x,"")] ->  return $ Validated x
              _ -> do
                   let err= inred . fromStr $ toJSString $ "can't read \"" ++ str ++ "\" as type " ++  show (typeOf x)
                   return $ NotValidated str err

-- | Validates a form or widget result against a validating procedure
--
-- @getOdd= getInt Nothing `validate` (\x -> return $ if mod x 2==0 then  Nothing else Just "only odd numbers, please")@
validate
  :: Widget a
     -> (a -> WState Perch IO (Maybe Perch))
     -> Widget a
validate  w val= static $ do
   idn <- genNewId
   wraw $ span ! id idn $ noHtml
   x <-  w
   View $ do
          me <- val x
          case me of
             Just str -> do
                  liftIO $ withElem idn $ build $ clear >> inred  str
                  return $ FormElm mempty Nothing
             Nothing  -> do
                  liftIO $ withElem idn $ build $ clear
                  return $ FormElm mempty $ Just x




-- | Generate a new string. Useful for creating tag identifiers and other attributes.
--
-- if the page is refreshed, the identifiers generated are the same.
genNewId :: (StateType m ~ MFlowState, MonadState  m) =>  m JSString
genNewId=  do
      st <- get
      let n= mfSequence st
          prefseq=  mfPrefix st
      put $ st{mfSequence= n+1}

      return $  toJSString $ 'p':show n++prefseq


-- | get the next ideitifier that will be created by genNewId
getNextId :: (StateType m ~ MFlowState,MonadState  m) =>  m JSString
getNextId=  do
      st <- get
      let n= mfSequence st
          prefseq=  mfPrefix st
      return $ toJSString $ 'p':show n++prefseq


-- | Display a text box and return a non empty String
getString  :: (StateType (View view m) ~ MFlowState,FormInput view,Monad(View view m),MonadIO m) =>
     Maybe String -> View view m String
getString ms = getTextBox ms
--     `validate`
--     \s -> if Prelude.null s then return (Just $ fromStr "")
--                    else return Nothing

inputString  :: (StateType (View view m) ~ MFlowState,FormInput view,Monad(View view m),MonadIO m) =>
     Maybe String -> View view m String
inputString= getString

-- | Display a text box and return an Integer (if the value entered is not an Integer, fails the validation)
getInteger :: (StateType (View view m) ~ MFlowState,FormInput view,  MonadIO m) =>
     Maybe Integer -> View view m  Integer
getInteger =  getTextBox

inputInteger :: (StateType (View view m) ~ MFlowState,FormInput view,  MonadIO m) =>
     Maybe Integer -> View view m  Integer
inputInteger= getInteger

-- | Display a text box and return a Int (if the value entered is not an Int, fails the validation)
getInt :: (StateType (View view m) ~ MFlowState,FormInput view, MonadIO m) =>
     Maybe Int -> View view m Int
getInt =  getTextBox

inputInt :: (StateType (View view m) ~ MFlowState,FormInput view, MonadIO m) =>
     Maybe Int -> View view m Int
inputInt =  getInt

inputFloat :: (StateType (View view m) ~ MFlowState,FormInput view, MonadIO m) =>
     Maybe Float -> View view m Float
inputFloat =  getTextBox

inputDouble :: (StateType (View view m) ~ MFlowState,FormInput view, MonadIO m) =>
     Maybe Double -> View view m Double
inputDouble =  getTextBox

-- | Display a password box
getPassword :: (FormInput view,StateType (View view m) ~ MFlowState,
     MonadIO m) =>
     View view m String
getPassword = getParam Nothing "password" Nothing

inputPassword :: (StateType (View view m) ~ MFlowState,FormInput view,
     MonadIO m) =>
     View view m String
inputPassword= getPassword

newtype Radio a= Radio a

--instance Eq JSString where
--  x== y = eqjs x y

--foreign import javascript safe  "$1 == $2"
--     eqjs ::  a -> a -> Bool


-- | Implement a radio button
-- the parameter is the name of the radio group
setRadio :: (FormInput view,  MonadIO m,
             Typeable a, Eq a, Show a) =>
            a -> JSString -> View view m  (Radio a)
setRadio v n= View $ do
  id <- genNewId
  st <- get
  put st{needForm= HasElems}
  me <- liftIO $ elemById id
  checked <-  case me of
       Nothing -> return "" 
       Just e  -> getProp e "checked"  
  let strs= if  checked=="true" then Just v else Nothing
--  let mn= if null strs then False else True
      ret= fmap  Radio  strs
      str = if typeOf v == typeOf(undefined :: String)
                   then unsafeCoerce v else show v
  return $ FormElm
      ( finput id "radio" (toJSString str) ( isJust strs ) Nothing `attrs` [("name",n)])
      ret

setRadioActive :: (Typeable a, Eq a, Show a) =>
                    a -> JSString -> Widget (Radio a)
setRadioActive rs x= setRadio rs x `raiseEvent` OnClick

-- | encloses a set of Radio boxes. Return the option selected
getRadio
  :: (Monad (View view m), Monad m, Functor m, FormInput view) =>
     [JSString -> View view m (Radio a)] -> View view m a
getRadio ws = View $ do
   id <- genNewId
   fs <- mapM (\w -> runView (w id)) ws
   let FormElm render mx = mconcat fs
   return $ FormElm render $ fmap (\(Radio r) -> r) mx


data CheckBoxes a= CheckBoxes [a]

instance Monoid (CheckBoxes a) where
  mappend (CheckBoxes xs) (CheckBoxes ys)= CheckBoxes $ xs ++ ys
  mempty= CheckBoxes []


-- | Display a text box and return the value entered if it is readable( Otherwise, fail the validation)
setCheckBox :: (FormInput view,  MonadIO m, Typeable a , Show a) =>
                Bool -> a -> View view m  (CheckBoxes a)
setCheckBox checked' v= View $ do
  n  <- genNewId
  st <- get
  put st{needForm= HasElems}
  me <- liftIO $ elemById n
  checked <- case me of
       Nothing ->  return $ if checked' then "true" else ""
       Just e  -> getProp e "checked"
  let strs= if  checked=="true" then [v] else []
      ret= Just $ CheckBoxes  strs
      showv= toJSString $ case typeOf v== typeOf (undefined ::String) of
               True -> unsafeCoerce v
               False -> show v
  return $ FormElm
      ( finput n "checkbox" showv ( checked' ) Nothing)
      ret


getCheckBoxes :: (Monad m, FormInput view) =>  View view m  (CheckBoxes a) ->  View view m  [a]
getCheckBoxes w= View $ do
   FormElm render mcb <- runView w
   return $ FormElm render $ case mcb of
     Just(CheckBoxes rs) -> Just rs
     _                   -> Nothing



whidden :: (MonadIO m, FormInput v,Read a, Show a, Typeable a) => a -> View v m a
whidden x= res where
 res= View $ do
      n <- genNewId
      let showx= case cast x of
                  Just x' -> x'
                  Nothing -> show x
      r <- getParam1 n `asTypeOf` typef res
      return . FormElm (finput n "hidden" (toJSString showx) False Nothing) $ valToMaybe r
      where
      typef :: View v m a -> StateT MFlowState m (ParamResult v a)
      typef = undefined




getTextBox
  :: (FormInput view, StateType (View view m) ~ MFlowState,
      MonadIO  m,
      Typeable a,
      Show a,
      Read a) =>
     Maybe a ->  View view m a
getTextBox ms  = getParam Nothing "text" ms


getParam
  :: (FormInput view,StateType (View view m) ~ MFlowState,
      MonadIO m,
      Typeable a,
      Show a,
      Read a) =>
     Maybe JSString -> JSString -> Maybe a -> View view m  a
getParam look type1 mvalue= View $ getParamS look type1 mvalue

getParamS look type1 mvalue= do
    tolook <- case look of
       Nothing  -> genNewId
       Just n -> return n
    let nvalue x = toJSString $ case x of
           Nothing  -> ""
           Just v   ->
              case cast v of
                 Just v' -> v'
                 Nothing -> show v
    st <- get

    put st{needForm= HasElems}
    r <- getParam1 tolook
    case r of
       Validated x        -> return $ FormElm (finput tolook type1 (nvalue $ Just x) False Nothing) $ Just x
       NotValidated s err -> return $ FormElm (finput tolook type1 (toJSString s) False Nothing <> err) $ Nothing
       NoParam            -> return $ FormElm (finput tolook type1 (nvalue mvalue) False Nothing) $ Nothing




-- | Display a multiline text box and return its content
getMultilineText :: (FormInput view
                 ,  MonadIO m)
                   => JSString
                 ->  View view m String
getMultilineText nvalue =  res where
 res= View $ do
    tolook <- genNewId
    r <- getParam1 tolook  `asTypeOf` typef res
    case r of
       Validated x        -> return $ FormElm (ftextarea tolook  x) $ Just x
       NotValidated s err -> return $ FormElm (ftextarea tolook   (toJSString s))  Nothing
       NoParam            -> return $ FormElm (ftextarea tolook  nvalue)  Nothing
    where
    typef :: View v m String -> StateT MFlowState m (ParamResult v a)
    typef = undefined

-- | A synonim of getMultilineText
textArea :: (FormInput view
                 ,  MonadIO m)
                   => JSString
                 ->  View view m String
textArea= getMultilineText

deriving instance Typeable JSRef

instance Show JSString where show= fromJSString

getBool :: (FormInput view,
      MonadIO m, Monad (View view m), Functor m) =>
      Bool -> String -> String -> View view m Bool
getBool mv truestr falsestr= do
   r <- getSelect $   setOption truestr (fromStr $ toJSString truestr)  <! (if mv then [("selected","true")] else [])
                  <|> setOption falsestr(fromStr $ toJSString falsestr) <! if not mv then [("selected","true")] else []
   if  r == truestr  then return True else return False



-- | Display a dropdown box with the options in the first parameter is optionally selected
-- . It returns the selected option.
getSelect :: (FormInput view,
      MonadIO m,Typeable a, Read a) =>
      View view m (MFOption a) ->  View view m  a
getSelect opts = res where
  res= View $ do
    tolook <- genNewId
    st <- get
    put st{needForm= HasElems}
    r <- getParam1 tolook `asTypeOf` typef res
--    setSessionData $ fmap MFOption $ valToMaybe r
    FormElm form mr <- (runView opts)
--
    return $ FormElm (fselect tolook  form)  $ valToMaybe r

    where
    typef :: View v m a -> StateT MFlowState m (ParamResult v a)
    typef = undefined

newtype MFOption a= MFOption a deriving Typeable

instance (FormInput view,Monad m, Functor m) => Monoid (View view m (MFOption a)) where
  mappend =  (<|>)
  mempty = Control.Applicative.empty

-- | Set the option for getSelect. Options are concatenated with `<|>`
setOption
  :: (Monad m, Monad (View view m), Show a, Eq a, Typeable a, FormInput view) =>
     a -> view -> View view m (MFOption a)
setOption n v = View $ do
--  mo <- getSessionData
  runView $ setOption1 n v False


-- | Set the selected option for getSelect. Options are concatenated with `<|>`
setSelectedOption
  :: (Monad m, Monad(View view m), Show a, Eq a, Typeable a, FormInput view) =>
     a -> view -> View view m (MFOption a)
setSelectedOption n v= View $ do
--  mo <- getSessionData
  runView $ setOption1 n v True
--   Just Nothing -> setOption1 n v True
--   Just (Just o) -> setOption1 n v $   n == o


setOption1 :: (FormInput view,
      Monad m, Typeable a, Eq a, Show a) =>
      a -> view -> Bool ->  View view m  (MFOption a)
setOption1 nam  val check= View $ do
    let n = if typeOf nam == typeOf(undefined :: String)
                   then unsafeCoerce nam
                   else show nam

    return . FormElm (foption (toJSString n) val check)  . Just $ MFOption nam


wlabel
  :: (Monad m, FormInput view) => view -> View view m a -> View view m a
wlabel str w =View $ do
   id <- getNextId
   FormElm render mx <- runView w
   return $ FormElm (ftag "label" str `attrs` [("for",id)] <> render) mx


-- passive reset button.
resetButton :: (FormInput view, Monad m) => JSString -> View view m ()
resetButton label= View $ return $ FormElm (finput  "reset" "reset" label False Nothing)
                        $ Just ()

inputReset :: (FormInput view, Monad m) => JSString -> View view m ()
inputReset= resetButton

-- passive submit button. Submit a form, but it is not trigger any event.
-- Unless you attach it with `trigger`
submitButton :: (Monad (View view m),StateType (View view m) ~ MFlowState,FormInput view, MonadIO m) => String -> View view m String
submitButton label=  getParam Nothing "submit" $ Just label


inputSubmit :: (Monad (View view m),StateType (View view m) ~ MFlowState,FormInput view, MonadIO m) => String -> View view m String
inputSubmit= submitButton

-- | active button. When clicked, return the first parameter
wbutton :: a -> JSString -> Widget a
wbutton x label= static $
   let label'= toJSString label in do
        input  ! atr "type" "submit" ! id   label' ! atr "value" label `pass` OnClick
        return x
      `continuePerch`  label'

-- | when creating a complex widget with many tags, this call indentifies which tag will receive the attributes of the (!) operator.
continuePerch :: Widget a -> ElemID -> Widget a
continuePerch w eid= View $ do
      FormElm f mx <- runView w
      return $ FormElm (c f) mx
      where
      c f =Perch $ \e' ->  do
         build f e'
         elemid eid

      elemid id= elemById id >>= return . fromJust


-- | Present a link. Return the first parameter when clicked
wlink :: (Show a, Typeable a) => a -> Perch -> Widget a
wlink x v= static $ do
    (a ! href ( toJSString $ "#/"++show1 x)   $ v) `pass` OnClick
    return x

   where
   show1 x | typeOf x== typeOf (undefined :: String) = unsafeCoerce x
           | otherwise= show x




-- | Concat a list of widgets of the same type, return a the first validated result
firstOf :: (FormInput view, Monad m, Functor m)=> [View view m a]  -> View view m a
firstOf xs= Prelude.foldl (<|>) noWidget xs

-- | from a list of widgets, it return the validated ones.
manyOf :: (FormInput view, MonadIO m, Functor m)=> [View view m a]  -> View view m [a]
manyOf xs=  (View $ do
      forms <- mapM runView  xs
      let vs  = mconcat $ Prelude.map (\(FormElm v _) ->   v) forms
          res1= catMaybes $ Prelude.map (\(FormElm _ r) -> r) forms
      return . FormElm vs $ Just res1)

-- | like manyOf, but does not validate if one or more of the widgets does not validate
allOf xs= manyOf xs `validate` \rs ->
      if length rs== length xs
         then return Nothing
         else return $ Just mempty

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

(<<<) :: (Monad m,  Monoid view)
          => (view ->view)
         -> View view m a
         -> View view m a
(<<<) v form= View $ do
  FormElm f mx <- runView form
  return $ FormElm (v  f) mx


infixr 5 <<<

-- | A parameter application with lower priority than ($) and direct function application
(<<) :: (t1 -> t) -> t1 -> t
(<<) tag content= tag $ toElem content

infixr 7 <<


-- | Append formatting code to a widget
--
-- @ getString "hi" <++ H1 << "hi there"@
--
-- It has a infix prority: @infixr 6@ higuer that '<<<' and most other operators
(<++) :: (Monad m, Monoid v)
      => View v m a
      -> v
      -> View v m a
(<++) form v= View $ do
  FormElm f mx <-  runView  form
  return $  FormElm ( f <> v) mx

infixr 6  ++>
infixr 6 <++
-- | Prepend formatting code to a widget
--
-- @bold << "enter name" ++> getString Nothing @
--
-- It has a infix prority: @infixr 6@ higuer that '<<<' and most other operators
(++>) :: (Monad m,  Monoid view)
       => view -> View view m a -> View view m a
html ++> w =  --  (html <>) <<< digest
 View $ do
  FormElm f mx <- runView w
  return $ FormElm (html  <>  f) mx



-- | Add attributes to the topmost tag of a widget
--
-- it has a fixity @infix 8@
infixl 8 <!
widget <! attribs= View $ do
      FormElm fs  mx <- runView widget
      return $ FormElm  (fs `attrs` attribs) mx -- (head fs `attrs` attribs:tail fs) mx
--      case fs of
--        [hfs] -> return $ FormElm  [hfs `attrs` attribs] mx
--        _ -> error $ "operator <! : malformed widget: "++ concatMap (unpack. toByteString) fs



instance  Attributable (Widget a) where
 (!) widget atrib = View $ do
      FormElm fs  mx <- runView widget
      return $ FormElm  (fs `attr` atrib) mx



-- | Empty widget that does not validate. May be used as \"empty boxes\" inside larger widgets.
--
-- It returns a non valid value.
noWidget ::  (FormInput view,
     Monad m, Functor m) =>
     View view m a
noWidget= Control.Applicative.empty

-- | a sinonym of noWidget that can be used in a monadic expression in the View monad. it stop the
-- computation in the Widget monad.
stop :: (FormInput view,
     Monad m, Functor m) =>
     View view m a
stop= Control.Applicative.empty


-- | Render raw view formatting. It is useful for displaying information.
wraw ::  Perch -> Widget ()
wraw x= View . return . FormElm x $ Just ()

-- | True if the widget has no valid input
isEmpty :: Widget a -> Widget Bool
isEmpty w= View $ do
  FormElm r mv <- runView w
  return $ FormElm r $ Just $ isNothing mv


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

-- | Get the session data of the desired type if there is any.
getSessionData ::  (StateType m ~ MFlowState,MonadState m,Typeable a) =>  m (Maybe a)
getSessionData =  resp where
 resp= gets mfData >>= \list  ->
    case M.lookup ( typeOf $ typeResp resp ) list of
      Just x  -> return . Just $ unsafeCoerce x
      Nothing -> return $ Nothing
 typeResp :: m (Maybe x) -> x
 typeResp= undefined

-- | getSessionData specialized for the View monad. if Nothing, the monadic computation
-- does not continue. getSData is a widget that does not validate when there is no data
--  of that type in the session.
getSData :: Typeable a =>Widget  a
getSData= View $ do
    r <- getSessionData
    return $ FormElm mempty r

-- | setSessionData ::  (StateType m ~ MFlowState, Typeable a) => a -> m ()
setSessionData  x=
  modify $ \st -> st{mfData= M.insert  (typeOf x ) (unsafeCoerce x) (mfData st)}

-- | a shorter name for setSessionData
setSData ::  (StateType m ~ MFlowState, MonadState  m,Typeable a) => a -> m ()
setSData= setSessionData

delSessionData x=
  modify $ \st -> st{mfData= M.delete (typeOf x ) (mfData st)}

delSData :: (StateType m ~ MFlowState, MonadState  m,Typeable a) => a -> m ()
delSData= delSessionData

---------------------------
data EvData =  NoData | Click Int (Int, Int) | Mouse (Int, Int) | MouseOut | Key Int deriving (Show,Eq,Typeable)
data EventData= EventData{ evName :: String, evData :: EvData} deriving (Show,Typeable)

--eventData :: MVar Dynamic
--eventData= unsafePerformIO . newMVar . toDyn $ EventData "OnLoad" NoData

resetEventData :: (StateType m ~ MFlowState, MonadState  m) => m ()
resetEventData=   modify $ \st -> st{ lastEvent= toDyn $ EventData "Onload" NoData}


getEventData :: (Typeable a,  StateType m ~ MFlowState, MonadState  m) => m a
getEventData = gets lastEvent >>= return . (flip fromDyn) (error "getEventData: event type not expected")

setEventData ::  (Typeable a, StateType m ~ MFlowState, MonadState  m) => a-> m ()
setEventData dat=  modify $ \st -> st{ lastEvent= toDyn dat}

getMEventData :: (Typeable a, StateType m ~ MFlowState, MonadState  m) => m (Maybe a)
getMEventData= gets lastEvent >>= return . fromDynamic

setIOEventData :: Typeable a => a -> IO ()
setIOEventData dat= do
  st <- takeMVar globalState
  putMVar globalState st{ lastEvent= toDyn dat}



class IsEvent a where
   eventName :: a -> JSString
   buildHandler :: Elem -> a  -> IO () -> IO()


foreign import javascript unsafe  "$1.addEventListener($2,$3)"
     addEvListener ::  Elem -> JSString -> JSFun (IO ()) -> IO ()

data OnLoad= OnLoad
instance  IsEvent  OnLoad    where
  eventName= const "load"
  buildHandler elem e io = do
      syncCallback NeverRetain False (setDat io (EventData (nevent e) NoData) )
      >>=  addEvListener elem (eventName e)
      
data OnUnload = OnUnLoas
instance  IsEvent  OnUnload   where
  eventName= const "unload"
  buildHandler elem e io =  syncCallback NeverRetain False   (setDat io $ EventData (nevent e) NoData)
                            >>=  addEvListener elem (eventName e) 
data OnChange= OnChange
instance  IsEvent  OnChange   where
  eventName= const "change"
  buildHandler elem e io=  syncCallback NeverRetain False  (setDat io $ EventData (nevent e) NoData)
                           >>=  addEvListener elem (eventName e) 
data OnFocus= OnFocus
instance  IsEvent  OnFocus   where
  eventName= const "focus"
  buildHandler elem e io=  syncCallback NeverRetain False  (setDat io $ EventData (nevent e) NoData)
                            >>= addEvListener elem (eventName e)
      
data OnBlur= OnBlur
instance  IsEvent  OnBlur   where
  eventName= const "blur"

data OnMouseMove= OnMouseMove
instance  IsEvent  OnMouseMove  where
  eventName= const "mousemove"
  buildHandler elem e io= syncCallback1 NeverRetain False (\r -> let Just (x,y)=fromJSRef r in  (setDat io $ EventData (nevent e) $ Mouse(x,y)))
                            >>= addEvListener elem (eventName e)
      
data OnMouseOver= OnMouseOver
instance  IsEvent  OnMouseOver  where
  eventName= const "mouseover"
  buildHandler elem e io=  syncCallback1 NeverRetain False (\r -> let Just (x,y)=fromJSRef r in (setDat io $  EventData (nevent e) $ Mouse(x,y)))
                            >>= addEvListener elem (eventName e)
      
data OnMouseOut= OnMouseOut
instance  IsEvent  OnMouseOut   where
  eventName= const "mouseout"
  buildHandler elem e io= syncCallback NeverRetain False (setDat io $  EventData (nevent e) $ MouseOut)
      >>= addEvListener elem (eventName e)
      
data OnClick= OnClick
instance  IsEvent  OnClick      where
  eventName= const "click"
  buildHandler elem e io= syncCallback2 NeverRetain False $ \r1 r2 -> do
      let Just i= fromJSRef r1
          Just (x,y)= fromJSRef r2
      setDat io $  EventData (nevent e) $ Click i (x,y)
      addEvListener elem (eventName e)
      
data OnDblClick= OnDblClick
instance  IsEvent  OnDblClick   where
  eventName= const "dblclick"
  buildHandler elem e io= syncCallback2 NeverRetain False $ \r1 r2 -> do
      let Just i= fromJSRef r1
          Just (x,y)= fromJSRef r2
      setDat io $  EventData (nevent e) $ Click i (x,y)
      addEvListener elem (eventName e)

      
data OnMouseDown= OnMouseDown
instance  IsEvent  OnMouseDown  where
  eventName= const "mousedowm"
  buildHandler elem e io= syncCallback2 NeverRetain False $ \r1 r2 -> do
      let Just i= fromJSRef r1
          Just (x,y)= fromJSRef r2
      setDat io $  EventData (nevent e) $ Click i (x,y)
      addEvListener elem (eventName e)

      
data OnMouseUp= OnMouseUp
instance  IsEvent  OnMouseUp    where
  eventName= const "mouseup"
  buildHandler elem e io= syncCallback2 NeverRetain False $ \r1 r2 -> do
      let Just i= fromJSRef r1
          Just (x,y)= fromJSRef r2
      setDat io $  EventData (nevent e) $ Click i (x,y)
      addEvListener elem (eventName e)

      
data OnKeyPress= OnKeyPress
instance  IsEvent  OnKeyPress  where
  eventName= const "keypress"
  buildHandler elem e io = syncCallback1 NeverRetain False (\r -> letJust i= formJSRef r in  (setDat io $ EventData (nevent e) $ Key i))
      >>= addEvListener elem (eventName e)
      
data OnKeyUp= OnKeyUp
instance  IsEvent OnKeyUp    where
  eventName= const "keyup"
  buildHandler elem e io = syncCallback NeverRetain False (\r -> letJust i= formJSRef r in  (setDat io $ EventData (nevent e) $ Key i))
      >>= addEvListener elem (eventName e)
      
data OnKeyDown= OnKeyDown
instance  IsEvent  OnKeyDown   where
  eventName= const "keydown"
  buildHandler elem e io = syncCallback1 NeverRetain False (\r -> letJust i= formJSRef r in  (setDat io $ EventData (nevent e) $ Key i))
      >>= addEvListener elem (eventName e)
      


nevent e= fromJSString $ eventName e

--setDat :: EventData -> m ()
setDat iohandler d =  unsafeCoerce $ do
            setIOEventData d
            iohandler




-- | triggers the event when it happens in the widget.
--
-- What happens then?
--
-- 1)The event reexecutes all the monadic sentence where the widget is, (with no re-rendering)
--
-- 2) with the result of this reevaluaution, executes the rest of the monadic computation
--
-- 3) update the DOM tree with the rendering of the reevaluation in 2).
--
-- As usual, If one step of the monadic computation return empty, the reevaluation finish
-- So the effect of an event can be restricted as much as you may need.
--
-- Neither the computation nor the tree in the upstream flow is touched.
-- (unless you use out of stream directives, like `at`)
--
-- monadic computations inside monadic computations are executed following recursively
-- the steps mentioned above. So an event in a component deep down could or could not
-- trigger the reexecution of the rest of the whole.
raiseEvent ::  IsEvent event  => Widget a -> event -> Widget a
raiseEvent w event = View $ do
   cont <- getCont
   FormElm render mx <- runView  w
   let iohandler = runCont cont
       nevent = eventName event

       render' = addEvent' (render :: Perch) event iohandler

   return $ FormElm render' mx
   where
   -- | create an element and add any event handler to it.
   -- This is a generalized version of addEvent
   addEvent' :: IsEvent a b => Perch -> a -> IO() -> Perch
   addEvent' be eevent iohandler= Perch $ \e -> do
        e' <- build be e
        let event= eventName eevent
        buildHandler e' eevent iohandler
        return e'

-- | A shorter synonym for `raiseEvent`
fire ::   IsEvent event => Widget a -> event -> Widget a
fire = raiseEvent

-- | A shorter and smoother synonym for `raiseEvent`
wake ::   IsEvent event => Widget a -> event -> Widget a
wake = raiseEvent

-- | A professional synonym for `raiseEvent`
react ::  IsEvent event => Widget a -> event -> Widget a
react = raiseEvent

-- | pass trough only if the event is fired in this DOM element.
-- Otherwise, if the code is executing from a previous event, the computation will stop
pass :: IsEvent event => Perch -> event -> Widget EventData
pass v event= static $ do
        resetEventData
        wraw v `wake` event
        e@(EventData typ _) <- getEventData
        continueIf (eventName event== typ) e

-- | return empty and the monadic computation stop if the condition is false.
-- If true, return the second parameter.
continueIf :: Bool -> a -> Widget a
continueIf True x  = return x
continueIf False _ = empty

-- | executes a widget each t milliseconds until it validates and return ()
wtimeout :: Int -> Widget () -> Widget ()
wtimeout t w= View $ do
    id <- genNewId
    let f= do
        me <- elemById  id
        case me of
         Nothing -> return ()
         Just e ->do
            r <- clearChildren e >> runWidget w e
            case r of
              Nothing -> f
              Just ()  -> return ()

    handler <- syncCallback NeverRetain  False
    
    let f= setTimeout t handler
    liftIO  f
    runView $ identified id w

foreign import javascript unsafe  "document.querySelectorAll($1)" setTimeout ::  Int ->  IO()

-- getting and running continuations

getCont ::(StateType m ~ MFlowState, MonadState  m) => m EventF
getCont = gets process


runCont :: EventF -> IO()
runCont (EventF x  fs)= x `bind` fs  >> return ()


bind :: IO (Maybe a) -> (a -> IO (Maybe  b)) -> IO (Maybe b)
bind x  f= do
   mr <- x
   case mr of
     Just r -> f r
     Nothing -> return Nothing

--bind x f = View $ do
--    FormElm form1 mk <- runView x
--    case mk of
--      Just k  -> do
--         FormElm form2 mk <- runView $ f k
--         return $ FormElm (form1 <> form2) mk
--      Nothing ->
--         return $ FormElm  form1  Nothing

globalState= unsafePerformIO $ newMVar mFlowState0

-- | run the widget as the content of a DOM element, the id is passed as parameter. All the
-- content of the element is erased previously and it is substituted by the new rendering
runWidgetId :: Widget b -> ElemID  -> IO (Maybe b)
runWidgetId ac id =  do
   me <- elemById id
   case me of
     Just e ->  do
      clearChildren e
      runWidget ac e
     Nothing -> do
          st <- unsafeCoerce $ takeMVar globalState
          (FormElm render mx, s) <- runStateT (runView ac) st
          liftIO $ putMVar globalState s
          return mx


-- | run the widget as the content of a DOM element
-- the new rendering is added to the element
runWidget :: Widget b -> Elem  -> IO (Maybe b)
runWidget action e = do
     st <- takeMVar globalState
     (FormElm render mx, s) <- runStateT (runView action) st
     liftIO $ putMVar globalState s
     build render e
     return mx



-- | add a header in the <header> tag
addHeader :: Perch -> IO ()
addHeader format= do
    head <- getHead
    build format head
    return ()

foreign import javascript unsafe "document.head" getHead :: IO Elem



-- | run the widget as the body of the HTML
runBody :: Widget a -> IO (Maybe a)
runBody w= do
  body <- getBody
  (flip runWidget) body w

--foreign import javascript unsafe "document.body" getBody :: IO Elem


foreign import javascript unsafe "$1.childNodes()" getChildren :: Elem -> IO (JSRef[Elem])
foreign import javascript unsafe "$2.insertBefore($1, $3)" addChildBefore :: Elem -> Elem -> Elem -> IO()
data UpdateMethod= Append | Prepend | Insert deriving Show

-- | Run the widget as the content of the element with the given id. The content can
-- be appended, prepended to the previous content or it can be the only content depending on the
-- update method.
at ::  JSString -> UpdateMethod -> Widget a -> Widget  a
at id method w= View $ do
 FormElm render mx <- (runView w)
 return $ FormElm  (set  render)  mx
 where
 set render = case method of
     Insert -> do
             forElems' id $ this clear <> render
             return ()
     Append -> do
             forElems' id render
             return ()
     Prepend -> do
            forElems' id $ Perch $ \e -> do
             es <- getChildren e
             case es of
               [] -> build render e >> return ()
               e':_ -> do
                     span <- newElem "span"
                     addChildBefore span e e'
                     build render span
                     return()
            return()


-- AJAX

data Method = GET | POST deriving Show
type URL= JSString

responseAjax :: IORef [(String,Maybe JSString)]
responseAjax = unsafePerformIO $ newIORef []

class (FromJSString (Maybe a), ToJSString  a) => JSType a
instance (FromJSString (Maybe a), ToJSString  a) => JSType a

-- | Invoke AJAX. 
-- `(a,b)` are the lists of parameters, a is normally `String` or `JSString`.
-- JSON is also supported for `b` and `c`. If you want to handle your data types, make a instance of
-- `JSType`
--
-- Note the de-inversion of control. There is no callback.
--
-- `ajax` can be combined with other Widgets using monadic, applicative or alternative combinators.
ajax :: ( JSType a, JSType  b, JSType  c,Typeable c)
     => Method -> URL -> [(a, b)] -> Widget (Maybe c)
ajax method url kv= View $ do
      id <- genNewId
      rs <- liftIO $ readIORef responseAjax
      case lookup id rs of
        Just rec -> liftIO $ do
               writeIORef responseAjax $ filter ((/= id). fst) rs

               return $ FormElm  mempty $  fmap fromJSString rec
        _ -> do
              proc <- gets process
              liftIO $ textRequest'  method url kv $ cb id proc
              return $ FormElm mempty Nothing


  where
  -- cb :: String -> (Widget a) -> [(b -> Widget c,ElemID)] -> Maybe d -> IO()
  cb id cont rec= do
    responses <- readIORef responseAjax
    liftIO $ writeIORef responseAjax $  (id, rec):responses
    runCont cont
    return ()


textRequest' :: (JSType a, JSType b, JSType c)
        => Method
        -> URL
        -> [(a, b)]
        -> (Maybe c -> IO ())
        -> IO ()
textRequest' m url kv cb = do
        _ <- ajaxReq (toJSString $ show m) url' True pd cb'     -- here postdata is ""
        return ()
        where
        cb' = mkCallback $ cb . fmap fromJSS'
        url' = case m of
               GET -> if null kv then toJSString url else catJSStr (toJSString "?") [toJSString url, toQueryString kv]
               POST -> toJSString url
        pd = case m of
               GET ->  toJSString ""
               POST -> if null kv then  toJSString "" else toQueryString kv

        fromJSS'= fromJust . fromJSString

mkCallback cb= syncCallback   NeverRetain False  cb

toQueryString :: (JSType a, JSType b) =>[(a, b)] -> JSString
toQueryString = catJSStr (toJSString "&") . Prelude.map (\(k,v) -> catJSStr (toJSString "=") [toJSString k,toJSString v])

foreign import javascript unsafe  "$1+$2" catJSStr  :: JSString -> JSString -> JSString

    -- function ajaxReq(method, url, async, postdata, cb) {
foreign import javascript unsafe
    "var xhr = new XMLHttpRequest();\
    \xhr.open($1, $2, $3);\
    \if($1 == 'POST') {\
        \xhr.setRequestHeader('Content-type',\
                             \'application/x-www-form-urlencoded');\
    \}\
    \xhr.onreadystatechange = function() {\
        \if(xhr.readyState == 4) {\
            \if(xhr.status == 200) {\
                \$5 xhr.responseText;\
            \} else {\
                 \$5 '';\
            \}\
        \}\
    \}\
    \xhr.send($4)"



     ajaxReq :: JSString    -- method
             -> JSString    -- url
             -> Bool        -- async?
             -> JSString    -- POST data
             ->(JSFun (JSRef (Maybe JSString) -> IO ()))
             -> IO ()



