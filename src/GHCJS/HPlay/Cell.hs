-----------------------------------------------------------------------------
--
-- Module      :  Cell
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  agocorona@gmail.com
-- Stability   :  experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverloadedStrings, CPP, ScopedTypeVariables #-}
module GHCJS.HPlay.Cell(Cell(..),boxCell,(.=),get,mkscell,scell, gcell, calc)  where
import Transient.Base
import Transient.Move
import Transient.Internals (runTransState)
import Transient.Backtrack
import GHCJS.HPlay.View
import Data.Typeable
import Unsafe.Coerce
import qualified Data.Map as M hiding ((!))
import System.IO.Unsafe

import Control.Monad.IO.Class
import Control.Monad
import Data.Maybe
import Control.Exception
import Data.List
import GHCJS.Perch
import Control.Exception

#ifdef ghcjs_HOST_OS

import Data.JSString hiding (empty)

#else

type JSString = String

#endif

data Cell  a = Cell { mk :: Maybe a -> Widget a
                    , setter ::  a -> IO ()
                    , getter ::  IO (Maybe a)}

--instance Functor Cell where
--  fmap f cell = cell{setter= \c x ->  c .= f x, getter = \cell -> get cell >>= return . f}



-- | creates a input box cell with polimorphic value, identified by a string.
-- the cell can be updated programatically
boxCell :: (Show a, Read a, Typeable a) => ElemID -> Cell a
boxCell id = Cell{ mk= \mv -> getParam  (Just id) "text" mv
                 , setter= \x -> do
                          me <- elemById id
                          case me of
                            Just e ->  setProp e "value" (toJSString $ show1 x)
                            Nothing -> return ()

                 , getter= getID id}

getID id = withElem id $ \e -> do
  ms <- getValue e
  case ms of
    Nothing -> return Nothing
    Just s  -> return $ read1  s
  where
  read1 s=
      if typeOf(typeIO getID) /= typestring
           then case readsPrec 0  s  of
               [(v,_)] -> v `seq` Just v
               _       -> Nothing
           else Just $ unsafeCoerce s

typeIO :: (ElemID -> IO (Maybe a)) -> a
typeIO = undefined

typestring= typeOf (undefined :: String)

show1 x= if typeOf x== typestring
        then unsafeCoerce x
        else show x

instance Attributable (Cell a) where
 (Cell mk setter getter) ! atr = Cell (\ma -> mk ma ! atr) setter getter



-- | Cell assignment
(.=) :: MonadIO m =>  Cell a -> a -> m ()
(.=) cell x = liftIO $ (setter cell )  x

get cell =  Transient $ liftIO (getter cell)


---- |  a cell value assigned to other cell
--(..=) :: Cell a -> Cell a -> Widget ()
--(..=) cell cell'= get cell' >>= (cell .= )

infixr 0 .=  -- , ..=

-- experimental: to permit cell arithmetic

--instance Num a => Num (Cell a) where
--  c + c'= Cell undefined undefined  $
--            do r1 <- getter c
--               r2 <- getter c'
--               return $  liftA2 (+) r1  r2
--
--  c * c'= Cell undefined undefined $
--            do r1 <- getter c
--               r2 <- getter c'
--               return $ liftA2 (+) r1  r2
--
--  abs c= c{getter=  getter c >>= return . fmap abs}
--
--  signum c= c{getter=  getter c >>= return . fmap signum}
--
--  fromInteger i= Cell  undefined undefined  . return $ Just $ fromInteger i


-- *  Spradsheet type cells
-- Implement a solver that allows circular dependencies . See
-- > http://tryplayg.herokuapp.com/try/spreadsheet.hs/edit

-- The recursive Cell calculation DSL BELOW ------


-- | within a `mkscell` formula, `gcell` get the the value of another cell using his name.
--
-- see http://tryplayg.herokuapp.com/try/spreadsheet.hs/edit
gcell ::   JSString -> Widget Double
gcell n= Widget $ do
  Vars vars <- getSData <|> return(Vars M.empty ) -- liftIO $ readIORef rvars
  case M.lookup n vars !> M.size vars of
    Just exp -> do inc n  exp;  exp
    Nothing -> error $ "cell not found: " ++ show n
  where
  inc n exp=  do
     Tries tries maxtries<- getSData <|> do
                                      Exprs exprs <- getSData
                                      return . Tries 0 $ 3 * (M.size $  exprs)
     if tries <= maxtries
       then  setData $ Tries (tries+1) maxtries
       else  back Loop

data Loop= Loop deriving (Show,Typeable)

instance Exception Loop

-- a parameter is a function of all of the rest
type Expr a = TransIO a

data Tries= Tries Int Int deriving Typeable
--rtries= unsafePerformIO $ newIORef $ (0::Int)
--maxtries=  3 * (M.size $ unsafePerformIO $ readIORef rexprs)

newtype Exprs= Exprs (M.Map JSString (Expr Double))
--rexprs :: IORef (M.Map JSString (Expr Double))
--rexprs= unsafePerformIO $ newIORef M.empty      -- initial expressions

newtype Vars= Vars (M.Map JSString (Expr Double))
--rvars :: IORef (M.Map JSString (Expr Double))
--rvars= unsafePerformIO $ newIORef M.empty       -- expressions actually used for each cell.
                                                -- initially, A mix of reexprs and rmodified
                                                -- and also contains the result of calculation

newtype Modified= Modified (M.Map JSString (Expr Double)) deriving Typeable
--rmodified :: IORef (M.Map JSString (Expr Double))
--rmodified= unsafePerformIO $ newIORef M.empty    -- cells modified by the user or by the loop detection mechanism


-- | make a spreadsheet cell. a spreadsheet cell is an input-output box that takes input values from
-- the user, has an expression associated and display the result value after executing `calc`
--
-- see http://tryplayg.herokuapp.com/try/spreadsheet.hs/edit
mkscell :: JSString -> Maybe Double -> Expr Double -> Widget Double
mkscell name val expr= mk (scell name expr) val

--both mx= local $ runCloud mx   <** runCloud ( atRemote (clustered $ mx >> empty :: Cloud()))


scell :: JSString -> Expr Double -> Cell Double
scell id  expr= Cell{ mk= \mv -> Widget $  do
                           Exprs exprs <- getSData <|> return (Exprs M.empty) -- readIORef rexprs
                           setData . Exprs $ M.insert id expr exprs

                           r <- norender $ getParam (Just id) "text"  mv `fire` OnKeyUp

                           Modified mod <-  getSData <|>  return(Modified M.empty)
                           setData . Modified  $ M.insert  id (return  r)  mod
                           return r

                    , setter= \x -> withElem id $ \e -> setProp e "value" (toJSString $ show1 x)

                    , getter= getID id}





-- | executes the spreadsheet adjusting the vaules of the cells created with `mkscell` and solving loops
--
-- see http://tryplayg.herokuapp.com/try/spreadsheet.hs/edit
calc :: Widget ()
calc= Widget $  do
  st <- getCont
  return() `onBack` (\(e::Loop) -> do removeVar st e; forward Loop !> "ONBACK")

  Modified nvs <- getSData  <|> error "no modified" -- liftIO $ readIORef rmodified

  when (not $ M.null nvs) $ do
            values <-  calc1
            mapM_ (\(n,v) -> boxCell n .= v)  values

--  liftIO $ writeIORef rmodified M.empty

  where
  run' st x=  runTransState st x >> return ()


  calc1  :: TransIO [(JSString,Double)]
  calc1= do
    setData $ Tries 0 -- liftIO $ writeIORef rtries 0
    Exprs cells    <- getSData <|> error "no exprs" -- liftIO $ readIORef rexprs
    Modified nvs   <- getSData <|> error "mo modified2" -- liftIO $ readIORef rmodified
    setData . Vars $ M.union nvs cells
    solve

--solve  :: M.Map JSString (Widget a) -> Widget (M.Map JSString a)
solve :: TransIO [(JSString,Double)]
solve = do
     Vars vars <- getSData <|> error "no vars" --  liftIO $ readIORef rvars
     mapM (solve1 vars) $ M.toList vars
     where

     solve1 vars (k,f)= do
        x <- f
        setData . Vars $ M.insert k (return x) vars
        return (k,x) :: TransIO (JSString,Double)



--  removeVar :: EventF -> SomeException -> IO () -- [(JSString,Double)]
removeVar st  = \(e:: Loop) ->  do -- runCloud $ both $ localIO $ do
    Modified nvs <- getSData <|>  error "no modified 3"-- readIORef rmodified
    Exprs exprs  <- getSData <|>  error " no Exprs2" --readIORef rexprs

    case  M.keys exprs \\ M.keys nvs of
      [] -> error "non solvable circularity in cell dependencies"
      (name:_) -> do
         mv <- liftIO $ getID name

         case mv of
            Nothing -> return ()
            Just v  -> do
                setData . Modified  $ M.insert name ( return v) nvs
                return ()  !> ("using",v)
                norender calc -- runTransState st (norender calc)
                return ()

  -- http://blog.sigfpe.com/2006/11/from-l-theorem-to-spreadsheet.html
  -- loeb ::  Functor f => f (t -> a) -> f a
  --  loeb x = fmap (\a ->  a (loeb  x)) x
  -- loeb :: [([a]-> a)] -> [a]
  -- loeb x=  map (\f ->  f (loeb  x)) x

--loeb :: [([a] -> IO a)] -> IO [a]
--loeb x= mapM (\f -> loeb x >>= f) x -- fail does not terminate



--loeb x=  map (\f ->  f (loeb  x)) x





instance (Num a,Eq a,Fractional a) =>Fractional (Widget a)where
     mf / mg = do
        f <- mf
        g <- mg
        return $ f  / g
     fromRational = error "fromRational not implemented"


instance (Num a,Eq a) => Num (Widget a) where
     fromInteger = return . fromInteger
     f + g = f >>= \x -> g >>= \y -> return $ x + y
     f * g = f >>= \x -> g >>= \y -> return $ x * y
     negate f = f >>= return . negate
     abs f =  f >>= return . abs
     signum f =  f >>= return . signum
