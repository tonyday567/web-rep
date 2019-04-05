{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Shared
  ( MuRef
  , reifyGraph
  ) where

import Control.Concurrent.MVar
import qualified System.Mem.StableName as SN
import Data.IntMap as IM
import Control.Monad.State

type Unique = Int
data Graph e = Graph [(Unique, e Unique)] Unique

class MuRef a where
  type DeRef a :: * -> *
  mapDeRef :: Applicative f => (a -> f u) -> a -> f (DeRef a u)


data State' a = State'
  { im :: IM.IntMap [(SN.StableName a, Unique)]
  , l :: [(Unique, DeRef a Unique)]
  , u :: Unique
  }

reifyGraph :: MuRef a => a -> IO (Graph (DeRef a))
reifyGraph mr = do
  rt1 <- newMVar IM.empty
  rt2 <- newMVar []
  uVar <- newMVar 0
  root <- findNodes rt1 rt2 uVar mr
  pairs <- readMVar rt2
  return (Graph pairs root)

{-
reifyGraph' :: MuRef a => a -> IO (Graph (DeRef a))
reifyGraph' mr = do
  -- evalState (State' IM.empty [] 0)
  root <- findNodes rt1 rt2 uVar mr
  pairs <- readMVar rt2
  return (Graph pairs root)
-}

newUnique x = do
      v0 <- takeMVar x
      let v' = succ v0
      putMVar x v'
      return v'

newUnique' :: (Num s, Monad m, MonadState s m) => m s
newUnique' = do
      v0 <- get
      let v' = v0+1
      put v'
      return v'

-- mylookup :: Unique -> IM.IntMap [(Unique, a)] -> Maybe a             
mylookup h tab =
      case IM.lookup (SN.hashStableName h) tab of
        Just t2 -> Prelude.lookup h t2
        Nothing -> Nothing

findNodes :: MuRef a =>
     MVar (IM.IntMap [(SN.StableName a, Unique)])
     -> MVar [(Unique, DeRef a Unique)] -> MVar Unique -> a -> IO Unique
findNodes rt1 rt2 uVar j
      | j `seq` True = do  -- force evaluation of j
          st <- SN.makeStableName j
          tab <- takeMVar rt1
          case mylookup st tab of
            Just v -> do
              putMVar rt1 tab
              return v
            Nothing -> do  
              var <- newUnique uVar
              putMVar rt1 $ IM.insertWith (++)
                                (SN.hashStableName st)
                                [(st, var)]
                                tab
              res <- mapDeRef (findNodes rt1 rt2 uVar) j
              tab' <- takeMVar rt2
              putMVar rt2 $ (var, res) : tab'
              return var
      | otherwise = error "findNodes : Failed strict evaluation"
        

{-
findNodes' ::
  (MonadState (State' a) IO, MuRef a) =>
  a ->
  IO Unique
findNodes' j
      | j `seq` True = do  -- force evaluation of j
          st <- SN.makeStableName j
          (State' tab l u) <- get
          case mylookup st tab of
            Just v -> return v
            Nothing -> do
              var <- newUnique' u
              putMVar rt1 $ IM.insertWith (++)
                                (SN.hashStableName st)
                                [(st, var)]
                                tab
              res <- mapDeRef (findNodes rt1 rt2 uVar) j
              tab' <- takeMVar rt2
              putMVar rt2 $ (var, res) : tab'
              return var
      | otherwise = error "findNodes : Failed strict evaluation"


-}

-- --
  

type Name = String
data TreeAst = Var Name
             | Const Int
             | UnOp TreeAst
             | BinOp TreeAst TreeAst deriving (Eq, Show)

data GraphAst s = GVar Name
                | GConst Int
                | GUnOp s
                | GBinOp s s deriving (Eq, Show)           
               

instance MuRef TreeAst where
  type DeRef TreeAst = GraphAst
  mapDeRef _ (Var v) = pure $ GVar v
  mapDeRef _ (Const c) = pure $ GConst c
  mapDeRef f (UnOp p) = GUnOp <$> f p
  mapDeRef f (BinOp p1 p2) = GBinOp <$> f p1 <*> f p2





                                 
  


-- testing testing

t0 = BinOp (Const 2) (Var "Potato")
rt0 = reifyGraph t0

data DistF a
  = ConcatF [a]
  | ConcatMapF String [a]
  | GroupByKeyF [a]
  | InputF FilePath
  deriving (Show, Functor, Foldable, Traversable)

newtype Dist a = Dist (DistF (Dist a)) deriving (Show)

instance MuRef (Dist a) where
  type DeRef (Dist a) = DistF
  mapDeRef f (Dist body) = case body of
    ConcatF xs      -> ConcatF <$> traverse f xs
    ConcatMapF n xs -> ConcatMapF n <$> traverse f xs
    GroupByKeyF xs  -> GroupByKeyF <$> traverse f xs
    InputF fn       -> pure (InputF fn)

data D
  = Concat [D]
  | ConcatMap String [D]
  | GroupByKey [D]
  | Input FilePath
  | Var' Int
  deriving (Show)

share :: Dist a -> IO (IntMap D, D)
share d = do
  Graph nodes s <- reifyGraph d
  let universe = IM.fromList nodes
      refs = insertWith (+) s 1 $ Prelude.foldr (\k -> insertWith (+) (fst k) 1) mempty nodes
      (urefs, mrefs) = IM.partition (==1) refs
      lut = intersectionWith const universe urefs
  return (mapWithKey (\k _ -> expand lut k) mrefs, expand lut s)

expand :: IntMap (DistF Int) -> Int -> D
expand m = go where
  go k = case IM.lookup k m of
    Nothing -> Var' k
    Just d -> case d of
      ConcatF xs      -> Concat (go <$> xs)
      ConcatMapF n xs -> ConcatMap n (go <$> xs)
      GroupByKeyF xs  -> GroupByKey (go <$> xs)
      InputF fn       -> Input fn
