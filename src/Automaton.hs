module Automaton(
  Automaton(Automaton, _dim, _automaton),
  Index,
  fromList,
  fromFunction,
  getNeighbours,
  asList,
  asFunction,
  maxIndex,
  indexed,
  insert
)where

import qualified Data.Vector as V


type Index = Int
data Automaton a = Automaton { _automaton :: V.Vector a, _dim :: (Int, Int)}
  deriving(Eq)


instance Show a => Show (Automaton a) where
  show x = show (_automaton x)


instance Functor Automaton where
  fmap f automaton = Automaton {
    _automaton = fmap f $ _automaton automaton
    , _dim = _dim automaton
  }


fromList :: Int -> Int -> [a] -> Automaton a
{-# INLINE fromList #-}
fromList a b l = Automaton {
  _automaton = V.fromList l
  , _dim = (a, b)
}


fromFunction :: Int -> Int -> (Int -> a) -> Automaton a
{-# INLINE fromFunction #-}
fromFunction a b f = Automaton {
  _automaton = V.generate (a*b) f
  , _dim = (a, b)
}


asList :: Automaton a -> [a]
{-# INLINE asList #-}
asList automaton = V.toList $ _automaton automaton


asFunction :: Automaton a -> (Index -> a)
{-# INLINE asFunction #-}
asFunction a = (\i -> cells V.! i)
  where cells = _automaton a


maxIndex :: Automaton a -> Index
{-# INLINE maxIndex #-}
maxIndex automaton = (fst dim) * (snd dim) - 1
  where dim = _dim automaton


indexed :: Automaton a -> Automaton (Index, a)
{-# INLINE indexed #-}
indexed automaton = Automaton {
  _automaton = V.indexed $ _automaton automaton
  , _dim = _dim automaton
}


getNeighbours :: Automaton a -> Index -> [a]
{-# INLINE getNeighbours #-}
getNeighbours a i =  map (\e -> cells V.! e) $ filter checkIndex indexList
  where checkIndex i' = if (i' < 0) || (i' > (maxIndex a)) then False else True
        dim@(dima, dimb) = _dim a
        indexList = getNeighboursIndex' dim i
        cells = _automaton a


getNeighboursIndex' :: (Int, Int) -> Index -> [Index]
{-# INLINE getNeighboursIndex' #-}
getNeighboursIndex' (a, b) i
  | (mod i a) == 0 = leftSide a i
  | a - (mod i a) == 1 = rightSide a i
  | otherwise = center a i


insert :: a -> Index -> Automaton a -> Automaton a
insert ele i = undefined


leftSide :: Int -> Index -> [Index]
{-# INLINE leftSide #-}
leftSide a i = [i+1, i-a, i+a, i-a+1, i+a+1]


rightSide :: Int -> Index -> [Index]
{-# INLINE rightSide #-}
rightSide a i = [i-1, i-a, i+a, i-a-1, i+a-1]


center :: Int -> Index -> [Index]
{-# INLINE center #-}
center a i = [i-1, i-a, i+a, i-a-1, i+a-1, i+1, i-a+1, i+a+1]
