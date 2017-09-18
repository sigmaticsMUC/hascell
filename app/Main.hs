module Main where

import Graphics.Gloss
import Types (Cell(D, L))
import qualified Automaton as A
import qualified Data.Vector as V
import qualified Display as D
import Graphics.Gloss.Data.ViewPort


toggle :: Cell -> Cell
{-# INLINE toggle #-}
toggle D = L
toggle L = D


rule :: A.Automaton Cell -> A.Automaton Cell
{-# INLINE rule #-}
rule atm = upd
  where indexed = A.indexed atm
        upd = fmap (\(i, e) -> core atm i e) indexed


core :: A.Automaton Cell -> A.Index -> Cell -> Cell
{-# INLINE core #-}
core a i c = cell
  where neighbours = A.getNeighbours a i
        numL = length $ filter (\cc -> L == cc) neighbours
        cell = if numL > 3 || numL < 2 then D else L


automaton :: A.Automaton Cell
{-# INLINE automaton #-}
automaton = A.fromFunction 200 200 (\i -> if isX 200 i then L else D)


isX :: Int -> A.Index -> Bool
{-# INLINE isX #-}
isX dima i = result
  where result = if diagA || diagB then True else False
        row = (div i dima) + 1
        diagA = (mod (i+row) dima) == 0
        diagB = (mod (i+1-row) dima) == 0



automaton' :: A.Automaton Cell
{-# INLINE automaton' #-}
automaton' = A.fromList 10 10 [D, D, D, D, D
                            , D, D, L, D, D
                            , D, L, L, L, D
                            , D, D, L, D, D
                            , D, D, D, D, D
                            , D, D, D, D, D
                            , D, D, L, D, D
                            , D, L, L, L, D
                            , D, D, L, D, D
                            , D, D, D, D, D
                            , D, D, D, D, D
                            , D, D, L, D, D
                            , D, L, L, L, D
                            , D, D, L, D, D
                            , D, D, D, D, D
                            , D, D, D, D, D
                            , D, D, L, D, D
                            , D, L, L, L, D
                            , D, D, L, D, D
                            , D, D, D, D, D]

displayFunction :: A.Automaton Cell -> Picture
{-# INLINE displayFunction #-}
displayFunction = D.toPicture D.simpleColoring


advance :: ViewPort -> Float -> A.Automaton Cell -> A.Automaton Cell
{-# INLINE advance #-}
advance v t automaton = rule automaton


main :: IO ()
main = simulate
  FullScreen
  white
  15
  automaton
  displayFunction
  advance


{-
main :: IO ()
main = display (InWindow "Nice Window" (400, 400) (10, 10)) white (displayFunction automaton)
-}
--newBitMap = bitmapOfByteString 200 200 (BitmapFormat rowOrder)
