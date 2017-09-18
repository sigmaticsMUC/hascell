module Display(
  toPicture,
  simpleColoring,
)where

import Graphics.Gloss
import qualified Automaton as A
import qualified Types as T
import Data.Vector (toList)
import Data.ByteString as BS hiding (map)
import Data.ByteString.Internal
import Data.ByteString.Char8 as C8 (pack)
import Data.Char (chr)
import Data.Binary
import Graphics.Gloss.Juicy
import Data.Bits
import Graphics.Gloss.Data.ViewPort


type RGB = [Int]

simpleColoring :: T.Cell -> RGB
{-# INLINE simpleColoring #-}
simpleColoring T.D = [255, 255, 255, 255]
simpleColoring T.L = [0, 0, 0, 255]


colorsToBytes :: [RGB] -> BS.ByteString
{-# INLINE colorsToBytes #-}
--colorsToBytes bytes = BS.concat $ Prelude.map toByte bytes
colorsToBytes bytes = BS.concat $ map toByte bytes


toByte :: RGB -> BS.ByteString
{-# INLINE toByte #-}
toByte rgb = BS.pack $ map fromIntegral rgb


toPicture :: (a->RGB) -> A.Automaton a -> Picture
{-# INLINE toPicture #-}
toPicture f automaton = bitmapOfByteString a b (BitmapFormat TopToBottom PxRGBA) bytes False
  where (a, b) = A._dim automaton
        bytes = colorsToBytes $ map f (toList $ A._automaton automaton)
