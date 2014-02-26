{-# LANGUAGE TemplateHaskell #-}

module Magic where

import Data.Bits ((.&.), shiftR)
import qualified Data.Vector.Generic as GV (Vector)
import qualified Data.Vector as BV
import qualified Data.Vector.Unboxed as UV
import qualified MagicTemplates as MT
import BoardMask

occMasksR = UV.fromList MT.occMasksR
magicsR =   UV.fromList MT.magicsR
shiftsR =   UV.fromList MT.shiftsR
occMasksB = UV.fromList MT.occMasksB
magicsB =   UV.fromList MT.magicsB
shiftsB =   UV.fromList MT.shiftsB

attackSetsB = $(MT.attackSets MT.Bishop)
attackSetsR = $(MT.attackSets MT.Rook)

attacksR :: Square -> BoardMask -> BoardMask
attacksR (Square sq) bm = (attackSetsR BV.! sq) UV.! hash
    where 
        occmask = occMasksR UV.! sq
        magic = magicsR UV.! sq
        shift = shiftsR UV.! sq
        hash = fromIntegral $ ((bm .&. occmask) * magic) `shiftR` shift :: Int

attacksB :: Square -> BoardMask -> BoardMask
attacksB (Square sq) bm = (attackSetsB BV.! sq) UV.! hash
    where
        occmask = occMasksB UV.! sq
        magic = magicsB UV.! sq
        shift = shiftsB UV.! sq
        hash = fromIntegral $ ((bm .&. occmask) * magic) `shiftR` shift :: Int
