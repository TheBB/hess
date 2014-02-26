{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module BitBoard where

import Data.Bits (Bits, (.&.), (.|.), shiftL, shiftR, testBit, complement)
import Data.List (intercalate, intersperse)
import Data.List.Split (chunksOf)
import qualified Data.Vector.Generic as GV
    (Vector, basicLength, basicUnsafeSlice, basicUnsafeIndexM, basicUnsafeFreeze, basicUnsafeThaw)
import qualified Data.Vector.Generic.Mutable as GMV
    (MVector, basicLength, basicUnsafeSlice, basicOverlaps, basicUnsafeNew, basicUnsafeRead, basicUnsafeWrite)
import Data.Vector.Unboxed (Vector, MVector, Unbox)
import Data.Word (Word64)
import BitBoardTemplates

-- {{{ Convenience functions

-- Bitwise "minus"
a .&!. b = a .&. complement b

-- Turns a binary representation into a list of bools
bitList :: Bits a => a -> [Bool]
bitList mask = [testBit mask n | n <- [63,62..0]]

-- Number of set bits
countOnes :: Bits a => a -> Int
countOnes = length . filter id . bitList

-- Indices of set bits
whichOnes :: Bits a => a -> [Int]
whichOnes = reverse . map fst . filter snd . zip [63,62..0] . bitList

-- }}}

-- {{{ Boardmask definition

-- A boardmask is nothing but a 64-bit number, one bit for each square
newtype BoardMask = BoardMask Word64
    deriving (Bits, Eq, Num, Real, Enum, Ord, Integral)

instance Show BoardMask where
    show = ('\n':)
         . intercalate "\n" 
         . map (intersperse ' ') 
         . chunksOf 8 
         . map (\b -> if b then '✖' else '·') 
         . bitList

-- This is needed to allow us to work with unboxed vectors of boardmasks
newtype instance MVector s BoardMask = MV_BoardMask (MVector s Word64)
newtype instance Vector BoardMask = V_BoardMask (Vector Word64)

instance GMV.MVector MVector BoardMask where
    basicLength (MV_BoardMask v) = GMV.basicLength v
    basicUnsafeSlice i j (MV_BoardMask v) = MV_BoardMask $ GMV.basicUnsafeSlice i j v
    basicOverlaps (MV_BoardMask v) (MV_BoardMask w) = GMV.basicOverlaps v w
    basicUnsafeNew n = GMV.basicUnsafeNew n >>= (return . MV_BoardMask)
    basicUnsafeRead (MV_BoardMask v) n = GMV.basicUnsafeRead v n >>= (return . BoardMask)
    basicUnsafeWrite (MV_BoardMask v) n (BoardMask val) = GMV.basicUnsafeWrite v n val

instance GV.Vector Vector BoardMask where
    basicLength (V_BoardMask v) = GV.basicLength v
    basicUnsafeSlice i j (V_BoardMask v) = V_BoardMask $ GV.basicUnsafeSlice i j v
    basicUnsafeIndexM (V_BoardMask v) n = GV.basicUnsafeIndexM v n >>= (return . BoardMask)
    basicUnsafeFreeze (MV_BoardMask v) = GV.basicUnsafeFreeze v >>= (return . V_BoardMask)
    basicUnsafeThaw (V_BoardMask v) = GV.basicUnsafeThaw v >>= (return . MV_BoardMask)

instance Unbox BoardMask

-- }}}

-- {{{ Common board masks

-- Files a through h
$(manyIntDecs
    (map (("file" ++) . replicate 1) ['A'..'H'])
    "BoardMask" $ iterate (`shiftR` 1) 0x8080808080808080)

-- Ranks 1 through 8
$(manyIntDecs
    (map (("rank" ++) . replicate 1) ['1'..'8']) 
    "BoardMask" $ iterate (`shiftL` 8) 0xff)

emptyBoard = 0x0 :: BoardMask
fullBoard = 0xffffffffffffffff :: BoardMask

-- }}}

-- {{{ Square definition

newtype Square = Square Int
    deriving (Eq, Num, Ord, Enum)

instance Show Square where
    show (Square sq) = ["hgfedcba" !! (sq `mod` 8), "12345678" !! (sq `div` 8)]

sqToMask :: Square -> BoardMask
sqToMask (Square sq) = 0x01 `shiftL` sq

sqsToMask :: [Square] -> BoardMask
sqsToMask = foldr (.|.) 0 . map sqToMask

-- h8 through a1
$(manyIntDecs
    (zipWith (\a b -> a:b:[]) (cycle ['h','g'..'a']) (concatMap (replicate 8) ['1'..'8']))
    "Square" [0..63])

-- }}}

-- {{{ Simple boardmask operations

-- Shifting a bitboard in a given direction
shiftN  = (.&!. rank1)             . (`shiftL` 8)
shiftS  = (.&!. rank8)             . (`shiftR` 8)
shiftW  = (.&!. fileH)             . (`shiftL` 1)
shiftE  = (.&!. fileA)             . (`shiftR` 1)
shiftNW = (.&!. (fileH .|. rank1)) . (`shiftL` 9)
shiftNE = (.&!. (fileA .|. rank1)) . (`shiftL` 7)
shiftSW = (.&!. (fileH .|. rank8)) . (`shiftR` 7)
shiftSE = (.&!. (fileA .|. rank8)) . (`shiftR` 9)
shifters = [shiftN, shiftS, shiftW, shiftE, shiftNW, shiftNE, shiftSW, shiftSE]

-- This function constructs an extender from a shifter
makeExtender f = foldr1 (.|.) . take 7 . drop 1 . iterate f

-- Extending a bitboard indefinitely in a given direction (useful for sliding pieces)
extenders@[extendN, extendS, extendW, extendE, extendNW, extendNE, extendSW, extendSE] = 
    map makeExtender shifters

-- Convenient lists
orthExtenders = [extendN,  extendS,  extendW,  extendE]
orthShifters  = [shiftN,   shiftS,   shiftW,   shiftE]
diagExtenders = [extendNW, extendNE, extendSW, extendSE]
diagShifters  = [shiftNW,  shiftNE,  shiftSW,  shiftSE]

-- }}}
