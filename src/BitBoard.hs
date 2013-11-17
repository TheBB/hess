{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module BitBoard where

import Data.Bits (Bits, (.&.), (.|.), shiftL, shiftR, testBit, complement)
import Data.List (intercalate, intersperse)
import Data.List.Split (chunksOf)
import qualified Data.Vector as V
import Data.Word (Word64)

-- {{{ BoardMask definition

newtype BoardMask = BoardMask Word64
    deriving (Bits, Eq, Num)

bitList :: BoardMask -> [Bool]
bitList mask = [testBit mask n | n <- [63,62..0]]

instance Show BoardMask where
    show = intercalate "\n" . map (intersperse ' ') . chunksOf 8 . map (\b -> if b then '✖' else '·') . bitList

fileVector = V.fromList [mask `shiftL` n | n <- [7,6..0]]
    where mask = BoardMask 0x0101010101010101
rankVector = V.fromList [mask `shiftL` n | n <- [0,8..56]]
    where mask = BoardMask 0x00000000000000ff
lVector gen = V.fromList [foldr (.|.) 0 $ map gen (enumFromThenTo 7 6 (8-n)) | n <- [0..8]]
rVector gen = V.fromList [foldr (.|.) 0 $ map gen (enumFromTo 0 (n-1)) | n <- [0..8]]
filesEVector = lVector file
filesWVector = rVector file
ranksNVector = lVector rank
ranksSVector = rVector rank

file   = (V.!) fileVector
filesE = (V.!) filesEVector
filesW = (V.!) filesWVector
rank   = (V.!) rankVector
ranksN = (V.!) ranksNVector
ranksS = (V.!) ranksSVector

a .&!. b = a .&. complement b
infixl 7 .&!.

mask `bShiftL` n = mask `shiftL` n .&!. filesE (n `mod` 8)
mask `bShiftR` n = mask `shiftR` n .&!. filesW (n `mod` 8)

diagSWNE = (diagSWNEVector V.!)
diagSWNEVector = V.fromList $ map BoardMask
    [ 0x0000000000000001
    , 0x0000000000000102
    , 0x0000000000010204
    , 0x0000000001020408
    , 0x0000000102040810
    , 0x0000010204081020
    , 0x0001020408102040
    , 0x0102040810204080
    , 0x0204081020408000
    , 0x0408102040800000
    , 0x0810204080000000
    , 0x1020408000000000
    , 0x2040800000000000
    , 0x4080000000000000
    , 0x8000000000000000
    ]

diagSENW = (diagSENWVector V.!) 
diagSENWVector = V.fromList $ map BoardMask
    [ 0x0000000000000080
    , 0x0000000000008040
    , 0x0000000000804020
    , 0x0000000080402010
    , 0x0000008040201008
    , 0x0000804020100804
    , 0x0080402010080402
    , 0x8040201008040201
    , 0x4020100804020100
    , 0x2010080402010000
    , 0x1008040201000000
    , 0x0804020100000000
    , 0x0402010000000000
    , 0x0201000000000000
    , 0x0100000000000000
    ]

square m n = file m .&. rank n

kingSquares :: BoardMask -> BoardMask -> BoardMask
kingSquares hard mask = complement hard .&. foldr1 (.|.) 
    [ (mask `shiftL` 1) .&!. filesE 1
    , (mask `shiftR` 1) .&!. filesW 1
    , (mask `shiftL` 8)
    , (mask `shiftL` 9) .&!. filesE 1
    , (mask `shiftL` 7) .&!. filesW 1
    , (mask `shiftR` 8)
    , (mask `shiftR` 7) .&!. filesE 1
    , (mask `shiftR` 9) .&!. filesW 1
    ]

knightSquares :: BoardMask -> BoardMask -> BoardMask
knightSquares hard mask = complement hard .&. foldr1 (.|.)
    [ (mask `shiftL` 10) .&!. filesE 2
    , (mask `shiftL` 17) .&!. filesE 1
    , (mask `shiftL` 15) .&!. filesW 1
    , (mask `shiftL` 6)  .&!. filesW 2
    , (mask `shiftR` 10) .&!. filesW 2
    , (mask `shiftR` 17) .&!. filesW 1
    , (mask `shiftR` 15) .&!. filesE 1
    , (mask `shiftR` 6)  .&!. filesE 2
    ]

pawnSquares :: BoardMask -> BoardMask -> Side -> BoardMask -> BoardMask
pawnSquares soft hard side mask = (captures .&. soft) .|. 
                                  (advances .&!. extend (block .&!. extendR mask) .&!. block)
    where
        (shift, extend, extendR) = if side == White 
                                       then (shiftL, extendN, extendS)
                                       else (shiftR, extendS, extendN)
        baserank = mask .&. rank (if side == White then 1 else 6) /= 0
        advances = mask `shift` 8 .|. (if baserank then mask `shift` 16 else 0)
        captures = case side of 
            White -> (mask `shift` 7 .&!. filesW 1) .|. (mask `shift` 9 .&!. filesE 1)
            Black -> (mask `shift` 7 .&!. filesE 1) .|. (mask `shift` 9 .&!. filesW 1)
        block = soft .|. hard

extendVertical shift mask = foldr1 (.|.) [mask `shift` n | n <- [8,16..56]]
extendN = extendVertical shiftL
extendS = extendVertical shiftR

extendHorizontal shift mask = foldr1 (.|.) [extendHorizontal' n (mask .&. rank n) | n <- [0..7]]
    where extendHorizontal' n mask = rank n .&. foldr1 (.|.) [mask `shift` k | k <- [1..7]]
extendW = extendHorizontal shiftL
extendE = extendHorizontal shiftR

extendDiag shift diag jumps mask = foldr1 (.|.) [extendDiag' n (mask .&. diag n) | n <- [0..14]]
    where extendDiag' n mask = diag n .&. foldr1 (.|.) [mask `shift` k | k <- jumps]
extendNE = extendDiag shiftL diagSWNE [7,14..49]
extendSW = extendDiag shiftR diagSWNE [7,14..49]
extendNW = extendDiag shiftL diagSENW [9,18..63]
extendSE = extendDiag shiftR diagSENW [9,18..63]

orthogonalSquares :: BoardMask -> BoardMask -> BoardMask -> BoardMask
orthogonalSquares soft hard mask = foldr1 (.|.)
    [ extendW mask .&!. extendW (block .&!. extendE mask)
    , extendE mask .&!. extendE (block .&!. extendW mask)
    , extendN mask .&!. extendN (block .&!. extendS mask)
    , extendS mask .&!. extendS (block .&!. extendN mask)
    ] .&!. hard
    where block = soft .|. hard

diagonalSquares :: BoardMask -> BoardMask -> BoardMask -> BoardMask
diagonalSquares soft hard mask = foldr1 (.|.)
    [ extendNW mask .&!. extendNW (block .&!. extendSE mask)
    , extendSE mask .&!. extendSE (block .&!. extendNW mask)
    , extendNE mask .&!. extendNE (block .&!. extendSW mask)
    , extendSW mask .&!. extendSW (block .&!. extendNE mask)
    ] .&!. hard
    where block = soft .|. hard

-- }}}

data Piece = Pawn | Rook | Knight | Bishop | Queen | King
    deriving (Eq)

data Side = White | Black
    deriving (Eq)
