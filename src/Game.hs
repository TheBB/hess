module Game where

import Data.Bits ((.|.))
import Data.List (intercalate, intersperse)
import Data.List.Split (chunksOf)
import Game.BoardMask
import Game.Magic

-- {{{ Types and instances

data Player = White | Black
    deriving (Eq, Show)

data Castle = WKing | WQueen | BKing | BQueen
    deriving (Eq, Show)

data Move = NrmMove Square Square | CstMove Castle
    deriving (Eq, Show)

data GameState = GameState
    { toMove    :: Player
    , wKing     :: BoardMask
    , wQueen    :: BoardMask
    , wRook     :: BoardMask
    , wBishop   :: BoardMask
    , wKnight   :: BoardMask
    , wPawn     :: BoardMask
    , bKing     :: BoardMask
    , bQueen    :: BoardMask
    , bRook     :: BoardMask
    , bBishop   :: BoardMask
    , bKnight   :: BoardMask
    , bPawn     :: BoardMask
    , castles   :: [Castle]
    , enPassant :: BoardMask
    , drawCount :: Int
    , moveNum   :: Int
    }

initState = GameState
    { toMove    = White
    , wKing     = sqToMask e1
    , wQueen    = sqToMask d1
    , wBishop   = sqsToMask [c1, f1]
    , wKnight   = sqsToMask [b1, g1]
    , wRook     = sqsToMask [a1, h1]
    , wPawn     = rank2
    , bKing     = sqToMask e8
    , bQueen    = sqToMask d8
    , bBishop   = sqsToMask [c8, f8]
    , bKnight   = sqsToMask [b8, g8]
    , bRook     = sqsToMask [a8, h8]
    , bPawn     = rank7
    , castles   = [WKing, WQueen, BKing, BQueen]
    , enPassant = 0
    , drawCount = 0
    , moveNum   = 1
    }

-- The colors are inverted since the developer is using a dark terminal
-- This is not an end-user function anyway
instance Show GameState where
    show gs = ('\n':)
         . intercalate "\n" 
         . map (intersperse ' ') 
         . chunksOf 8 
         $ pcs
        where
            pcs = foldr1 (zipWith max) $ map (\(f,c) -> map (iff c '\183') (bitList (f gs)))
                [ (wKing, '\9818'),   (wQueen, '\9819'),  (wRook, '\9820')
                , (wBishop, '\9821'), (wKnight, '\9822'), (wPawn, '\9823')
                , (bKing, '\9812'),   (bQueen, '\9813'),  (bRook, '\9814')
                , (bBishop, '\9815'), (bKnight, '\9816'), (bPawn, '\9817')
                ]
            iff a b True = a
            iff a b False = b

-- }}}

-- {{{ Various functions for working with gamestates

whitePcs gs = foldr1 (.|.) . map ($ gs) $ [wKing, wQueen, wBishop, wKnight, wRook, wPawn]
blackPcs gs = foldr1 (.|.) . map ($ gs) $ [bKing, bQueen, bBishop, bKnight, bRook, bPawn]
myPcs gs  = if toMove gs == White then whitePcs gs else blackPcs gs
oppPcs gs = if toMove gs == Black then whitePcs gs else blackPcs gs

-- }}}
