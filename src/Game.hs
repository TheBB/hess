module Game where

import Data.Bits ((.|.), shiftL)
import Data.Char (isNumber, ord)
import Data.List (intercalate, intersperse)
import Data.List.Split (chunksOf)
import Game.BoardMask
import Game.Magic

-- {{{ Types and instances

data Player = White | Black
    deriving (Eq)

instance Show Player where
    show White = "w"
    show Black = "b"

instance Read Player where
    readsPrec _ ('w':xs) = [(White, xs)]
    readsPrec _ ('b':xs) = [(Black, xs)]

data Castle = WKing | WQueen | BKing | BQueen
    deriving (Eq)

instance Show Castle where
    show WKing = "K"
    show WQueen = "Q"
    show BKing = "k"
    show BQueen = "q"

instance Read Castle where
    readsPrec _ ('K':xs) = [(WKing, xs)]
    readsPrec _ ('Q':xs) = [(WQueen, xs)]
    readsPrec _ ('k':xs) = [(BKing, xs)]
    readsPrec _ ('q':xs) = [(BQueen, xs)]

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
    , enPassant :: Maybe Square
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
    , enPassant = Nothing
    , drawCount = 0
    , moveNum   = 1
    }

emptyState = GameState White 0 0 0 0 0 0 0 0 0 0 0 0 [] Nothing 0 1

-- The colors are inverted since the developer is using a dark terminal
-- This is not an end-user function anyway
instance Show GameState where
    show gs = (++ "\n" ++ miniFEN gs)
            . ('\n':)
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

wPcsFuns = [wKing, wQueen, wBishop, wKnight, wRook, wPawn]
bPcsFuns = [bKing, bQueen, bBishop, bKnight, bRook, bPawn]

wPcs gs = foldr1 (.|.) . map ($ gs) $ wPcsFuns
bPcs gs = foldr1 (.|.) . map ($ gs) $ bPcsFuns

myOrOp :: (GameState -> a) -> (GameState -> a) -> GameState -> a
myOrOp wf bf gs = if toMove gs == White then wf gs else bf gs

[myKing, myQueen, myBishop, myKnight, myRook, myPawn] = zipWith myOrOp wPcsFuns bPcsFuns
[opKing, opQueen, opBishop, opKnight, opRook, opPawn] = zipWith myOrOp bPcsFuns wPcsFuns
myPcs = myOrOp wPcs bPcs
opPcs = myOrOp bPcs wPcs

miniFEN gs = intercalate " "
    [ show (toMove gs)
    , concatMap show (castles gs)
    , maybe "-" show (enPassant gs)
    , show (drawCount gs)
    , show (moveNum gs)
    ]

toFEN gs = boardString gs ++ " " ++ miniFEN gs
    where 
        boardString gs = intercalate "/"
                       . map compress
                       . chunksOf 8
                       . foldr1 (zipWith max) 
                       . map (\(f,c) -> map (iff c ' ') (bitList (f gs)))
                       $ [ (wKing, 'K'), (wQueen, 'Q'), (wRook, 'R')
                         , (wBishop, 'B'), (wKnight, 'N'), (wPawn, 'P')
                         , (bKing, 'k'), (bQueen, 'q'), (bRook, 'r')
                         , (bBishop, 'b'), (bKnight, 'n'), (bPawn, 'p')
                         ]
          
        compress s@(' ':_) = let (spaces, rest) = span (==' ') s in show (length spaces) ++ compress rest
        compress (x:xs) = x : compress xs
        compress [] = []

        iff a b True = a
        iff a b False = b

fromFEN fen = setupState { toMove = read moveS
                         , castles = map (read . (:[])) castlesS
                         , enPassant = if epS == "-" then Nothing else Just (strToSq epS)
                         , drawCount = read drawCountS
                         , moveNum = read moveNumS
                         }
    where
        [boardS, moveS, castlesS, epS, drawCountS, moveNumS] = words fen
        setupState = acc 63 emptyState boardS

        acc n gs _
            | n < 0 = gs
        acc _ gs [] = gs
        acc n gs ('/':xs) = acc (n - n `mod` 8 - 1) gs xs
        acc n gs (x:xs)
            | isNumber x = acc (next n (ord x - ord '0')) gs xs
        acc n gs ('K':xs) = acc (next n 1) (gs { wKing = wKing gs .|. (1 `shiftL` n) }) xs
        acc n gs ('k':xs) = acc (next n 1) (gs { bKing = bKing gs .|. (1 `shiftL` n) }) xs
        acc n gs ('Q':xs) = acc (next n 1) (gs { wQueen = wQueen gs .|. (1 `shiftL` n) }) xs
        acc n gs ('q':xs) = acc (next n 1) (gs { bQueen = bQueen gs .|. (1 `shiftL` n) }) xs
        acc n gs ('R':xs) = acc (next n 1) (gs { wRook = wRook gs .|. (1 `shiftL` n) }) xs
        acc n gs ('r':xs) = acc (next n 1) (gs { bRook = bRook gs .|. (1 `shiftL` n) }) xs
        acc n gs ('B':xs) = acc (next n 1) (gs { wBishop = wBishop gs .|. (1 `shiftL` n) }) xs
        acc n gs ('b':xs) = acc (next n 1) (gs { bBishop = bBishop gs .|. (1 `shiftL` n) }) xs
        acc n gs ('N':xs) = acc (next n 1) (gs { wKnight = wKnight gs .|. (1 `shiftL` n) }) xs
        acc n gs ('n':xs) = acc (next n 1) (gs { bKnight = bKnight gs .|. (1 `shiftL` n) }) xs
        acc n gs ('P':xs) = acc (next n 1) (gs { wPawn = wPawn gs .|. (1 `shiftL` n) }) xs
        acc n gs ('p':xs) = acc (next n 1) (gs { bPawn = bPawn gs .|. (1 `shiftL` n) }) xs
        acc n gs (_:xs) = acc (next n 1) gs xs
        next n step = max (n - step) (n - n `mod` 8)

-- }}}
