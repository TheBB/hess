{-# LANGUAGE TemplateHaskell #-}

module Game where

import Data.Bits ((.|.), shiftL)
import Data.Char (isNumber, ord)
import Data.List (intercalate, intersperse)
import Data.List.Split (chunksOf)
import Data.Maybe (isJust)
import qualified Data.Set as S
import Game.BoardMask
import Game.Magic
import GameTemplates

-- {{{ Types and instances

-- {{{ Player

data Player = White | Black
    deriving (Eq)

instance Show Player where
    show White = "w"
    show Black = "b"

instance Read Player where
    readsPrec _ ('w':xs) = [(White, xs)]
    readsPrec _ ('b':xs) = [(Black, xs)]

-- }}}

-- {{{ Castle, Castles

data Castle = BQueen | BKing | WQueen | WKing
    deriving (Eq, Ord)

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

type Castles = S.Set Castle

-- }}}

-- {{{ Move

data Move = NrmMove Square Square | CstMove Castle
    deriving (Eq, Show)

-- }}}

-- {{{ GameState

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
    , castles   :: Castles
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
    , castles   = S.fromList [WKing, WQueen, BKing, BQueen]
    , enPassant = Nothing
    , drawCount = 0
    , moveNum   = 1
    }

emptyState = GameState White 0 0 0 0 0 0 0 0 0 0 0 0 S.empty Nothing 0 1

$(makeUpdaters [ "wKing", "wQueen", "wRook", "wBishop", "wKnight", "wPawn"
               , "bKing", "bQueen", "bRook", "bBishop", "bKnight", "bPawn"
               ])

-- The colors are inverted since the developer is using a dark terminal
-- This is not an end-user function anyway
pcsMap = [ (wKing,    updwKing,    '\9818',  'K')
         , (wQueen,   updwQueen,   '\9819',  'Q')
         , (wRook,    updwRook,    '\9820',  'R')
         , (wBishop,  updwBishop,  '\9821',  'B')
         , (wKnight,  updwKnight,  '\9822',  'N')
         , (wPawn,    updwPawn,    '\9823',  'P')
         , (bKing,    updbKing,    '\9812',  'k')
         , (bQueen,   updbQueen,   '\9813',  'q')
         , (bRook,    updbRook,    '\9814',  'r')
         , (bBishop,  updbBishop,  '\9815',  'b')
         , (bKnight,  updbKnight,  '\9816',  'n')
         , (bPawn,    updbPawn,    '\9817',  'p')
         ]

unicodePcsMap = map (\(a,_,b,_) -> (a,b)) pcsMap
fenPcsMap     = map (\(a,_,_,b) -> (a,b)) pcsMap

instance Show GameState where
    show gs = (++ "\n" ++ miniFEN gs)
            . ('\n':)
            . intercalate "\n" 
            . map (intersperse ' ') 
            . chunksOf 8 
            $ pcs
        where
            pcs = foldr1 (zipWith max) $ map (\(f,c) -> map (iff c '\183') (bitList (f gs))) unicodePcsMap
            iff a b True = a
            iff a b False = b

-- }}}

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

-- {{{ FEN functions

miniFEN :: GameState -> String
miniFEN gs = intercalate " "
    [ show (toMove gs)
    , S.foldr (\c s -> s ++ show c) "" (castles gs)
    , maybe "-" show (enPassant gs)
    , show (drawCount gs)
    , show (moveNum gs)
    ]

toFEN :: GameState -> String
toFEN gs = boardString gs ++ " " ++ miniFEN gs
    where 
        boardString gs = intercalate "/"
                       . map compress
                       . chunksOf 8
                       . foldr1 (zipWith max) 
                       . map (\(f,c) -> map (iff c ' ') (bitList (f gs)))
                       $ fenPcsMap

        compress s@(' ':_) = let (spaces, rest) = span (==' ') s in show (length spaces) ++ compress rest
        compress (x:xs) = x : compress xs
        compress [] = []

        iff a b True = a
        iff a b False = b

-- This FEN parser is relatively permissive
-- The board state must come first, and ranks must be separated by forward slashes
-- Any character not in KQRBNPkqrbnp or numeric will be assumed to represent a single empty square
-- It is not required to fill out a rank, thus for example // will denote an empty rank
-- The other entries can come in any order, except for the numeric entries:
-- A numeric entry at the end is assumed to denote move number, any other should be the draw counter
-- Except for the board state, fields can be repeated, in which case later occurences will count
fromFEN :: String -> Maybe GameState
fromFEN fen = modify setupState rest
    where
        boardString:rest = words fen
        setupState = acc 63 emptyState boardString

        -- acc forms the actual board state
        acc n gs _
            | n < 0 = gs
        acc _ gs [] = gs

        -- after a forward slash, step to the next rank as measured by mod 8
        acc n gs ('/':xs) = acc (n - n `mod` 8 - 1) gs xs

        -- after a number, step forward that many squares
        acc n gs (x:xs)
            | isNumber x = acc (next n (ord x - ord '0')) gs xs

        -- if a piece is encountered, bitwise or into the correct boardmask
        acc n gs (x:xs)
            | x `elem` pcsString = acc (next n 1) (updater gs (getter gs .|. (1 `shiftL` n))) xs
            where (getter, updater, _, _) = head . filter (\(_,_,_,a) -> a == x) $ pcsMap

        -- silently ignore erroneous pieces
        acc n gs (_:xs) = acc (next n 1) gs xs

        -- prevents us from stepping to the next rank unless we encounter a forward slash
        next n step = max (n - step) (n - n `mod` 8)

        pcsString = "KQRBNPkqrbnp"

        -- modify updates the auxiliary information (castling, etc.)
        modify gs [] = Just gs
        modify gs ([]:rest) = modify gs rest

        -- player to move
        modify gs ("w":rest) = modify gs { toMove = White } rest
        modify gs ("b":rest) = modify gs { toMove = Black } rest

        -- en passant
        modify gs ("-":rest) = modify gs { enPassant = Nothing } rest
        modify gs (sqString:rest)
            | isJust sq = modify gs { enPassant = sq } rest
            where sq = strToSq sqString

        -- castling
        modify gs (cs:rest)
            | all (`elem` "KQkq") cs = modify gs { castles = S.fromList . map (read . (:[])) $ cs } rest

        -- numeric inputs, assume it refers to the draw counter unless it's the final entry
        modify gs (n:rest)
            | all isNumber n = case null rest of
                                   True -> Just gs { moveNum = read n }
                                   False -> modify gs { drawCount = read n } rest

        -- fail on erroneous data
        modify _ _ = Nothing

-- }}}

-- }}}
