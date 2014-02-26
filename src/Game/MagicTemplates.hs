module Game.MagicTemplates
    ( occMasksR
    , magicsR
    , shiftsR
    , occMasksB
    , magicsB
    , shiftsB
    , attackSets
    , Piece (Bishop, Rook)
    ) where

import Data.Bits ((.|.), (.&.), shiftL, shiftR)
import Data.List (sortBy)
import Language.Haskell.TH
import Game.BoardMask

-- {{{ Occupancy masks and magic numbers

-- These are generated a priori and hardcoded

occMasksR =
    [ 0x000101010101017e, 0x000202020202027c, 0x000404040404047a, 0x0008080808080876
    , 0x001010101010106e, 0x002020202020205e, 0x004040404040403e, 0x008080808080807e
    , 0x0001010101017e00, 0x0002020202027c00, 0x0004040404047a00, 0x0008080808087600
    , 0x0010101010106e00, 0x0020202020205e00, 0x0040404040403e00, 0x0080808080807e00
    , 0x00010101017e0100, 0x00020202027c0200, 0x00040404047a0400, 0x0008080808760800
    , 0x00101010106e1000, 0x00202020205e2000, 0x00404040403e4000, 0x00808080807e8000
    , 0x000101017e010100, 0x000202027c020200, 0x000404047a040400, 0x0008080876080800
    , 0x001010106e101000, 0x002020205e202000, 0x004040403e404000, 0x008080807e808000
    , 0x0001017e01010100, 0x0002027c02020200, 0x0004047a04040400, 0x0008087608080800
    , 0x0010106e10101000, 0x0020205e20202000, 0x0040403e40404000, 0x0080807e80808000
    , 0x00017e0101010100, 0x00027c0202020200, 0x00047a0404040400, 0x0008760808080800
    , 0x00106e1010101000, 0x00205e2020202000, 0x00403e4040404000, 0x00807e8080808000
    , 0x007e010101010100, 0x007c020202020200, 0x007a040404040400, 0x0076080808080800
    , 0x006e101010101000, 0x005e202020202000, 0x003e404040404000, 0x007e808080808000
    , 0x7e01010101010100, 0x7c02020202020200, 0x7a04040404040400, 0x7608080808080800
    , 0x6e10101010101000, 0x5e20202020202000, 0x3e40404040404000, 0x7e80808080808000
    ] :: [BoardMask]

magicsR =
    [ 0xa180022080400230, 0x0040100040022000, 0x0080088020001002, 0x0080080280841000
    , 0x4200042010460008, 0x04800a0003040080, 0x0400110082041008, 0x008000a041000880
    , 0x10138001a080c010, 0x0000804008200480, 0x00010011012000c0, 0x0022004128102200
    , 0x000200081201200c, 0x202a001048460004, 0x0081000100420004, 0x4000800380004500
    , 0x0000208002904001, 0x0090004040026008, 0x0208808010002001, 0x2002020020704940
    , 0x8048010008110005, 0x6820808004002200, 0x0a80040008023011, 0x00b1460000811044
    , 0x4204400080008ea0, 0xb002400180200184, 0x2020200080100380, 0x0010080080100080
    , 0x2204080080800400, 0x0000a40080360080, 0x02040604002810b1, 0x008c218600004104
    , 0x8180004000402000, 0x488c402000401001, 0x4018a00080801004, 0x1230002105001008
    , 0x8904800800800400, 0x0042000c42003810, 0x008408110400b012, 0x0018086182000401
    , 0x2240088020c28000, 0x001001201040c004, 0x0a02008010420020, 0x0010003009010060
    , 0x0004008008008014, 0x0080020004008080, 0x0282020001008080, 0x50000181204a0004
    , 0x0102042111804200, 0x40002010004001c0, 0x0019220045508200, 0x020030010060a900
    , 0x0008018028040080, 0x0088240002008080, 0x0010301802830400, 0x00332a4081140200
    , 0x008080010a601241, 0x0001008010400021, 0x0004082001007241, 0x0211009001200509
    , 0x8015001002441801, 0x0801000804000603, 0x0c0900220024a401, 0x0001000200608243
    ] :: [BoardMask]

occMasksB =
    [ 0x0040201008040200, 0x0000402010080400, 0x0000004020100a00, 0x0000000040221400
    , 0x0000000002442800, 0x0000000204085000, 0x0000020408102000, 0x0002040810204000
    , 0x0020100804020000, 0x0040201008040000, 0x00004020100a0000, 0x0000004022140000
    , 0x0000000244280000, 0x0000020408500000, 0x0002040810200000, 0x0004081020400000
    , 0x0010080402000200, 0x0020100804000400, 0x004020100a000a00, 0x0000402214001400
    , 0x0000024428002800, 0x0002040850005000, 0x0004081020002000, 0x0008102040004000
    , 0x0008040200020400, 0x0010080400040800, 0x0020100a000a1000, 0x0040221400142200
    , 0x0002442800284400, 0x0004085000500800, 0x0008102000201000, 0x0010204000402000
    , 0x0004020002040800, 0x0008040004081000, 0x00100a000a102000, 0x0022140014224000
    , 0x0044280028440200, 0x0008500050080400, 0x0010200020100800, 0x0020400040201000
    , 0x0002000204081000, 0x0004000408102000, 0x000a000a10204000, 0x0014001422400000
    , 0x0028002844020000, 0x0050005008040200, 0x0020002010080400, 0x0040004020100800
    , 0x0000020408102000, 0x0000040810204000, 0x00000a1020400000, 0x0000142240000000
    , 0x0000284402000000, 0x0000500804020000, 0x0000201008040200, 0x0000402010080400
    , 0x0002040810204000, 0x0004081020400000, 0x000a102040000000, 0x0014224000000000
    , 0x0028440200000000, 0x0050080402000000, 0x0020100804020000, 0x0040201008040200
    ] :: [BoardMask]

magicsB =
    [ 0x2910054208004104, 0x02100630a7020180, 0x5822022042000000, 0x2ca804a100200020
    , 0x0204042200000900, 0x2002121024000002, 0x80404104202000e8, 0x812a020205010840
    , 0x8005181184080048, 0x1001c20208010101, 0x1001080204002100, 0x1810080489021800
    , 0x0062040420010a00, 0x5028043004300020, 0xc0080a4402605002, 0x08a00a0104220200
    , 0x0940000410821212, 0x001808024a280210, 0x040c0422080a0598, 0x4228020082004050
    , 0x0200800400e00100, 0x020b001230021040, 0x00090a0201900c00, 0x004940120a0a0108
    , 0x0020208050a42180, 0x001004804b280200, 0x2048020024040010, 0x0102c04004010200
    , 0x020408204c002010, 0x02411100020080c1, 0x102a008084042100, 0x0941030000a09846
    , 0x0244100800400200, 0x4000901010080696, 0x0000280404180020, 0x0800042008240100
    , 0x0220008400088020, 0x04020182000904c9, 0x0023010400020600, 0x0041040020110302
    , 0x0412101004020818, 0x8022080a09404208, 0x1401210240484800, 0x0022244208010080
    , 0x1105040104000210, 0x2040088800c40081, 0x8184810252000400, 0x4004610041002200
    , 0x040201a444400810, 0x4611010802020008, 0x80000b0401040402, 0x0020004821880a00
    , 0x8200002022440100, 0x0009431801010068, 0x1040c20806108040, 0x0804901403022a40
    , 0x2400202602104000, 0x0208520209440204, 0x040c000022013020, 0x2000104000420600
    , 0x0400000260142410, 0x0800633408100500, 0x00002404080a1410, 0x0138200122002900
    ] :: [BoardMask]

shiftsR = map ((64-) . countOnes) occMasksR
shiftsB = map ((64-) . countOnes) occMasksB

-- }}}

-- {{{ Bishops and rooks 

data Piece = Bishop | Rook
    deriving (Eq, Show)

occMask Rook (Square sq) = occMasksR !! sq
occMask Bishop (Square sq) = occMasksB !! sq
magic Rook (Square sq) = magicsR !! sq
magic Bishop (Square sq) = magicsB !! sq
shift Rook (Square sq) = shiftsR !! sq
shift Bishop (Square sq) = shiftsB !! sq
exts Rook = orthExtenders
exts Bishop = diagExtenders

-- }}}

-- {{{ Attack sets generation

occVar pc sq i = foldr (.|.) 0
               $ map ((1 `shiftL`) . (whichOnes (occMask pc sq) !!)) (whichOnes i)
numVars pc sq = 1 `shiftL` countOnes (occMask pc sq) :: Int
occVars pc sq = map (occVar pc sq) [0..(numVars pc sq - 1)]
hash pc sq bm = fromIntegral $ (bm * magic pc sq) `shiftR` shift pc sq :: Int
attack pc sq bm = toInteger
                . foldr1 (.|.)
                . map (\e -> let y = e sqbm in y .&!. e (y .&. bm))
                $ exts pc
    where sqbm = sqToMask sq
attackSet pc sq = map snd
                . sortBy (\(a,_) (b,_) -> compare a b)
                . map (\bm -> (hash pc sq bm, attack pc sq bm))
                $ occVars pc sq

attackSets :: Piece -> Q Exp
attackSets pc = do
    runIO $ putStrLn $ "Computing attack sets for " ++ show pc ++ "..."
    return $ bVecCons (ListE [
        uVecCons (ListE [bmCons (LitE (IntegerL n)) | n <- attackSet pc sq])
        | sq <- [h1..a8]])
    where
        bmCons = AppE (ConE (mkName "BoardMask"))
        uVecCons = AppE (VarE (mkName "UV.fromList"))
        bVecCons = AppE (VarE (mkName "BV.fromList"))

-- }}}
