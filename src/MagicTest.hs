import BitBoard
import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import Data.List (nub)

occVar :: (Square -> BoardMask) -> Square -> Int -> BoardMask
occVar fn sq i = foldl (.|.) 0 $ map ((0x1 `shiftL`) . (whichOnes (fn sq) !!)) (whichOnes i)

occVars :: (Square -> BoardMask) -> Square -> [BoardMask]
occVars fn sq = map (occVar fn sq) [0..(i-1)]
    where i = 0x1 `shiftL` countOnes (fn sq)

prOccVar :: [BoardMask -> BoardMask] -> (Square -> BoardMask) -> Square -> Int -> BoardMask
prOccVar extenders fn sq i = foldl1 (.|.) $ map prFind extenders
    where
        var = occVar fn sq i
        sqrBM = sqToMask sq
        prFind fn = let y = fn sqrBM .&. var
                    in y .&!. fn y

prOccVars extenders fn sq = map (prOccVar extenders fn sq) [0..(i-1)]
    where i = 0x1 `shiftL` countOnes (fn sq)

rookOccVars     = occVars rookOccupancyMask
bishopOccVars   = occVars bishopOccupancyMask
prRookOccVars   = prOccVars orthExtenders rookOccupancyMask
prBishopOccVars = prOccVars diagExtenders bishopOccupancyMask

-- The following two functions are OK for testing, but too inefficient for brute-force search,
-- should that become necessary.
testRookMagic :: Square -> BoardMask -> Bool
testRookMagic sq magic = length (nub hashes) == length (nub hashesAndProccs)
    where
        hashes = map ((`shiftR` rookMagicShift sq) . (* magic)) $ rookOccVars sq
        hashesAndProccs = zip hashes $ prRookOccVars sq

testBishopMagic :: Square -> BoardMask -> Bool
testBishopMagic sq magic = length (nub hashes) == length (nub hashesAndProccs)
    where
        hashes = map ((`shiftR` bishopMagicShift sq) . (* magic)) $ bishopOccVars sq
        hashesAndProccs = zip hashes $ prBishopOccVars sq

rookMagicOK = all (\sq -> testRookMagic sq (rookMagic sq)) [0..63]
bishopMagicOK = all (\sq -> testBishopMagic sq (bishopMagic sq)) [0..63]
