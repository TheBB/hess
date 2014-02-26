module BoardMaskTemplates 
    ( manyIntDecs
    ) where

import Language.Haskell.TH

manyIntDecs :: [String] -> String -> [Integer] -> Q [Dec]
manyIntDecs names cons defs = return $ zipWith mkDec namesQ defs
    where
        namesQ = map (VarP . mkName) names
        consQ = ConE (mkName cons)
        mkDec name def = ValD name (NormalB (AppE consQ (LitE (IntegerL def)))) []
