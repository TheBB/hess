module GameTemplates where

import Language.Haskell.TH

makeUpdaters :: [String] -> Q [Dec]
makeUpdaters (n:ns) = do
    let extFun = mkName ("upd" ++ n)
    let intFun = mkName n
    gs <- newName "gs"
    val <- newName "val"
    let def = ValD (VarP extFun) 
                   (NormalB
                       (LamE 
                           [VarP gs, VarP val]
                           (RecUpdE
                               (VarE gs)
                               [(intFun, VarE val)])))
                   []
    rest <- makeUpdaters ns
    return (def:rest)
makeUpdaters [] = return []
