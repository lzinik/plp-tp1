module PPON where

import Documento

data PPON
  = TextoPP String
  | IntPP Int
  | ObjetoPP [(String, PPON)]
  deriving (Eq, Show)

pponAtomico :: PPON -> Bool
pponAtomico p = case p of 
        TextoPP s -> True
        IntPP i -> True
        ObjetoPP z -> False

pponObjetoSimple :: PPON -> Bool
pponObjetoSimple (ObjetoPP xs) = foldr (\ x rec -> pponAtomico (snd x) && rec) True xs
pponObjetoSimple _ = False

intercalar :: Doc -> [Doc] -> Doc
intercalar _ [] = vacio
intercalar sep (x:xs) = x <+> foldr (\ d rec -> sep <+> d <+> rec) vacio xs

entreLlaves :: [Doc] -> Doc
entreLlaves [] = texto "{ }"
entreLlaves ds =
  texto "{"
    <+> indentar
      2
      ( linea
          <+> intercalar (texto "," <+> linea) ds
      )
    <+> linea
    <+> texto "}"

aplanar :: Doc -> Doc
aplanar = foldDoc vacio (\s rec -> texto s <+> rec) (\_ rec -> texto " " <+> rec)

pponADoc :: PPON -> Doc
pponADoc p = case p of
    TextoPP s -> texto (show s)
    IntPP i   -> texto (show i)
    ObjetoPP objeto ->
        if (pponObjetoSimple (ObjetoPP objeto))
        then aplanar (objetoToDoc objeto)
        else objetoToDoc objeto
  where
    objetoToDoc :: [(String, PPON)] -> Doc
    objetoToDoc tuplas = entreLlaves (map (\(s, pp) -> texto (show s) <+> texto ": " <+> formato pp) tuplas)

    formato :: PPON -> Doc
    formato pp =
        if pponAtomico pp
        then pponADoc pp
        else if pponObjetoSimple pp
             then aplanar (pponADoc pp)
             else pponADoc pp 