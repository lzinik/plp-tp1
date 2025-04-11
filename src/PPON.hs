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

-- | Utilizamos recursión primitiva dado que en cada paso, necesitamos acceder a la subestructura del objeto (PPON y, segunda posición del ObjetoPP).
pponADoc :: PPON -> Doc
pponADoc (TextoPP s)= texto (show s)
pponADoc (IntPP i)= texto (show i)
pponADoc objeto = if pponObjetoSimple objeto then aplanar (docCreate objeto) else docCreate objeto
    where
      docCreate(ObjetoPP xs)= entreLlaves (map (\ (x, y)-> texto (show x)<+>texto ": "<+>pponADoc y) xs)