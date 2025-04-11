module Documento
  ( Doc,
    vacio,
    linea,
    texto,
    foldDoc,
    (<+>),
    indentar,
    mostrar,
    imprimir,
  )
where

data Doc
  = Vacio
  | Texto String Doc
  | Linea Int Doc
  deriving (Eq, Show)

vacio :: Doc
vacio = Vacio

linea :: Doc
linea = Linea 0 Vacio

texto :: String -> Doc
texto t | '\n' `elem` t = error "El texto no debe contener saltos de línea"
texto [] = Vacio
texto t = Texto t Vacio

foldDoc :: b -> (String -> b -> b) -> (Int -> b -> b) -> Doc -> b 
foldDoc cVacio cTexto cLinea docu = case docu of
            Vacio     -> cVacio
            Texto s d -> cTexto s (rec d)
            Linea n d -> cLinea n (rec d)
     where rec = foldDoc cVacio cTexto cLinea        

-- NOTA: Se declara `infixr 6 <+>` para que `d1 <+> d2 <+> d3` sea equivalente a `d1 <+> (d2 <+> d3)`
-- También permite que expresiones como `texto "a" <+> linea <+> texto "c"` sean válidas sin la necesidad de usar paréntesis.
infixr 6 <+>

(<+>) :: Doc -> Doc -> Doc
d1 <+> d2 = foldDoc
                d2     -- d1 Vacio
                (\t1 r1 -> case r1 of                         --d1 Texto se fija que viene en la recurcion si viene texto concatena 
                          Texto t2 r2 -> Texto (t1 ++ t2) r2  -- y sigue con esa misma recurcion. cumplo el invariante que no permite (Texto " " (Texto " " (Vacio))
                          _           -> Texto t1 r1)         -- si la recucion trae vacio o linea sigue el mismo camino esto si cumple el invariante  
                (\n1 r1 -> Linea n1 r1)                       -- d1 linea en caso de tener un doc que tiene (Linea 2 (Linea 3)) en invariante no aclara
            d1                   

indentar :: Int -> Doc -> Doc
indentar i = foldDoc Vacio 
                     (\t r -> Texto t r)              -- no tocar los textos
                     (\n r -> Linea (n + i) r)        -- aumentar solo la indentación

espacios :: Int -> String
espacios n = [ ' ' | _ <- [1..n] ]

mostrar :: Doc -> String
mostrar = foldDoc "" (\ t r -> t ++ r) (\ i r -> ('\n' : espacios i) ++ r )

-- | Función dada que imprime un documento en pantalla
imprimir :: Doc -> IO ()
imprimir d = putStrLn (mostrar d) 