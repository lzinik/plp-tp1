module Tests where

import Documento
import PPON
import Test.HUnit

additionalTests :: Test
additionalTests =
  test
    [ "Ejercicio 2 - Test adicional" ~: testsEj2Adicionales,
      "Ejercicio 3 - Test adicional" ~: testsEj3Adicionales,
      "Ejercicio 4 - Test adicional" ~: testsEj4Adicionales,
      "Ejercicio 6 - Test adicional" ~: testsEj6Adicionales,
      "Ejercicio 7 - Test adicional" ~: testsEj7Adicionales,
      "Ejercicio 8 - Test adicional" ~: testsEj8Adicionales,
      "Ejercicio 9 - Test adicional" ~: testsEj9Adicionales
    ]

testsEj2Adicionales :: Test
testsEj2Adicionales =
  test
    [ vacio <+> texto "a" ~?= texto "a",
      texto "a" <+> vacio ~?= texto "a",
      linea <+> linea <+> texto "a" ~?= linea <+> (linea <+> texto "a"),
      texto "a" <+> linea <+> linea <+> texto "b" ~?= texto "a" <+> (linea <+> (linea <+> texto "b"))
    ]

testsEj3Adicionales :: Test
testsEj3Adicionales =
  test
    [ -- | indentación 0
      indentar 0 (texto "a" <+> linea <+> texto "b") ~?= texto "a" <+> linea <+> texto "b",
      -- | indentación anidada
      indentar 1 (indentar 1 (linea <+> texto "a")) ~?= indentar 2 (linea <+> texto "a"),
      -- | indentación con múltiples líneas
      indentar 4 (linea <+> linea <+> texto "a") ~?= indentar 4 (linea <+> linea <+> texto "a")
    ]

testsEj4Adicionales :: Test
testsEj4Adicionales =
  test
    [ -- | indentación anidada múltiple
      mostrar (indentar 2 (texto "a" <+> indentar 2 (linea <+> texto "b" <+> indentar 2 (linea <+> texto "c")))) ~?= "a\n    b\n      c",
     
      -- | múltiples líneas vacías con indentación
      mostrar (indentar 2 (texto "a" <+> linea <+> linea <+> linea <+> texto "b")) ~?= "a\n  \n  \n  b",
     
      -- | texto vacío con indentación
      mostrar (indentar 2 (texto "" <+> linea <+> texto "b")) ~?= "\n  b",
     
      -- | textos con indentación concatenados
      mostrar (indentar 2 (texto "a" <+> texto "b" <+> linea <+> texto "c" <+> texto "d")) ~?= "ab\n  cd"
    ]

objetoVacio, objetoNesteado3ple, objetoRandom :: PPON
objetoVacio = ObjetoPP []
objetoNesteado3ple = ObjetoPP [("a", ObjetoPP [("b", ObjetoPP [("c", IntPP 1)])])]
objetoRandom = ObjetoPP [("saludo", TextoPP "hola"), ("numero", IntPP 42), ("objvacio", objetoVacio)]

testsEj6Adicionales :: Test
testsEj6Adicionales =
  test
    [ -- | vacío
      pponObjetoSimple objetoVacio ~?= True,
      
      -- | valores atómicos mixtos
      pponObjetoSimple (ObjetoPP [("texto", TextoPP "hola"), ("numero", IntPP 42)]) ~?= True,
      
      -- | solo texto
      pponObjetoSimple (ObjetoPP [("nombre", TextoPP "Juan"), ("apellido", TextoPP "Pérez")]) ~?= True,
      
      -- | solo números
      pponObjetoSimple (ObjetoPP [("edad", IntPP 25), ("altura", IntPP 180)]) ~?= True,
      
      -- | un solo campo atómico
      pponObjetoSimple (ObjetoPP [("solo", TextoPP "único")]) ~?= True,
      
      -- | campos anidados (no simple)
      pponObjetoSimple (ObjetoPP [("a", ObjetoPP [("b", TextoPP "c")])]) ~?= False,
      
      -- | campos mixtos (atómico y no atómico)
      pponObjetoSimple (ObjetoPP [("texto", TextoPP "hola"), ("objeto", ObjetoPP [("a", TextoPP "b")])]) ~?= False,
      
      -- | múltiples niveles
      pponObjetoSimple (ObjetoPP [("a", ObjetoPP [("b", ObjetoPP [("c", TextoPP "d")])])]) ~?= False
    ]

doc1, doc2, doc3 :: Doc
doc1 = texto "hola"
doc2 = linea <+> texto "mundo"
doc3 = texto "!"

testsEj7Adicionales :: Test
testsEj7Adicionales =
  test
    [ -- Tests para mostrar con intercalar. --
      
      -- | lista vacía
      mostrar (intercalar (texto " ") []) ~?= "",
      
      -- | un solo elemento
      mostrar (intercalar (texto " ") [doc1]) ~?= "hola",
      
      -- | dos elementos
      mostrar (intercalar (texto " ") [doc1, doc2]) ~?= "hola \nmundo",
      
      -- | elementos vacíos
      mostrar (intercalar (texto " ") [vacio, doc1, vacio, doc2]) ~?= " hola  \nmundo",
      
      -- | múltiples líneas
      mostrar (intercalar (texto " ") [doc1, linea, doc2, linea, doc3]) ~?= "hola \n \nmundo \n !",
      
      
      -- Tests para mostrar con entreLlaves. --

      -- | lista vacía
      mostrar (entreLlaves []) ~?= "{ }",
      
      -- | un solo elemento
      mostrar (entreLlaves [doc1]) ~?= "{\n  hola\n}",
      
      -- | elementos vacíos
      mostrar (entreLlaves [vacio, vacio]) ~?= "{\n  ,\n  \n}",
      
      -- | múltiples líneas
      mostrar (entreLlaves [doc1, linea, doc2]) ~?= "{\n  hola,\n  \n  ,\n  \n  mundo\n}",
      
      -- | elementos complejos
      mostrar (entreLlaves [indentar 2 doc1, doc2, indentar 4 doc3]) ~?= "{\n  hola,\n  \n  mundo,\n  !\n}"
    ]

testsEj8Adicionales :: Test
testsEj8Adicionales =
  test
    [ -- | documento vacío
      mostrar (aplanar vacio) ~?= "",
     
      -- | documento vacío intermedio
      mostrar (aplanar (doc1 <+> vacio <+> doc2)) ~?= "hola mundo",
     
      -- | múltiples líneas
      mostrar (aplanar (doc1 <+> linea <+> linea <+> doc2)) ~?= "hola   mundo",
     
      -- | líneas y texto
      mostrar (aplanar (doc1 <+> linea <+> doc2 <+> linea <+> doc3)) ~?= "hola  mundo !"
    ]

testsEj9Adicionales :: Test
testsEj9Adicionales =
  test
    [ -- | objeto vacío
      mostrar (pponADoc objetoVacio) ~?= "{ }",
      
      -- | objeto simple con valores atómicos
      mostrar (pponADoc (ObjetoPP [("a", TextoPP "x"), ("b", IntPP 1)])) ~?= "{ \"a\": \"x\", \"b\": 1 }",
      
      -- | objeto con múltiples niveles de anidamiento
      mostrar (pponADoc objetoNesteado3ple) ~?= "{\n  \"a\": {\n    \"b\": { \"c\": 1 }\n  }\n}",
      
      -- | objeto con campos mixtos
      mostrar (pponADoc objetoRandom) ~?= "{\n  \"saludo\": \"hola\",\n  \"numero\": 42,\n  \"objvacio\": { }\n}",
      
      -- | objeto con múltiples objetos anidados
      mostrar (pponADoc (ObjetoPP [("nested", objetoNesteado3ple), ("nested2", objetoNesteado3ple)])) ~?= "{\n  \"nested\": {\n    \"a\": {\n      \"b\": { \"c\": 1 }\n    }\n  },\n  \"nested2\": {\n    \"a\": {\n      \"b\": { \"c\": 1 }\n    }\n  }\n}"
    ] 