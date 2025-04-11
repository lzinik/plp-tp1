module Tests where

import Documento
import PPON
import Test.HUnit

additionalTests :: Test
additionalTests =
  test
    [ "Ejercicio 2 - Tests Adicionales" ~: testsEj2Adicionales,
      "Ejercicio 3 - Tests Adicionales" ~: testsEj3Adicionales,
      "Ejercicio 4 - Tests Adicionales" ~: testsEj4Adicionales,
      "Ejercicio 6 - Tests Adicionales" ~: testsEj6Adicionales,
      "Ejercicio 7 - Tests Adicionales" ~: testsEj7Adicionales,
      "Ejercicio 8 - Tests Adicionales" ~: testsEj8Adicionales,
      "Ejercicio 9 - Tests Adicionales" ~: testsEj9Adicionales
    ]

testsEj2Adicionales :: Test
testsEj2Adicionales =
  test
    [
      texto "hola" <+> texto " mundo" ~?= texto "hola mundo",
      texto "a" <+> linea <+> texto "b" ~?= texto "a" <+> (linea <+> texto "b")
    ]

testsEj3Adicionales :: Test
testsEj3Adicionales =
  test
    [
      indentar 2 (texto "hola") ~?= texto "hola",
      indentar 2 (linea <+> texto "hola") ~?= linea <+> texto "  hola"
    ]

testsEj4Adicionales :: Test
testsEj4Adicionales =
  test
    [
      mostrar (texto "hola") ~?= "hola",
      mostrar (texto "hola" <+> linea <+> texto "mundo") ~?= "hola\nmundo"
    ]

testsEj6Adicionales :: Test
testsEj6Adicionales =
  test
    [
      pponObjetoSimple objetoVacio ~?= False,
      pponObjetoSimple objetoSimple ~?= True
    ]

testsEj7Adicionales :: Test
testsEj7Adicionales =
  test
    [
      mostrar (intercalar (texto ", ") [a, b]) ~?= "a, b",
      mostrar (entreLlaves [a, b]) ~?= "{\n  a,\n  b\n}"
    ]

testsEj8Adicionales :: Test
testsEj8Adicionales =
  test
    [
      mostrar (aplanar (a <+> linea <+> b)) ~?= "a b",
      mostrar (aplanar (texto "hola" <+> linea <+> texto "mundo")) ~?= "hola mundo"
    ]

testsEj9Adicionales :: Test
testsEj9Adicionales =
  test
    [
      mostrar (pponADoc objetoVacio) ~?= "{ }",
      mostrar (pponADoc objetoSimple) ~?= "{ \"a\": 1, \"b\": \"2\" }"
    ]

a, b, c :: Doc
a = texto "a"
b = texto "b"
c = texto "c"

objetoVacio, objetoSimple :: PPON
objetoVacio = ObjetoPP []
objetoSimple = ObjetoPP [("a", IntPP 1), ("b", TextoPP "2")] 