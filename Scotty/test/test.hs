{-# LANGUAGE OverloadedStrings #-}
module Main where

import Classes
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Data.Maybe
import Test.HUnit

-- Constantes de resultados esperados
mediaResults :: [Float]
mediaResults = [57.22222, 19.3, 66.28, 167.11111]

medianResults :: [Float]
medianResults = [59, 19, 67, 167]

modaResults :: [Maybe Float]
modaResults = [Just 66, Just 18.333334, Just 71, Nothing]

varResults :: [Float]
varResults = [154.2564, 5.168421, 104.96, 29.281048]

desvioResults :: [Float]
desvioResults = [12.42, 2.273416, 10.244999, 5.4111967]

cvResults :: [Float]
cvResults = [0.21704856, 0.11779358, 0.1545715, 0.0323808832]

-- Testa média, moda e mediana
testMedia :: [Float] -> Int -> [Test] -> [Test] -- Concatena todos os testes de média numa lista só
testMedia [] numTest testLst = testLst
testMedia (x:valores) numTest testLst = testMedia valores (numTest+1) (testLst ++ [TestLabel ("Falha! Valor esperado -> " ++ show (mediaResults !! numTest)) $ objetoCase])
    where objetoCase = TestCase $ assertBool "Média correta" (abs (x - (mediaResults !! numTest)) < 1e-6)

testMedian :: [Float] -> Int -> [Test] -> [Test] -- Concatena todos os testes de mediana numa lista só
testMedian [] numTest testLst = testLst
testMedian (x:valores) numTest testLst = testMedian valores (numTest+1) (testLst ++ [TestLabel ("Falha! Valor esperado -> " ++ show (medianResults !! numTest)) $ objetoCase])
    where objetoCase = TestCase $ assertBool "Mediana correta" (abs (x - (medianResults !! numTest)) < 1e-6)

compareModa :: Maybe Float -> Maybe Float -> Bool
compareModa Nothing Nothing = True
compareModa Nothing result = False
compareModa x Nothing = False
compareModa (Just x) (Just result) = abs (x - result) < 1e-6

testModa :: [Maybe Float] -> Int -> [Test] -> [Test]
testModa [] numTest testLst = testLst
testModa (x:valores) numTest testLst = testModa valores (numTest+1) (testLst ++ [TestLabel ("Falha! Valor esperado -> " ++ show (modaResults !! numTest)) $ objetoCase]) 
    where objetoCase = TestCase $ assertBool "Moda correta" (compareModa x (modaResults !! numTest))

groupMedia :: [[Float]] -> Test -- Agrupa o teste de todas as médias em um único teste
groupMedia dados = TestList $ testMedia mediaLst 0 [] 
    where
        mediaLst = map (mediaData) tableLst
        tableLst = map (retornaTabela) dados

groupMedian :: [[Float]] -> Test -- Agrupa o teste de todas as medianas em um único teste
groupMedian dados = TestList $ testMedian medianLst 0 [] 
    where
        medianLst = map (medianData) tableLst
        tableLst = map (retornaTabela) dados

groupModa :: [[Float]] -> Test -- Agrupa o teste de todas as modas em um único teste
groupModa dados = TestList $ testModa modaLst 0 []
    where
        modaLst = map (modaData) tableLst
        tableLst = map (retornaTabela) dados

-- Testa variância, desvio e coeficiente de variação
testVar :: [Float] -> Int -> [Test] -> [Test] -- Concatena todos os testes de variância numa lista só
testVar [] numTest testLst = testLst
testVar (x:valores) numTest testLst = testVar valores (numTest+1) (testLst ++ [TestLabel ("Falha! Valor esperado -> " ++ show (varResults !! numTest)) $ objetoCase])
    where objetoCase = TestCase $ assertBool "Variância correta" (abs (x - (varResults !! numTest)) < 1e-6)

testDesvio :: [Float] -> Int -> [Test] -> [Test] -- Concatena todos os testes de variância numa lista só
testDesvio [] numTest testLst = testLst
testDesvio (x:valores) numTest testLst = testDesvio valores (numTest+1) (testLst ++ [TestLabel ("Falha! Valor esperado -> " ++ show (desvioResults !! numTest)) $ objetoCase])
    where objetoCase = TestCase $ assertBool "Desvio correto" (abs (x - (desvioResults !! numTest)) < 1e-6)

testCv :: [Float] -> Int -> [Test] -> [Test] -- Concatena todos os testes de variância numa lista só
testCv [] numTest testLst = testLst
testCv (x:valores) numTest testLst = testCv valores (numTest+1) (testLst ++ [TestLabel ("Falha! Valor esperado -> " ++ show (cvResults !! numTest)) $ objetoCase])
    where objetoCase = TestCase $ assertBool "Coeficiente de variação correto" (abs (x - (cvResults !! numTest)) < 1e-6)

groupVar :: [[Float]] -> Test -- Agrupa o teste de todas as variâncias em um único teste
groupVar dados = TestList $ testVar varLst 0 [] 
    where
        varLst = map (\x -> varData x $ mediaData x) tableLst
        tableLst = map (retornaTabela) dados

groupDesvio :: [[Float]] -> Test -- Agrupa o teste de todas as variâncias em um único teste
groupDesvio dados = TestList $ testDesvio desvioLst 0 [] 
    where
        desvioLst = map (\x -> desvioData x $ varData x (mediaData x)) tableLst
        tableLst = map (retornaTabela) dados

groupCv :: [[Float]] -> Test -- Agrupa o teste de todas as variâncias em um único teste
groupCv dados = TestList $ testCv cvLst 0 [] 
    where
        cvLst = map (\x -> cvData x (desvioData x $ varData x (mediaData x)) $ mediaData x) tableLst
        tableLst = map (retornaTabela) dados

groupAll :: [[Float]] -> Test
groupAll dataLst = TestList $ [cv, desvio, var, moda, median, media]
    where
        cv = groupCv dataLst
        desvio = groupDesvio dataLst
        var = groupVar dataLst
        moda = groupModa dataLst
        median = groupMedian dataLst
        media = groupMedia dataLst

-- Pega os dados
consultaData :: [Int] -> Connection -> [[Float]] -> IO() -- Consulta as listas de dados a serem analisados no banco de dados
consultaData [] conn dataLst = do
    resultTestes <- runTestTT $ groupAll dataLst
    print resultTestes

consultaData (x:numGrupo) conn dataLst = do
    let group = "dataGroup" ++ show x
    dados <- query conn "SELECT valor FROM dados WHERE grupo == ?" (Only group) :: IO [Only Float]
    let dataRtrn = map (fromOnly) dados
    consultaData numGrupo conn (dataLst ++ [dataRtrn])

main :: IO ()
main = do
    conn <- open "test/dataset.db"
    consultaData [1, 2, 3, 4] conn []
    close conn
