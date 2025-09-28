
module Server where

import Classes
import Data.Maybe 
import Text.Read (readMaybe)

-- Media, moda ou mediana
mmmGet :: [Float] -> (Float, Float, Float) -- (Media, moda, mediana)
mmmGet dataLst = (med, moda, medi)
    where
        dataTable = retornaTabela dataLst
        med = mediaData dataTable
        moda = fromMaybe 0 $ modaData dataTable
        medi = medianData dataTable

-- Desvio padrão e Coeficiente de Variação
desvioCVGet :: [Float] -> (Float, Float) -- (Desvio Padrão, coeficiente de variação)
desvioCVGet dataLst = (dp, cv)
    where
        dataTable = retornaTabela dataLst
        media = mediaData dataTable
        dp = desvioData dataTable $ varData dataTable media 
        cv = cvData dataTable dp media

-- Dados de distribuição normal
normalGet :: [Float] -> (Float, Float)
normalGet dataLst = (media, var)
    where
        dataTable = retornaTabela dataLst
        media = mediaData dataTable
        var = varData dataTable media

-- Retorna a tabela
tabelaGet :: [Float] -> [(Float, Float, Int)]
tabelaGet dataLst = retornaTabela dataLst

requestData :: IO [Float]
requestData = do
    content <- readFile "app/input.txt"
    let dataLst = map read (words content) :: [Float]
    return dataLst
