
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

-- Variância, Desvio padrão e Coeficiente de Variação
desvioCVarGet :: [Float] -> (Float, Float, Float) -- (Desvio Padrão, coeficiente de variação)
desvioCVarGet dataLst = (var, dp, cv)
    where
        dataTable = retornaTabela dataLst
        media = mediaData dataTable
        var = varData dataTable media
        dp = desvioData dataTable var 
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
    content <- readFile "input.txt"
    let dataLst = map read (words content) :: [Float]
    return dataLst
