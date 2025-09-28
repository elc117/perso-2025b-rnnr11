{-# LANGUAGE OverloadedStrings #-}

module Server where

import Classes
import Web.Scotty
import qualified Data.Text.Lazy as TL

lst :: [Float]
lst = [31, 41, 47, 53, 57, 61, 65, 67, 73, 35, 41, 48, 54, 59, 64, 66, 68, 73, 35, 42, 50, 55, 60, 65, 66, 69, 74] -- Essa constante é pra verificar se os cálculos estão sendo operados, seja corretamente ou não

-- Media, moda ou mediana
mmmGet :: [Float] -> (Float, Float, Float) -- (Media, moda, mediana)
mmmGet dataLst = (med, moda, medi)
    where
        dataTable = retornaTabela dataLst
        med = mediaData dataTable
        moda = modaData dataTable
        medi = medianData dataTable

-- Desvio padrão e Coeficiente de Variação
desvioCVGet :: [Float] -> (Float, Float) -- (Desvio Padrão, coeficiente de variação)
desvioCVGet dataLst = (dp, cv)
    where
        dataTable = retornaTabela dataLst
        media = mediaData dataTable
        dp = desvioData dataTable $ varData dataTable media 
        cv = cvData dataTable media

-- Dados de distribuição normal
normalGet :: [Float] -> (Float, Float)
normalGet dataLst = (media, var)
    where
        dataTable = retornaTabela dataLst
        media = mediaData dataTable
        var = varData dataTable media

-- Frequências
