{-# LANGUAGE OverloadedStrings #-}

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

lst :: [Float]
lst = [31, 41, 47, 53, 57, 61, 65, 67, 73, 35, 41, 48, 54, 59, 64, 66, 68, 73, 35, 42, 50, 55, 60, 65, 66, 69, 74] -- Essa constante é pra verificar se os cálculos estão sendo operados, seja corretamente ou não

insertLst :: [Float] -> Connection -> IO ()
insertLst [] conn = do close conn
insertLst (x:lst) conn = do
    execute conn "INSERT INTO tables (grupo, valor) VALUES (?,?)" ("dataGroup1" :: String, x :: Float)
    insertLst lst conn

main :: IO ()
main = do
    conn <- open "dataset.db"
    insertLst lst conn
