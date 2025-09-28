{-# LANGUAGE OverloadedStrings #-}

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

-- O objetivo deesse código é inserir os grupos de dados no banco sem que eu tenha que ficar inserindo manualmente. Ele não vai ser usado na execução do código, mas acho bom manter
-- Como são dados feitos apenas para fim de teste, serão apenas grupos numéricos não nomeados.

lst :: [Float]
lst = [31, 41, 47, 53, 57, 61, 65, 67, 73, 35, 41, 48, 54, 59, 64, 66, 68, 73, 35, 42, 50, 55, 60, 65, 66, 69, 74] 

lst1 :: [Float]
lst1 = [15, 17, 16, 18, 17, 19, 20, 16, 18, 21, 22, 19, 18, 17, 20, 23, 21, 19, 18, 22]

lst2 :: [Float]
lst2 = [45, 52, 60, 71, 68, 55, 49, 63, 77, 81, 74, 66, 59, 72, 69, 54, 62, 80, 75, 70, 58, 65, 61, 78, 73]

lst3 :: [Float]
lst3 = [158, 162, 160, 165, 170, 168, 172, 174, 166, 161, 169, 173, 167, 164, 171, 175, 159, 163]

insertLst :: [Float] -> Int -> Connection -> IO ()
insertLst [] num conn = putStrLn(show num ++ "º Grupo Inserido.")
insertLst (x:lst) num conn = do
    execute conn "INSERT INTO dados (grupo, valor) VALUES (?,?)" ("dataGroup" ++ show num :: String, x :: Float)
    insertLst lst num conn

main :: IO ()
main = do
    conn <- open "dataset.db"
    insertLst lst 1 conn
    insertLst lst1 2 conn
    insertLst lst2 3 conn
    insertLst lst3 4 conn
    close conn
