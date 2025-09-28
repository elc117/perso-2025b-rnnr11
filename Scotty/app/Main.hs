{-# LANGUAGE OverloadedStrings #-}

module Main where

import Server
import Web.Scotty
import qualified Data.Text.Lazy as TL
import Control.Monad.IO.Class (liftIO)

main :: IO ()
main = scotty 3000 $ do
    get "/medmedimod" $ do
        dados <- liftIO requestData 
        text $ TL.pack $ (show (mmmGet dados) ++ "\n")

    get "/desviocvar" $ do
        dados <- liftIO requestData
        text $ TL.pack $ (show (desvioCVarGet dados) ++ "\n")

    get "/normal" $ do
        dados <- liftIO requestData
        text $ TL.pack $ (show (normalGet dados) ++ "\n")

    get "/table" $ do
        dados <- liftIO requestData
        text $ TL.pack $ (show (tabelaGet dados) ++ "\n")
