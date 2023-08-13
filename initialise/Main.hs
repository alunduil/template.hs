module Main where

-- TODO Add logging

data Options = Options { name :: String , author :: String, email :: String, licence :: String, path :: String }



main :: IO ()
main = putStrLn "Hello, Haskell!"
