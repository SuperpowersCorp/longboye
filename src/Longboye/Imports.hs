module Longboye.Imports ( clean ) where

clean :: [String] -> IO ()
clean = putStrLn . ("Imports.clean - coming soon - would clean these paths: " ++) . show
