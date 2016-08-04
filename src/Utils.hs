module Utils where

type List a = [a]

printLn :: Show a => a -> IO ()
printLn = putStrLn . show

putStrLnWs :: List String -> IO ()
putStrLnWs = putStrLn . unwords

putStrLns :: List String -> IO ()
putStrLns = putStr . unlines
