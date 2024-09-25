import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad

printLine :: Integer -> IO Integer
printLine 0 =  putStrLn "" >> pure 0
printLine n =  putStr "*" >> printLine (n - 1)

printLine' :: Int -> IO ()
printLine' n = replicateM_ n (putStr "*") >> putStrLn ""
