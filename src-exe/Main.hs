import Data.Bravely.FS.Parser
import System.Environment
import System.Exit

main :: IO ()
main = do
    args <- getArgs
    if (null args) then
        getProgName >>= (\n -> putStrLn $ "Usage: " ++ n ++ " <directories to convert>") >> exitFailure
    else
        mapM_ processFsDir args