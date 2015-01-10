import System.Environment (getArgs)
import SgfParser
import SgfUtil

main = do input_str <- fmap head getArgs
          case parseSgf igo_parser input_str of
            Left  err -> putStrLn $ show err
            Right val -> putStrLn $ show val
    
