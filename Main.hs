import System.Environment (getArgs, getProgName)

import Parse (parseFile)
import TypeCheck.Infer (infer)
import Javascript (javascript)

compile filename = do
  p <- parseFile filename
  case p of
    Right ast -> do
      case infer ast of
        Right t -> putStrLn $ javascript ast
        Left err -> putStrLn err
    Left err -> putStrLn $ show err

main = do
  args <- getArgs
  prog <- getProgName
  case args of
    [filename] -> compile filename
    _ -> putStrLn $ "Usage: " ++ prog ++ " <filename>"
