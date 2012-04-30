module Main where
import IO hiding (try)
import Control.Monad
import qualified Data.Map as M
import Text.ParserCombinators.Parsec

data Word = Word String | Num Int deriving (Ord,Eq)

instance Show Word where
    show (Num n) = show n
    show (Word s) = s

type Stack = [Word] 
type Dict = M.Map Word (Stack -> Stack)
type Env = (Stack,Stack,Dict)

basicDict :: Dict
basicDict = M.fromList [(Word "+",\ (Num x:Num y:rest) -> (Num (x+y)):rest),
                        (Word "-",\ (Num x:Num y:rest) -> (Num (x-y)):rest),
                        (Word "*",\ (Num x:Num y:rest) -> (Num (x*y)):rest),
                        (Word "drop",\ (_:rest) -> rest),
                        (Word "dup",\ (x:rest) -> x:x:rest),
                        (Word "flip",\ (x:y:rest) -> y:x:rest)]

enter :: [Word] -> Env -> Env 
enter [] env = env 
enter i@(w:ws) (d,s,dict) | head (reverse i) == Word "." = eval (d++ws,s,dict)
                          | otherwise = enter ws (d++[w],s,dict)

eval :: Env -> Env
eval e@([],s,dict) = e
eval ((n@(Num _):ws),s,dict) = eval (ws,n:s,dict)
eval ((w@(Word _):ws),s,dict) = case M.lookup w dict of 
                                  Just fn -> (ws,fn s,dict) 
                                -- This needs to recur. 
                                  Nothing -> error "Unbound"

readWord :: String -> Word 
readWord inp = case parse readExpr "Small.hs" inp of Left _ -> error "Parse error"
                                                     Right val -> val
    where readNum = many1 digit >>= \n -> return $ Num $ read n
          symbol = oneOf "+-*/:;."
          readString = many1 (alphaNum <|> symbol) >>= \s -> return $ Word s
          readExpr :: Parser Word
          readExpr = (try readNum) <|> readString 

runInterpreter :: Env -> IO () 
runInterpreter env@(dStack,sStack,dict) = do 
  putStr "> " 
  hFlush stdout
  input <- liftM words getLine 
  let env'@(d,s,_) = enter (map readWord input) env 
  putStrLn $ show d ++ show s
  runInterpreter env'

main :: IO ()
main = runInterpreter ([],[],basicDict)
