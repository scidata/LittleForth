module Main where
import IO hiding (try)
import Control.Monad
import qualified Data.Map as M
import Text.ParserCombinators.Parsec

data Word = Word String | Num Int deriving (Ord,Eq)
data Def = PrimDef (Stack -> Stack) | Def Stack 

instance Show Word where
    show (Num n) = show n
    show (Word s) = s

type Stack = [Word] 
type Dict = M.Map Word Def
type Env = (Stack,Dict)

basicDict :: Dict
basicDict = M.fromList [(Word "+",PrimDef $ \ (Num x:Num y:rest) -> (Num (y+x)):rest),
                        (Word "-",PrimDef $ \ (Num x:Num y:rest) -> (Num (y-x)):rest),
                        (Word "*",PrimDef $ \ (Num x:Num y:rest) -> (Num (y*x)):rest),
                        (Word "/",PrimDef $ \ (Num x:Num y:rest) -> (Num (y `div` x)):rest),
                        (Word "drop",PrimDef $ \ (_:rest) -> rest),
                        (Word "dup",PrimDef $ \ (x:rest) -> x:x:rest),
                        (Word "flip",PrimDef $ \ (x:y:rest) -> y:x:rest)]

eval :: [Word] -> Env -> Env 
eval [] env = env
eval ((Word ":"):ws) env = makeDef ws env 
eval (w@(Num _):ws) (s,dict) = eval ws (w:s,dict)
eval (w@(Word _):ws) (s,dict) = case M.lookup w dict of 
                                    Just fn -> eval ws $ applyDef fn s dict 
                                    Nothing -> error "Unbound"

makeDef :: [Word] -> Env -> Env 
makeDef ws (s,dict) = eval rest (s,M.insert name (Def body) dict)
    where ((name:body),(_:rest)) = break (==(Word ";")) ws

applyDef :: Def -> [Word] -> Dict -> Env
applyDef (PrimDef fn) args dict = (fn args,dict)
applyDef (Def fn) args dict = eval fn (args,dict)

readWord :: String -> Word 
readWord inp = case parse readExpr "Small.hs" inp of Left _ -> error "Parse error"
                                                     Right val -> val
    where readNum = many1 digit >>= \n -> return $ Num $ read n
          symbol = oneOf "+-*/:;."
          readString = many1 (alphaNum <|> symbol) >>= \s -> return $ Word s
          readExpr :: Parser Word
          readExpr = (try readNum) <|> readString 

runInterpreter :: Env -> IO () 
runInterpreter env@(stack,dict) = do 
  putStr "> " 
  hFlush stdout
  input <- liftM words getLine 
  let env'@(s,_) = eval (map readWord input) env 
  putStrLn $ show $ reverse s
  runInterpreter env'

main :: IO ()
main = runInterpreter ([],basicDict)
