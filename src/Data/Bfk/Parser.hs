module Data.Bfk.Parser (parseInstructions, BfkInstruction (..), ParseInstructionError (..)) where

import Data.Maybe
import qualified Data.Vector as V

-- Brain**kの命令
data BfkInstruction = IncrPtr | DecrPtr | IncrValue | DecrValue | OutputValue | ReadValue | Loop (V.Vector BfkInstruction) deriving (Show, Eq)

data ParseInstructionError = InvalidCharacter Char Int | UnclosedDelimiter Int deriving (Eq) -- エラー定義。

instance Show ParseInstructionError where
  show (InvalidCharacter c i) = c : " is invalid character at " ++ show i ++ "."
  show (UnclosedDelimiter p) = "unclosed delimiter near " ++ show (p + 1)

parseInstructions :: String -> Either ParseInstructionError (V.Vector BfkInstruction)
parseInstructions program = parseInstructionsImpl 0 program $ Right V.empty
  where
    parseInstructionsImpl :: Int -> String -> Either (Int -> ParseInstructionError) (V.Vector BfkInstruction) -> Either ParseInstructionError (V.Vector BfkInstruction)
    parseInstructionsImpl _ [] (Right insts) = Right insts
    parseInstructionsImpl column [] (Left errFunc) = Left $ errFunc column
    parseInstructionsImpl column (c : program) insts =
      case c of
        '[' -> case parseLoopInstruction program $ Right V.empty of -- 直前の命令がループでなく、ループ開始時に場合に呼ばれる
          Right (remainedProgram, loopInsts) ->
            let loop = Right $ Loop loopInsts
                newInsts = (V.++) <$> insts <*> sequence (V.singleton loop)
             in parseInstructionsImpl (column + (length program - length remainedProgram)) remainedProgram newInsts
          Left errFunc -> Left $ errFunc column
        ']' -> Left $ UnclosedDelimiter column
        _ -> case parseUnaryInstruction c of
          Right inst -> parseInstructionsImpl (column + 1) program $ (V.++) <$> insts <*> Right (fromMaybe V.empty (sequence $ V.singleton inst))
          Left errFunc -> Left $ errFunc column
    parseUnaryInstruction :: Char -> Either (Int -> ParseInstructionError) (Maybe BfkInstruction) -- 単項演算子群のパース
    parseUnaryInstruction '>' = Right $ Just IncrPtr
    parseUnaryInstruction '<' = Right $ Just DecrPtr
    parseUnaryInstruction '+' = Right $ Just IncrValue
    parseUnaryInstruction '-' = Right $ Just DecrValue
    parseUnaryInstruction '.' = Right $ Just OutputValue
    parseUnaryInstruction ',' = Right $ Just ReadValue
    parseUnaryInstruction '\t' = Right Nothing 
    parseUnaryInstruction ' ' = Right Nothing 
    parseUnaryInstruction '\n' = Right Nothing 
    parseUnaryInstruction s = Left $ InvalidCharacter s -- 上どれにもマッチしない場合. ループは別処理なのでここにはまらない
    parseLoopInstruction :: String -> Either (Int -> ParseInstructionError) (V.Vector BfkInstruction) -> Either (Int -> ParseInstructionError) (String, V.Vector BfkInstruction)
    parseLoopInstruction [] _ = Left UnclosedDelimiter
    parseLoopInstruction ('[' : program) insts = case parseLoopInstruction program $ Right V.empty of -- ループ処理パース中に子ループに突入する場合に呼ばれる
      Right (remainedProgram, loopInsts) -> parseLoopInstruction remainedProgram $ (V.++) <$> insts <*> Right (V.singleton $ Loop loopInsts) -- これまで得られた命令列(insts)にloop処理 loopInstsをconcatする
      Left s -> Left s
    parseLoopInstruction (']' : program) (Right insts) = sequence (program, Right insts) -- parseLoopInstructionsの終了条件
    parseLoopInstruction (c : cs) insts = parseLoopInstruction cs $ (V.++) <$> insts <*> maybe (Right V.empty) sequence (V.singleton <$> sequence (parseUnaryInstruction c))
