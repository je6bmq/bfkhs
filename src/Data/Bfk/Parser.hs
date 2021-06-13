module Data.Bfk.Parser (parseInstructions, BfkInstruction (..), ParseInstructionError (..)) where

import Control.Applicative

-- Brain**kの命令
data BfkInstruction = IncrPtr | DecrPtr | IncrValue | DecrValue | OutputValue | ReadValue | Loop [BfkInstruction] deriving (Show, Eq)

data ParseInstructionError = InvalidCharacter Char Int | UnclosedDelimiter Int deriving (Eq) -- エラー定義。

instance Show ParseInstructionError where
  show (InvalidCharacter c i) = c : " is invalid character at " ++ show i ++ "."
  show (UnclosedDelimiter p) = "unclosed delimiter near " ++ show (p + 1)

parseInstructions :: String -> Either ParseInstructionError [BfkInstruction]
parseInstructions program = parseInstructionsImpl 0 program $ Right []
  where
    parseInstructionsImpl :: Int -> String -> Either (Int -> ParseInstructionError) [BfkInstruction] -> Either ParseInstructionError [BfkInstruction]
    parseInstructionsImpl _ [] (Right insts) = Right insts
    parseInstructionsImpl column [] (Left errFunc) = Left $ errFunc column
    parseInstructionsImpl column (c : program) insts =
      case c of
        '[' -> case parseLoopInstruction program $ Right [] of -- 直前の命令がループでなく、ループ開始時に場合に呼ばれる
          Right (remainedProgram, loopInsts) ->
            let loop = Right $ Loop loopInsts
                newInsts = (++) <$> insts <*> sequence [loop]
             in parseInstructionsImpl (column + (length program - length remainedProgram)) remainedProgram newInsts
          Left errFunc -> Left $ errFunc column
        ']' -> Left $ UnclosedDelimiter column
        _ -> case parseUnaryInstruction c of
          Right inst -> parseInstructionsImpl (column + 1) program $ (++) <$> insts <*> sequence [Right inst]
          Left errFunc -> Left $ errFunc column
    parseUnaryInstruction :: Char -> Either (Int -> ParseInstructionError) BfkInstruction -- 単項演算子群のパース
    parseUnaryInstruction '>' = Right IncrPtr
    parseUnaryInstruction '<' = Right DecrPtr
    parseUnaryInstruction '+' = Right IncrValue
    parseUnaryInstruction '-' = Right DecrValue
    parseUnaryInstruction '.' = Right OutputValue
    parseUnaryInstruction ',' = Right ReadValue
    parseUnaryInstruction s = Left $ InvalidCharacter s -- 上どれにもマッチしない場合. ループは別処理なのでここにはまらない
    parseLoopInstruction :: String -> Either (Int -> ParseInstructionError) [BfkInstruction] -> Either (Int -> ParseInstructionError) (String, [BfkInstruction])
    parseLoopInstruction [] _ = Left UnclosedDelimiter
    parseLoopInstruction ('[' : program) insts = case parseLoopInstruction program $ Right [] of -- ループ処理パース中に子ループに突入する場合に呼ばれる
      Right (remainedProgram, loopInsts) -> parseLoopInstruction remainedProgram $ (++) <$> insts <*> Right [Loop loopInsts] -- これまで得られた命令列(insts)にloop処理 loopInstsをconcatする
      Left s -> Left s
    parseLoopInstruction (']' : program) (Right insts) = sequence (program, Right insts) -- parseLoopInstructionsの終了条件
    parseLoopInstruction (c : cs) insts = parseLoopInstruction cs $ (++) <$> insts <*> sequence [parseUnaryInstruction c]
