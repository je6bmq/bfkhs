import Data.Bfk.Parser
import qualified Data.Vector as V
import Test.HUnit

main :: IO ()
main = do
  runTestTT $
    TestList
      [parseUnaryInstructionsTest, parsePairInstructionsTest, parseComplicatedInstructionsTest]
  return ()

parseUnaryInstructionsTest :: Test
parseUnaryInstructionsTest =
  TestList
    [ "parse simple increment pointer" ~: parseInstructions ">" ~?= Right (V.singleton IncrPtr),
      "parse simple decrement pointer" ~: parseInstructions "<" ~?= Right (V.singleton DecrPtr),
      "parse simple increment value" ~: parseInstructions "+" ~?= Right (V.singleton IncrValue),
      "parse simple decrement value" ~: parseInstructions "-" ~?= Right (V.singleton DecrValue),
      "parse simple output value" ~: parseInstructions "." ~?= Right (V.singleton OutputValue),
      "parse simple read pointer" ~: parseInstructions "," ~?= Right (V.singleton ReadValue),
      "parse lough multiple instructions" ~: parseInstructions "><+-.," ~?= Right (V.fromList [IncrPtr, DecrPtr, IncrValue, DecrValue, OutputValue, ReadValue])
    ]

parsePairInstructionsTest =
  TestList
    [ "prase only left bracket (error)" ~: parseInstructions "[" ~?= Left (UnclosedDelimiter 0),
      "parse only right bracket (error)" ~: parseInstructions "]" ~?= Left (UnclosedDelimiter 0),
      "parse simple bracket" ~: parseInstructions "[]" ~?= Right (V.singleton (Loop V.empty)),
      "parsenested bracket" ~: parseInstructions "[[[]]]" ~?= Right (V.singleton (Loop (V.singleton (Loop $ V.singleton (Loop V.empty))))),
      "parsemultiple bracket" ~: parseInstructions "[][][]" ~?= Right (V.fromList [Loop V.empty, Loop V.empty, Loop V.empty]),
      "parseinside left bracket (error)" ~: parseInstructions "[[]" ~?= Left (UnclosedDelimiter 0),
      "parse inside right bracket (error)" ~: parseInstructions "[]]" ~?= Left (UnclosedDelimiter 1)
    ]

parseComplicatedInstructionsTest =
  TestList
    [ "parse complicated test 1" ~: parseInstructions "[.,[+-]]" ~?= Right (V.singleton (Loop $ V.fromList [OutputValue, ReadValue, Loop (V.fromList [IncrValue, DecrValue])])),
      "parse complicated test 2" ~: parseInstructions "[[+-]><]" ~?= Right (V.fromList [Loop (V.fromList [Loop $ V.fromList [IncrValue, DecrValue], IncrPtr, DecrPtr])]),
      "parse complicated test 3" ~: parseInstructions "[+-[>-.-<]]" ~?= Right (V.fromList [Loop $ V.fromList [IncrValue, DecrValue, Loop (V.fromList [IncrPtr, DecrValue, OutputValue, DecrValue, DecrPtr])]]),
      "parse complicated test 4" ~: parseInstructions "[+-[>-.-<][]]" ~?= Right (V.fromList [Loop (V.fromList [IncrValue, DecrValue, Loop (V.fromList [IncrPtr, DecrValue, OutputValue, DecrValue, DecrPtr]), Loop V.empty])]),
      "parse complicated test 5" ~: parseInstructions "[>[>[-.-]<]<]" ~?= Right (V.fromList [Loop (V.fromList [IncrPtr, Loop (V.fromList [IncrPtr, Loop (V.fromList [DecrValue, OutputValue, DecrValue]), DecrPtr]), DecrPtr])])
    ]