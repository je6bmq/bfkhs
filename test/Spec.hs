import Data.Bfk.Parser
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
    [ "parse simple increment pointer" ~: parseInstructions ">" ~?= Right [IncrPtr],
      "parse simple decrement pointer" ~: parseInstructions "<" ~?= Right [DecrPtr],
      "parse simple increment value" ~: parseInstructions "+" ~?= Right [IncrValue],
      "parse simple decrement value" ~: parseInstructions "-" ~?= Right [DecrValue],
      "parse simple output value" ~: parseInstructions "." ~?= Right [OutputValue],
      "parse simple read pointer" ~: parseInstructions "," ~?= Right [ReadValue],
      "parse lough multiple instructions" ~: parseInstructions "><+-.," ~?= Right [IncrPtr, DecrPtr, IncrValue, DecrValue, OutputValue, ReadValue]
    ]

parsePairInstructionsTest =
  TestList
    [ "prase only left bracket (error)" ~: parseInstructions "[" ~?= Left (UnclosedDelimiter 0),
      "parse only right bracket (error)" ~: parseInstructions "]" ~?= Left (UnclosedDelimiter 0),
      "parse simple bracket" ~: parseInstructions "[]" ~?= Right [Loop []],
      "parsenested bracket" ~: parseInstructions "[[[]]]" ~?= Right [Loop [Loop [Loop []]]],
      "parsemultiple bracket" ~: parseInstructions "[][][]" ~?= Right [Loop [], Loop [], Loop []],
      "parseinside left bracket (error)" ~: parseInstructions "[[]" ~?= Left (UnclosedDelimiter 0),
      "parse inside right bracket (error)" ~: parseInstructions "[]]" ~?= Left (UnclosedDelimiter 1)
    ]

parseComplicatedInstructionsTest =
  TestList
    [ "parse complicated test 1" ~: parseInstructions "[.,[+-]]" ~?= Right [Loop [OutputValue, ReadValue, Loop [IncrValue, DecrValue]]],
      "parse complicated test 2" ~: parseInstructions "[[+-]><]" ~?= Right [Loop [Loop [IncrValue, DecrValue], IncrPtr, DecrPtr]],
      "parse complicated test 3" ~: parseInstructions "[+-[>-.-<]]" ~?= Right [Loop [IncrValue, DecrValue, Loop [IncrPtr, DecrValue, OutputValue, DecrValue, DecrPtr]]],
      "parse complicated test 4" ~: parseInstructions "[+-[>-.-<][]]" ~?= Right [Loop [IncrValue, DecrValue, Loop [IncrPtr, DecrValue, OutputValue, DecrValue, DecrPtr], Loop []]],
      "parse complicated test 5" ~: parseInstructions "[>[>[-.-]<]<]" ~?= Right [Loop [IncrPtr, Loop [IncrPtr, Loop [DecrValue, OutputValue, DecrValue], DecrPtr], DecrPtr]]
    ]