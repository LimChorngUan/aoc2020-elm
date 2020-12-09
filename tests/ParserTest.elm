module ParserTest exposing (suite)
import Parser exposing (parseListInt, parseListStr, parseNestedArrayString)
import Expect exposing (equal)
import Test exposing (..)
import Array

data1 : String
data1 = """
1
2
3
4
"""


data2 : String
data2 = """
1-3 a: abcde
1-3 b: cdefg
2-9 c: ccccccccc
"""


data3 : String
data3 = """
..#
#..
.#.
"""


suite : Test
suite =
    describe "Parser"
        [ test "parseListInt should transform a long string to a list of integer" <|
              \_ -> parseListInt data1
                  |> equal [1, 2, 3, 4]
        , test "parseListStr should transform a long string to a list of string" <|
              \_ -> parseListStr data2
                  |> equal ["1-3 a: abcde", "1-3 b: cdefg", "2-9 c: ccccccccc"]
        , test "parseNestedArrayString should transform a long string to an array of arrays of string" <|
              \_ -> parseNestedArrayString data3
                  |> equal (Array.fromList [Array.fromList [".", ".", "#"], Array.fromList ["#", ".", "."], Array.fromList [".", "#", "."]])
        ]