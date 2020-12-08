module ParserTest exposing (suite)
import Parser exposing (parseListInt, parseListStr)
import Expect exposing (equal)
import Test exposing (..)

intStr : String
intStr = """
1
2
3
4
"""

strStr : String
strStr = """
1-3 a: abcde
1-3 b: cdefg
2-9 c: ccccccccc
"""


suite : Test
suite =
    describe "Parser"
        [ test "parseListInt should transform a long string to a list of integer" <|
              \_ -> parseListInt intStr
                  |> equal [1, 2, 3, 4]
        , test "parseListStr should transform a long string to a list of string" <|
              \_ -> parseListStr strStr
                  |> equal ["1-3 a: abcde", "1-3 b: cdefg", "2-9 c: ccccccccc"]
        ]