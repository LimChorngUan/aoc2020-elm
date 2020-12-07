module ParserTest exposing (suite)
import Parser exposing (parseListInt)
import Expect exposing (equal)
import Test exposing (..)


intStr = """
1
2
3
4
"""

suite : Test
suite =
    describe "parseListInt"
        [ test "should transform string to a list of integer" <|
              \_ -> parseListInt intStr
                  |> equal [1, 2, 3 , 4]
        ]