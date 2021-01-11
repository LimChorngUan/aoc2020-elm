module Day18 exposing (doYourMath)

import Parser exposing ((|.), (|=), Parser)
import Pratt exposing (infixLeft, literal)


doYourMath : String -> Int
doYourMath data =
    data
        |> String.lines
        |> List.filterMap (Result.toMaybe << Parser.run parser)
        |> List.sum


parser : Parser Int
parser =
    Parser.succeed identity
        |= mathExpression
        |. Parser.end


mathExpression : Parser Int
mathExpression =
    Pratt.expression
        { oneOf =
            [ literal Parser.int
            , parenthesizedExpression
            ]
        , andThenOneOf =
            [ infixLeft 1 (Parser.symbol "*") (*)

            -- Change precedence to 1 for part1
            , infixLeft 2 (Parser.symbol "+") (+)
            ]
        , spaces = Parser.spaces
        }


parenthesizedExpression : Pratt.Config Int -> Parser Int
parenthesizedExpression config =
    Parser.succeed identity
        |. Parser.symbol "("
        |= Pratt.subExpression 0 config
        |. Parser.symbol ")"
