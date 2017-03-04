module LambdaParser exposing
    ( parseTerm
    , parseVarName
    )


import Char

import Parser exposing ((<*), (*>), (+++), (>>=), Parser)
import Lambda exposing (Term)



-- need to pass str argument explicitly because of eager evaluation
parseTerm : Parser Term
parseTerm str =
    let
        pureTerms = parseApp +++ parseLambda +++ parseVar
    in
        (pureTerms +++ Parser.parens pureTerms) str




parseApp : Parser Term
parseApp =
    let
        appTerm = (parseVar +++ Parser.parens parseTerm) <* Parser.whitespace
    in
        appTerm >>= \func ->
        Parser.many1 appTerm >>= \args ->
        Parser.return (Lambda.multiApp func args)

parseLambda : Parser Term
parseLambda str =
    (Parser.char '\\' *>
    Parser.whitespace *>
    Parser.many1 (parseVarName <* Parser.whitespace) >>= \bound ->
    Parser.char '.' *>
    Parser.whitespace *>
    parseTerm >>= \t ->
    Parser.return (Lambda.multiVarLambda bound t)) str

parseSingleCharVar : Parser Lambda.VarName
parseSingleCharVar =
    Parser.sat isAlphanum >>= \char ->
    Parser.return (String.fromChar char)

parseLongVarName : Parser Lambda.VarName
parseLongVarName =
    Parser.char '`' *>
    Parser.many (Parser.sat isAlphanum) >>= \name ->
    Parser.char '`' *>
    Parser.return (String.fromList name)

parseVarName : Parser Lambda.VarName
parseVarName =
    parseSingleCharVar +++ parseLongVarName

parseVar : Parser Term
parseVar = Parser.map Lambda.Var parseVarName



extractParsedWithDefault : Term -> Parser.Parsed Term -> Term
extractParsedWithDefault t p =
    List.head p
    |> Maybe.map Tuple.first
    |> Maybe.withDefault t


isAlpha : Char -> Bool
isAlpha c =
    (Char.isUpper c) || (Char.isLower c)


isAlphanum : Char -> Bool
isAlphanum c =
    (isAlpha c) || (Char.isDigit c)
