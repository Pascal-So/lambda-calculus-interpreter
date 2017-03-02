import Html exposing (text, Html)


type alias Parser a = (String -> Parsed a)

type alias Parsed a = List (a, String)

pAnyChar : Parser Char
pAnyChar str = 
  case String.uncons str of
    Nothing ->
      []
    Just (fst, rest) ->
      [(fst, rest)]

pInt : Parser Int
pInt =
  parserMaybe (pChar '-' || pChar '+') >>= \maybeSign ->
  parserMany1 (pOneOf "0123456789") >>= \digits ->
  return (
    let
      num = String.fromList digits 
        |> String.toInt 
        |> Result.withDefault 0
      isMinus = maybeSign == Just '-'
    in
      if isMinus then -num else num
  )

pChar : Char -> Parser Char
pChar c = pSat ((==)c)

pString : String -> Parser String
pString str input =
  let
    len = String.length str
  in
    if String.startsWith str input then
      [(str, String.dropLeft len input )]
    else
      []

pWhitespace : Parser String
pWhitespace = 
  pOneOf " \t\n"
    |> parserMany
    |> parserMap (String.fromList)

pWhile : (Char -> Bool) -> Parser String
pWhile predicate str =
  let
    list = String.toList str
    matching = takeWhile predicate list
      |> String.fromList
    rest = dropWhile predicate list
      |> String.fromList
  in
    [(matching, rest)]

pSat : (Char -> Bool) -> Parser Char
pSat f = 
  pAnyChar
    |> parserFilter f


pOneOf : String -> Parser Char
pOneOf s = 
  (\c -> String.contains (String.fromChar c) s)
    |> pSat



parserMaybe : Parser a -> Parser (Maybe a)
parserMaybe p = 
  parserMap Just p || return Nothing
    |> parserHead

parserMany : Parser a -> Parser (List a)
parserMany p = parserMany1 p || return []

parserMany1 : Parser a -> Parser (List a)
parserMany1 p = p >>= \x ->
                parserMany p >>= \xs ->
                return (x::xs)


(<<<) : Parser a -> Parser b -> Parser a
(<<<) pa pb = 
  pa >>= \a ->
  pb >>= \_ ->
  return a
  
(>>>) : Parser a -> Parser b -> Parser b
(>>>) pa pb = 
  pa >>= \_ ->
  pb >>= \b ->
  return b

(||) : Parser a -> Parser a -> Parser a
(||) a b str =
  let
    resA = a str
  in
    if List.isEmpty resA then
      b str
    else
      resA

return : a -> Parser a
return a str = [(a, str)]

(>>=) : Parser a -> (a -> Parser b) -> Parser b
(>>=) p f str =
  p str
    |> List.map (\(a, rest) -> f a rest)
    |> List.concat


parsedHead : Parsed a -> Parsed a
parsedHead p = 
  case p of
    [] -> []
    (x::xs) -> [x]

parserHead : Parser a -> Parser a
parserHead p = parsedHead << p

parsedMap : (a -> b) -> Parsed a -> Parsed b
parsedMap f = List.map (\(a, str) -> (f a, str))

parserMap : (a -> b) -> Parser a -> Parser b
parserMap f p = parsedMap f << p
  
parsedFilter : (a -> Bool) -> Parsed a -> Parsed a
parsedFilter f = List.filter (\(a, _) -> f a)

parserFilter : (a -> Bool) -> Parser a -> Parser a
parserFilter f p = parsedFilter f << p

parens : Parser a -> Parser a
parens p = pChar '(' >>> p <<< pChar ')'

pEOF : Parser ()
pEOF str =
  case str of
    "" -> [((), "")]
    _  -> []
    



main = text <| toString <| parens (pWhitespace >>> pInt <<< pWhitespace) "(  -2  )"






-- more stuff that should be in stdlibs

takeWhile : (a -> Bool) -> List a -> List a
takeWhile f l =
  case l of
    [] -> []
    (x::xs) ->
      if f x then
        x :: takeWhile f xs
      else
        []

dropWhile : (a -> Bool) -> List a -> List a
dropWhile f l =
  case l of
    [] -> []
    (x::xs) ->
      if f x then
        dropWhile f xs
      else
        x::xs
