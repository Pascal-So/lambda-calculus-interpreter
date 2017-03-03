module Parser exposing (..)

type alias Parser a = (String -> Parsed a)

type alias Parsed a = List (a, String)

parens : Parser a -> Parser a
parens p = char '(' *> p <* char ')'

eof : Parser ()
eof str =
  case str of
    "" -> [((), "")]
    _  -> []

anyChar : Parser Char
anyChar str =
  case String.uncons str of
    Nothing ->
      []
    Just (fst, rest) ->
      [(fst, rest)]

int : Parser Int
int =
  maybe (pChar '-' +++ pChar '+') >>= \maybeSign ->
  many1 (pOneOf "0123456789") >>= \digits ->
  return (
    let
      num = String.fromList digits
        |> String.toInt
        |> Result.withDefault 0
      isMinus = maybeSign == Just '-'
    in
      if isMinus then -num else num
  )

char : Char -> Parser Char
char c = pSat ((==)c)

string : String -> Parser String
string str input =
  let
    len = String.length str
  in
    if String.startsWith str input then
      [(str, String.dropLeft len input )]
    else
      []

whitespace : Parser String
whitespace =
  oneOf " \t\n"
    |> many
    |> map (String.fromList)

while : (Char -> Bool) -> Parser String
while predicate str =
  let
    list = String.toList str
    matching = takeWhile predicate list
      |> String.fromList
    rest = dropWhile predicate list
      |> String.fromList
  in
    [(matching, rest)]

restOfLine : Parser String
restOfLine =
  while ((/=) '\n') <* pChar '\n'

sat : (Char -> Bool) -> Parser Char
sat f =
  anyChar
    |> filter f


oneOf : String -> Parser Char
oneOf s =
  (\c -> String.contains (String.fromChar c) s)
    |> sat



maybe : Parser a -> Parser (Maybe a)
maybe p =
  map Just p +++ return Nothing
    |> head

many : Parser a -> Parser (List a)
many p = many1 p +++ return []

many1 : Parser a -> Parser (List a)
many1 p = p >>= \x ->
                many p >>= \xs ->
                return (x::xs)


(<*) : Parser a -> Parser b -> Parser a
(<*) pa pb =
  pa >>= \a ->
  pb >>= \_ ->
  return a

(*>) : Parser a -> Parser b -> Parser b
(*>) pa pb =
  pa >>= \_ ->
  pb >>= \b ->
  return b

(+++) : Parser a -> Parser a -> Parser a
(+++) a b str =
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

head : Parser a -> Parser a
head p = parsedHead << p

parsedMap : (a -> b) -> Parsed a -> Parsed b
parsedMap f = List.map (\(a, str) -> (f a, str))

map : (a -> b) -> Parser a -> Parser b
map f p = parsedMap f << p

parsedFilter : (a -> Bool) -> Parsed a -> Parsed a
parsedFilter f = List.filter (\(a, _) -> f a)

filter : (a -> Bool) -> Parser a -> Parser a
filter f p = parsedFilter f << p





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
