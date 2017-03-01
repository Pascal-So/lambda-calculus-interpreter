import Html exposing (text, Html)


type alias Parser a = (String -> Parsed a)

type alias Parsed a = List (a, String)

pChar : Parser Char
pChar str = 
  case String.uncons str of
    Nothing ->
      []
    Just (fst, rest) ->
      [(fst, rest)]

pWhitespace : Parser ()
pWhitespace str =
  [((), String.trimLeft str)]



(||) : Parser a -> Parser a -> Parser a
(||) a b str =
  a str ++ b str

return : a -> Parser a
return a str = [(a, str)]

(>>=) : Parser a -> (a -> Parser a) -> Parser a
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
  







main = text ""