module UtilityFunctions exposing
    ( last
    , transposeTupleMaybe
    , init
    , indexList
    , sequenceResults
    , updateArr
    )

import Array exposing (Array)

last : List a -> Maybe a
last = List.head << List.reverse

transposeTupleMaybe : (Maybe a, Maybe b) -> Maybe (a, b)
transposeTupleMaybe pair =
    case pair of
        (Just a, Just b) -> Just (a, b)
        _                -> Nothing


init : List a -> Maybe (List a)
init = Maybe.map List.reverse << List.tail << List.reverse

indexList : List a -> List (Int, a)
indexList = List.indexedMap (,)

sequenceResults : List (Result x a) -> Result x (List a)
sequenceResults = List.foldr (Result.map2 (::)) (Ok [])

updateArr : Int -> (a -> a) -> Array a -> Array a
updateArr pos func arr =
  Array.get pos arr
  |> Maybe.map func
  |> Maybe.map (\upd -> Array.set pos upd arr)
  |> Maybe.withDefault arr