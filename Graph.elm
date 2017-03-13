module Graph exposing 
    ( Graph
    , showGraph
    , addEdges
    , addEdge
    , fromVerts
    , getValue
    , hasLoop
    , dfs
    , topoSort
    , getOutEdges
    , getSize
    )

import Array exposing (Array)
import Set exposing (Set)

indexList : List a -> List (Int, a)
indexList = List.indexedMap (,)


updateArr : Int -> (a -> a) -> Array a -> Array a
updateArr pos func arr =
  Array.get pos arr
  |> Maybe.map func
  |> Maybe.map (\upd -> Array.set pos upd arr)
  |> Maybe.withDefault arr
  

type alias Graph a = Array (a, Set Int)

showGraph : Graph a -> List String
showGraph graph = 
    Array.toList graph
    |> List.map (\(val, conns) -> 
           toString val ++ " - " ++ toString (Set.toList conns)
       )


addEdge : Int -> Int -> Graph a -> Graph a
addEdge a b =
  updateArr a (Tuple.mapSecond (Set.insert b))

addEdges : Int -> Set Int -> Graph a -> Graph a
addEdges a bs =
  updateArr a (Tuple.mapSecond (Set.union bs))
  

fromVerts : List a -> Graph a
fromVerts lst =
  lst
  |> List.map (\node -> (node, Set.empty))
  |> Array.fromList


getOutEdges : Int -> Graph a -> Maybe (Set Int)
getOutEdges id = Maybe.map Tuple.second << Array.get id

getValue : Int -> Graph a -> Maybe a
getValue id = Maybe.map Tuple.first << Array.get id

mapHead : (a -> a) -> List a -> List a
mapHead f lst =
    case lst of
        []      -> []
        (x::xs) -> (f x) :: xs

topoSort_ : Array Bool -> Int -> List Int -> Graph g -> (Array Bool, List Int)
topoSort_ seen id sorted graph =
    if Array.get id seen |> Maybe.withDefault False then -- already seen
        (seen, sorted)
    else
        let
            (newSeen, newSorted) = getOutEdges id graph
                |> Maybe.withDefault Set.empty
                |> Set.toList
                |> List.foldl (\id (seen, sorted) -> topoSort_ seen id sorted graph) ((Array.set id True seen), sorted)
        in
            (newSeen, id::newSorted)
    

topoSort : Graph a -> List Int
topoSort graph =
    let
        size = getSize graph
        seen = Array.repeat size False
    in
        List.range 0 (size-1)
        |> List.foldl (\id (seen, sorted) -> topoSort_ seen id sorted graph) (seen, [])
        |> Tuple.second


getFirstId : (a -> Bool) -> Array a -> Maybe Int
getFirstId pred arr =
    Array.toIndexedList arr
    |> List.filter (\(id, val) ->
           pred val
       )
    |> List.map Tuple.first
    |> List.head
    
getSize : Graph a -> Int
getSize = Array.length


hasLoop_ : Array Bool -> Array Bool -> Int -> Graph a -> (Array Bool, Bool)
hasLoop_ active seen currentId graph =
    let
        alreadySeen = Array.get currentId seen |> Maybe.withDefault False
        alreadyActive = Array.get currentId active |> Maybe.withDefault False
    in
        if alreadySeen then
            (seen, alreadyActive)
        else
            let
                newSeen = Array.set currentId True seen
                newActive = Array.set currentId True active
            in
                getOutEdges currentId graph
                |> Maybe.withDefault Set.empty
                |> Set.foldl (\id (seen, loop) -> 
                       let
                           (newSeen, newLoop) = hasLoop_ newActive seen id graph
                       in
                           (newSeen, newLoop || loop)
                   ) (newSeen, False)
        

hasLoop : Graph a -> Bool
hasLoop graph = 
    let
        size = getSize graph
        noneSeen = Array.repeat size False
        noneActive = noneSeen
        visitRest seen =
            case getFirstId not seen of
                Nothing -> False
                Just id ->
                    let
                        (newSeen, loop) = hasLoop_ noneActive seen id graph
                    in
                        if loop then
                            True
                        else
                            visitRest newSeen
    in
        visitRest noneSeen


dfs_ : (Int -> a -> b -> b) -> b -> Array Bool -> Int -> Graph a -> (Array Bool, b)
dfs_ onDiscover state seen currentId graph =
    let
        alreadySeen = Array.get currentId seen |> Maybe.withDefault False
    in
        if alreadySeen then
            (seen, state)
        else
            let
                newSeen = Array.set currentId True seen
                newState = getValue currentId graph
                    |> Maybe.map (\val ->
                            onDiscover currentId val state
                        )
                    |> Maybe.withDefault state
            in
                getOutEdges currentId graph
                |> Maybe.withDefault Set.empty
                |> Set.foldl (\id (seen, state) -> 
                       dfs_ onDiscover state seen id graph
                   ) (newSeen, newState)



dfs : (Int -> a -> b -> b) -> b -> Graph a -> b
dfs onDiscover state graph = 
    let
        size = getSize graph
        noneSeen = Array.repeat size False
        visitRest seen state =
            case getFirstId not seen of
                Nothing -> state
                Just id ->
                    let
                        (nSeen, nState) = dfs_ onDiscover state seen id graph
                    in
                        visitRest nSeen nState
    in
        visitRest noneSeen state

prefixSum : List number -> List number
prefixSum = Maybe.withDefault [] << List.tail << List.scanl (+) 0

arrayPrefixSum : Array number -> Array number
arrayPrefixSum = Array.fromList << prefixSum << Array.toList
  

getSubGraph : Set Int -> Graph a -> Graph a
getSubGraph keep graph =
    let
        oldSize = getSize graph
        pos = Set.foldl (\id -> Array.set id 1) (Array.repeat oldSize 0) keep
            |> arrayPrefixSum
            |> Array.map ((+)1)
        mapIndex id = Array.get id pos
          |> Maybe.withDefault 0
    in
        Array.toList graph
        |> indexList
        |> List.filter (\(id, val) ->
              Set.member id keep
           )
        |> List.map Tuple.second
        |> List.map (Tuple.mapSecond (Set.map mapIndex))
        |> Array.fromList
