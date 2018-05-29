module PatternSearch exposing (Match, Threshold, filterList)

import EditDistance exposing (levenshtein)
import List.Extra exposing (minimumBy)
import String exposing (fromList)


type alias Text =
    String


type alias Pattern =
    String


type alias Length =
    Int


type alias Distance =
    Int


type alias CharList =
    List Char


type alias Match obj =
    { words : List ( Text, Pattern, Distance )
    , element : obj
    }


type alias Threshold =
    { maxDistance : Distance
    , minMatches : Int
    }


type alias Tokenizer =
    String -> List String


type alias Stringifier obj =
    obj -> String


second : ( a, b, c ) -> b
second ( _, b, _ ) =
    b


third : ( a, b, c ) -> c
third ( _, _, c ) =
    c


matchPattern : Threshold -> List ( CharList, Length ) -> ( CharList, Length ) -> Maybe ( Text, Pattern, Distance )
matchPattern { maxDistance } patternTokens ( textToken, textLength ) =
    let
        filter : ( CharList, Length ) -> Maybe ( Text, Pattern, Distance )
        filter ( patternToken, patternLength ) =
            let
                distance : Int
                distance =
                    if abs (textLength - patternLength) > maxDistance then
                        maxDistance + 1
                    else
                        levenshtein textToken patternToken
            in
                if distance > maxDistance then
                    Nothing
                else
                    Just ( fromList textToken, fromList patternToken, distance )
    in
        List.filterMap filter patternTokens
            |> minimumBy third


matchText : Threshold -> List ( CharList, Length ) -> List ( CharList, Length ) -> List ( Text, Pattern, Distance )
matchText threshold patternTokens textTokens =
    let
        matches : List ( Text, Pattern, Distance )
        matches =
            List.filterMap (matchPattern threshold patternTokens) textTokens
                |> List.sortBy third
                |> List.Extra.uniqueBy second
    in
        if List.length matches < threshold.minMatches then
            []
        else
            matches


matchObj : Tokenizer -> Stringifier obj -> Threshold -> List ( CharList, Length ) -> obj -> Maybe (Match obj)
matchObj tokenizer stringifier threshold patternTokens obj =
    let
        textTokens : List ( CharList, Length )
        textTokens =
            stringifier obj
                |> tokenizer
                |> List.map (\s -> ( String.toList s, String.length s ))
    in
        case matchText threshold patternTokens textTokens of
            [] ->
                Nothing

            matches ->
                Just { words = matches, element = obj }


filterList : Tokenizer -> Stringifier obj -> Threshold -> String -> List obj -> List (Match obj)
filterList tokenizer stringifier threshold pattern list =
    let
        matcher : obj -> Maybe (Match obj)
        matcher =
            tokenizer pattern
                |> List.map (\s -> ( String.toList s, String.length s ))
                |> matchObj tokenizer stringifier threshold
    in
        List.filterMap matcher list
