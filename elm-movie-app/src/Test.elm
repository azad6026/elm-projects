module Test exposing (..)

import Array
import Html
import Regex


pattern : String
pattern =
    "\\w..\\d\\d:\\d\\d (a\\.m\\.|p\\.m\\.)"


newPaattern : String
newPaattern =
    "\\d\\d:\\d\\d"


caseSensitivePattern : String
caseSensitivePattern =
    "\\j\\u\\l\\y"


options : Regex.Options
options =
    { caseInsensitive = True, multiline = False }


maybeRegex : Maybe Regex.Regex
maybeRegex =
    Regex.fromStringWith options caseSensitivePattern


regex : Regex.Regex
regex =
    Maybe.withDefault Regex.never maybeRegex


apollo11 : String
apollo11 =
    """
   On July 16, 1969, the del09:32 a.m. massive Saturn V rocket 
   lifted off from NASA's Kennedy Space Center at
   del09:32 a.m. EDT. Four days later, on July 20, Neil 
   Armstrong and Buzz Aldrin del09:32 a.m. landed on the Moon.
   """


foundRegx : Bool
foundRegx =
    Regex.contains regex apollo11


launchTimes : List Regex.Match
launchTimes =
    Regex.find regex apollo11


matches : List String
matches =
    List.map (\match -> match.match) launchTimes


finalMatch : String
finalMatch =
    Regex.replace regex (\_ -> "testme") apollo11


splittdPattern : List String
splittdPattern =
    Regex.split regex apollo11


escapeErth : Float -> Float -> String
escapeErth myVelocity mySpeed =
    if myVelocity > 11.186 then
        "Godspeed"

    else if mySpeed == 7.67 then
        "Stay in orbit"

    else
        "Come back"


calculateDays : Int -> String
calculateDays days =
    case days of
        1 ->
            "Monday"

        4 ->
            "Thursday"

        _ ->
            "Friday"


rangeList : List Int
rangeList =
    List.range 1 5


isMember : Bool
isMember =
    List.member "Jayne" [ "Kaylee", "Jayne", "Malcolm" ]


isEMpty : Bool
isEMpty =
    List.isEmpty [ "Dolores", "Teddy" ]


reverseList : List String
reverseList =
    List.reverse [ "Dolores", "Teddy" ]


appendedList : List String
appendedList =
    List.append [ "Dolores", "Teddy" ] [ "Kaylee", "Jayne", "Malcolm" ]


anotherAppendedList : List Int
anotherAppendedList =
    List.append (List.append [ 1, 2 ] [ 3, 4 ]) [ 5, 6 ]


concaatedList : List Int
concaatedList =
    List.concat [ [ 1, 2 ], [ 3, 4 ], [ 5, 6 ] ]


addToFront : List Int
addToFront =
    1 :: [ 2, 3 ]


addToBack : List Int
addToBack =
    [ 1, 2 ] ++ [ 3, 4 ]


pList : Html.Html msg
pList =
    let
        ( before, after ) =
            List.partition (\t -> t >= 3) [ 1, 2, 3, 4, 5, 6, 6, 7, 8, 9 ]
    in
    Html.div []
        [ Html.p [] [ Html.text ("Before: " ++ String.join ", " (List.map String.fromInt before)) ]
        , Html.p [] [ Html.text ("After: " ++ String.join ", " (List.map String.fromInt after)) ]
        ]


isEvil : String -> Bool
isEvil name =
    List.member name [ "Joffrey", "Ramsay" ]


partitionEvil : ( List String, List String )
partitionEvil =
    List.partition isEvil [ "Samwell", "Joffrey", "Hodor", "Ramsay" ]


isOdd : Int -> Bool
isOdd number =
    if remainderBy 2 number /= 0 then
        False

    else
        True


isItOdd : List Int
isItOdd =
    List.filter isOdd [ 0, 1, 2, 3, 4, 5 ]


guardians : List String
guardians =
    [ "Star-lord", "Groot", "Gamora", "Drax", "Rocket" ]


lengths : List Int
lengths =
    List.map String.length guardians


filterLengths : List Int
filterLengths =
    List.filter (\x -> x < 6) lengths


hyphaneGuardins : List Bool
hyphaneGuardins =
    List.map (\x -> String.contains "-" x) guardians


hyphaneGuardians : List Bool
hyphaneGuardians =
    List.map (String.contains "-") guardians


fromBool : Bool -> String
fromBool b =
    if b then
        "True"

    else
        "False"


viewHyphenCheck : Html.Html msg
viewHyphenCheck =
    Html.ul []
        (List.map2
            (\name hasHyphen ->
                Html.li []
                    [ Html.text
                        (name
                            ++ " - "
                            ++ (if hasHyphen then
                                    "✅"

                                else
                                    "❌"
                               )
                        )
                    ]
            )
            guardians
            hyphaneGuardians
        )


powerOfSome : Float
powerOfSome =
    4 ^ 2 ^ 3


makeArray : Array.Array number
makeArray =
    Array.map (\x -> x * 2) (Array.fromList [ 1, 2, 3 ])


arrayGuardians : Array.Array String
arrayGuardians =
    Array.fromList [ "Star-lord", "Groot", "Gamora", "Drax", "Rocket" ]


arrayLengths : Array.Array Int
arrayLengths =
    Array.map String.length arrayGuardians



-- filtering


arrayFilter : Array.Array Int
arrayFilter =
    Array.filter (\x -> x < 6) arrayLengths


arrayIsOdd : Int -> Bool
arrayIsOdd number =
    if remainderBy 2 number == 0 then
        False

    else
        True


filterArrayFilter =
    Array.filter isOdd (Array.fromList [ 0, 1, 2, 3, 4, 5 ])


isHost : String -> Bool
isHost name =
    List.member name [ "Dolores", "Teddy" ]


westWorldCharacters : Array.Array String
westWorldCharacters =
    Array.fromList [ "William", "Dolores", "Teddy" ]


filterNames : Array.Array String
filterNames =
    Array.filter isHost westWorldCharacters


arrayMap : Array.Array number
arrayMap =
    Array.map (\x -> x * 2) (Array.fromList [ 1, 2, 3 ])


myArray : Array.Array number
myArray =
    Array.fromList [ 1, 2, 3, 4 ]


foldLeft : number
foldLeft =
    Array.foldl (+) 0 myArray


foldLeftStar : number
foldLeftStar =
    Array.foldl (*) 1 myArray


foldRight : number
foldRight =
    Array.foldr (+) 0 myArray


foldGuardians : Array.Array String
foldGuardians =
    Array.fromList [ "Star-lord", "Groot", "Gamora", "Drax", "Rocket" ]


outputFold : Int
outputFold =
    Array.foldl (\x a -> String.length x + a) 0 foldGuardians


foldRrigthMyArray : Array.Array number
foldRrigthMyArray =
    Array.fromList [ 4, 2, 3 ]


foldRightMyArray : number
foldRightMyArray =
    Array.foldr (^) 1 myArray



-- Tuples


myTuple : ( Int, String )
myTuple =
    ( 1, "Hello" )



-- wrongTuple : List ( number, number )
-- wrongTuple =  [ ( 1, 2 ), ( 3, 4, 5 ) ]


validateEmail email =
    let
        emailPattern =
            "\\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}\\b"

        regex2 =
            Maybe.withDefault Regex.never <|
                Regex.fromString emailPattern

        isValid =
            Regex.contains regex2 email
    in
    if isValid then
        ( "Valid email", "green" )

    else
        ( "Invalid email", "red" )


outputEmail : ( String, String )
outputEmail =
    validateEmail "thedude@rubix.com"


mpFirst =
    Tuple.mapFirst (\_ -> "Jim") ( "Roy", "Pam" )


mapReverse =
    Tuple.mapFirst String.reverse ( "live", "life" )


mapSecond =
    Tuple.mapSecond (\x -> x + 1) ( "fringe", 100 )


mapContains =
    Tuple.mapSecond (\x -> x + 1) ( "fringe", 100 )


mapBoth =
    Tuple.mapBoth String.length sqrt ( "Newman", 9 )



-- records


type alias TVShow =
    { creator : String, episodes : Int, name : String }



-- .creator = (\record -> record.creator)
-- .episodes = (\record -> record.episodes)
-- .name = (\record -> record.name)
-- firefly =
--     { creator = "Joss Whedon", episodes = 14, name = "Firefly" }


firefly =
    TVShow "Joss Whedon" 14 "Firefly"



-- fringe =
--     { creator = "J. J. Abrams", episodes = 100, name = "Fringe" }


fringe =
    TVShow "J. J. Abrams" 100 "Fringe"


wire =
    { creator = "David Simon", episodes = 60, name = "The Wire" }


sapiens =
    { author = "Yuval Harari", name = "Sapiens", published = 2015 }



-- array of records


tvShows =
    [ firefly, fringe ]


theCretor =
    .creator firefly



-- wrong speial unction name .mom)
-- noSpecialFunction = .mom firefly
-- sorting with Lists


descending a b =
    case compare a b of
        LT ->
            GT

        GT ->
            LT

        EQ ->
            EQ


sortNumbers =
    List.sortWith descending [ 316, 320, 312, 370, 337, 318, 314 ]



--  Sorting records


sortByEpisodes tvShow1 tvShow2 =
    compare tvShow1.episodes tvShow2.episodes


sortIt =
    List.sortWith sortByEpisodes [ fringe, firefly, wire ]



--  Simpleer way to sort records by a field iinside a List


sortItSimple =
    List.sortBy .episodes [ fringe, firefly, wire ]


sortByName =
    List.sortBy String.length [ "Olivia", "Peter", "Walter", "Nina" ]


main : Html.Html msg
main =
    Html.div []
        [ Html.p []
            [ Html.text (escapeErth 11.3 7.3) ]
        , Html.span []
            [ Html.text (String.join "," matches) ]
        , Html.span []
            [ Html.text (calculateDays 4) ]
        , Html.span []
            [ Html.text finalMatch ]
        , Html.ul []
            (List.map (\match -> Html.li [] [ Html.text match ]) reverseList)
        , Html.ul []
            (List.map (\n -> Html.li [] [ Html.text (String.fromInt n) ]) addToBack)
        , pList
        , Html.p []
            [ Html.text (String.join ", " (List.map String.fromInt isItOdd)) ]
        , Html.p []
            (List.map (\b -> Html.li [] [ Html.text (fromBool b) ]) hyphaneGuardians)
        , viewHyphenCheck
        , Html.p []
            [ Html.text (String.fromFloat powerOfSome) ]
        , Html.p []
            [ Html.text (Tuple.first outputEmail ++ "(" ++ Tuple.second outputEmail ++ ")") ]
        , Html.p []
            [ Html.text theCretor ]
        , Html.ul []
            (List.map (\name -> Html.li [] [ Html.text name ]) sortByName)
        ]
