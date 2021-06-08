module Main exposing (init, main)

import Browser
import FontAwesome.Attributes as Icon
import FontAwesome.Brands as Icon
import FontAwesome.Icon as Icon exposing (Icon)
import FontAwesome.Solid as Icon
import Html exposing (..)
import Html.Attributes exposing (alt, class, href, src, style, type_)
import Html.Events exposing (onClick)
import Http
import Json.Decode exposing (Decoder, Error(..), field, string)
import Random
import RemoteData exposing (WebData)


type alias Flags =
    { searchServiceUrl : String
    , searchApiKey : String
    , version : String
    }


type alias Model =
    { searchServiceUrl : String
    , searchApiKey : String
    , count : Int
    , sign : Maybe SignResult
    , version : String
    }


type alias Highway =
    { highway : String
    , imageName : String
    }


type alias SignResult =
    { url : String
    , title : String
    , description : String
    , highways : List Highway
    , taxonomy : String
    , countyType : String
    , county : String
    , state : String
    }


type Msg
    = GotCount (WebData String)
    | GotSign (WebData (List SignResult))
    | RandomUpdated Int
    | Refresh


randomOffset : Int -> Random.Generator Int
randomOffset count =
    Random.int 0 (count - 1)


newRandom : Int -> Cmd Msg
newRandom max =
    Random.generate RandomUpdated (randomOffset max)


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { searchServiceUrl = flags.searchServiceUrl
      , searchApiKey = flags.searchApiKey
      , count = 0
      , sign = Maybe.Nothing
      , version = flags.version
      }
    , countRequest flags.searchServiceUrl flags.searchApiKey
    )


buildCountUrl : String -> String -> String
buildCountUrl url apiKey =
    url ++ "/docs/$count?&api-version=2020-06-30-Preview&api-key=" ++ apiKey


buildSignUrl : String -> String -> Int -> String
buildSignUrl url apiKey offset =
    url ++ "/docs?api-version=2020-06-30-Preview&api-key=" ++ apiKey ++ "&$skip=" ++ String.fromInt offset ++ "&$top=1"


highwayDecoder : Decoder (List Highway)
highwayDecoder =
    Json.Decode.list
        (Json.Decode.map2 Highway
            (field "Highway" string)
            (field "ImageName" string)
        )


signDecoder : Decoder (List SignResult)
signDecoder =
    field "value"
        (Json.Decode.list
            (Json.Decode.map8 SignResult
                (field "ImageId" string)
                (field "Title" string)
                (field "Description" string)
                (field "Highways" highwayDecoder)
                (field "Taxonomy" string)
                (field "CountyType" string)
                (field "County" string)
                (field "State" string)
            )
        )


countRequest : String -> String -> Cmd Msg
countRequest url apiKey =
    Http.get
        { url = buildCountUrl url apiKey
        , expect = Http.expectString (RemoteData.fromResult >> GotCount)
        }


signRequest : String -> String -> Int -> Cmd Msg
signRequest url apiKey offset =
    Http.get
        { url = buildSignUrl url apiKey offset
        , expect = Http.expectJson (RemoteData.fromResult >> GotSign) signDecoder
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Refresh ->
            ( model, newRandom model.count )

        RandomUpdated random ->
            ( model, signRequest model.searchServiceUrl model.searchApiKey random )

        GotSign signList ->
            case signList of
                RemoteData.Success signs ->
                    let
                        sign =
                            List.head signs
                    in
                    ( { model | sign = sign }, Cmd.none )

                _ ->
                    ( { model | sign = Maybe.Nothing }, Cmd.none )

        GotCount result ->
            case result of
                RemoteData.Success c ->
                    let
                        countRes =
                            String.toInt c
                    in
                    case countRes of
                        Just count ->
                            ( { model | count = count }, newRandom count )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )


viewSignTitle : String -> Html Msg
viewSignTitle title =
    div [ class "card-header" ]
        [ div [ class "card-header-title" ]
            [ h3 [ class "title has-text-centered has-text-weight-medium is-4" ]
                [ text title
                ]
            ]
        ]


viewSignImage : String -> String -> Html Msg
viewSignImage imageId title =
    let
        url =
            "https://sign.sagebrushgis.com/" ++ imageId ++ "/" ++ imageId ++ "_l.jpg"
    in
    div [ class "card-image" ]
        [ figure [ class "image is-4by3 width" ]
            [ img [ src url, style "opacity" "1", style "transition" "opacity 0.15s linear 0s", alt title ]
                []
            ]
        ]


viewLocationItem : String -> Html Msg
viewLocationItem location =
    span [ class "is-size-7 has-text-weight-light is-underline pr-1" ]
        [ text location
        ]


viewSignDescription : String -> Html Msg
viewSignDescription desc =
    div [ class "card-content" ]
        [ div [ class "content" ]
            [ p [ class "is-size-6" ]
                [ text desc
                ]
            ]
        ]


viewHighway : Highway -> Html Msg
viewHighway highway =
    img [ class "shield-padding", alt highway.highway, src ("https://shield.sagebrushgis.com/Shields/20x/" ++ highway.imageName) ]
        []


viewLocation : SignResult -> List (Html Msg)
viewLocation sign =
    [ List.map (\h -> viewHighway h) sign.highways
    , [ Icon.viewIcon Icon.mapMarkerAlt ]
    , String.split "|" sign.taxonomy |> fixLocation sign.state sign.county sign.countyType |> List.map (\l -> viewLocationItem l)
    ]
        |> List.concat


fixLocation : String -> String -> String -> List String -> List String
fixLocation state county countyType location =
    let
        rev =
            List.reverse location

        country =
            Maybe.withDefault "" (List.head rev)

        place =
            Maybe.withDefault "" (List.head location)

        count =
            List.length location
    in
    if count < 4 then
        [ county ++ " " ++ countyType, state, country ]

    else
        [ place, county ++ " " ++ countyType, state, country ]


viewSignHighways : SignResult -> Html Msg
viewSignHighways sign =
    nav [ class "card-footer level" ]
        [ div [ class "level-left" ]
            [ div [ class "level-item" ]
                (viewLocation sign)
            ]
        ]


viewSign : SignResult -> Html Msg
viewSign res =
    div [ class "card" ]
        [ viewSignTitle res.title
        , viewSignImage res.url res.title
        , viewSignDescription res.description
        , viewSignHighways res
        ]


viewCountOrError : Model -> Html Msg
viewCountOrError model =
    case model.sign of
        Nothing ->
            text ""

        Just results ->
            viewSign results


viewFooter : String -> Html Msg
viewFooter version =
    footer [ class "footer" ]
        [ div [ class "content has-text-centered" ]
            [ div [ class "level" ]
                [ div [ class "level-lefr" ]
                    [ div [ class "level-item" ]
                        [ a [ href "https://github.com/zmaillard/random-highway-sign-elm" ]
                            [ Icon.github |> Icon.present |> Icon.styled [ Icon.fa2x ] |> Icon.view ]
                        ]
                    ]
                , div [ class "level-right" ]
                    [ div [ class "level-item" ]
                        [ a [ href "https://random.roadsign.pictures" ]
                            [ text "random.roadsign.pictures"
                            ]
                        ]
                    , div [ class "level-item" ]
                        [ text ("(v " ++ version ++ ")")
                        ]
                    ]
                ]
            ]
        ]


view : Model -> Html Msg
view model =
    div []
        [ section [ class "section" ]
            [ div [ class "columns has-same-height is-gapless is-centered" ]
                [ div [ class "column is-two-thirds" ]
                    [ viewCountOrError model
                    ]
                ]
            ]
        , div [ class "buttons has-addons is-centered" ]
            [ button [ class "button", type_ "button", onClick Refresh ]
                [ Icon.viewIcon Icon.random
                , text "Refresh"
                ]
            ]
        , viewFooter model.version
        ]


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
