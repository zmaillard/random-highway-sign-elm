module Main exposing (init, main)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav
import FontAwesome.Attributes as Icon
import FontAwesome.Brands as Icon
import FontAwesome.Icon as Icon exposing (Icon)
import FontAwesome.Solid as Icon
import Html exposing (..)
import Html.Attributes exposing (alt, class, href, src, style, type_)
import Html.Events exposing (onClick)
import Http
import Json.Decode exposing (Decoder, Error(..), field, int, string)
import Random
import RemoteData exposing (RemoteData(..), WebData)
import Route exposing (Route)
import Url exposing (Url)


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
    , loading : Bool
    , route : Route
    , navKey : Nav.Key
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
    = GotCount (WebData Int)
    | GotSign (WebData (List SignResult))
    | RandomUpdated Int
    | Refresh
    | LinkClicked UrlRequest
    | UrlChanged Url


randomOffset : Int -> Random.Generator Int
randomOffset count =
    Random.int 0 (count - 1)


newRandom : Int -> Cmd Msg
newRandom max =
    Random.generate RandomUpdated (randomOffset max)


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url navKey =
    let
        parsedRoute =
            Route.parseUrl url
    in
    ( { searchServiceUrl = flags.searchServiceUrl
      , searchApiKey = flags.searchApiKey
      , count = 0
      , sign = Maybe.Nothing
      , version = flags.version
      , loading = True
      , route = parsedRoute
      , navKey = navKey
      }
    , countRequest parsedRoute flags.searchServiceUrl flags.searchApiKey
    )


buildCountUrl : String -> String -> String -> String
buildCountUrl queryFilter url apiKey =
    url ++ "/docs?$count=true&api-version=2020-06-30-Preview&api-key=" ++ apiKey ++ queryFilter


buildSignUrl : String -> String -> String -> Int -> String
buildSignUrl queryFilter url apiKey offset =
    url ++ "/docs?api-version=2020-06-30-Preview&api-key=" ++ apiKey ++ "&$skip=" ++ String.fromInt (offset - 1) ++ "&$top=1" ++ queryFilter


highwayDecoder : Decoder (List Highway)
highwayDecoder =
    Json.Decode.list
        (Json.Decode.map2 Highway
            (field "Highway" string)
            (field "ImageName" string)
        )


countDecoder : Decoder Int
countDecoder =
    field "@odata.count" int


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


countRequest : Route -> String -> String -> Cmd Msg
countRequest route url apiKey =
    Http.get
        { url = buildCountUrl (getFilter route) url apiKey
        , expect = Http.expectJson (RemoteData.fromResult >> GotCount) countDecoder
        }


getFilter : Route -> String
getFilter route =
    case route of
        Route.Highway highway ->
            "&$filter=Highways/any(h: h/Highway eq '" ++ highway ++ "')"

        Route.State state ->
            "&$filter=State eq '" ++ state ++ "'"

        Route.All ->
            ""


signRequest : Route -> String -> String -> Int -> Cmd Msg
signRequest route url apiKey offset =
    Http.get
        { url = buildSignUrl (getFilter route) url apiKey offset
        , expect = Http.expectJson (RemoteData.fromResult >> GotSign) signDecoder
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.navKey (Url.toString url) )

                Browser.External url ->
                    ( model, Nav.load url )

        UrlChanged url ->
            let
                newRoute =
                    Route.parseUrl url
            in
            ( { model | route = newRoute }, Cmd.none )

        Refresh ->
            ( { model | loading = True }, newRandom model.count )

        RandomUpdated random ->
            ( model, signRequest model.route model.searchServiceUrl model.searchApiKey random )

        GotSign signList ->
            case signList of
                RemoteData.Success signs ->
                    let
                        sign =
                            List.head signs
                    in
                    ( { model | sign = sign, loading = False }, Cmd.none )

                _ ->
                    ( { model | sign = Maybe.Nothing, loading = False }, Cmd.none )

        GotCount result ->
            case result of
                RemoteData.Success c ->
                    ( { model | count = c }, newRandom c )

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
    a [ href ("/highway/" ++ highway.highway) ]
        [ img [ class "shield-padding", alt highway.highway, src ("https://shield.sagebrushgis.com/Shields/20x/" ++ highway.imageName) ]
            []
        ]


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
                        [ text ("(" ++ version ++ ")")
                        ]
                    ]
                ]
            ]
        ]


refreshIcon : Bool -> Html Msg
refreshIcon isLoading =
    if isLoading then
        Icon.viewStyled [ Icon.spin ] Icon.spinner

    else
        Icon.viewIcon Icon.random


viewRefrshButton : Bool -> Html Msg
viewRefrshButton isLoading =
    div [ class "buttons has-addons is-centered" ]
        [ button [ class "button", type_ "button", onClick Refresh ]
            [ refreshIcon isLoading
            , text "Refresh"
            ]
        ]


currentView : Model -> Html Msg
currentView model =
    div []
        [ section [ class "section" ]
            [ div [ class "columns has-same-height is-gapless is-centered" ]
                [ div [ class "column is-two-thirds" ]
                    [ viewCountOrError model
                    ]
                ]
            ]
        , viewRefrshButton model.loading
        , viewFooter model.version
        ]


view : Model -> Document Msg
view model =
    { title = "Random Highway Sign"
    , body = [ currentView model ]
    }


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }
