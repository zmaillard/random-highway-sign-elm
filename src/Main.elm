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
import Json.Decode exposing (Decoder, Error(..), andThen, field, float, int, string, succeed)
import Json.Decode.Pipeline exposing (required)
import Random
import RemoteData exposing (RemoteData(..), WebData)
import Route exposing (Route)
import Url exposing (Url)
import Iso8601
import Time

type alias Flags =
    { searchServiceUrl : String
    , searchApiKey : String
    , version : String
    , mapToken : String
    }


type alias Model =
    { searchServiceUrl : String
    , searchApiKey : String
    , mapToken : String
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
    , countySlug : String
    , countrySlug : String
    , location : List Float
    , tags : List String
    , dateTaken : String
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
      , mapToken = flags.mapToken
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
    url ++ "/docs?api-version=2020-06-30-Preview&api-key=" ++ apiKey ++ "&$skip=" ++ String.fromInt offset ++ "&$top=1" ++ queryFilter


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

monthLookup : Time.Month -> Int
monthLookup month =
  case month of
    Time.Jan -> 1 
    Time.Feb ->  2 
    Time.Mar ->  3
    Time.Apr -> 4
    Time.May -> 5
    Time.Jun ->  6
    Time.Jul ->  7
    Time.Aug ->  8
    Time.Sep -> 9
    Time.Oct ->  10
    Time.Nov ->  11
    Time.Dec ->  12


formatDate : Time.Posix -> String
formatDate time =
  let
    day = String.fromInt <| Time.toDay Time.utc time
    month = String.fromInt  <| monthLookup <| Time.toMonth  Time.utc time
    year = String.fromInt <| Time.toYear Time.utc time
  in

  day ++ "/" ++ month ++ "/" ++ year
dateDecoder : Decoder String
dateDecoder = 
  
  Json.Decode.andThen
    (\s -> 
      case (Iso8601.toTime s) of
        Ok timeStr ->
          Json.Decode.succeed (formatDate timeStr)
        Err a ->
          Json.Decode.fail ("Incorrect Time")
    )
    Json.Decode.string

locationDecoder : Decoder (List Float)
locationDecoder =
    field "coordinates"
        (Json.Decode.list float)


signDecoder : Decoder SignResult
signDecoder =
    succeed SignResult
        |> required "ImageId" string
        |> required "Title" string
        |> required "Description" string
        |> required "Highways" highwayDecoder
        |> required "Taxonomy" string
        |> required "CountyType" string
        |> required "County" string
        |> required "State" string
        |> required "CountySlug" string
        |> required "CountrySlug" string
        |> required "Location" locationDecoder
        |> required "Tags" (Json.Decode.list string)
        |> required "DateTaken" dateDecoder


signListDecoder : Decoder (List SignResult)
signListDecoder =
    field "value"
        (Json.Decode.list
            signDecoder
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
        Route.Place fullPlace ->
            "&$filter=Navigation/any(n:n eq '" ++ fullPlace ++ "')"

        Route.Tag tag ->
            "&$filter=Tags/any(t:t eq '" ++ tag ++ "')"

        Route.County state countySlug ->
            "&$filter=CountySlug eq '" ++ countySlug ++ "' and State eq '" ++ state ++ "'"

        Route.Country country ->
            "&$filter=CountrySlug eq '" ++ country ++ "'"

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
        , expect = Http.expectJson (RemoteData.fromResult >> GotSign) signListDecoder
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
            ( { model | route = newRoute }, countRequest newRoute model.searchServiceUrl model.searchApiKey )

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


mapUrl : SignResult -> String -> String
mapUrl sign token =
    let
        latitude =
            String.fromFloat (Maybe.withDefault 0.0 (List.head sign.location))

        longitude =
            String.fromFloat (Maybe.withDefault 0.0 (List.head (List.reverse sign.location)))

        coords =
            """{"type":"Point","coordinates":[""" ++ latitude ++ "," ++ longitude ++ """ ]}"""
    in
    "https://api.mapbox.com/styles/v1/mapbox/streets-v11/static/geojson(" ++ Url.percentEncode coords ++ ")/" ++ latitude ++ "," ++ longitude ++ ",11/1024x200?access_token=" ++ token


viewMap : SignResult -> String -> Html Msg
viewMap sign token =
    img [ src (mapUrl sign token) ]
        []


viewTag : String -> Html Msg
viewTag tag =
    a [ href ("/tag/" ++ tag) ]
        [ span [ class "is-size-7 tag is-rounded" ]
            [ text tag
            ]
        ]


viewSignDescription : SignResult -> String -> Html Msg
viewSignDescription sign token =
    div [ class "card-content" ]
        [ div [ class "content" ]
            [ p [ class "is-size-6" ]
                [ text sign.description
                ]
            , p [ class "is-size-7" ]
                [ text sign.dateTaken
                ]
            , div [ class "tags" ] (List.map (\t -> viewTag t) sign.tags)
            , viewMap sign token
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
    , String.split "|" sign.taxonomy |> fixLocation sign -- |> List.map (\l -> viewLocationItem l)
    ]
        |> List.concat


countyLink : String -> String -> String -> Html Msg
countyLink display countySlug state =
    a [ class "button is-text is-marginless is-paddingless is-small", href ("/county/" ++ state ++ "/" ++ countySlug) ]
        [ text display
        ]


placeLink : String -> String -> Html Msg
placeLink placeName taxonomy =
    a [ class "button is-text is-marginless is-paddingless is-small", href ("/place/" ++ taxonomy) ]
        [ text placeName
        ]


stateLink : String -> Html Msg
stateLink state =
    a [ class "button is-text is-marginless is-paddingless is-small", href ("/state/" ++ state) ]
        [ text state
        ]


countryLink : String -> String -> Html Msg
countryLink display countrySlug =
    a [ class "button is-text is-marginless is-paddingless is-small", href ("/country/" ++ countrySlug) ]
        [ text display
        ]


fixLocation : SignResult -> List String -> List (Html Msg)
fixLocation sign location =
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
        [ countyLink (sign.county ++ " " ++ sign.countyType) sign.countySlug sign.state, stateLink sign.state, countryLink country sign.countrySlug ]

    else
        [ placeLink place (country ++ "|" ++ sign.state ++ "|" ++ place), countyLink (sign.county ++ " " ++ sign.countyType) sign.countySlug sign.state, stateLink sign.state, countryLink country sign.countrySlug ]


viewSignHighways : SignResult -> Html Msg
viewSignHighways sign =
    nav [ class "card-footer level" ]
        [ div [ class "level-left" ]
            [ div [ class "level-item" ]
                (viewLocation sign)
            ]
        ]


viewSign : SignResult -> String -> Html Msg
viewSign res token =
    div [ class "card" ]
        [ viewSignTitle res.title
        , viewSignImage res.url res.title
        , viewSignDescription res token
        , viewSignHighways res
        ]


viewCountOrError : Model -> Html Msg
viewCountOrError model =
    case model.sign of
        Nothing ->
            text ""

        Just results ->
            viewSign results model.mapToken


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
