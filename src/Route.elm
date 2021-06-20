module Route exposing (..)

import Url exposing (Url)
import Url.Parser exposing (..)
import Html exposing (a)

type Route
  = All
  | State String
  | County String String
  | Country String
  | Place String 
  | Highway String
  | Tag String


parseUrl : Url -> Route
parseUrl url =
  case parse matchRoute url of
    Just route ->
      route

    Nothing ->
      All


matchRoute : Parser (Route -> a) a
matchRoute =
  oneOf
    [ map All top
    , map Highway (s "highway" </> string)
    , map State (s "state" </> string)
    , map Country (s "country" </> string)
    , map Place (s "place" </> string )
    , map Tag (s "tag" </> string )
    , map County (s "county" </> string </> string)
    ]