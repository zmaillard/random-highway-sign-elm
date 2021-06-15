module Route exposing (..)

import Url exposing (Url)
import Url.Parser exposing (..)

type Route
  = All
  | State String
  | Highway String

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
    ]