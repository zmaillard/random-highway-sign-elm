port module Ports exposing (..)

-- Ports
port loadImage : String -> Cmd msg
port receiveImageUpdates : (String -> msg) -> Sub msg
