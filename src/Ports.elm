port module Ports exposing (..)

type alias ImagePortData =
  { id : String
  , contents : String
  , filename : String
  }

port fileSelected : String -> Cmd msg
port saveFile : (String,String) -> Cmd msg

port fileContentRead : (ImagePortData -> msg) -> Sub msg
