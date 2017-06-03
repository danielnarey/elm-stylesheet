port module Stylesheets exposing (main)

import Stylesheet
import BasicUse

{-|
-}
port files : Stylesheet.CssFileStructure -> Cmd msg


{-|
-}
main : Program Never () Never
main =
  let
    command =
      BasicUse.myStylesheet
        |> Stylesheet.toFileStructure "myCssFile.css"
        |> files

  in
    { init = ( (), command )
    , update = \_ _ -> ( (), Cmd.none )
    , subscriptions = \_ -> Sub.none
    }
      |> Platform.program
