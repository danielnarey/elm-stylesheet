module BasicUse exposing (myStylesheet)

{-| A simple, self-contained example for danielnarey/elm-stylesheet
@docs myStylesheet
-}

import CssBasics exposing (CssValue(..), UnitType(..))
import Stylesheet exposing (..)
import Stylesheet.FontImport exposing (newFontFamily, withVariants, importFonts)
import Html
import Html.Attributes as Attr
import Color


palette =
  { blue = Color.rgb 0 102 255
  , purple = Color.rgb 153 51 153
  }


fonts =
  [ "Orbitron"
  , "Roboto"
  , "sans-serif"
  ]


weight =
  { normal = 400
  , bold = 700
  }


selectors =
  { myClass = "myClass"
  , myId = "myId"
  }


{-| An example stylesheet
-}
myStylesheet : Stylesheet
myStylesheet =
  let
    myClassStyles =
      newRuleSet
        |> withSelectors
          [ Class selectors.myClass ]
        |> withDeclarations
          [ ("font-family", FontStack fonts)
          , ("font-weight", Num weight.normal)
          , ("font-size", Unit 2 Em)
          , ("color", Col palette.blue)
          , ("text-align", Str "center")
          ]

    myIdStyles =
      newRuleSet
        |> withSelectors
          [ Id selectors.myId ]
        |> withDeclarations
          [ ("font-weight", Num weight.bold)
          , ("font-size", Unit 3 Em)
          , ("color", Col palette.purple)
          ]

    orbitronFont =
      newFontFamily "Orbitron"
        |> withVariants
          [ weight.normal |> toString
          , weight.bold |> toString
          ]

  in
    newStylesheet
      |> withRules
        [ myClassStyles
        , myIdStyles
        ]
      |> importFonts
        [ orbitronFont ]


main =
  let
    line1 =
      [ Html.text "Hello, World!" ]
        |> Html.p
          [ Attr.class selectors.myClass ]

    line2 =
      [ Html.text "Hello, Universe!" ]
        |> Html.p
          [ Attr.class selectors.myClass
          , Attr.id selectors.myId
          ]

  in
    [ myStylesheet
      |> toStyleNode
    , line1
    , line2
    ]
      |> Html.div []
