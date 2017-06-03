module Stylesheet.Shorthand exposing
  ( Shorthand, toRuleSetList, toStylesheet )

{-|
## Shorthand representation of a CSS rule set
@docs Shorthand

# Converting from shorthand to standard representations
@docs toRuleSetList, toStylesheet

-}

import CssBasics exposing (Declaration)
import Stylesheet exposing (Selector, RuleSet, Stylesheet)


{-| A simplified rule set record without a media query option. Useful for
building longer stylesheets when you don't want to use the constructor
functions.

    short =
      [ { selectors =
          [ Tag "body"
          , Tag "button"
          , Tag "input"
          , Tag "select"
          , Tag "textarea"
          ]
        , declarations =
          [ ("font-family", Str "Roboto")
          ]
        }

      , { selectors =
          [ Tag "code"
          , Tag "pre"
          ]
        , declarations =
          [ ("font-family", Str "Monaco")
          ]
        }

      ]

-}
type alias Shorthand =
  { selectors : List Selector
  , declarations : List Declaration
  }


{-| Convert a list of `Shorthand` rule sets to a `RuleSet` list

    short
      |> toRuleSetList
-}
toRuleSetList : List Shorthand -> List RuleSet
toRuleSetList shorthand =
  let
    convert s =
      { selectors = s.selectors
      , declarations = s.declarations
      , mediaQuery = Nothing
      }

  in
    shorthand
      |> List.map convert


{-| Construct a `Stylesheet` from a list of `Shorthand` rule sets

    short
      |> toStylesheet
-}
toStylesheet : List Shorthand -> Stylesheet
toStylesheet shorthand =
  Stylesheet.newStylesheet
    |> Stylesheet.withRules (shorthand |> toRuleSetList)
