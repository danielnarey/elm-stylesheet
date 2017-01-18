module Stylesheet.FontImport exposing
  ( FontFamily, newFontFamily, withVariants, addVariant, withSubsets, addSubset
  , importFonts
  )


{-|

## Helpers for importing Google Fonts

See
[examples/BasicUse.elm](https://github.com/danielnarey/elm-stylesheet/tree/master/examples)
for a full working example.

# Font Family Representation
@docs FontFamily

# Constructing a Font Family
@docs newFontFamily, withVariants, addVariant, withSubsets, addSubset

# Adding an Import Directive to a Stylesheet
@docs importFonts

-}


import Stylesheet exposing (Stylesheet, addImport)
import Toolkit.Operators exposing (..)
import Toolkit.Helpers as Helpers
import Html exposing (Html)


{-| Represents a family of fonts that may have multiple variants and character
subsets. Used to construct import directives for Google Fonts.
-}
type alias FontFamily =
  { name : String
  , variants : List String
  , subsets : List String
  }


{-| Initialize a new font family; the string argument provides the font name
-}
newFontFamily : String -> FontFamily
newFontFamily name =
  FontFamily name [] []


{-| Add a list of variants to a font family, *replacing* any existing variants
-}
withVariants : List String -> FontFamily -> FontFamily
withVariants variantList family =
  { family
  | variants =
      variantList
  }


{-| Add a new variant to a font family, *retaining* any existing variants
-}
addVariant : String -> FontFamily -> FontFamily
addVariant variant family =
  { family
  | variants =
      family.variants
        |:: variant
  }


{-| Add a list of character subsets (e.g., "cyrillic") to a font family,
*replacing* any existing subsets
-}
withSubsets : List String -> FontFamily -> FontFamily
withSubsets subsetList family =
  { family
  | subsets =
      subsetList
  }


{-| Add a new character subset to a font family, *retaining* any existing
subsets
-}
addSubset : String -> FontFamily -> FontFamily
addSubset subset family =
  { family
  | subsets =
      family.subsets
        |:: subset
  }


{-| Given a list of `FontFamily` records and a stylesheet, generate an import
directive containing an API query that will retreive the specified
families/variants from Google Fonts and prepend it to the stylesheet
-}
importFonts : List FontFamily -> Stylesheet number -> Stylesheet number
importFonts fontFamilies stylesheet =
  let
    encodeUrl familyList =
      fontFamilies
        .|> encodeFamily
        |> String.join "|"
        |> (++) "https://fonts.googleapis.com/css?family="

    encodeFamily family =
      family.name
        |> encodeName
        |++ family.variants ||> encodeVariants
        |++ family.subsets ||> encodeSubsets

    encodeName name =
      name
        |> String.words
        |> String.join "+"

    encodeVariants variants =
      if variants |> List.isEmpty then
        ""
      else
        variants
          |> String.join ","
          |> (++) ":"

    encodeSubsets subsets =
      if subsets |> List.isEmpty then
        ""
      else
        subsets
          |> String.join ","
          |> (++) "&subset="

  in
    stylesheet
      |> addImport (fontFamilies |> encodeUrl)
