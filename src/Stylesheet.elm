module Stylesheet exposing
  ( Stylesheet, RuleSet, Selector(..), MatchValue(..), CssFileStructure
  , newRuleSet, withSelectors, addSelector, withDeclarations, addDeclaration
  , withMediaQuery, newStylesheet, withImports, addImport, withPrepends
  , addPrepend, withRules, withRuleSets, addRuleSet, scoped, toCssString
  , toFileStructure, toStyleNode
  )


{-|

## A CSS implementation with helpful constructors for generating a global stylesheet

This library builds off of
[CssBasics](http://package.elm-lang.org/packages/danielnarey/elm-css-basics/latest)
to allow you to generate a stylesheet and embed it in your Elm
program's view. The basic workflow for using this library is (1) create your
rule sets, consisting of selectors (identifying elements) and declarations
(defining styles), (2) add your rule sets to a new stylesheet along with any
import URLs needed to access external resources (e.g., Google fonts), and (3)
render your stylesheet to a string, an HTML `<style>` node, or a
data structure for exporting to a .css file.

See
[examples/BasicUse.elm](https://github.com/danielnarey/elm-stylesheet/tree/master/examples)
for a full working example.


# CSS Representation
@docs Stylesheet, RuleSet, Selector, MatchValue

# Constructing Rule Sets
@docs newRuleSet, withSelectors, addSelector, withDeclarations, addDeclaration
@docs withMediaQuery

# Constructing a Stylesheet
@docs newStylesheet, withImports, addImport, withPrepends, addPrepend
@docs withRules, withRuleSets, addRuleSet, scoped

# Compiling/Rendering a Stylesheet
@docs toCssString, toStyleNode, toFileStructure, CssFileStructure

-}

import CssBasics exposing (Declaration)
import Html exposing (Html)
import Html.Attributes as Attributes
import Color exposing (Color)


-- CSS REPRESENTATION

{-| A stylesheet consists of one or more rule sets and, optionally: (1) a list
of external style resources to import; (2) a list of inline CSS code snippets
to prepend above the stylesheet's rule statements. By default, a stylesheet
applies globally to the HTML document, but it also contains a `scoped` attribute
that can be set to `True` to take advantage of CSS scoping in HTML 5
(currently only implemented in the Firefox browser).
-}
type alias Stylesheet =
  { imports : List String
  , prepends : List String
  , rules : List RuleSet
  , scoped : Bool
  }


{-| A rule set consists of one or more selectors that define a set of elements
(and/or pseudo-elements) and one or more style declarations that apply to those
elements.
-}
type alias RuleSet =
  { selectors : List Selector
  , declarations : List Declaration
  , mediaQuery : Maybe String
  }


{-| A selector defines the set of elements (and/or pseudo-elements) to which a
set of style declarations apply. Specifications for each of the selector types
may be found
[here](https://developer.mozilla.org/en-US/docs/Web/CSS/CSS_Selectors).

The `Any` selector will apply a rule set to every element in the DOM. This is
sometimes used to set more generic browser rendering options.

    Any
      --> "*"

The next four are the most common selectors, selecting for an HTML tag,
an `id` attribute, a `class` attribute, and an exact or partial match to an
attribute value, respectively:

    Tag "div"
      --> div

    Id "identifier"
      --> #identifier

    Class "class-name"
      --> .class-name

    Attribute ("href", StartsWith "#")
      --> [href^="#"]

The following combinator selectors allow basic selectors to be put together in a
variety of ways to contextually refine a query. See
[Stylesheet.Combinators](http://package.elm-lang.org/packages/danielnarey/elm-stylesheet/latest/Stylesheet-Combinators)
for a set of pipeline functions that provide a nicer syntax for generating
combinator selectors.

    Combined [Tag "a", Class "button", Attribute ("href", StartsWith "#")]
      --> a.button[href^="#"]

    Descendant (Tag "article", Tag "p")
      --> article p

    Child (Tag "ul", Tag "li")
      --> ul > li

    Sibling (Tag "p", Tag "ul")
      --> p ~ ul

    Adjacent (Tag "p", Tag "ul")
      --> p + ul

    PseudoClass (Tag "li", [ "nth-child(1)", "hover" ])
      --> li:nth-child(1):hover

    PseudoElement (Tag "li", "after")
      --> li::after

On occasion, it might be simpler and more readable just to define the selector
using CSS code. That is what the `CssCode` type is for:

    CssCode "article p ~ ul > li:nth-child(1)"
      --> article p ~ ul > li:nth-child(1)

The `CssCode` type key can also be used to create a `@font-face` rule as
follows:

    newRuleSet
      |> addSelector (CssCode "@font-face")
      |> withDeclarations
        [ ("font-family", Str "myFont")
        , ("src", Str "url(my_font.woff)")
        ]

-}
type Selector
  = Any
  | Tag String
  | Id String
  | Class String
  | Attribute (String, MatchValue)
  | Combined (List Selector)
  | Descendant (Selector, Selector)
  | Child (Selector, Selector)
  | Sibling (Selector, Selector)
  | Adjacent (Selector, Selector)
  | PseudoClass (Selector, List String)
  | PseudoElement (Selector, String)
  | CssCode String


{-| Represents an expression that defines a set of matching values for a given
HTML attribute. Specifications for attribute selectors may be found
[here](https://developer.mozilla.org/en-US/docs/Web/CSS/Attribute_selectors).
`IsDefined` corresponds to [attr], `Exactly` corresponds to [attr=value],
`Includes` corresponds to [attr~=value], `StartsWith` corresponds to
[attr^=value], `EndsWith` corresponds to [attr$=value], `Contains`
corresponds to [attr*=value], and `Prefix` corresponds to [attr|=value].
-}
type MatchValue
  = IsDefined
  | Exactly String
  | Includes String
  | StartsWith String
  | EndsWith String
  | Contains String
  | Prefix String


-- CONSTRUCTING RULE SETS

{-| Initialize a new rule set
-}
newRuleSet : RuleSet
newRuleSet =
  { selectors = []
  , declarations = []
  , mediaQuery = Nothing
  }


{-| Add a list of selectors to a rule set, *replacing* any existing selectors
-}
withSelectors : List Selector -> RuleSet -> RuleSet
withSelectors selectorList ruleSet =
  { ruleSet
  | selectors =
      selectorList
  }


{-| Add a new selector to a rule set, *retaining* any existing selectors
-}
addSelector : Selector -> RuleSet -> RuleSet
addSelector newSelector ruleSet =
  { ruleSet
  | selectors =
      newSelector
        |> List.singleton
        |> List.append ruleSet.selectors
  }


{-| Add a list of style declarations to a rule set, *replacing* any existing
declarations
-}
withDeclarations : List Declaration -> RuleSet -> RuleSet
withDeclarations declarationList ruleSet =
  { ruleSet
  | declarations =
      declarationList
  }


{-| Add a new style declaration to a rule set, *retaining* any existing
declarations
-}
addDeclaration : Declaration -> RuleSet -> RuleSet
addDeclaration newDeclaration ruleSet =
  { ruleSet
  | declarations =
      newDeclaration
        |> List.singleton
        |> List.append ruleSet.declarations
  }


{-| Restrict the scope of a rule set by applying a media query. The first
argument is a media type, given as a string, and the second argument is a list
of media features, given as declarations. *Replaces* any existing media queries.

    myRuleSet
      |> withMediaQuery "screen" [("min-width", Unit 1250 Px)]
-}
withMediaQuery : String -> List Declaration -> RuleSet -> RuleSet
withMediaQuery mediaType mediaFeatures ruleSet =
  let
    mediaFeatureToString (feature, value) =
      [ "("
      , feature
      , ":"
      , value
        |> CssBasics.encodeCssValue
      , ")"
      ]
        |> String.concat

    maybeAnd =
      case mediaFeatures of
        [] ->
          ""

        _ ->
          " and "

  in
  { ruleSet
  | mediaQuery =
      [ "@media "
      , mediaType
      , maybeAnd
      , mediaFeatures
        |> List.map mediaFeatureToString
        |> String.join " and "
      ]
        |> String.concat
        |> Just
  }


-- CONSTRUCTING A STYLESHEET

{-| Initialize a new stylesheet
-}
newStylesheet : Stylesheet
newStylesheet =
  { imports = []
  , prepends = []
  , rules = []
  , scoped = False
  }


{-| Add a list of imports to a stylesheet, *replacing* any existing imports
-}
withImports : List String -> Stylesheet -> Stylesheet
withImports importList stylesheet =
  { stylesheet
  | imports =
      importList
  }


{-| Add a new import to a stylesheet, *retaining* any existing imports
-}
addImport : String -> Stylesheet -> Stylesheet
addImport newImport stylesheet =
  { stylesheet
  | imports =
      newImport
        |> List.singleton
        |> List.append stylesheet.imports
  }


{-| Insert one or more strings of CSS code into a stylesheet after its imports
and before its rule statements; this constructor will *replace* any existing
prepends
-}
withPrepends : List String -> Stylesheet -> Stylesheet
withPrepends prependList stylesheet =
  { stylesheet
  | prepends =
      prependList
  }


{-| Add a string of CSS code into a stylesheet after its imports (and after
any existing prepends) and before its rule statements; this constructor will
*retain* any existing prepends
-}
addPrepend : String -> Stylesheet -> Stylesheet
addPrepend newPrepend stylesheet =
  { stylesheet
  | prepends =
      newPrepend
        |> List.singleton
        |> List.append stylesheet.prepends
  }


{-| Add a list of rule sets to a stylesheet, *replacing* any existing rule sets
-}
withRules : List RuleSet -> Stylesheet -> Stylesheet
withRules ruleList stylesheet =
  { stylesheet
  | rules =
      ruleList
  }


{-| Alias for `withRules`
-}
withRuleSets : List RuleSet -> Stylesheet -> Stylesheet
withRuleSets ruleList stylesheet =
  stylesheet
    |> withRules ruleList


{-| Add a new rule set to a stylesheet, *retaining* any existing rule sets
-}
addRuleSet : RuleSet -> Stylesheet -> Stylesheet
addRuleSet newRuleSet stylesheet =
  { stylesheet
  | rules =
      newRuleSet
        |> List.singleton
        |> List.append stylesheet.rules
  }


{-| Set the stylesheet's `scoped` attribute to `True`. In an HTML 5 compliant
browser, the stylesheet will only be applied to the element on which
`embedStylesheet` is called and all of that element's children. As of October
2016 this scoping feature is only embedded in the Firefox browser.
-}
scoped : Stylesheet -> Stylesheet
scoped stylesheet =
  { stylesheet
  | scoped =
      True
  }


-- COMPILING/RENDERING A STYLESHEET

{-| Returns the compiled stylesheet as a string of CSS code
-}
toCssString : Stylesheet -> String
toCssString stylesheet =
  let
    importDirectives =
      stylesheet.imports
        |> List.map (\a -> "@import url('" ++ a ++ "');")
        |> String.concat

    prependedCss =
      stylesheet.prepends
        |> String.concat

    ruleStatements =
      stylesheet.rules
        |> List.map ruleSetToString
        |> String.concat

  in
    [ importDirectives
    , prependedCss
    , ruleStatements
    ]
      |> String.concat


{-| Returns an `Html.node` with a `<style>` tag, which contains the stylesheet
rendered as a string of CSS code
-}
toStyleNode : Stylesheet -> Html msg
toStyleNode stylesheet =
  [ stylesheet
    |> toCssString
    |> Html.text
  ]
    |> Html.node "style"
      [ stylesheet.scoped
        |> Attributes.scoped
      ]


{-| Returns the compiled stylesheet in the format required to export it to a
.css file using rtfeldman's [elm-css](https://github.com/rtfeldman/elm-css)
Node module.

-}
toFileStructure : String -> Stylesheet -> CssFileStructure
toFileStructure filename stylesheet =
  [ { filename = filename
    , content = stylesheet |> toCssString
    , success = True
    }
  ]


{-| Alias for [rtfeldman/elm-css/Css/File/CssFileStructure](https://github.com/rtfeldman/elm-css/blob/master/src/Css/File.elm)
-}
type alias CssFileStructure =
  List
    { filename : String
    , content : String
    , success : Bool
    }



-- INTERNAL


{-| Encode a rule set as a CSS string
-}
ruleSetToString : RuleSet -> String
ruleSetToString ruleSet =
  let
    selectorsAndDeclarations =
      [ ruleSet.selectors
        |> List.map selectorToString
        |> String.join ","

      , "{"

      , ruleSet.declarations
        |> List.map CssBasics.encodeDeclaration
        |> String.concat

      , "}"

      ]
        |> String.concat

  in
    case ruleSet.mediaQuery of
      Nothing ->
        selectorsAndDeclarations

      Just query ->
        [ query
        , "{"
        , selectorsAndDeclarations
        , "}"
        ]
          |> String.concat


{-| Encode a selector as a CSS string
-}
selectorToString : Selector -> String
selectorToString selector =
  case selector of
    Any ->
      "*"

    Tag tagName ->
      tagName

    Id idString ->
      "#" ++ idString

    Class className ->
      "." ++ className

    Attribute (attrName, expr) ->
      [ "["
      , attrName
      , expr
        |> matchExprToString
      , "]"
      ]
        |> String.concat

    Combined selectorList ->
      selectorList
        |> List.map selectorToString
        |> String.concat

    Descendant (ancestor, descendant) ->
      [ ancestor
      , descendant
      ]
        |> List.map selectorToString
        |> String.join " "

    Child (parent, child) ->
      [ parent
      , child
      ]
        |> List.map selectorToString
        |> String.join ">"

    Sibling (criterion, target) ->
      [ criterion
      , target
      ]
        |> List.map selectorToString
        |> String.join "~"

    Adjacent (criterion, target) ->
      [ criterion
      , target
      ]
        |> List.map selectorToString
        |> String.join "+"

    PseudoClass (target, pseudoList) ->
      pseudoList
        |> (::) (target |> selectorToString)
        |> String.join ":"

    PseudoElement (target, pseudo) ->
      [ target
        |> selectorToString
      , "::"
      , pseudo
      ]
        |> String.concat

    CssCode expr ->
      expr


{-| Encode a matching expression as a CSS string
-}
matchExprToString : MatchValue -> String
matchExprToString matchValue =
  case matchValue of
    IsDefined ->
      ""

    Exactly value ->
      "=\"" ++ value ++ "\""

    Includes value ->
      "~=\"" ++ value ++ "\""

    StartsWith value ->
      "^=\"" ++ value ++ "\""

    EndsWith value ->
      "$=\"" ++ value ++ "\""

    Contains value ->
      "*=\"" ++ value ++ "\""

    Prefix value ->
      "|=\"" ++ value ++ "\""
