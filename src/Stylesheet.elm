module Stylesheet exposing
  ( Stylesheet, RuleSet, Selector(..), MatchValue(..), newRuleSet
  , withSelectors, addSelector, withDeclarations, addDeclaration
  , newStylesheet, withImports, addImport, withPrepends, addPrepend, withRules
  , withRuleSets, addRuleSet, scoped, toCssString, toStyleNode
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
embed the stylesheet at the root level of your HTML DOM.

See
[examples/BasicUse.elm](https://github.com/danielnarey/elm-stylesheet/tree/master/examples)
for a full working example.


# CSS Representation
@docs Stylesheet, RuleSet, Selector, MatchValue

# Constructing Rule Sets
@docs newRuleSet, withSelectors, addSelector, withDeclarations, addDeclaration

# Constructing a Stylesheet
@docs newStylesheet, withImports, addImport, withPrepends, addPrepend
@docs withRules, withRuleSets, addRuleSet, scoped

# Compiling/Rendering a Stylesheet
@docs toCssString, toStyleNode

-}

import Toolkit.Operators exposing (..)
import Toolkit.Helpers as Helpers
import CssBasics as Css
import Html exposing (Html)
import Html.Attributes as Attributes
import String
import List
import Color exposing (Color)


-- CSS REPRESENTATION

{-| A stylesheet consists of one or more rule sets and, optionally: (1) a list
of external style resources to import; (2) a list of inline CSS code snippets
to prepend above the stylesheet's rule statements. By default, a stylesheet
applies globally to the HTML document, but it also contains a `scoped` attribute
that can be set to `True` to take advantage of CSS scoping in HTML 5
(currently only implemented in the Firefox browser).
-}
type alias Stylesheet number =
  { imports : List String
  , prepends : List String
  , rules : List (RuleSet number)
  , scoped : Bool
  }


{-| A rule set consists of one or more selectors that define a set of elements
(and/or pseudo-elements) and one or more style declarations that apply to those
elements.
-}
type alias RuleSet number =
  { selectors : List Selector
  , declarations : List (Css.Declaration number)
  }


{-| A selector defines the set of elements (and/or pseudo-elements) to which a
set of style declarations apply. Specifications for each of the selector types
may be found
[here](https://developer.mozilla.org/en-US/docs/Web/CSS/CSS_Selectors).
Here are some use examples:

    Tag "div"
      --> div

    Id "identifier"
      --> #identifier

    Class "class-name"
      --> .class-name

    Attribute (Tag "a", "href", StartsWith "#")
      --> a[href^="#"]

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

    At ("media", "screen and (min-width: 700px)")
      --> @media screen and (min-width: 700px)

On occasion, it might be simpler and more readable just to define the selector
using CSS code. That is what the `CssCode` type is for:

    CssCode "article p ~ ul > li:nth-child(1)"
      --> article p ~ ul > li:nth-child(1)

See
[Stylesheet.Combinators](http://package.elm-lang.org/packages/danielnarey/elm-stylesheet/latest/Stylesheet/Combinators)
for an alternative syntax that may be used to
construct combinator selectors.

-}
type Selector
  = Any
  | Tag String
  | Id String
  | Class String
  | Attribute (Selector, String, MatchValue)
  | Descendant (Selector, Selector)
  | Child (Selector, Selector)
  | Sibling (Selector, Selector)
  | Adjacent (Selector, Selector)
  | PseudoClass (Selector, List String)
  | PseudoElement (Selector, String)
  | At (String, String)
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
newRuleSet : RuleSet number
newRuleSet =
  RuleSet [] []


{-| Add a list of selectors to a rule set, *replacing* any existing selectors
-}
withSelectors : List Selector -> RuleSet number -> RuleSet number
withSelectors selectorList ruleSet =
  { ruleSet
  | selectors =
      selectorList
  }


{-| Add a new selector to a rule set, *retaining* any existing selectors
-}
addSelector : Selector -> RuleSet number -> RuleSet number
addSelector newSelector ruleSet =
  { ruleSet
  | selectors =
      ruleSet.selectors
        |:: newSelector
  }


{-| Add a list of style declarations to a rule set, *replacing* any existing
declarations
-}
withDeclarations : List (Css.Declaration number) -> RuleSet number -> RuleSet number
withDeclarations declarationList ruleSet =
  { ruleSet
  | declarations =
      declarationList
  }


{-| Add a new style declaration to a rule set, *retaining* any existing
declarations
-}
addDeclaration : Css.Declaration number -> RuleSet number -> RuleSet number
addDeclaration newDeclaration ruleSet =
  { ruleSet
  | declarations =
      ruleSet.declarations
        |:: newDeclaration
  }


-- CONSTRUCTING A STYLESHEET

{-| Initialize a new stylesheet
-}
newStylesheet : Stylesheet number
newStylesheet =
  Stylesheet [] [] [] False


{-| Add a list of imports to a stylesheet, *replacing* any existing imports
-}
withImports : List String -> Stylesheet number -> Stylesheet number
withImports importList stylesheet =
  { stylesheet
  | imports =
      importList
  }


{-| Add a new import to a stylesheet, *retaining* any existing imports
-}
addImport : String -> Stylesheet number -> Stylesheet number
addImport newImport stylesheet =
  { stylesheet
  | imports =
      stylesheet.imports
        |:: newImport
  }


{-| Insert one or more strings of CSS code into a stylesheet after its imports
and before its rule statements; this constructor will *replace* any existing
prepends
-}
withPrepends : List String -> Stylesheet number -> Stylesheet number
withPrepends prependList stylesheet =
  { stylesheet
  | prepends =
      prependList
  }


{-| Add a string of CSS code into a stylesheet after its imports (and after
any existing prepends) and before its rule statements; this constructor will
*retain* any existing prepends
-}
addPrepend : String -> Stylesheet number -> Stylesheet number
addPrepend prepend stylesheet =
  { stylesheet
  | prepends =
      stylesheet.prepends
        |:: prepend
  }


{-| Add a list of rule sets to a stylesheet, *replacing* any existing rule sets
-}
withRules : List (RuleSet number) -> Stylesheet number -> Stylesheet number
withRules ruleList stylesheet =
  { stylesheet
  | rules =
      ruleList
  }


{-| Alias for `withRules`
-}
withRuleSets : List (RuleSet number) -> Stylesheet number -> Stylesheet number
withRuleSets ruleList stylesheet =
  stylesheet
    |> withRules ruleList


{-| Add a new rule set to a stylesheet, *retaining* any existing rule sets
-}
addRuleSet : RuleSet number -> Stylesheet number -> Stylesheet number
addRuleSet newRuleSet stylesheet =
  { stylesheet
  | rules =
      stylesheet.rules
        |:: newRuleSet
  }


{-| Set the stylesheet's `scoped` attribute to `True`. In an HTML 5 compliant
browser, the stylesheet will only be applied to the element on which
`embedStylesheet` is called and all of that element's children. As of October
2016 this scoping feature is only embedded in the Firefox browser.
-}
scoped : Stylesheet number -> Stylesheet number
scoped stylesheet =
  { stylesheet
  | scoped =
      True
  }


-- COMPILING/RENDERING A STYLESHEET

{-| Returns the compiled stylesheet as a string of CSS code
-}
toCssString : Stylesheet number -> String
toCssString stylesheet =
  let
    importDirectives =
      stylesheet.imports
        .|> (\a -> "@import url('" ++ a ++ "');")
        |> String.concat

    prependedCss =
      stylesheet.prepends
        |> String.concat

    ruleStatements =
      stylesheet.rules
        .|> ruleSetToString
        |> String.concat

  in
    importDirectives
      |++ prependedCss
      |++ ruleStatements


{-| Returns an `Html.node` with a `<style>` tag, which contains the stylesheet
rendered as a string of CSS code
-}
toStyleNode : Stylesheet number -> Html number
toStyleNode stylesheet =
  [ stylesheet
    |> toCssString
    |> Html.text
  ]
    |> Html.node "style"
      [ stylesheet.scoped
        |> Attributes.scoped
      ]


{-| Format a prepended CSS string by remvoing comments and deleting excessive
whitespace
-}
formatCssString : String -> String
formatCssString string =
  let
    removeComments string =
      string
        |> Helpers.applyList (string ||> getIndices .|> uncurry String.slice)
        |> String.join ""

    getIndices string =
      string
        |> Helpers.apply2 (startIndices, endIndices)
        |> Helpers.zip

    startIndices string =
      string
        |> String.indexes ("*/")
        .|> (+) 2
        |> (::) 0

    endIndices string =
      string
        |> String.indexes ("/*")
        |:: string ||> String.length

  in
    string
      |> removeComments
      |> String.words
      |> String.join " "


{-| Encode a rule set as a CSS string
-}
ruleSetToString : RuleSet number -> String
ruleSetToString ruleSet =
  let
    selectors =
      ruleSet.selectors
        .|> selectorToString
        |> String.join ","

    declarations =
      ruleSet.declarations
        .|> Css.encodeDeclaration
        |> String.concat

  in
    selectors
      |++ "{" ++ declarations ++ "}"


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

    Attribute (target, attrName, expr) ->
      target
        |> selectorToString
        |++ "[" ++ attrName
        |++ expr ||> matchExprToString
        |++ "]"


    Descendant (ancestor, descendant) ->
      [ ancestor
      , descendant
      ]
        .|> selectorToString
        |> String.join " "

    Child (parent, child) ->
      [ parent
      , child
      ]
        .|> selectorToString
        |> String.join ">"

    Sibling (criterion, target) ->
      [ criterion
      , target
      ]
        .|> selectorToString
        |> String.join "~"

    Adjacent (criterion, target) ->
      [ criterion
      , target
      ]
        .|> selectorToString
        |> String.join "+"

    PseudoClass (target, pseudoList) ->
      pseudoList
        |> (::) (target ||> selectorToString)
        |> String.join ":"

    PseudoElement (target, pseudo) ->
      target
        |> selectorToString
        |++ "::" ++ pseudo

    At (keyword, expr) ->
      "@" ++ keyword ++ " "
        |++ expr

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
