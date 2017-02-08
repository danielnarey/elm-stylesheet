module Stylesheet.Combinators exposing
  ( combinedWith, descendantOf, childOf, siblingOf, adjacentTo, pseudoClass
  , pseudoElement
  )

{-|

## An alternative syntax for constructing combinator and pseudo selectors

# Combinators
@docs combinedWith, descendantOf, childOf, siblingOf, adjacentTo

# Pseudo Classes and Elements
@docs pseudoClass, pseudoElement

-}


import Stylesheet exposing (Selector(..))


{-| Constructor function to combine two selectors; intended to be
used semantically as a pipeline function

    Tag "a" |> combineWith (Class "button")

    --> a.button
-}
combinedWith : Selector -> Selector -> Selector
combinedWith criterion target =
  Combined [target, criterion]


{-| Constructor function to create a descendent selector; intended to be
used semantically as a pipeline function

    Tag "p" |> descendantOf (Tag "article")

    --> article p
-}
descendantOf : Selector -> Selector -> Selector
descendantOf ancestor descendant =
  Descendant (ancestor, descendant)


{-| Constructor function to create a child selector; intended to be used
semantically as a pipeline function

    Tag "li" |> childOf (Tag "ul")

    --> ul > li
-}
childOf : Selector -> Selector -> Selector
childOf parent child =
  Child (parent, child)


{-| Constructor function to create a sibling selector; intended to be used
semantically as a pipeline function

    Tag "ul" |> siblingOf (Tag "p")

    --> p ~ ul
-}
siblingOf : Selector -> Selector -> Selector
siblingOf criterion target =
  Sibling (criterion, target)


{-| Constructor function to create an adjacent selector; intended to be used
semantically as a pipeline function

    Tag "ul" |> adjacentTo (Tag "p")

    --> p + ul
-}
adjacentTo : Selector -> Selector -> Selector
adjacentTo criterion target =
  Adjacent (criterion, target)


{-| Constructor function to create a pseudo class selector; intended to be used
semantically as a pipeline function

    Tag "li" |> pseudoClass "hover"

    --> li:hover
-}
pseudoClass : String -> Selector -> Selector
pseudoClass pseudoName target =
  PseudoClass (target, pseudoName |> List.singleton)


{-| Constructor function to create a pseudo element selector; intended to be
used semantically as a pipeline function

    Tag "li" |> pseudoElement "after"

    --> li::after
-}
pseudoElement : String -> Selector -> Selector
pseudoElement pseudoName target =
  PseudoElement (target, pseudoName)
