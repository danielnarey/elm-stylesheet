module Stylesheet.Combinators exposing
  ( descendantOf, childOf, siblingOf, adjacentTo, pseudoClass, pseudoElement
  )

{-|

## An alternative syntax for constructing combinator and pseudo selectors

# Combinators
@docs descendantOf, childOf, siblingOf, adjacentTo

# Pseudo Classes and Elements
@docs pseudoClass, pseudoElement

-}


import Stylesheet exposing (Selector(..))


{-| Constructor function to create a descendent selector; intended to be
used semantically as a pipeline function

    Tag "p" |> descendantOf (Tag "article")
-}
descendantOf : Selector -> Selector -> Selector
descendantOf ancestor descendant =
  Descendant (ancestor, descendant)


{-| Constructor function to create a child selector; intended to be used
semantically as a pipeline function

    Tag "li" |> childOf (Tag "ul")
-}
childOf : Selector -> Selector -> Selector
childOf parent child =
  Child (parent, child)


{-| Constructor function to create a sibling selector; intended to be used
semantically as a pipeline function

    Tag "ul" |> siblingOf (Tag "p")
-}
siblingOf : Selector -> Selector -> Selector
siblingOf criterion target =
  Sibling (criterion, target)


{-| Constructor function to create an adjacent selector; intended to be used
semantically as a pipeline function

    Tag "ul" |> adjacentTo (Tag "p")
-}
adjacentTo : Selector -> Selector -> Selector
adjacentTo criterion target =
  Adjacent (criterion, target)


{-| Constructor function to create a pseudo class selector; intended to be used
semantically as a pipeline function

    Tag "li" |> pseudoClass "hover"
-}
pseudoClass : String -> Selector -> Selector
pseudoClass pseudoName target =
  PseudoClass (target, pseudoName |> List.singleton)


{-| Constructor function to create a pseudo element selector; intended to be
used semantically as a pipeline function

    Tag "li" |> pseudoElement "after"
-}
pseudoElement : String -> Selector -> Selector
pseudoElement pseudoName target =
  PseudoElement (target, pseudoName)
