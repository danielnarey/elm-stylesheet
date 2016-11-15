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
descendantOf s1 s2 =
  Descendant (s1, s2)


{-| Constructor function to create a child selector; intended to be used
semantically as a pipeline function

    Tag "li" |> childOf (Tag "ul")
-}
childOf : Selector -> Selector -> Selector
childOf s1 s2 =
  Child (s1, s2)


{-| Constructor function to create a sibling selector; intended to be used
semantically as a pipeline function

    Tag "ul" |> siblingOf (Tag "p")
-}
siblingOf : Selector -> Selector -> Selector
siblingOf s1 s2 =
  Sibling (s1, s2)


{-| Constructor function to create an adjacent selector; intended to be used
semantically as a pipeline function

    Tag "ul" |> adjacentTo (Tag "p")
-}
adjacentTo : Selector -> Selector -> Selector
adjacentTo s1 s2 =
  Adjacent (s1, s2)


{-| Constructor function to create a pseudo class selector; intended to be used
semantically as a pipeline function

    Tag "li" |> pseudoClass "hover"
-}
pseudoClass : Selector -> String -> Selector
pseudoClass s pseudo =
  PseudoClass (s, [ pseudo ])


{-| Constructor function to create a pseudo element selector; intended to be
used semantically as a pipeline function

    Tag "li" |> pseudoElement "after"
-}
pseudoElement : Selector -> String -> Selector
pseudoElement s pseudo =
  PseudoElement (s, pseudo)
