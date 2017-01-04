module Coyo exposing
  ( liftCoyo
  , fmap
  , chain
  , ap
  , pure
  , lowerCoyo
  )


import Html exposing (text)
import List exposing (map, concatMap)


type Coyoneda f a
    = Coyoneda f a


liftCoyo : b -> Coyoneda (a -> a) b
liftCoyo =
    Coyoneda identity


fmap : (b -> c) -> Coyoneda (a -> b) d -> Coyoneda (a -> c) d
fmap fn coyo =
    case coyo of
        Coyoneda f val ->
            Coyoneda (fn << f) val
            
chain : (b -> Coyoneda (a -> b1) (List a)) -> Coyoneda (a1 -> b) (List a1) -> Coyoneda (a2 -> a2) (List b1)
chain fn coyo =
    case coyo of
        Coyoneda f val ->
            liftCoyo <| concatMap (lowerCoyo << fn << f) val

ap : Coyoneda (a -> b -> c) (List a) -> Coyoneda (a1 -> b) (List a1) -> Coyoneda (a2 -> a2) (List c)
ap f g =
    chain (\fn -> fmap fn <| g) <| f

toList : a -> List a
toList fn =
    fn :: []

concat : Coyoneda (a -> b) (List a) -> Coyoneda (a1 -> b) (List a1) -> Coyoneda (a2 -> a2) (List b)
concat u v = liftCoyo <| append (lowerCoyo u) <| lowerCoyo v

pure : b -> Coyoneda (a -> a) (List b)
pure =
    liftCoyo << toList

empty : Coyoneda (a -> a) (List b)
empty =
    liftCoyo []

lowerCoyo : Coyoneda (a -> b) (List a) -> List b
lowerCoyo coyo =
    case coyo of
        Coyoneda f val ->
            map f val
