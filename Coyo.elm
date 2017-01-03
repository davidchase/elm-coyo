module Coyo exposing
  ( liftCoyo
  , fmap
  , lowerCoyo
  )


import Html exposing (text)
import List exposing (map, concatMap)


type Coyoneda f a
    = Coyoneda f a


liftCoyo : b -> Coyoneda (a -> a) b
liftCoyo =
    Coyoneda identity


fmap : (a -> b) -> Coyoneda (a -> a) a -> Coyoneda (a -> b) a
fmap fn coyo =
    case coyo of
        Coyoneda f val ->
            Coyoneda (fn << f) val


lowerCoyo : Coyoneda (a -> b) (List a) -> List b
lowerCoyo coyo =
    case coyo of
        Coyoneda f val ->
            map f val
