module Coyo exposing
  ( liftCoyo
  , fmap
  , chain
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
            
            
chain fn coyo =
    case coyo of
        Coyoneda f val ->
            liftCoyo <| concatMap (lowerCoyo << fn << f) val



lowerCoyo : Coyoneda (a -> b) (List a) -> List b
lowerCoyo coyo =
    case coyo of
        Coyoneda f val ->
            map f val
