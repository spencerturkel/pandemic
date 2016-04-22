{-# LANGUAGE MultiParamTypeClasses #-}

module Pairing where

import Control.Lens hiding ((:<))
import Control.Comonad.Cofree
import Control.Monad.Free

import Action

class (Functor f, Functor g) => Pairing f g where
  pair :: (a -> b -> r) -> f a -> g b -> r

instance Pairing ((->) c) ((,) c) where
  pair f g (x, y) = uncurry f (g x, y)

instance Pairing f g => Pairing (Cofree f) (Free g) where
  pair p (a :< _) (Pure x) = p a x
  pair p (_ :< fs) (Free gs) = pair (pair p) fs gs

instance Pairing ActionF CoActionF where
  pair f action coaction =
    let
      (x, y) =
        case action of
          Drive city k -> (k, (coaction^.driveH) city)
          DirectFlight city k -> (k, (coaction^.directFlightH) city)
          CharterFlight city k -> (k, (coaction^.charterFlightH) city)
          ShuttleFlight city k -> (k, (coaction^.shuttleFlightH) city)
          Build city k -> (k, (coaction^.buildH) city)
          Treat color k -> (k, (coaction^.treatH) color)
          GiveCard ref k -> (k, (coaction^.giveCardH) ref)
          TakeCard city k -> (k, (coaction^.takeCardH) city)
          DiscoverCure ref k -> (k, (coaction^.discoverCureH) ref)
          RoleAbility ability k -> (k, (coaction^.roleAbilityH) ability)
    in
      pair f x y
