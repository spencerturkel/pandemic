module RunAction where

import Action
import CoAction
import Target

runAction :: Target -> Action -> Maybe Target
runAction t a = case a of
  Drive city -> coDrive t city
  DirectFlight city -> coDirectFlight t city
  CharterFlight city -> coCharterFlight t city
  ShuttleFlight city -> coShuttleFlight t city
  Build city -> coBuild t city
  Treat color -> coTreat t color
  GiveCard player card -> coGiveCard t player card
  TakeCard card -> coTakeCard t card
  DiscoverCure cards -> coDiscoverCure t cards
  RoleAbility ability -> coRoleAbility t ability
