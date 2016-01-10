module Main where

import Prelude (class Functor, Unit, ($), const, bind, pure, not, unit)

import Control.Monad.Aff (runAff)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (throwException)

import Halogen (HalogenEffects(), ComponentDSL, Natural, ComponentHTML, Component, component, gets, modify, runUI)
import Halogen.Util (appendToBody, onLoad)
import Halogen.HTML.Indexed as H
import Halogen.HTML.Events.Indexed as E

-- | The state of the component
type State = { on :: Boolean }

initialState :: State
initialState = { on: false }

-- | The query algebra for the component
data Query a
  = ToggleState a
  | GetState (Boolean -> a)

-- | The component definition
ui :: forall g. (Functor g) => Component State Query g
ui = component render eval
  where

  render :: State -> ComponentHTML Query
  render state =
    H.div_
      [ H.h1_
          [ H.text "Toggle Button ... !" ]
      , H.button
          [ E.onClick (E.input_ ToggleState) ]
          [ H.text (if state.on then "On" else "Off") ]
      ]

  eval :: Natural Query (ComponentDSL State Query g)
  eval (ToggleState next) = do
    modify (\state -> { on: not state.on })
    pure next
  eval (GetState continue) = do
    value <- gets _.on
    pure (continue value)

main :: Eff (HalogenEffects ()) Unit
main = runAff throwException (const (pure unit)) $ do
  app <- runUI ui initialState
  onLoad $ appendToBody app.node
