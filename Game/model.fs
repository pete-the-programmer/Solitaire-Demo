module Solitaire.Model

open System
open Cards

type StackCard = {
  card: Card
  isFaceUp: bool
} with 
    override this.ToString() =
      if this.isFaceUp then
        this.card.ToString()
      else 
        "###"

type Game = {
  deck: Card list
  table: Card list
  stacks: StackCard list list
  aces: Card list list
}

type Phase = 
  | General
  | SelectingSourceStack
  | SelectingNumCards of int
  | SelectingTargetStack of int * int
  | SelectingAceSource
  | PlayerHasWon

type MultiPhaseGame = {
  game: Game
  phase: Phase
}