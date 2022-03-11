module Solitaire.Website.Update
open Cards
open Solitaire.Model
open Solitaire.Actions

type StackCardLocation = {
  stacknum: int;
  cardnum: int;
}

type CardSelection =
  | NoSelection
  | TableCardSelected
  | StackCardSelected of StackCardLocation

type TargetSelection = 
  | StackTarget of int
  | AceTarget of int

type WebCommands =  // must be a DU
  | SelectCard of CardSelection
  | PlaceCard of TargetSelection
  | DrawCards

type WebGame = {
  selectedCard: CardSelection
  game: Game
}


let initialise args = 
  newDeck 
  |> shuffle 
  |> deal
  |> fun x -> {game = x; selectedCard = NoSelection}

  
let update message webgame =
  printfn "%A" message
  match message with
  | DrawCards -> 
      {webgame with 
        game = applyCommand Solitaire.Actions.DrawCards webgame.game
        selectedCard = NoSelection
      }
  | SelectCard selection -> 
      {webgame with selectedCard = selection}
  | PlaceCard target -> 
      match target, webgame.selectedCard with 
      | _, NoSelection -> webgame
      | AceTarget _, TableCardSelected ->
          {webgame with 
            game = applyCommand TableToAce webgame.game
            selectedCard = NoSelection 
          }
      | AceTarget _, StackCardSelected selection -> 
          {webgame with 
            game = applyCommand (StackToAce selection.stacknum) webgame.game
            selectedCard = NoSelection 
          } 
      | StackTarget toStack, TableCardSelected ->       
          {webgame with 
            game = applyCommand (TableToStack toStack) webgame.game
            selectedCard = NoSelection 
          } 
      | StackTarget toStack, StackCardSelected selection ->       
          {webgame with 
            game = 
              applyCommand (
                {
                  sourceStack=selection.stacknum
                  numCards=(webgame.game.stacks[selection.stacknum-1].Length - selection.cardnum) 
                  targetStack=toStack
                }
                |> MoveCards
              ) webgame.game
            selectedCard = NoSelection 
          } 
