module Solitaire.Website.Views

open Bolero
open Bolero.Html
open Cards
open Solitaire.Model
open Solitaire.Website.Update

let suits = [SYMBOL_HEART; SYMBOL_DIAMOND; SYMBOL_CLUB; SYMBOL_SPADE]

let wrap = List.singleton  // shortcut to wrap a thing in a list

type CardDisplay = {
  card: Card
  isFaceUp: bool
  isSelected: bool
  selection: CardSelection
}

type Main = Template<"wwwroot/main.html">
let main = Main()

let SuitNumber card =
  match card with 
  | Hearts _ -> 1
  | Diamonds _ -> 3
  | Clubs _ -> 0
  | Spades _ -> 2
  | Joker -> 4

let viewCard dispatch cardDisplay =
  match cardDisplay with 
  | { CardDisplay.isFaceUp=false } -> Main.CardBack().Elt()
  | { card=card; isSelected=isSelected; selection=selection } -> 
    Main.Card()
      .NumberOffset(card.Number.Ordinal - 1 |> string)
      .SuitOffset(SuitNumber card |> string)
      .CardText(card.ToString())
      .Selected(if isSelected then "selected" else "notselected")
      .CardClicked(fun _ -> selection |> SelectCard |> dispatch )
      .Elt()

let viewStack dispatch webgame stackNum =
  Main.Stack()
    .StackNum(stackNum.ToString())
    .StackCards(
      webgame.game.stacks[stackNum - 1]
      |> List.mapi ( fun cardnum card -> 
          let location={stacknum=stackNum; cardnum=cardnum}
          { 
            card=card.card
            isFaceUp=card.isFaceUp
            isSelected=(webgame.selectedCard=StackCardSelected location)
            selection=StackCardSelected location
          }
          |> viewCard dispatch
      )
      |> concat
    )
    .DropClicked(fun _ -> stackNum |> StackTarget |> PlaceCard |> dispatch )
    .Elt()

let viewStacks dispatch webgame =
  [1..6]
  |> List.map (viewStack dispatch webgame)
  |> concat

let private viewAceStack dispatch webgame aceStackNum =
  Main.AceStack()
    .Symbol(suits[aceStackNum - 1])
    .AceCards(
      webgame.game.aces[aceStackNum - 1]
      |> List.map ( fun card -> 
          {
            card=card
            isFaceUp=true
            isSelected=false
            selection=NoSelection
          } 
          |> viewCard dispatch
      )
      |> concat
    )
    .DropClicked(fun _ -> aceStackNum |>  AceTarget |> PlaceCard |> dispatch)
    .Elt()

let private viewAces dispatch webgame =
  [1..4]
  |> List.map (viewAceStack dispatch webgame)
  |> concat

let private viewTable dispatch webgame =
  match webgame.game.table with 
  | [] -> text ""
  | [card] -> 
      {
        card=card
        isFaceUp=true
        isSelected=(webgame.selectedCard=TableCardSelected)
        selection=NoSelection
      } 
      |> viewCard dispatch
    | topcard::rest -> 
        let facedowns = 
          rest
          |> List.map (fun card -> 
            {
              card=card
              isFaceUp=false
              isSelected=false
              selection=NoSelection
            }
            |> viewCard dispatch
          )
        let faceup = 
          {
            card=topcard
            isFaceUp=true
            isSelected=(webgame.selectedCard=TableCardSelected)
            selection=TableCardSelected
          }
          |> viewCard dispatch
        facedowns @ [ faceup ]
        |> concat

let private viewDeck dispatch webgame : Node =
  webgame.game.deck
  |> List.map (fun _ -> Main.CardBack().Elt() )
  |> concat

let mainPage webgame dispatch = 
  main
    .SelectionMode(if webgame.selectedCard = NoSelection then "mode_unselected" else "mode_selected")
    .Deck(viewDeck dispatch webgame)
    .Table(viewTable dispatch webgame)
    .Aces(viewAces dispatch webgame)
    .Stacks(viewStacks dispatch webgame)
    .DrawSomeCards(fun _ -> DrawCards |> dispatch)
    .Elt()
