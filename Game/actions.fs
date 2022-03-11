module Solitaire.Actions

open System
open Cards
open Solitaire.Model

let (|Number|_|) (ch:Char) =
  match Char.GetNumericValue(ch) with
  | -1.0 -> None
  | a -> a |> int |> Some

let deal shuffledDeck = 
  let emptyGame = {
    deck = shuffledDeck |> List.except [Joker]
    table = []
    stacks = []
    aces = List.init 4 (fun _ -> [])
  }
  [6..-1..1] 
  |>  List.fold (fun game i -> 
        let newStack = 
          game.deck 
          |> List.take i                        // flip the last card
          |> List.mapi (fun n card -> { isFaceUp = (n = i - 1); card=card}) 
        {game with
          stacks = game.stacks @ [ newStack ]
          deck = game.deck |> List.skip i
        }
      
      ) emptyGame

let private drawCards game =
  let withEnoughCardsToDraw =
    match game.deck.Length with
    | n when n < 3 -> 
      {game with  
        deck = game.deck @ game.table
        table = []
      }
    | _ -> game
  // in case there is less than 3 remaining
  let cardsToTake = Math.Min(3, withEnoughCardsToDraw.deck.Length)  
  {withEnoughCardsToDraw with
    table = 
      (withEnoughCardsToDraw.deck |> List.take cardsToTake)
      @ withEnoughCardsToDraw.table
    deck = withEnoughCardsToDraw.deck |> List.skip cardsToTake
  }

type Card with 
  member this.Number =
    match this with 
    | Hearts a    
    | Diamonds a  
    | Clubs a     
    | Spades a   -> a
    | Joker      -> failwith "Joker?!?!?"

type CardNumber with 
  member this.Ordinal =
    match this with 
    | Ace   -> 1 
    | Two   -> 2
    | Three -> 3 
    | Four  -> 4 
    | Five  -> 5 
    | Six   -> 6 
    | Seven -> 7 
    | Eight -> 8 
    | Nine  -> 9 
    | Ten   -> 10
    | Jack  -> 11
    | Queen -> 12 
    | King  -> 13

let (|IsRed|_|) (card:Card) =
  match card with 
  | Hearts _
  | Diamonds _ -> Some card
  | _ -> None

let (|IsBlack|_|) (card:Card) =
  match card with 
  | IsRed _ -> None
  | _ -> Some card

let private canAddToStack (stack: StackCard list) (card:Card) =
  if stack = [] && card.Number = King then 
    true
  else
    let bottomCard = stack |> List.last
    match bottomCard.card, card with 
    | IsRed a, IsBlack b
    | IsBlack a, IsRed b 
        when a.Number.Ordinal = b.Number.Ordinal + 1
            -> true
    | _, _  -> false

let private canMoveCardsBetweenStacks sourceStack numCards targetStack game =
  // make things a bit easier to call the above function
  //  by making the arguments the same as the move...() function
  let stack = game.stacks[targetStack - 1]
  let card = 
    game.stacks[sourceStack - 1] 
    |> List.skip ( game.stacks[sourceStack - 1].Length - numCards )
    |> List.head
  canAddToStack stack card.card

let private addToStack (stackNum:int) (card:Card) (stacks: StackCard list list) =
  let updatedStack = stacks[stackNum] @ [ { isFaceUp=true; card=card} ]
  stacks |> List.updateAt stackNum updatedStack

let private tableToStack stackNum game =
  let stack = game.stacks[stackNum]
  match game.table with 
  | [] -> game // do nothing
  | [a]-> 
    {game with 
      table = []; 
      stacks = game.stacks |> addToStack stackNum a 
    }
  | a::rest -> 
    {game with 
      table = rest; 
      stacks = game.stacks |> addToStack stackNum a 
    }

let private flipNext stack =
  let numFaceUp =
    stack 
    |> List.filter (fun a -> a.isFaceUp)
    |> List.length
  match stack.Length, numFaceUp with 
  | 0, _ -> stack // no cards to flip
  | n, 0 -> // none face up
    stack
    |> List.updateAt 
        (n - 1) 
        {stack[n - 1] with isFaceUp=true}
  | _, _ -> stack //anything else

let private moveCardsBetweenStacks sourceStack numCards targetStack game =
  // remember - on screen we start at one, but lists start at zero
  let numCardsInStack = game.stacks[sourceStack - 1].Length
  // do the move
  let moving = game.stacks[sourceStack - 1] |> List.skip ( numCardsInStack - numCards )
  let source = game.stacks[sourceStack - 1] |> List.take ( numCardsInStack - numCards )
  let target = game.stacks[targetStack - 1] @ moving
  // flip next card?
  let sourceFlipped = flipNext source
  //reconstruct the game
  { game with 
      stacks = 
        game.stacks 
        |> List.updateAt (sourceStack - 1) sourceFlipped 
        |> List.updateAt (targetStack - 1) target 
  }

let private acesStackNum card =
  match card with 
  | Hearts _ -> 0
  | Diamonds _ -> 1
  | Clubs _ -> 2
  | Spades _ -> 3
  | Joker _ -> failwith "AAAAH! A Joker!?!?"

let private canAddToAce cards game =
  match cards with 
  | [] -> false
  | [card]
  | card::_ ->
    let stackNum = acesStackNum card
    let target = game.aces[stackNum] |> List.rev // (so we can easily see the last card as the "head") 
    match target, card with 
    | [], c when c.Number = Ace -> true
    | [a], c 
    | a::_, c 
        when a.Number.Ordinal = c.Number.Ordinal - 1 
        -> true
    | _ -> false

let private canAddToAceFromStack sourceStack game =
  // Make it easier to call the above function for stacks
  let cards = 
    game.stacks[sourceStack - 1] 
    |> List.map (fun a -> a.card)
    |> List.rev
  canAddToAce cards game

let private addToAce card game =
  let stackNum = acesStackNum card
  let target = game.aces[stackNum] @ [card]
  {game with 
    aces =
      game.aces
      |> List.updateAt stackNum target
  }

let private moveToAceFromStack sourceStack game =
  match game.stacks[sourceStack - 1] with 
  | [] -> game
  | [a] -> 
    let addedToAce = addToAce a.card game
    {addedToAce with 
      stacks = 
        game.stacks 
        |> List.updateAt (sourceStack - 1) [] 
    }
  | a ->
    //we need the last card, not the first
    let source, moving = 
      a 
      |> List.splitAt ( a.Length - 1 )
    let sourceFlipped = flipNext source
    let addedToAce = addToAce moving.Head.card game
    {addedToAce with 
      stacks = 
        game.stacks 
        |> List.updateAt (sourceStack - 1) sourceFlipped 
    }


let private moveToAceFromTable game =
  match game.table with 
  | [] -> game
  | [a] -> 
    let addedToAce = addToAce a game
    {addedToAce with table = [] }
  | a::rest -> 
    let addedToAce = addToAce a game
    {addedToAce with table = rest }

// The _external_ arguments for "MoveCards"
type MoveArgs = { sourceStack: int; numCards: int; targetStack: int; }

type SolitaireCommands = 
  | DrawCards
  | TableToStack of int
  | MoveCards of MoveArgs
  | TableToAce
  | StackToAce of int

let applyCommand (cmd: SolitaireCommands) (game: Game) =
  match cmd with 
  | DrawCards      
      -> game |> drawCards
  | TableToStack a
      when (a >= 1 && a <= 6)
      &&   canAddToStack (game.stacks[a - 1]) (game.table.Head)
      -> game |> tableToStack (a - 1)
  | MoveCards args 
      when (args.targetStack >= 1 && args.targetStack <= 6) 
      &&   canMoveCardsBetweenStacks args.sourceStack args.numCards args.targetStack game
      -> game |> moveCardsBetweenStacks args.sourceStack args.numCards args.targetStack
  | TableToAce
      when canAddToAce game.table game  
      -> game |> moveToAceFromTable
  | StackToAce sourceStack 
      when (sourceStack >= 1 && sourceStack <= 6) 
      &&   canAddToAceFromStack sourceStack game
      -> game |> moveToAceFromStack sourceStack
  | _ -> game

