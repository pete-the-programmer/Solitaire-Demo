module Solitaire.Website.Main

open Elmish
open Bolero
open Bolero.Html
open Cards
open Solitaire.Model
open Solitaire.Website.Update
open Solitaire.Website.Views


type MyApp() =
  inherit ProgramComponent<WebGame, WebCommands>()

  override this.Program =
    Program.mkSimple initialise update mainPage
