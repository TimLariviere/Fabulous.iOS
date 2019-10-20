namespace CounterApp

open Fabulous
open Fabulous.iOS
open UIKit
open CoreGraphics

module App =
    type Model = 
      { Count : int 
        Step : int
        TimerOn : bool }

    type Msg = 
        | Increment 
        | Decrement 
        | Reset
        | SetStep of int
        | TimerToggled of bool
        | TimedTick

    type CmdMsg =
        | TickTimer
        
    let timerCmd () =
        async { do! Async.Sleep 200
                return TimedTick }
        |> Cmd.ofAsyncMsg
        
    let mapCmdMsgToCmd cmdMsg =
        match cmdMsg with
        | TickTimer -> timerCmd()
        
    let initModel () = { Count = 0; Step = 1; TimerOn=false }
        
    let init () = initModel () , []
    
    let update msg model =
        match msg with
        | Increment -> { model with Count = model.Count + model.Step }, []
        | Decrement -> { model with Count = model.Count - model.Step }, []
        | Reset -> init ()
        | SetStep n -> { model with Step = n }, []
        | TimerToggled on -> { model with TimerOn = on }, (if on then [ TickTimer ] else [])
        | TimedTick -> if model.TimerOn then { model with Count = model.Count + model.Step }, [ TickTimer ] else model, [] 

    let view model dispatch =
        View.UIViewController(
            title = "Fabulous.iOS CounterApp sample",
            view = View.UIView(
                backgroundColor = UIColor.White,
                subviews = [
                    View.UIButton(
                        frame = CGRect(100., 100., 100., 50.),
                        title = "Increment"
                    )
                ]
            )
        )
        
    let program = 
        Program.mkProgramWithCmdMsg init update view mapCmdMsgToCmd

type App() as window =
    inherit UIWindow(UIScreen.MainScreen.Bounds)
    
    do window.MakeKeyAndVisible()

    let runner =
        App.program
        |> Program.withConsoleTrace
        |> iOSProgram.run window
