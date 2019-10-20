namespace Fabulous.iOS

open Fabulous
open UIKit

type iOSHost(window: UIWindow) =
    interface IHost with
        member __.GetRootView() =
            match window.RootViewController with
            | null -> failwith "No root view"
            | rootView -> rootView :> obj 

        member __.SetRootView(rootView) =
            match rootView with
            | :? UIViewController as viewController -> window.RootViewController <- viewController
            | _ -> failwithf "Incorrect model type: expected a UIViewController but got a %O" (rootView.GetType())

/// Program module - functions to manipulate program instances
[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module iOSProgram =    
    let private syncDispatch (dispatch: 'msg -> unit) =
        fun msg ->
            Xamarin.Essentials.MainThread.InvokeOnMainThreadAsync(fun () -> dispatch msg).Start()
            
    let private syncAction (fn: unit -> unit) =
        fun () ->
            Xamarin.Essentials.MainThread.InvokeOnMainThreadAsync(fun () -> fn()).Start()

    let runWith app arg program =
        let host = iOSHost(app)

        program
        |> Program.withCanReuseView ViewHelpers.canReuseView
        |> Program.withSyncDispatch syncDispatch
        |> Program.withSyncAction syncAction
        |> Program.runWithFabulous host arg
        
    let run app program =
        runWith app () program