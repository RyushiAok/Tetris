namespace Tetris

open System 
open Elmish
open Avalonia
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.Input
open Avalonia.FuncUI
open Avalonia.FuncUI.Elmish
open Avalonia.FuncUI.Components.Hosts
open Avalonia.Threading 

type MainWindow() as this =
    inherit HostWindow()

    do
        base.Title <- "Tetris"
        base.Width <- 450.0
        base.Height <- 600.0 

        let keyDownHandler (_state: Game.State) =
            let sub dispatch =  
                this.KeyDown.Add (fun eventArgs ->
                    match eventArgs.Key with
                    | Key.RightShift -> Game.Msg.RotL
                    | Key.LeftShift  -> Game.Msg.RotL
                    | Key.Space -> Game.Msg.RotR
                    | Key.S -> Game.Msg.Down
                    | Key.A -> Game.Msg.Left
                    | Key.D -> Game.Msg.Right 
                    | Key.E -> Game.Msg.Hold 
                    | _ -> Game.Msg.Empty
                    |> dispatch )
                |> ignore
            Cmd.ofSub sub


        let timer (_state: Game.State) =
            let sub dispatch =
                let invoke () =
                    Game.Update |> dispatch
                    true
                DispatcherTimer.Run(Func<_>(invoke), TimeSpan.FromMilliseconds 10.0)
                |> ignore 
            Cmd.ofSub sub

        Elmish.Program.mkSimple (fun () -> Game.init ) Game.update Game.view
        |> Program.withHost this 
        |> Program.withSubscription timer
        |> Program.withSubscription keyDownHandler
        |> Program.run
 

type App() =
    inherit Application()

    override this.Initialize() =
        this.Styles.Load "avares://Avalonia.Themes.Default/DefaultTheme.xaml"
        this.Styles.Load "avares://Avalonia.Themes.Default/Accents/BaseDark.xaml"

    override this.OnFrameworkInitializationCompleted() =
        match this.ApplicationLifetime with
        | :? IClassicDesktopStyleApplicationLifetime as desktopLifetime -> desktopLifetime.MainWindow <- MainWindow()
        | _ -> ()


module Program =
    [<EntryPoint>]
    let main argv = 
        AppBuilder
            .Configure<App>()
            .UsePlatformDetect()
            .UseSkia()
            .StartWithClassicDesktopLifetime(argv)