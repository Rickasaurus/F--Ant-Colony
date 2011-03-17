//
// This is Richard Minerich's F# Ant Colony Silverlight Ediiton
// Visit my Blog at http://RichardMinerich.com
// This code is free to be used for anything you like as long as I am properly acknowledged.
//
// The basic Silverlight used here is based on Phillip Trelford's Missile Command Example
// http://www.trelford.com/blog/post/MissileCommand.aspx
//

namespace AntsEverywhereApp

open System.Windows

#if SILVERLIGHT
type App() as app =
    inherit Application()

    let gameController = new GameController()
    
    do  app.Startup.Add(fun e ->
        gameController.SetInitParams(e.InitParams)
        app.RootVisual <- gameController)
    do  app.Exit.Add(fun _ -> (gameController :> System.IDisposable).Dispose())
#else
module StartUp =
    open System

    let app = new Application()
    let gameController = new GameController()

    do  app.Exit.Add(fun _ -> (gameController :> System.IDisposable).Dispose())
    let win = new Window(Content = gameController)
    [<STAThread>]
    do app.Run win |> ignore
#endif
