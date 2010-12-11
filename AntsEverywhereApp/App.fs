//
// This is Richard Minerich's F# Ant Colony Silverlight Ediiton
// Visit my Blog at http://RichardMinerich.com
// This code is free to be used for anything you like as long as I am properly acknowledged.
//
// The basic Silverlight used here is based on Phillip Trelford's Missle Command Example
// http://www.trelford.com/blog/post/MissileCommand.aspx
//

namespace AntsEverywhereApp

open System.Windows

type App() as app =
    inherit Application()
    let game = new GameControl()
    do  app.Startup.Add(fun _ -> app.RootVisual <- game)
    do  app.Exit.Add(fun _ -> (game :> System.IDisposable).Dispose())