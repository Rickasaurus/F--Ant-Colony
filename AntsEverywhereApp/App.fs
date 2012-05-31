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
    open System.Text.RegularExpressions

    let app = new Application()
    let gameController = new GameController()

    do  app.Exit.Add(fun _ -> (gameController :> System.IDisposable).Dispose())
    let win = new Window(Content = gameController)
    
    let tryParse arg =
      let reOPts = RegexOptions.Compiled ||| RegexOptions.IgnoreCase ||| RegexOptions.ExplicitCapture
      let m = Regex.Match(arg,@"^(?<k>[a-z][a-z0-9_-]*),(?<v>.+)$",reOPts)
      if not m.Success
        then  None
        else  Some(m.Groups.["k"].Value,m.Groups.["v"].Value)

    //HACK: put this in so the start-up AI doesn't have to be hard-coded [PB]
    [<STAThread;EntryPoint>]
    let main args =
      let args = args |> Array.choose (tryParse)
      if args.Length > 0 
        then  let blackK,blackV = args.[0]
              if args.Length > 1 
                then  let redK,redV = args.[1]
                      [blackV;redV] |> gameController.AddItemsFromDisk
                      gameController.StartWithAI blackK redK
                else  Seq.singleton blackV |> gameController.AddItemsFromDisk
                      gameController.StartWithAI blackK blackK
        else  gameController.StartWithDefaults()
      app.Run win
#endif
