//
// This is Richard Minerich's F# Ant Colony Silverlight Ediiton
// Visit my Blog at http://RichardMinerich.com
// This code is free to be used for anything you like as long as I am properly acknowledged.
//
// The basic Silverlight used here is based on Phillip Trelford's Missile Command Example
// http://www.trelford.com/blog/post/MissileCommand.aspx
//

namespace AntsEverywhereApp

open System
open System.Text
open System.IO
open System.Windows
open System.Windows.Controls
open System.Text.RegularExpressions

open AntsEverywhereLib.Types
open AntsEverywhereLib.UserTypes
open AntsEverywhereLib.World

open LoadingHelpers

module Params = 
    let numCycles = 1500

#if SILVERLIGHT
type App() as app =
    inherit Application()

    let onExit func = do app.Exit.Add(fun _ -> (func()))
    let setContent mainControl = app.RootVisual <- mainControl
    let startApp () = ()
    let onWinner wintext = ()
    let loaded = [] 
    let logIt msg = ()

    do GameControl.startGame onExit setContent startApp onWinner loaded Params.numCycles logIt |> ignore
#else
module StartUp =
    let logfile = "antwars.log"

    // Set up App
    let app = new Application()
    let win = ref null
    let setContent mainControl =  win := new Window(Content = mainControl)
    let onExit func = do app.Exit.Add(fun _ -> (func()))
    let startApp () = app.Run !win
    let onWinner wintext = do MessageBox.Show(wintext, "Contest Winner!", MessageBoxButton.OK, MessageBoxImage.Information) |> ignore

    let logIt str = File.AppendAllText(logfile, str)

    // Command Line Parameter Parsing
    let tryParse arg =
      let reOPts = RegexOptions.Compiled ||| RegexOptions.IgnoreCase ||| RegexOptions.ExplicitCapture
      let m = Regex.Match(arg,@"^(?<k>[a-z][a-z0-9_-]*),(?<v>.+)$",reOPts)
      if not m.Success
        then  None
        else  Some(m.Groups.["k"].Value,m.Groups.["v"].Value)

    [<STAThread;EntryPoint>]
    let main args =
        let antfiles = args |> Array.choose (tryParse) |> Array.map (fun (k,v) -> v)
        let loaded = args |> Seq.choose (fun fileName -> use stream = File.OpenRead(fileName) in loadAIFromStream stream)
        GameControl.startGame onExit setContent startApp onWinner loaded Params.numCycles logIt
#endif
