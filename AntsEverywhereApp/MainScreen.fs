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
open System.Collections.Generic
open System.Reflection
open System.Windows
open System.Windows.Controls
open System.Windows.Data
open System.Windows.Input
open System.Windows.Media
open System.Windows.Shapes
open System.Windows.Threading

type MainScreen () as this =
    inherit UserControl ()

    //
    // Phillip Trelford's great pattern for IDisposables
    //

    let mutable disposables = []
    let remember (disposable : IDisposable) = disposables <- disposable :: disposables
    let dispose (d:IDisposable) = d.Dispose()
    let forget () = disposables |> List.iter dispose; disposables <- []

    let outerLayout = new StackPanel()
    let layout = Grid()
    do this.Content <- layout

    member t.SwitchControls newControl = 
        layout.Children.Clear()
        layout.Children.SafeAdd newControl

    interface System.IDisposable with
        member this.Dispose() = forget()