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
open System.Windows
open System.Windows.Controls
open System.Windows.Data
open System.Windows.Input
open System.Windows.Media
open System.Windows.Shapes
open System.Windows.Threading
open System.Threading.Tasks

open AntsEverywhereLib.Types
open AntsEverywhereLib.UserTypes
open AntsEverywhereLib.World

type SimulationResult = 
    {
        RedAI: IAntBehavior
        BlackAI: IAntBehavior
        Winner: IAntBehavior
        Control: SimulationControl
    }

and SimulationControl () as this =
    inherit UserControl ()

    let mutable disposables = []
    let remember disposable = disposables <- disposable :: disposables
    let dispose (d:IDisposable) = d.Dispose()
    let forget () = disposables |> List.iter dispose; disposables <- []

    let simulationEndedEvent = Event<SimulationResult>();

    let width, height = 350.0, 350.0
    
    let wm, hm = width / float (xSize + 1), height / float (ySize + 1)
    let offset x y = x * wm, y * hm  
    do  this.Width <- width; this.Height <- height
#if SILVERLIGHT
    let canvas = Canvas(Background=SolidColorBrush(Colors.Orange), Cursor=Cursors.Stylus)
#else
    let canvas = Canvas(Background=SolidColorBrush(Colors.Orange))
#endif

    let layout = Grid()
    let syncCtxt = System.Threading.SynchronizationContext.Current 

    let mutable lastScore = None
    let updateScore s =
        lastScore |> Option.iter (fun t -> layout.Children.Remove t |> ignore)
        let t = TextBlock(Text=s)
        t.HorizontalAlignment <- HorizontalAlignment.Center
        t.VerticalAlignment <- VerticalAlignment.Top
        t.Foreground <- SolidColorBrush Colors.White
        lastScore <- Some t
        layout.Children.SafeAdd t

    let mutable lastMessage = None
    let updateMessage s =
        lastMessage |> Option.iter (fun t -> layout.Children.Remove t |> ignore)
        let t = TextBlock(Text=s)
        t.HorizontalAlignment <- HorizontalAlignment.Center
        t.VerticalAlignment <- VerticalAlignment.Center
        t.Foreground <- SolidColorBrush Colors.White
        lastMessage <- Some t
        layout.Children.SafeAdd t

    let drawAnt x y antColor = 
        let color = match antColor with
                    | AntColor.Black -> Colors.Black
                    | AntColor.Red -> Colors.Red
        let brush = SolidColorBrush(color) :> Brush
        let transform = TranslateTransform(X = x, Y = y)       
        let radius = wm * 0.5
        let yoffset = radius / 2.0
        let xoffset = radius / 6.0
        Ellipse(Width=radius, Height=radius, Fill=brush, RenderTransform = TranslateTransform(X = x + xoffset, Y = y + yoffset))
        |> canvas.Children.SafeAdd
        Ellipse(Width=radius, Height=radius, Fill=brush, RenderTransform = TranslateTransform(X = x + (radius / 1.5) + xoffset, Y = y + yoffset))
        |> canvas.Children.SafeAdd

    let drawFood x y = 
        let color = Colors.Green
        let brush = SolidColorBrush(color) :> Brush
        let transform = TranslateTransform(X = x, Y = y)
        Ellipse(Width=1.0 * wm,Height=1.0 * hm, Fill=brush, RenderTransform=transform)
        |> canvas.Children.SafeAdd

    let makeGradiant quantity max = (float quantity / float max)
    let drawPheromone x y antColor amount =
        let color = match antColor with                    
                    | AntColor.Black -> Colors.Gray
                    | AntColor.Red -> Colors.Purple
        let opacity = makeGradiant amount maxAntDropPheromoneQunatity
        let brush = SolidColorBrush(color, Opacity = opacity) :> Brush
        let transform = TranslateTransform(X = x, Y = y)        
        Rectangle(Width=1.0 * wm,Height=1.0 * hm, Fill=brush, RenderTransform=transform)
        |> canvas.Children.SafeAdd
        |> ignore

    let drawScore bName bFood rName rFood remaing = 
        do updateScore (sprintf "Black (%s): %d vs Red (%s): %d\nRemaining Cycles %d" bName bFood rName rFood remaing)

    let drawUpdates (world: TheWorld) =
        canvas.Children.Clear()
        canvas.Background <- (SolidColorBrush(Colors.Orange) :> Brush)
        world
        |> Map.iter (fun uid cell -> let x, y = uid2xy uid
                                     let ox, oy = offset (float x) (float y)
                                     cell.Pheromones |> Map.iter (fun color amount -> if amount > 0 then drawPheromone ox oy color amount)
                                     if cell.Food > 0 then drawFood ox oy                                     
                                     if cell.Ant.IsSome then drawAnt ox oy cell.Ant.Value.Color)

    let gameTimer = DispatcherTimer()
    let rec startGame (blackAI : IAntBehavior) (redAI : IAntBehavior) maxCycles  = 
        let world = ref (buildWorldInitialWorld())
        drawUpdates !world

        let foodToWin = int <| double (Map.fold (fun s k v -> s + v.Food) 0 !world) * percentFoodToWin
        let cycles = ref 0

        let updateScoreAndCheckForWinner (world: TheWorld) = 
            cycles := !cycles + 1
            let mutable bScore = 0
            let mutable rScore = 0
            for (k,v) in world |> Map.toSeq do
                match v.Ant with
                | None -> ()
                | Some (ant) -> if ant.Color = AntColor.Black then bScore <- bScore + 1
                                elif ant.Color = AntColor.Red then rScore <- rScore + 1

            drawScore blackAI.Name bScore redAI.Name rScore (maxCycles  - !cycles)
            if bScore = 0 || rScore = 0 || !cycles > maxCycles then
                if bScore > rScore then Some("Black", blackAI)
                elif rScore > bScore then Some("Red", redAI)
                else None
            else None

        let updateworld = 
            async {
                world := worldCycle blackAI redAI !world 
            }

        gameTimer.Interval <- TimeSpan.FromMilliseconds(10.0)
        gameTimer.Tick
        |> Observable.subscribe (fun _ ->
            try
                Async.StartImmediate updateworld
                drawUpdates !world
                let winner = updateScoreAndCheckForWinner !world
                match winner with
                    | None -> ()
                    | Some (color, ai) ->
                        updateMessage (sprintf "%s (%s) Won!" color ai.Name)
                        gameTimer.Stop()
                        simulationEndedEvent.Trigger({ RedAI = redAI; BlackAI = blackAI; Winner = ai; Control = this })
            with e -> 
                updateMessage (sprintf "%O" e.Message)
                gameTimer.Stop()
        )
        |> remember
        gameTimer.Start()

        {new IDisposable with member this.Dispose() = gameTimer.Stop()} |> remember

    do this.IsVisibleChanged.Subscribe (fun (args: DependencyPropertyChangedEventArgs) -> if (args.NewValue :?> bool) then gameTimer.Start() else gameTimer.Stop() ) |> ignore
    let clickedEvent = new Event<_>()

    do  layout.Children.SafeAdd canvas
        this.Content <- layout

    do 
        updateMessage ""
        this.MouseLeftButtonUp
        |> Event.add (fun _ -> clickedEvent.Trigger())
    
    [<CLIEvent>]
    member this.ClickedEvent = clickedEvent.Publish

    [<CLIEvent>]
    member this.SimulationEndedEvent = simulationEndedEvent.Publish

    member this.StartSimulation blackAI redAI maxCylces =
        forget()
        startGame blackAI redAI maxCylces
    
    member this.IsRunning = gameTimer.IsEnabled
   
    interface System.IDisposable with
        member this.Dispose() = forget()