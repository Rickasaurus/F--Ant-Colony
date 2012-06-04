//
// This is Richard Minerich's F# Ant Colony Silverlight Ediiton
// Visit my Blog at http://RichardMinerich.com
// This code is free to be used for anything you like as long as I am properly acknowledged.
//
// The basic Silverlight used here is based on Phillip Trelford's Missile Command Example
// http://www.trelford.com/blog/post/MissileCommand.aspx
//

module AntsEverywhereLib.World

open System

open Types
open Helpers
open AntsEverywhereLib.UserTypes

let BlackAntNest = new Nest( 0, 0, nestSize - 1, nestSize - 1 )
let RedAntNest = new Nest( 1 + xSize - nestSize, 1 + ySize - nestSize, nestSize - 1, nestSize - 1)

let (|InBlackNest|InRedNest|Neither|) (x,y) = 
    if BlackAntNest.IsInBounds x y then InBlackNest
    elif RedAntNest.IsInBounds x y then InRedNest
    else Neither

let getAntNest (ant: Ant) =
    match ant.Color with
    | AntColor.Black -> BlackAntNest
    | AntColor.Red -> RedAntNest

//let emptyPheromoneSet = Map.ofSeq 
//                        <| seq { let colors = Enum.GetValues(typeof<AntColor>) :?> AntColor array
//                                 let smells = Enum.GetValues(typeof<PheromoneType>) :?> PheromoneType array
//                                 for color in colors do 
//                                    for smell in smells do 
//                                        yield (color, smell), 0 }

let emptyPheromoneSet = Map.ofSeq 
                        <| seq { let colors = [| AntColor.Black; AntColor.Red |]
                                 for color in colors do 
                                    yield color, 0 }

let defaultCell id = {Id = id; Food = 0; Ant = None; CellType = FieldCell; Pheromones = emptyPheromoneSet }
let defaultBlackAnt = Some <| Ant(AntColor.Black, 0)
let defaultRedAnt = Some <| Ant(AntColor.Red, 0)

let buildWorldInitialWorld () =
    let rnd = new System.Random() in 
        Map.ofSeq <|
        seq { for x in 0 .. xSize do
                for y in 0 .. ySize do
                    let uid = uid (x, y)
                    let defaultcell = defaultCell uid
                    match x, y with
                    | InBlackNest -> yield uid, { defaultcell with Ant = defaultBlackAnt; CellType = NestCell(AntColor.Black) }
                    | InRedNest ->   yield uid, { defaultcell with Ant = defaultRedAnt; CellType = NestCell(AntColor.Red) }
                    | Neither ->     if chanceOfFood > rnd.NextDouble()
                                        then yield uid, { defaultcell with Food = rnd.Next(minGeneratedFoodPerSquare, maxGeneratedFoodPerSquare) }
                                        else yield uid, defaultcell 
            }

let getAntViews (world: TheWorld) = 
    let getWorldCell x y = Map.tryFind (uid (x,y)) world
    Map.fold
        (fun state (uid: UID) cell ->
            let x, y = (uid.X, uid.Y)
            match cell.Ant with
            | None -> state
            | Some(ant) ->
                let visibleCells = [ getWorldCell x (y - 1); getWorldCell x (y + 1); getWorldCell (x - 1) y; getWorldCell (x + 1) y ]
                                   |> List.choose id
                state @ [ant, cell, visibleCells, getAntNest ant])
        [] world

let getAntActions (bBehave: IAntBehavior) (rBehave: IAntBehavior) (views: (Ant * WorldCell * WorldCell list * Nest) list) =
    let getAntBehavior (ant: Ant) =
        match ant.Color with
        | AntColor.Black -> bBehave
        | AntColor.Red -> rBehave
    views |> List.map (fun (ant, cell, antView, nest) -> let behavior = getAntBehavior ant in 
                                                            cell, behavior.Behave (AntView(ant, ant)) (AntCellView (cell, ant, nest)) (AntNearbyView.FromWorldCells antView ant nest))
let buildDependentTransaction (expectedCells: WorldCell list) actions = 
    let predicate = (fun (world: TheWorld) -> expectedCells |> List.forall (fun (cell: WorldCell) -> (Map.find cell.Id world) = cell))
    let action = (fun (iworld: TheWorld) -> 
        List.fold (fun (cworld: TheWorld) (id, action) ->
            Map.add id (action cworld.[id]) cworld) iworld actions)
    predicate, action  

let dropPheromonesInTargetCell antColor quantity target = 
    let newValue = max (target.Pheromones.[antColor] + quantity) maxCellPheromoneQuantity in 
        { target with Pheromones = target.Pheromones.Add(antColor, newValue ) } 

let woundAntInTargetCell oldtarget =
    match oldtarget.Ant with
    | None -> oldtarget
    | Some (ant) ->
        let newWounds = ant.Wounds + 1 
        if newWounds >= maxAntWounds then { oldtarget with Ant = None } // Ant Dies
        else { oldtarget with Ant = Some <| ant.UpdateWounds(newWounds) }

let getWorldChangeTransactions actions =
    seq { for source, action in actions do
            let ant = Option.get source.Ant
            match action with
            | Nothing -> ()
            | Move (target) -> if Option.isSome target.Ant then ()
                               else yield buildDependentTransaction 
                                            [ source; target.WorldCell ]
                                            [ source.Id,           (fun oldcell -> { oldcell with Ant = None });
                                              target.WorldCell.Id, (fun oldtarget -> { oldtarget with Ant = source.Ant }) ]
            | TakeFood (target) -> if target.WorldCell.Food <= 0 then ()
                                   else 
                                       let foodToGet = min (target.WorldCell.Food) (maxFoodAntCanCarry - ant.FoodCarried)
                                       yield buildDependentTransaction
                                                [ source; target.WorldCell ]
                                                [ target.WorldCell.Id, (fun oldtarget -> { oldtarget with Food = oldtarget.Food - foodToGet });
                                                  source.Id,           (fun oldcell -> { oldcell with Ant = Some <| ant.UpdateFood(ant.FoodCarried + foodToGet) } ) ]
            | DropFood (target) -> if target.WorldCell.Food >= maxTotalFoodPerSquare then ()
                                   else 
                                       let foodToDrop = min (maxTotalFoodPerSquare - target.WorldCell.Food) (ant.FoodCarried)
                                       yield buildDependentTransaction
                                                [ source; target.WorldCell ]
                                                [ target.WorldCell.Id, (fun oldtarget -> { oldtarget with Food = oldtarget.Food + foodToDrop });
                                                  source.Id,           (fun oldcell -> { source with Ant = Some <| ant.UpdateFood(ant.FoodCarried - foodToDrop) }) ] 
            | DropPheromone (target, quantity) -> yield buildDependentTransaction [] [ target.WorldCell.Id, dropPheromonesInTargetCell ant.Color quantity ]
            | Attack (target) -> yield buildDependentTransaction [ source; target.WorldCell ] [ target.WorldCell.Id, woundAntInTargetCell ]
    }


let spawnAnts (world: TheWorld) =
    world 
    |> Map.map (fun uid cell -> 
        if cell.Ant.IsNone && cell.Food >= spawnFood then
            match uid.X, uid.Y with
            | InBlackNest -> { cell with Ant = Some <| Ant(AntColor.Black); Food = cell.Food - spawnFood }
            | InRedNest -> { cell with Ant = Some <| Ant(AntColor.Red); Food = cell.Food - spawnFood }
            | Neither -> cell
        else cell)

let degradePheromones (world: TheWorld) = 
    world 
    |> Map.map (fun uid cell -> { cell with Pheromones = cell.Pheromones |> Map.map (fun key quantity -> max (quantity - 1) 0) } )

let applyWorldTransactions (oldWorld: TheWorld) changes = 
    Seq.fold (fun (world: TheWorld) (pred, action) ->
                if pred world 
                then action world  
                else world) 
                oldWorld changes

let uid2xy (uid: UID) = uid.X, uid.Y

let worldCycle bPlayer rPlayer world : TheWorld =
    world            
    |> getAntViews
    |> getAntActions bPlayer rPlayer
    |> Seq.randomPermute
    |> getWorldChangeTransactions
    |> applyWorldTransactions world
    |> degradePheromones
    |> spawnAnts