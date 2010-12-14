//
// This is Richard Minerich's F# Ant Colony Silverlight Ediiton
// Visit my Blog at http://RichardMinerich.com
// This code is free to be used for anything you like as long as I am properly acknowledged.
//
// The basic Silverlight used here is based on Phillip Trelford's Missile Command Example
// http://www.trelford.com/blog/post/MissileCommand.aspx
//

module HardishAI

open AntsEverywhereLib.Types
open AntsEverywhereLib.Helpers

let rnd = System.Random(int System.DateTime.Now.Ticks)

type TestAntBehavior() =
    interface IAntBehavior with
        member x.Name = "Rick's Hardish" 
        member x.Behave me here locations nest = 

            let isMyHome node = node.CellType = WorldCellType.NestCell(me.Color)
            let locationsWithoutAnts = locations |> List.filter  (fun node -> node.Ant = None)

            let (|HasFood|HasMaxFood|HasNoFood|) (ant: Ant) = 
                if ant.FoodCarried = 0 then HasNoFood
                elif ant.FoodCarried = maxFoodAntCanCarry then HasMaxFood
                else HasFood

            let (|NearHome|_|) (locations: WorldCell list) =
                let homeNodes = locations |> List.filter (fun node -> isMyHome node)
                if List.isEmpty homeNodes then None
                else Some homeNodes
             
            let (|AwayFromHome|NearHome|) (locations: WorldCell list) =
                let homeLocations, awayLocations = locations |> List.partition (fun node -> isMyHome node)
                if List.isEmpty homeLocations then AwayFromHome awayLocations
                else NearHome homeLocations 

            let (|CanDrop|CantDrop|) (locations: WorldCell list) =
                let dropFoodLocations = locations |> List.filter (fun node -> not (node.IsFullOfFood))
                if List.isEmpty dropFoodLocations then CantDrop
                else CanDrop dropFoodLocations

            let (|HasUnownedFood|_|) (locations: WorldCell list) = 
                let foodLocations = locations |> List.filter (fun node -> node.HasFood && not (isMyHome node))
                if List.isEmpty foodLocations then None
                else Some foodLocations

            let (|HasPheromonesAndNoAnt|_|) (locations: WorldCell list) =
                let pheromoneLocations = locations |> List.filter (fun node -> node.Ant = None) |> List.filter (fun node -> node.HasPheromone me.Color)
                if List.isEmpty pheromoneLocations then None
                else Some pheromoneLocations

            let (|HasNoAnt|_|) (locations: WorldCell list) =
                let emptyLocations = locations |> List.filter (fun node -> node.Ant = None)
                if List.length emptyLocations > 0 then
                    Some (emptyLocations)
                else None
            
            let (|ShortestDistanceWithNoAnt|_|)  (locations: WorldCell list) =
                let noAnts = locations |> List.filter (fun node -> node.Ant = None)
                if List.length noAnts > 0 then Some (noAnts |> List.minBy (fun node -> nest.Distance node))
                else None

            let maxFood = List.maxBy (fun node -> node.Food)
            let minPhero = List.minBy (fun node -> node.Pheromones.[me.Color])
            let noAnts = List.filter (fun node -> node.Ant = None)

            // [snippet:Simple Pheromone-Using Ant Colony AI]
            match me with
            | HasFood
            | HasMaxFood -> 
                match locations with                    
                | NearHome homeCells -> 
                    match homeCells with
                    | CanDrop dropCells -> DropFood dropCells.Head
                    | HasNoAnt noAntCells -> Move (List.random noAntCells)
                    | _ -> Nothing
                | AwayFromHome allCells -> 
                    match here.Pheromones.[me.Color] with
                    | n when n < 20 -> DropPheromone (here, 100 - n)
                    | _ -> match allCells with
                           | HasNoAnt noAnts when rnd.Next(0, 3) = 0 -> Move (List.random noAnts)
                           | ShortestDistanceWithNoAnt node -> Move node
                           | _ -> Nothing
            | HasNoFood -> 
                match locations with
                | HasNoAnt noAnts when rnd.Next(0, 3) = 0 -> Move (List.random noAnts)                        
                | HasUnownedFood foodCells -> TakeFood (maxFood foodCells)
                | HasPheromonesAndNoAnt pheroCells -> Move (minPhero pheroCells)
                | HasNoAnt noAntCells -> Move (List.random noAntCells)
                | _ -> Nothing
