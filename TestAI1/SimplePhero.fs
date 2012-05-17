//
// This is Richard Minerich's F# Ant Colony Silverlight Ediiton
// Visit my Blog at http://RichardMinerich.com
// This code is free to be used for anything you like as long as I am properly acknowledged.
//
// The basic Silverlight used here is based on Phillip Trelford's Missile Command Example
// http://www.trelford.com/blog/post/MissileCommand.aspx
//

module HardishAI

open AntsEverywhereLib.UserTypes
open AntsEverywhereLib.Helpers

let rnd = System.Random(int System.DateTime.Now.Ticks)

type TestAntBehavior() =
    interface IAntBehavior with
        member x.Name = "Rick's Hardish" 
        member x.Behave me here locations = 

            let locationsWithoutAnts = locations |> List.filter  (fun node -> node.Ant = None)

            let (|CarryingFood|CarryingMaxFood|CarryingNoFood|) (ant: AntView) =
                if ant.CarryingMaxFood  then CarryingFood
                elif ant.CarryingFood then CarryingMaxFood
                else CarryingNoFood

            let (|NearHome|_|) (locations: AntCellView list) =
                let homeNodes = locations |> List.filter (fun node -> node.IsMyNest)
                if List.isEmpty homeNodes then None
                else Some homeNodes
             
            let (|AwayFromHome|NearHome|) (locations: AntCellView list) =
                let homeLocations, awayLocations = locations |> List.partition (fun node -> node.IsMyNest)
                if List.isEmpty homeLocations then AwayFromHome awayLocations
                else NearHome homeLocations 

            let (|CanDrop|CantDrop|) (locations: AntCellView list) =
                let dropFoodLocations = locations |> List.filter (fun node -> not (node.IsFullOfFood))
                if List.isEmpty dropFoodLocations then CantDrop
                else CanDrop dropFoodLocations

            let (|HasUnownedFood|_|) (locations: AntCellView list) = 
                let foodLocations = locations |> List.filter (fun node -> node.HasFood && not (node.IsMyNest))
                if List.isEmpty foodLocations then None
                else Some foodLocations

            let (|HasPheromonesAndNoAnt|_|) (locations: AntCellView list) =
                let pheromoneLocations = locations |> List.filter (fun node -> node.Ant = None) |> List.filter (fun node -> node.HasFriendlyPheromone)
                if List.isEmpty pheromoneLocations then None
                else Some pheromoneLocations

            let (|HasNoAnt|_|) (locations: AntCellView list) =
                let emptyLocations = locations |> List.filter (fun node -> node.Ant = None)
                if List.length emptyLocations > 0 then
                    Some (emptyLocations)
                else None
            
            let (|ShortestDistanceWithNoAnt|_|)  (locations: AntCellView list) =
                let noAnts = locations |> List.filter (fun node -> node.Ant = None)
                if List.length noAnts > 0 then Some (noAnts |> List.minBy (fun node -> node.DistanceToNest))
                else None

            let maxFood = List.maxBy (fun (node: AntCellView) -> node.FoodContained)
            let minPhero = List.minBy (fun (node: AntCellView) -> node.FriendlyPheromoneQuantity)
            let noAnts = List.filter (fun (node: AntCellView) -> node.ContainsAnt)


            // [snippet:Simple Pheromone-Using Ant Colony AI]
            match me with
            | HasMaxFood -> 
                match locations with                    
                | NearHome homeCells -> 
                    match homeCells with
                    | CanDrop dropCells -> DropFood dropCells.Head
                    | HasNoAnt noAntCells -> Move (List.random noAntCells)
                    | _ -> Nothing
                | AwayFromHome allCells -> 
                    match here.FriendlyPheromoneQuantity with
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
