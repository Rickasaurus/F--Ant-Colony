// This is Richard Minerich's F# Ant Colony Silverlight Ediiton
// Visit my Blog at http://RichardMinerich.com
// This code is free to be used for anything you like as long as I am properly acknowledged.
//
// The basic Silverlight used here is based on Phillip Trelford's Missle Command Example
// http://www.trelford.com/blog/post/MissileCommand.aspx
//

module AntsEverywhereExmampleAI

open AntsEverywhereLib.Types

let randomGen = new System.Random()

let getRandomVal min max =  
    lock randomGen (fun () -> randomGen.Next(min, max)) 

type TestAntBehavior() =
    interface IAntBehavior with
        member x.Name = "SimplePhero"
        member x.Behave me here locations nest = 

            let isMyHome node = node.CellType = WorldCellType.NestCell(me.Color)

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

            let (|SmellsPheromones|_|) (locations: WorldCell list) =
                let pheromoneLocations = locations |> List.filter (fun node -> node.HasPheromone me.Color)
                if List.isEmpty pheromoneLocations then None
                else Some pheromoneLocations

            let findHomeDirectionCell (locations: WorldCell list) = locations |> List.minBy (fun node -> nest.Distance node)
            let homeDirectionCell = findHomeDirectionCell locations

            let randomEmptyLocation locations = 
                let emptyLocations = locations |> List.filter (fun node -> node.Ant = None)
                if List.length emptyLocations > 0 then
                    Some (List.nth emptyLocations (getRandomVal 0 (List.length emptyLocations)))
                else
                    None
            
            let maxFood = List.maxBy (fun node -> node.Food)
            let maxPhero = List.maxBy (fun node -> node.Pheromones.[me.Color])

            match me with  
            | HasFood
            | HasMaxFood -> 
                match locations with 
                | HasUnownedFood cells 
                    when not (here.HasPheromone me.Color) -> 
                        DropPheromone (here, 100)
                | NearHome homeCells -> 
                    match homeCells with
                    | CanDrop dropCells -> DropFood dropCells.Head
                    | CantDrop -> Move homeCells.Head
                | AwayFromHome allCells -> 
                    if not homeDirectionCell.ContainsAnt 
                    then Move homeDirectionCell
                    else match randomEmptyLocation locations with
                            | Some location -> Move location
                            | None -> Nothing                                                                                 
            | HasNoFood -> 
                match locations with                         
                | HasUnownedFood foodCells -> 
                    TakeFood (maxFood foodCells)
                | SmellsPheromones pheroCells -> 
                    Move (maxPhero pheroCells)
                | _ -> match randomEmptyLocation locations with
                        | Some location -> Move location
                        | None -> Nothing
