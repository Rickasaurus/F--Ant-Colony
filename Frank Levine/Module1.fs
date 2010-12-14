//
// This is Richard Minerich's F# Ant Colony Silverlight Ediiton
// Visit my Blog at http://RichardMinerich.com
// This code is free to be used for anything you like as long as I am properly acknowledged.
//
// The basic Silverlight used here is based on Phillip Trelford's Missile Command Example
// http://www.trelford.com/blog/post/MissileCommand.aspx
//

module AntsEverywhereExmampleAI

open AntsEverywhereLib.Types

let randomGen = new System.Random()

let getRandomVal min max = 
    lock randomGen (fun () -> randomGen.Next(min, max))

type TestAntBehavior() =
    interface IAntBehavior with
        member x.Name = "Frank_Levine"
        member x.Behave me here locations nest =
           
            // This Ant's basic strategy is this:
            // If you have food and are near the nest
            //      drop the food
            // If you can't carry anymore food (bur are not near the nest)
            //      head back to the nest with the following exception
            //          if the current cell (here) has <40 phereomones, replenish the supply back to 100
            // If you're not dropping off food or heading home, you're foraging
            //      The logic for foraging is:
            //      If you see food, take it (this applies even when you have food but aren't full)
            //      If you see pheromones, move to the pheromone that is farthest from the nest
            //          if all pheromones are closer to the nest than you, then make a random move
            //      Otherwise you'e in the middle of nowhere, wanter randomly
            //
            // Special note on 'Traffic Control':  Inbound ants always yield to outbound ants
            //                                     This seems reasonable since the inbound ants
            //                                     Know where they're going and the outbound ones
            //                                     Are dependent on the pheromone trail
           
           
           
            //                                    
            // helper functions
            let isNest (cell: WorldCell) = cell.CellType = WorldCellType.NestCell(me.Color)
           
            // how do I negate a function?!?  this seems a bit heavy-handed
            let isNotNest (cell: WorldCell) =
                if isNest cell then
                    false
                else
                    true

            // nest cells that can receive food
            let nestCells = locations |> List.filter isNest
                                      |> List.filter (fun c -> c.IsFullOfFood = false)

            // all empty neighbors, sorted so we can get at the closest and farthest ones from the nest
            // first = closest to nest
            // last = farthest from nest
            let emptyNeighbors = locations |> List.filter (fun c -> c.ContainsAnt = false)
                                           |> List.sortBy (fun c -> nest.Distance(c))                                     

            // all empty neighbors with my pheromones
            let emptyNeighborsWithP = emptyNeighbors |> List.filter( fun c -> c.HasPheromone(me.Color))                                                   
                                                     |> List.sortBy( fun c -> nest.Distance(c))
                                                     |> List.toArray

            // all neighbors with food, ordered by the amount of food decending
            let neighborsWithFood = locations |> List.filter (isNotNest)
                                              |> List.filter (fun c -> c.HasFood)
                                              |> List.sortBy (fun c -> c.Food)
                                              |> List.rev

            // functions to make the code below more readable
            // NullMove does nothing (like when you're boxed in)
            // RandomMove is... Random
            let NullMove = fun() -> Move here

            let RandomMove = fun () ->
                let i = getRandomVal 0 emptyNeighbors.Length
                Move (List.nth emptyNeighbors i)


            // maximum amount of pheromone to leave on a cell
            let MAX_PHERO = 100;
           
            // when returning to the nest, add more pheromones when the cell
            // has less than this number
            let REFRESH_THRESHOLD = 50;



            // active pattern to determine the ant's high-level state           
            let (|ShouldDropFood|Forage|ReturnToNest|) (ant: Ant) =
                let haveAvailableNestCells = (nestCells.IsEmpty = false)
                match ant with
                    | a when a.HasFood && haveAvailableNestCells -> ShouldDropFood
                    | a when a.IsFullOfFood -> ReturnToNest
                    | _ -> Forage

            // active pattern to decide if we need to refresh pheromones
            let (|NeedsRefresh|NoRefresh|) (cell: WorldCell) =
                match cell.Pheromones.[me.Color] with
                    | x when x < REFRESH_THRESHOLD ->
                        let amt = MAX_PHERO - x     // amt is the number of pheromones required to bring this cell back to 100
                        NeedsRefresh amt
                    | _ -> NoRefresh    // there are enough for now

            // gets the relative distance to the nest
            // relativeDist > 0 --> cell is farther from the nest than 'here'
            // relativeDist < 0 --> cell is closer to the nest than 'here'                   
            let relativeDist (cell: WorldCell) =
                let dHere = nest.Distance(here)
                let dCell = nest.Distance(cell)
                dCell - dHere

            // function to get the last thing from an array
            let last (arr: 'a[]) =
                arr.[arr.Length-1]

            // the ant parameter isn't used, but I don't know how to make a
            // parameterless active pattern
            let (|AdjacentToFood|AdjacentToPheromone|NoMansLand|) (ant: Ant) =
                if neighborsWithFood.Length > 0 then
                    AdjacentToFood
                elif emptyNeighborsWithP.Length > 0 && relativeDist (last emptyNeighborsWithP) > 0. then   
                    // remember emptyNeighborsWithP is sorted
                    AdjacentToPheromone (last emptyNeighborsWithP)
                else
                    NoMansLand

            // The Actual logic...

            if emptyNeighbors.IsEmpty then
                NullMove()
            else
                match me with
                | ShouldDropFood -> DropFood nestCells.Head               
                | ReturnToNest ->
                    match here with
                    | NeedsRefresh amt -> DropPheromone (here, amt)                   
                    | NoRefresh -> Move emptyNeighbors.Head
                | Forage ->
                    match me with
                    | AdjacentToFood -> TakeFood neighborsWithFood.Head                   
                    | AdjacentToPheromone pheroCell -> Move pheroCell
                    | NoMansLand -> RandomMove()