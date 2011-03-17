namespace AntsEverywhereApp

[<AutoOpen>]
module Misc =
    type System.Windows.Controls.UIElementCollection with
         member x.SafeAdd control =
            x.Add control |> ignore

    type System.Windows.Controls.ItemCollection with
         member x.SafeAdd control =
            x.Add control |> ignore

