module RenameThisProjet

open AntsEverywhereLib.UserTypes

type MyAntBehavior() =
    interface IAntBehavior with
        member __.Name = "RenameThisProject"
        member __.Behave me here locations = 
          (* put your ant colony behavior here *)
          Nothing
