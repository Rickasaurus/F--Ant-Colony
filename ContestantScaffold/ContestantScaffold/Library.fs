module RenameThisProjet

open AntsEverywhereLib.UserTypes

type MyAntBehavior() =
    interface IAntBehavior with
        member __.Name = "Put Your Name Here"
        member __.Behave me here locations = 
          (* put your ant colony behavior here *)
          Nothing
          