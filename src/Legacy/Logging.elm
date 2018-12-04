module Legacy.Logging exposing (log, todo)

{-| Debug.todo
-}


todo =
    identity


{-| Debugging
-}
log msg a =
    identity a
