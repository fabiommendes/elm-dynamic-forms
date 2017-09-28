module DynForms.Util exposing (partition)

{-| Random utility functions
-}


{-| Partition string in three parts from left to right: head, sep, tail.

If separator is not found, it returns (st, "", "")

-}
partition : String -> String -> ( String, String, String )
partition sep st =
    let
        indexes =
            String.indexes sep st
    in
    case indexes of
        [] ->
            ( st, "", "" )

        n :: _ ->
            partitionAt n sep st


{-| Partition string in three parts from right to left: head, sep, tail.

If separator is not found, it returns ("", "", st)

-}
rpartition : String -> String -> ( String, String, String )
rpartition sep st =
    let
        indexes =
            String.indexes sep st
    in
    case indexes of
        [] ->
            ( "", "", st )

        indices ->
            List.foldl (\x y -> x) 0 indices
                |> (\n -> partitionAt n sep st)


partitionAt : Int -> String -> String -> ( String, String, String )
partitionAt n sep st =
    let
        sepSize =
            String.length sep

        left =
            String.left n st

        right =
            String.slice (n + sepSize) (String.length st) st
    in
    ( left, sep, right )
