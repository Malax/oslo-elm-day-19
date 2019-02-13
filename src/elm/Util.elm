module Util exposing (chunkList)


chunkList : Int -> List a -> List (List a)
chunkList size list =
    case list of
        [] ->
            []

        _ ->
            List.take size list :: chunkList size (List.drop size list)
