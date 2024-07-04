module Sort exposing (..)

lexicographic : List Order -> Order
lexicographic =
  let
    f prev next =
      case prev of
        EQ -> next
        _ -> prev
  in
  List.foldr f EQ

nullsLast : (a -> a -> Order) -> Maybe a -> Maybe a -> Order
nullsLast compareA m1 m2 =
  case (m1, m2) of
    (Nothing, Nothing) -> EQ
    (Nothing, _) -> GT
    (_, Nothing) -> LT
    (Just v1, Just v2) -> compareA v1 v2
