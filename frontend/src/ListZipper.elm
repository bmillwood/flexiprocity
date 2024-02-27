module ListZipper exposing (..)

type alias Context a =
  { before : List a
  , after : List a
  }

toList : Context a -> List a
toList { before, after } =
  List.foldl (\x a -> x :: a) after before

findFirst : (Int -> a -> Bool) -> List a -> Maybe ((Int, a), Context a)
findFirst p xs =
  let
    go index before unsearched =
      case unsearched of
        [] -> Nothing
        y :: ys ->
          if p index y
          then Just ((index, y), { before = before, after = ys })
          else go (index + 1) (y :: before) ys
  in
  go 0 [] xs
