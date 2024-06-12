module Common exposing (..)



-- Extra `Maybe` functions


{-| Returns `this` if it's `Just` a value, otherwise computes `lazyThat`. -}
orElse : (() -> Maybe a) -> Maybe a -> Maybe a
orElse lazyThat this =
  case this of
    Just a -> Just a
    Nothing -> lazyThat ()

{-| Repeatedly applies `f` to `a1` as long as it returns `Just` a value. -}
greedy : (a -> Maybe a) -> a -> a
greedy f a1 =
  case f a1 of
    Nothing -> a1
    Just a2 -> greedy f a2

{-| Repeatedly applies `f` to `a1` as long as it returns `Just` a value.
Returns `Nothing` if `f` immediately returns `Nothing`. -}
greedy1 : (a -> Maybe a) -> a -> Maybe a
greedy1 f a1 = Maybe.map (greedy f) (f a1)



-- Extra `Result` functions


{-| Same as Result.andThen with the argument order flipped. -}
resultBind : Result err a -> (a -> Result err b) -> Result err b
resultBind result andThen =
  case result of
    Ok a -> andThen a
    Err e -> Err e

{-| _Both results must pass._ Errors are accumulated. -}
andCheck : Result (List err) a -> Result (List err) b -> Result (List err) ()
andCheck result2 result1 =
  case ( result1, result2 ) of
    ( Err x, Err y ) -> Err (x ++ y)
    ( Err x, Ok _  ) -> Err x
    ( Ok _ , Err y ) -> Err y
    ( Ok _ , Ok _  ) -> Ok ()

{-| _Both results must pass._ Errors are accumulated. The return value of the
left result is kept.
-}
andCheckL : Result (List err) b -> Result (List err) a -> Result (List err) b
andCheckL result2 result1 =
  case ( result1, result2 ) of
    ( Err x, Err y ) -> Err (x ++ y)
    ( Err x, Ok _  ) -> Err x
    ( Ok _ , Err y ) -> Err y
    ( Ok _ , Ok b  ) -> Ok b

{-| _Both results must pass._ Errors are accumulated. Both return values are kept. -}
andCheckRL : Result (List err) b -> Result (List err) a -> Result (List err) ( a, b )
andCheckRL result2 result1 =
  case ( result1, result2 ) of
    ( Err x, Err y ) -> Err (x ++ y)
    ( Err x, Ok _  ) -> Err x
    ( Ok _ , Err y ) -> Err y
    ( Ok a , Ok b  ) -> Ok ( a, b )

{-| Fails if the `guard` fails. Otherwise succeeds with `value`.

Try using this after a pipe to signify a "return value" after a
bunch of checks:

    Ok "Passed the first test"
    |> alsoCheck (Ok "Passed the second test")
    |> yielding "Final result!"
-}
yielding : a ->  Result err b -> Result err a
yielding value guard = Result.map (\_ -> value) guard

{-| Creates a result that succeeds if all results in the list succeed
and fails otherwise. The values or errors are accumulated.
-}
allMustSucceed : List (Result (List err) a) -> Result (List err) (List a)
allMustSucceed results =
  let ( fails, succs ) = resultsPartition results
  in  if List.length succs == List.length results then
        Ok succs
      else Err fails

resultsPartition : List (Result (List a) b) -> ( List a, List b )
resultsPartition results =
  case results of
    [] -> ([], [])
    (Err errs :: t) ->
      let (es, ss) = resultsPartition t
      in  (errs ++ es, ss)
    (Ok succ :: t) ->
      let (es, ss) = resultsPartition t
      in  (es, succ :: ss)



-- Extra `List` functions


{-| Get the list element at `index`. -}
listGet : Int -> List a -> Maybe a
listGet index l =
  case l of
    x :: xs -> case index of
      0 -> Just x
      _ -> listGet (index - 1) xs
    [] -> Nothing

{-| Removes the element at `index` and inserts `repl` in its place. -}
listReplace : Int -> List a -> List a -> List a
listReplace index repl l =
  case l of
    x :: xs -> case index of
      0 -> repl ++ xs
      _ -> x :: listReplace (index - 1) repl xs
    [] -> []

{-| Set an index in a list. Leaves the list unchanged if the index doesn't exist. -}
listSet : Int -> a -> List a -> List a
listSet index elem = listReplace index [ elem ]

{-| Deletes the element at `index`. -}
listDelete : Int -> List a -> List a
listDelete index = listReplace index []

{-| Splits `l` into two lists at the first element where `f` returns `Just`. -}
listFindFirstWhere : (a -> Maybe b) -> List a -> Maybe ( List a, b, List a )
listFindFirstWhere f l =
  case l of
    [] -> Nothing
    x :: xs -> case f x of
      Just y -> Just ( [], y, xs )
      Nothing -> listFindFirstWhere f xs
        |> Maybe.map (\( l1, y, l2 ) -> ( x :: l1, y, l2 ))

{-| Splits `l` into two lists at the first element satisfying `predicate`. -}
listSplitOnFirst : (a -> Bool) -> List a -> Maybe ( List a, a, List a )
listSplitOnFirst predicate =
  listFindFirstWhere (\x -> if predicate x then Just x else Nothing)

listIndexedMap2 : (Int -> a -> b -> c) -> List a -> List b -> List c
listIndexedMap2 f l1 l2 =
  List.indexedMap (\i ( a, b ) -> f i a b) (List.map2 Tuple.pair l1 l2)

listTraverseST : (st -> a -> ( st, b )) -> st -> List a -> ( st, List b )
listTraverseST f st la =
  case la of
    [] -> ( st, [] )
    x::xs ->
      let ( st1, y ) = f st x
          ( st2, ys ) = listTraverseST f st1 xs in
          ( st2, y :: ys )

listRemoveDuplicates : List a -> List a
listRemoveDuplicates l =
  case l of
    [] -> []
    x :: xs ->
      let simpl = listRemoveDuplicates xs in
      if List.member x simpl then simpl else x :: simpl

{-| Maps `f` over `l`, but use the element as a default if
`f` returns `Nothing`. Returns `Nothing` if all `f`s produce `Nothing`. -}
listOneMustSucceed : (a -> Maybe a) -> List a -> Maybe (List a)
listOneMustSucceed f l =
  let mapped = List.map f l in
  if List.all (\m -> m == Nothing) mapped then Nothing
  else Just (List.map2 Maybe.withDefault l mapped)