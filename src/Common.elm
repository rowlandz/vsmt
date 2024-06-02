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


{-| Splits `l` into two lists at the first element satisfying `predicate`. -}
listSplitOnFirst : (a -> Bool) -> List a -> Maybe ( List a, a, List a )
listSplitOnFirst predicate l =
  case l of
    [] -> Nothing
    (x::xs) ->
      if predicate x then
        Just ( [], x, xs )
      else
        listSplitOnFirst predicate xs
        |> Maybe.map (\( l1, y, l2 ) -> ( x::l1, y, l2 ))