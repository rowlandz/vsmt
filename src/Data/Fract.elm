module Data.Fract exposing (..)

type alias Fract =
  { num : Int
  , den : Int
  }

{-| Make a fraction -}
mk_ : Int -> Int -> Fract
mk_ num den = { num = num, den = den }

{-| Reduce a fraction to simplest form with nonnegative denominator -}
simpl : Fract -> Fract
simpl fract =
  if fract.num < 0 && fract.den < 0 then
    simplTailRec 2 (abs fract)
  else if fract.num < 0 || fract.den < 0 then
    neg_ (simplTailRec 2 (abs fract))
  else
    simplTailRec 2 fract

simplTailRec : Int -> Fract -> Fract
simplTailRec d { num, den } =
  if min num den < d then
    { num = num, den = den }
  else if modBy d num == 0 && modBy d den == 0 then
    simplTailRec d { num = num // d, den = den // d }
  else
    simplTailRec (d + 1) { num = num, den = den }

neg_ : Fract -> Fract
neg_ { num, den } = { num = -num, den = den }

abs : Fract -> Fract
abs { num, den } = { num = Basics.abs num, den = Basics.abs den }

add : Fract -> Fract -> Fract
add r1 r2 =
  simpl
    { num = r1.num * r2.den + r2.num * r1.den
    , den = r1.den * r2.den
    }

sub : Fract -> Fract -> Fract
sub r1 r2 =
  simpl
    { num = r1.num * r2.den - r2.num * r1.den
    , den = r1.den * r2.den
    }

mul : Fract -> Fract -> Fract
mul r1 r2 =
  simpl
    { num = r1.num * r2.num
    , den = r2.den * r2.den
    }

div : Fract -> Fract -> Fract
div r1 r2 =
  simpl
    { num = r1.num * r2.den
    , den = r1.den * r2.num
    }

recip_ : Fract -> Fract
recip_ { num, den } = { num = den, den = num }

toString : Fract -> String
toString { num, den } =
  if den == 1 then
    String.fromInt num
  else if den == 0 then
    "inf"
  else
    String.fromInt num ++ "/" ++ String.fromInt den