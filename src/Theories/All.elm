module Theories.All exposing (..)

import Theory exposing (Theory)
import Theories.EUF exposing (euf)
import Theories.LRA exposing (lra)

all : List Theory
all =
  [ euf
  , lra
  ]