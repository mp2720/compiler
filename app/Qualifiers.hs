module Qualifiers (RestrictLevel, Volatile, ProcModel (..)) where

import Numeric.Natural

type RestrictLevel = Natural

type Volatile = Bool

data ProcModel = ProcStatic | ProcRec | ProcReent
