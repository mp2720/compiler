{-# LANGUAGE TypeFamilies #-}

module HR where

import Numeric.Natural (Natural)
import Qualifiers

type family XAnn e

type family XIdent e

type family XConst e

data HTop e
  = HTopProcDef (XAnn e) (HProcDef e)
  | HTopStaticVarDef (XAnn e) (HStaticVarDef e)
  | HTopStaticArrayDef (XAnn e) (HStaticArrayDef e)
  | HTopConstDef (XAnn e) (HConstDef e)

data HProcDef e = HProcDef (XIdent e) ProcModel [HProcParam e] (Maybe HProcReturnValuesNum) [HStmt e]

type HProcReturnValuesNum = Natural

data HProcParam e = HProcParam RestrictLevel (XIdent e)

data HVarQualifiers = HVarQualifiers RestrictLevel Volatile

newtype HArrayQualifiers = HArrayQualifiers RestrictLevel

data HStaticVarDef e = HStaticVarDef (XIdent e) HVarQualifiers (XConst e)

data HStaticArrayDef e = HStaticArrayDef (XIdent e) HArrayQualifiers (HArrayElements e)

data HConstDef e = HConstDef (XIdent e) (XConst e)

data HArrayElements e = HArrayElements
  { hArraySize :: Maybe (XConst e),
    hArrayBlocks :: [HArrayBlock e]
  }

data HArrayBlock e = HArrayBlock
  { hArrayBlockSize :: Maybe (XConst e),
    hArrayBlockElements :: [XConst e],
    hArrayBlockRestUnset :: Bool
  }

data HLabeledStmt e = LabeledStmt (XIdent e) (HStmt e)

data HStmt e
  = HAssign (XAnn e) [HLeftExpr e] (HRightExpr e)
  | HStmtExpr (XAnn e) (HAnyExpr e)
  | HGoto (XAnn e) (HRightExpr e)
  | HIf (XAnn e) (HLogicExpr e) [HLabeledStmt e] (Maybe [HLabeledStmt e])
  | HWhile (XAnn e) (HLogicExpr e) [HLabeledStmt e]
  | HDoWhile (XAnn e) [HLabeledStmt e] (HLogicExpr e)
  | HReturn (XAnn e) [HAnyExpr e]
  | HAutoScalarDef (XAnn e) (XIdent e) HVarQualifiers (Maybe (HAnyExpr e))
  | HAutoArrayDef (XAnn e) (XIdent e) HArrayQualifiers (XConst e)
  | HLocalStaticVarDef (XAnn e) (HStaticVarDef e)
  | HLocalStaticArrayDef (XAnn e) (HStaticArrayDef e)
  | HLocalConstDef (XAnn e) (HConstDef e)

data HLeftExpr e
  = HDeref (XAnn e) (HRightExpr e)
  | HIdent (XAnn e) (XIdent e)
  | HArrayElement (XAnn e) (HRightExpr e) (HRightExpr e)

data HRelOp = HEq | HLt | HGt | HLEq | HGEq

data HLogicBinOp = HAnd | HOr

data HLogicExpr e
  = HRelOpApp (XAnn e) (HRightExpr e) HRelOp (HRightExpr e)
  | HNot (XAnn e) (HAnyExpr e)
  | HLogicBinOpApp (XAnn e) (HAnyExpr e) HLogicBinOp (HAnyExpr e)

data HBinOp = HAdd | HSub | HBitOr | HBitAnd

data HUnOp
  = HNeg
  | HBitNot
  | HExtendSign
  | HSwapHalves
  | HShiftLeft
  | HShiftRight
  | HShiftRightSigned

type HReturnValIndex = Natural

data HRightExpr e
  = HBinOpApp (XAnn e) (HRightExpr e) HBinOp (HRightExpr e)
  | HUnOpApp (XAnn e) HUnOp (HRightExpr e)
  | HProcCall (XAnn e) (XIdent e) [HRightExpr e] (Maybe HReturnValIndex)
  | HLeftExpr (XAnn e) (HLeftExpr e)
  | HConst (XAnn e) (XConst e)

data HAnyExpr e = HRightExpr (HRightExpr e) | HLogicExpr (HLogicExpr e)
