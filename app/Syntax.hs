module Syntax
  ( STop,
  )
where

newtype STop = STop [SDef]

data SDef
  = SGlobalVarDef SStaticVarDef
  | SProcDef
      { sProcDefName :: String,
        sProcDefExecModel :: SProcExecModel,
        sProcDefParams :: [SProcParam],
        sProcDefRetValuesNum :: Int
      }

data SStaticVarDef
  = SStaticScalarVarDef String (Maybe Int)
  | SStaticArrayVarDef String [SArrayElementsList]

data SArrayElementsList = SArrayElementsList (Maybe Int) [Int]

data SProcExecModel
  = SProcStatic
  | SProcRecursive
  | SProcReentrant
  | SProcISR

newtype SProcParam = SProcParam String

data SStmt
  = SIf SRExpr [SLabeledStmt] [SLabeledStmt]
  | SWhile SRExpr [SLabeledStmt]
  | SDoWhile [SStmt] SRExpr
  | SGoto SLExpr
  | SReturn [SRExpr]
  | SAssign SLExpr SRExpr
  | SStmtExpr SRExpr
  | SVarDecl

data SLabeledStmt = SLabeledStmt (Maybe String) SStmt

data SLExpr = SDeref SRExpr | SVariable

data SArithmBinOperator = SAdd | SSub | SBitOr | SBitAnd

data SLogicalBinOperator = SOr | SAnd | SEq | SGt | SLt | SGEq | SLEq

data SRExpr
  = SLExpr SLExpr
  | SArithmBin SRExpr SArithmBinOperator SRExpr
  | SLogicalBin SRExpr SLogicalBinOperator SRExpr
  | SBitNot SRExpr
  | SNot SRExpr
  | SNeg SRExpr
  | SProcCall String [SRExpr]
  | SSizeof String
  | SROL SRExpr
  | SROR SRExpr
  | SASL SRExpr
  | SASR SRExpr
  | SExtend SRExpr
  | SSwapHalves SRExpr
  | SRef SRExpr
