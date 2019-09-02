{-# LANGUAGE OverloadedStrings #-}

module TacticsV2 where

import Control.Monad.Except
import Control.Monad.Trans.State.Strict
import Data.Bifunctor
import Data.Function
import GHC
import GHC.Generics
import MarkerUtils
import Outputable
import Pipes
import Pipes.Core
import Polysemy
import Refinery.ProofState
import Refinery.Tactic.Internal
import Sem.Fresh
import Type
import Types



newtype CType = CType { unCType :: Type }

instance Outputable CType where
  ppr (CType t) = ppr t

instance Eq CType where
  (==) = eqType `on` unCType

instance Ord CType where
  compare = nonDetCmpType `on` unCType



data Judgement = Judgement
  { jHoleId     :: Integer
  , jHypothesis :: [(OccName, CType)]
  , jGoal       :: CType
  }
  deriving (Eq, Ord, Generic)

instance Outputable Judgement where
  ppr (Judgement i hy g) = vcat
    [ "hole " <+> ppr i <+> ":" <+> ppr g
    , text "hypothesis: " <+> vcat (fmap ppr hy)
    ]


data TacticError
  = UndefinedHypothesis OccName
  | GoalMismatch String CType
  | UnsolvedSubgoals [Judgement]

instance Outputable TacticError where
  ppr (UndefinedHypothesis n) = "undefined hy: " <+> ppr n
  ppr (GoalMismatch g (CType t)) = "goal mismatch: " <+> text g <+> ppr t
  ppr (UnsolvedSubgoals gs) = "unsolved subgoals: " <+> vcat (fmap ppr gs)


type Tactics r = TacticT Judgement LExpr (ProvableT Judgement (ExceptT TacticError (Sem r)))
type Tactic r = Tactics r ()

type Rule r = RuleT Judgement LExpr (ProvableT Judgement (ExceptT TacticError (Sem r))) LExpr


runTacticT
    :: forall m
     . Monad m
    => TacticT Judgement LExpr m ()
    -> Judgement
    -> m (LExpr, [Judgement])
runTacticT (TacticT t) j =
    fmap (second reverse)
    $ flip runStateT []
    $ runEffect
    $ server +>> (hoist lift $ unProofStateT $ execStateT t j)
  where
    server :: Judgement -> Server Judgement LExpr (StateT [Judgement] m) LExpr
    server j' = do
      lift $ modify (j':)
      h <- pure $ Todo $ jHoleId j'
      respond h >>= server


newSubgoal :: Member (Fresh Integer) r => [(OccName, CType)] -> CType -> Rule r
newSubgoal hy g = subgoal =<< newJudgement hy g


newJudgement
    :: ( Member (Fresh Integer) r
       , MonadTrans t
       , Monad (t (ProvableT Judgement (ExceptT TacticError (Sem r))))
       )
    => [(OccName, CType)]
    -> CType
    -> t (ProvableT Judgement (ExceptT TacticError (Sem r))) Judgement
newJudgement hy g = do
  i <- sem fresh
  pure $ Judgement i hy g


sem
    :: MonadTrans t
    => Sem r a
    -> t (ProvableT Judgement (ExceptT TacticError (Sem r))) a
sem = lift . lift . lift

