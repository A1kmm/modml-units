{-#LANGUAGE NoMonomorphismRestriction,DeriveDataTypeable #-}
module ModML.Units.UnitsDAEModel
where

import qualified ModML.Core.BasicDAEModel as B
import qualified Control.Monad.State as S
import qualified Control.Monad.Reader as R
import qualified Control.Monad.Identity as I
import qualified Data.Data as D
import qualified Data.TypeHash as D
import qualified Data.Map as M
import qualified Control.Monad as M
import qualified Control.Monad.Trans as M
import qualified Data.List as L

newtype BaseUnit = BaseUnit Int deriving (Eq, Ord, D.Typeable, D.Data)
instance Show BaseUnit
  where
    showsPrec _ (BaseUnit i) = showString "BaseUnit_" . shows i

mkBaseUnit name =
    do
      u <- M.liftM BaseUnit allocateID
      annotateModel u "nameIs" name
      return u

data BaseUnitTag = BaseUnitTag deriving (D.Typeable, D.Data)
baseUnitTag = D.typeCode BaseUnitTag

contextMkBaseUnit name tag =
    contextTaggedID baseUnitTag tag BaseUnit (\u -> annotateModel (BaseUnit u) "nameIs" name)

data Units = Units Double (M.Map BaseUnit Double) deriving (Eq, Ord, D.Typeable, D.Data)


dimensionlessE = Units 1.0 M.empty
dimensionless = return dimensionlessE
singletonUnit b = Units 1.0 (M.singleton b 1)
isDimensionless (Units _ x)
    | M.null x = True
    | otherwise = False

unitsMultiplier :: Double -> Units
unitsMultiplier m = Units m M.empty

unitsTimes :: Units -> Units -> Units
(Units m1 u1) `unitsTimes` (Units m2 u2) = Units (m1*m2) (M.filter (/=0) (M.unionWith (+) u1 u2))
u1 $*$ u2 = do
  u1' <- u1
  u2' <- u2
  return $ u1' `unitsTimes` u2'

unitsPow :: Units -> Double -> Units
(Units m1 u1) `unitsPow` v = Units (m1**v) (M.map (*v) u1)
u1 $**$ mup = do
  u1' <- u1
  return $ u1' `unitsPow` mup

mkCombinedUnitsE :: Double -> [(BaseUnit, Double)] -> Units
mkCombinedUnitsE m = Units m . M.fromList
mkCombinedUnitsX :: Monad m => Double -> [(ModelBuilderT m BaseUnit, Double)] -> ModelBuilderT m Units
mkCombinedUnitsX m ml =
  do
    l <- sequence $ map (\(ma, b) ->
                             do
                               a <- ma
                               return (a, b)
                        ) ml
    return $ mkCombinedUnitsE m l


data RealVariable = RealVariable Units Int deriving (Eq, Ord, D.Typeable, D.Data)
variableId (RealVariable _ a) = a
instance Show RealVariable
    where
      showsPrec _ v = showString "Variable_" . shows (variableId v)

data RealEquation = RealEquation RealExpression RealExpression deriving (Eq, Ord, D.Typeable, D.Data)

data BoolExpression =
    -- A constant true or false.
    BoolBasicExpression B.BoolExpression |
    -- A common subexpression tagged expression.
    BoolCommonSubexpressionE BoolCommonSubexpression |
    -- Logical and of two expressions.
    BoolExpression `And` BoolExpression |
    -- Logical not of an expression.
    Not BoolExpression |
    -- Logical or of two expressions.
    BoolExpression `Or` BoolExpression |
    RealExpression `LessThan` RealExpression |
    RealExpression `Equal` RealExpression
                   deriving (Eq, Ord, D.Typeable, D.Data)

data RealExpression =
    -- A core expression with units applied:
    RealWithUnits Units B.RealExpression |
    -- A free variable...
    RealVariableE RealVariable |
    -- The bound variable of the integration...
    BoundVariableE |
    -- The derivative of an expression with respect to the bound variable...
    Derivative RealExpression |
    -- A common subexpression tagged expression.
    RealCommonSubexpressionE RealCommonSubexpression |
    -- If x {- then -} b {- else -} b
    If BoolExpression RealExpression RealExpression |
    -- A sum of two expressions.
    RealExpression `Plus` RealExpression |
    -- First expression minus second expression.
    RealExpression `Minus` RealExpression |
    -- The product of two expressions.
    RealExpression `Times` RealExpression |
    -- a `Divided` {- by -} b
    RealExpression `Divided` RealExpression |
    -- a `Power` b - a to the power of b.
    RealExpression `Power` RealExpression |
    -- The floor function...
    Floor RealExpression |
    -- The ceiling function...
    Ceiling RealExpression |
    -- LogBase a b = log_a b
    LogBase RealExpression RealExpression |
    -- Trigonometric functions...
    Sin RealExpression |
    Tan RealExpression |
    Cos RealExpression |
    ASin RealExpression |
    ATan RealExpression |
    ACos RealExpression |
    Sinh RealExpression |
    Tanh RealExpression |
    Cosh RealExpression |
    ASinh RealExpression |
    ATanh RealExpression |
    ACosh RealExpression
          deriving (Eq, Ord, D.Typeable, D.Data)

class (Ord a) => CommonSubexpression a
  where
    commonSubexpressionId :: a -> Int

data RealCommonSubexpression = RealCommonSubexpression Int RealExpression deriving (Eq, Ord, D.Typeable, D.Data)
instance CommonSubexpression RealCommonSubexpression
  where
    commonSubexpressionId (RealCommonSubexpression a _) = a

data BoolCommonSubexpression = BoolCommonSubexpression Int BoolExpression deriving (Eq, Ord, D.Typeable, D.Data)
instance CommonSubexpression BoolCommonSubexpression
  where
    commonSubexpressionId (BoolCommonSubexpression a _) = a

data AnyCommonSubexpression = FromRealCommonSubexpression RealCommonSubexpression | FromBoolCommonSubexpression BoolCommonSubexpression deriving (Eq, Ord, D.Typeable, D.Data)
instance CommonSubexpression AnyCommonSubexpression
  where
    commonSubexpressionId (FromRealCommonSubexpression r) = commonSubexpressionId r
    commonSubexpressionId (FromBoolCommonSubexpression b) = commonSubexpressionId b

data UnitsDAEModel = UnitsDAEModel {
        -- The equations which apply for this model at all times...
        equations :: [RealEquation],
        -- The boundary conditions (equations which apply at the starting
        -- point, and/or after interventions). Note that other equations also
        -- apply at these times as well.
        boundaryEquations :: [(BoolExpression, RealEquation)],
        -- Expressions which controls when the solver needs to be restarted.
        -- The values should cross zero at the point when the solver needs to
        -- be restarted.
        interventionRoots :: [RealExpression],
        -- An expression which the solver should try to keep positive.
        forcedInequalities :: [RealExpression],
        checkedConditions :: [(String, BoolExpression)],
        variables :: [RealVariable],
        commonSubexpressions :: [AnyCommonSubexpression],
        annotations :: M.Map (String, String) String,
        contextTaggedIDs :: M.Map (D.TypeCode, D.TypeCode) Int,
        nextID :: Int
    } deriving (Eq, Ord, D.Typeable, D.Data)

type ModelBuilderT m a = S.StateT UnitsDAEModel (R.ReaderT Units m) a
type ModelBuilder a = ModelBuilderT I.Identity a

nullModel = UnitsDAEModel { equations = [], boundaryEquations = [], interventionRoots = [],
                            forcedInequalities = [], checkedConditions = [], variables = [],
                            commonSubexpressions = [], annotations = M.empty,
                            contextTaggedIDs = M.empty, nextID = 0
                          }

runInCore :: Monad m => ModelBuilderT m Units -> ModelBuilderT m a -> B.ModelBuilderT m a
runInCore indepunits m = do
    coremod <- S.get
    let startingModel =
          nullModel { annotations = B.annotations coremod,
                      contextTaggedIDs = B.contextTaggedIDs coremod,
                      nextID = B.nextID coremod
                    }
    -- Step 1: Run with our dummy model with dimensionless independent variable
    -- units to get the units to use...
    (indepunits', model) <- M.lift $ flip R.runReaderT dimensionlessE $
                             S.runStateT indepunits startingModel
    -- Step 2: Use the units obtained to run the model builder...
    M.lift $ flip R.runReaderT indepunits' $
       S.evalStateT m model
    

unitsToCore :: Monad m => ModelBuilderT m Units -> ModelBuilderT m a -> B.ModelBuilderT m ()
unitsToCore indepunits x =
    let
        mergeInto coremod = do
          x
          m <- S.get
          mergeIntoCoreModelWithValidation coremod m
    in
      do
        coremod <- S.get
        coremod' <- runInCore indepunits (mergeInto coremod)
        S.put coremod'

mergeIntoCoreModelWithValidation :: Monad m => B.BasicDAEModel -> UnitsDAEModel -> ModelBuilderT m B.BasicDAEModel
mergeIntoCoreModelWithValidation coremod unitmod =
    do
      eqns <- mapM translateRealEquation (equations unitmod)
      beqns <- mapM (\(c, v) -> M.liftM2 (,) (translateBoolExpression c)
                                             (translateRealEquation v))
                 (boundaryEquations unitmod)
      iroots <- mapM (M.liftM snd . translateRealExpression) (interventionRoots unitmod)
      forcedIeqs <- mapM (M.liftM snd . translateRealExpression) (forcedInequalities unitmod)
      checkedConds <- mapM (\(s, mv) -> translateBoolExpression mv >>= (\v -> return $ (s, v)))
                        (checkedConditions unitmod)
      vars <- mapM translateVariable (variables unitmod)
      cses <- mapM translateSubexpression (commonSubexpressions unitmod)
      return $
        B.BasicDAEModel {
         B.equations = (B.equations coremod) ++ eqns,
         B.boundaryEquations = (B.boundaryEquations coremod) ++ beqns,
         B.interventionRoots = (B.interventionRoots coremod) ++ iroots,
         B.forcedInequalities = (B.forcedInequalities coremod) ++ forcedIeqs,
         B.checkedConditions = (B.checkedConditions coremod) ++ checkedConds,
         B.variables = (B.variables coremod) ++ vars,
         B.commonSubexpressions = (B.commonSubexpressions coremod) ++ cses,
         B.annotations = annotations unitmod,
         B.contextTaggedIDs = contextTaggedIDs unitmod,
         B.nextID = nextID unitmod
       }

describeUnits :: Monad m => Units -> ModelBuilderT m String
describeUnits (Units mup m)
    | M.null m = return (show mup)
    | otherwise = (if mup /= 1.0 then M.liftM ((shows mup "*")++) else id) $ M.liftM (L.intercalate "*") $ mapM (uncurry describeAppliedBaseUnit) (M.toList m)
describeAppliedBaseUnit :: Monad m => BaseUnit -> Double -> ModelBuilderT m String
describeAppliedBaseUnit _ 0 = return ""
describeAppliedBaseUnit u 1 = describeBaseUnit u
describeAppliedBaseUnit u ex = do
  u1 <- describeBaseUnit u
  return $ (showString u1 . showString "^" . shows ex) ""
describeBaseUnit u = do
  a <- getAnnotation u "nameIs"
  case a
    of
      Nothing -> return $ show u
      Just v -> return v

binaryRealUnitsMatchFn :: Monad m => (Units -> B.RealExpression -> B.RealExpression -> ModelBuilderT m a) -> RealExpression -> RealExpression -> ModelBuilderT m a
binaryRealUnitsMatchFn f ex1 ex2 =
    do
      (u1, ex1') <- translateRealExpression ex1
      (u2, ex2') <- translateRealExpression ex2
      if u1 == u2
        then
          f u1 ex1' ex2'
        else
          do
            u1s <- describeUnits u1
            u2s <- describeUnits u2
            error $ (showString "Units mismatch on binary operator - " . showString u1s . showString " differs from ") u2s
unaryRealSameUnits f ex =
    do
      (u, ex') <- translateRealExpression ex
      return $ (u, f ex')
unaryDimensionless f ex =
    do
      (u, ex') <- translateRealExpression ex
      if isDimensionless u
        then
          return (dimensionlessE, f ex')
        else
          do
            us <- describeUnits u
            error $ (showString "Units mismatch - expected dimensionless units, got ") us

translateRealEquation (RealEquation ex1 ex2) = binaryRealUnitsMatchFn (\u a b -> return $ B.RealEquation a b) ex1 ex2

translateBoolExpression (BoolBasicExpression e) = return $ e
translateBoolExpression (BoolCommonSubexpressionE bce) = M.liftM B.BoolCommonSubexpressionE (translateBoolCommonSubexpression bce)
translateBoolExpression (ex1 `And` ex2) = M.liftM2 B.And (translateBoolExpression ex1) (translateBoolExpression ex2)
translateBoolExpression (Not ex1) = M.liftM B.Not (translateBoolExpression ex1)
translateBoolExpression (ex1 `Or` ex2) = M.liftM2 B.Or (translateBoolExpression ex1) (translateBoolExpression ex2)
translateBoolExpression (ex1 `LessThan` ex2) = binaryRealUnitsMatchFn (\u a b -> return $ a `B.LessThan` b) ex1 ex2
translateBoolExpression (ex1 `Equal` ex2) = binaryRealUnitsMatchFn (\u a b -> return $ a `B.Equal` b) ex1 ex2

runWithUnits :: ModelBuilder a -> a
runWithUnits v =
  fst $ I.runIdentity $ flip R.runReaderT dimensionlessE $ S.runStateT v nullModel

translateRealExpression (RealWithUnits u ex) = return $ (u, ex)
translateRealExpression (RealVariableE (RealVariable u v)) = return $ (u, B.RealVariableE (B.RealVariable v))
translateRealExpression BoundVariableE = do
  bu <- boundUnits
  return (bu, B.BoundVariableE)
translateRealExpression (Derivative (ex)) = do
  (u, ex') <- translateRealExpression ex
  bu <- boundUnits
  return $ (u `unitsTimes` (bu `unitsPow` (-1)), B.Derivative ex')
translateRealExpression (RealCommonSubexpressionE rce) = do
  (u, ex) <- translateRealCommonSubexpression rce
  return $ (u, B.RealCommonSubexpressionE ex)
translateRealExpression (If b1 r1 r2) = do
  cond <- translateBoolExpression b1
  binaryRealUnitsMatchFn (\u ex1 ex2 -> return (u, B.If cond ex1 ex2)) r1 r2
translateRealExpression (r1 `Plus` r2) =
  binaryRealUnitsMatchFn (\u ex1 ex2 -> return $ (u, ex1 `B.Plus` ex2)) r1 r2
translateRealExpression (r1 `Minus` r2) =
  binaryRealUnitsMatchFn (\u ex1 ex2 -> return $ (u, ex1 `B.Minus` ex2)) r1 r2
translateRealExpression (r1 `Times` r2) = do
  (u1, ex1) <- translateRealExpression r1
  (u2, ex2) <- translateRealExpression r2
  return (u1 `unitsTimes` u2, ex1 `B.Times` ex2)
translateRealExpression (r1 `Divided` r2) = do
  (u1, ex1) <- translateRealExpression r1
  (u2, ex2) <- translateRealExpression r2
  return (u1 `unitsTimes` (u2 `unitsPow` (-1)), ex1 `B.Divided` ex2)
translateRealExpression (r1 `Power` r2) = do
  (u1, ex1) <- translateRealExpression r1
  (_, ex2) <- translateRealExpression r2
  cr2 <- tryEvaluateRealAsConstant r2
  let u = case cr2
          of
            Nothing -> dimensionlessE
            Just n -> u1 `unitsPow` n
  return (u, ex1 `B.Power` ex2)
translateRealExpression (Floor r1) = unaryRealSameUnits B.Floor r1
translateRealExpression (Ceiling r1) = unaryRealSameUnits B.Ceiling r1
translateRealExpression (LogBase r1 r2) = do
  (u1@(Units _ m1), ex1) <- translateRealExpression r1
  (u2@(Units _ m2), ex2) <- translateRealExpression r2
  if isDimensionless u1 && isDimensionless u2
    then
      return $ (dimensionlessE, B.LogBase ex1 ex2)
    else if (M.size m1 /= M.size m2) || (u1 /= (u2 `unitsPow` (((snd . M.elemAt 0) m1) / ((snd . M.elemAt 0) m2))))
      then
        do
          u1s <- describeUnits u1
          u2s <- describeUnits u2
          error $ (showString "Units mismatch - " . showString u1s .
                   showString " does not differ by a dimensionless power from " .
                   showString u2s) ""
      else
          return $ (dimensionlessE, B.LogBase ex1 ex2)
translateRealExpression (Sin r1) = unaryDimensionless B.Sin r1
translateRealExpression (Tan r1) = unaryDimensionless B.Tan r1
translateRealExpression (Cos r1) = unaryDimensionless B.Cos r1
translateRealExpression (ASin r1) = unaryDimensionless B.ASin r1
translateRealExpression (ATan r1) = unaryDimensionless B.ATan r1
translateRealExpression (ACos r1) = unaryDimensionless B.ACos r1
translateRealExpression (Sinh r1) = unaryDimensionless B.Sinh r1
translateRealExpression (Tanh r1) = unaryDimensionless B.Tanh r1
translateRealExpression (Cosh r1) = unaryDimensionless B.Cosh r1
translateRealExpression (ASinh r1) = unaryDimensionless B.ASinh r1
translateRealExpression (ATanh r1) = unaryDimensionless B.ATanh r1
translateRealExpression (ACosh r1) = unaryDimensionless B.ACosh r1

translateVariable (RealVariable _ n) = return $ B.RealVariable n
translateSubexpression (FromRealCommonSubexpression rcs) =
    M.liftM B.FromRealCommonSubexpression $ M.liftM snd $ translateRealCommonSubexpression rcs
translateSubexpression (FromBoolCommonSubexpression bcs) =
    M.liftM B.FromBoolCommonSubexpression $ translateBoolCommonSubexpression bcs
translateRealCommonSubexpression (RealCommonSubexpression i ex) = do
  (u, ex') <- translateRealExpression ex
  return $ (u, B.RealCommonSubexpression i ex')
translateBoolCommonSubexpression (BoolCommonSubexpression i ex) = do
  ex' <- translateBoolExpression ex
  return $ B.BoolCommonSubexpression i ex'

tryEvaluateRealAsConstant r = M.liftM B.tryEvaluateRealAsConstant (M.liftM snd (translateRealExpression r))
tryEvaluateBoolAsConstant b = M.liftM B.tryEvaluateRealAsConstant (M.liftM snd (translateRealExpression b))

buildModelT u = B.buildModelT . unitsToCore u
buildModel u = B.buildModel . unitsToCore u

x `newEqM` y = S.modify (\m -> m { equations = (RealEquation x y):(equations m) })
x `newEqX` y = do
  x' <- x
  y' <- y
  x' `newEqM` y'
newEq = newEqX

newBoundaryEqM c x y = S.modify (\m -> m { boundaryEquations = (c, (RealEquation x y)):(boundaryEquations m) })
newBoundaryEqX c x y = do
  c' <- c
  x' <- x
  y' <- y
  newBoundaryEqM c' x' y'
newBoundaryEq = newBoundaryEqX

newInterventionRootM x = S.modify (\m -> m { interventionRoots = x:(interventionRoots m)})
newInterventionRootX x =
    do
      x' <- x
      newInterventionRootM x'
newInterventionRoot = newInterventionRootX

newForcedInequalityM x = S.modify (\m -> m { forcedInequalities = x:(forcedInequalities m)})
newForcedInequalityX x =
    do
      x' <- x
      newForcedInequalityM x'
newForcedInequality = newForcedInequalityX

msg `newCheckedConditionM` x = S.modify (\m -> m { checkedConditions = (msg, x):(checkedConditions m) })
m `newCheckedConditionX` x = do
  x' <- x
  m `newCheckedConditionM` x'
newCheckedCondition = newCheckedConditionX

annotateModel :: (Show a, Show b, Show c, Monad m) => a -> b -> c -> ModelBuilderT m ()
annotateModel s p o = S.modify (\m -> m { annotations = M.insert ((show s), (show p)) (show o) (annotations m) })

getAnnotation :: (Show a, Show b, Monad m) => a -> b -> ModelBuilderT m (Maybe String)
getAnnotation s p = do
  am <- S.gets annotations
  return $ M.lookup (show s, show p) am

registerCommonSubexpression s =
    S.modify (\m -> m { commonSubexpressions = s:(commonSubexpressions m)})

allocateID :: Monad m => ModelBuilderT m Int
allocateID = S.modify (\m -> m { nextID = (+1) $ nextID m}) >> (S.gets $ flip (-) 1 . nextID)

boolConstantM :: Monad m => Bool -> ModelBuilderT m BoolExpression
boolConstantM = return . BoolBasicExpression . B.BoolConstant

trueM :: Monad m => ModelBuilderT m BoolExpression
trueM = boolConstantM True
falseM :: Monad m => ModelBuilderT m BoolExpression
falseM = boolConstantM False

boolCommonSubexpressionM :: Monad m => BoolExpression -> ModelBuilderT m BoolExpression
boolCommonSubexpressionM e =
  do
    id <- allocateID
    let bcs = BoolCommonSubexpression id e
    registerCommonSubexpression (FromBoolCommonSubexpression bcs)
    return $ BoolCommonSubexpressionE bcs

boolCommonSubexpressionX e =
    e >>= boolCommonSubexpressionM

boolCommonSubexpression me = do
  ex <- boolCommonSubexpressionX me
  return (return ex)

andM :: Monad m => BoolExpression -> BoolExpression -> ModelBuilderT m BoolExpression
a `andM` b = return $ a `And` b
(.&&.) :: Monad m => ModelBuilderT m BoolExpression -> ModelBuilderT m BoolExpression -> ModelBuilderT m BoolExpression
(.&&.) = S.liftM2 And
andX = (.&&.)

orM :: Monad m => BoolExpression -> BoolExpression -> ModelBuilderT m BoolExpression
a `orM` b = return $ a `Or` b
(.||.) :: Monad m => ModelBuilderT m BoolExpression -> ModelBuilderT m BoolExpression -> ModelBuilderT m BoolExpression
(.||.) = S.liftM2 Or
orX = (.||.)

notM :: Monad m => BoolExpression -> ModelBuilderT m BoolExpression
notM = return . Not
notX :: Monad m => ModelBuilderT m BoolExpression -> ModelBuilderT m BoolExpression
notX = S.liftM Not

lessThanM :: Monad m => RealExpression -> RealExpression -> ModelBuilderT m BoolExpression
a `lessThanM` b = return $ a `LessThan` b
lessThanX :: Monad m => ModelBuilderT m RealExpression -> ModelBuilderT m RealExpression -> ModelBuilderT m BoolExpression
a `lessThanX` b = do
  a' <- a
  b' <- b
  a' `lessThanM` b'
(.<.) = lessThanX

equalM :: Monad m => RealExpression -> RealExpression -> ModelBuilderT m BoolExpression
a `equalM` b = return $ a `Equal` b
equalX :: Monad m => ModelBuilderT m RealExpression -> ModelBuilderT m RealExpression -> ModelBuilderT m BoolExpression
a `equalX` b = do
  a' <- a
  b' <- b
  a' `equalM` b'
(.==.) = equalX

realConstantE :: Units -> Double -> RealExpression
realConstantE u v = RealWithUnits u $ B.RealConstant v
realConstantM :: Monad m => Units -> Double -> ModelBuilderT m RealExpression
realConstantM u v = return $ realConstantE u v
realConstant u v = do
  u' <- u
  realConstantM u' v

realCommonSubexpressionM :: Monad m => RealExpression -> ModelBuilderT m RealExpression
realCommonSubexpressionM e =
  do
    id <- allocateID
    let rcs = RealCommonSubexpression id e
    registerCommonSubexpression (FromRealCommonSubexpression rcs)
    return $ RealCommonSubexpressionE rcs
realCommonSubexpressionX me = me >>= realCommonSubexpressionM

realCommonSubexpression me = do
  ex <- realCommonSubexpressionX me
  return (return ex)


mkNewRealVariable :: Monad m => Units -> ModelBuilderT m RealVariable
mkNewRealVariable u = do
  id <- allocateID
  let v = RealVariable u id
  S.modify (\m -> m { variables = v:(variables m) } )
  return v

mkNewNamedRealVariable u name = do
  v <- mkNewRealVariable u
  annotateModel v "nameIs" name
  return v

mkNewRealVariableM :: Monad m => Units -> ModelBuilderT m (ModelBuilderT m RealVariable)
mkNewRealVariableM u = S.liftM return (mkNewRealVariable u)

realVariableM :: Monad m => RealVariable -> ModelBuilderT m RealExpression
realVariableM = return . RealVariableE 
realVariableX :: Monad m => ModelBuilderT m RealVariable -> ModelBuilderT m RealExpression
realVariableX = S.liftM RealVariableE
realVariable = realVariableX

newRealVariableE u =
  realVariable (mkNewRealVariable u)
newNamedRealVariableE u name = do
  realVariable (mkNewNamedRealVariable u name)

newRealVariable :: Monad m => ModelBuilderT m Units -> ModelBuilderT m (ModelBuilderT m RealExpression)
newRealVariable u = do
  u' <- u
  v <- newRealVariableE u'
  return (return v)

newNamedRealVariable u name = do
  u' <- u
  v <- newNamedRealVariableE u' name
  return (return v)

boundVariableM :: Monad m => ModelBuilderT m RealExpression
boundVariableM = return $ BoundVariableE
boundVariableX = boundVariableM
boundVariable = boundVariableM

derivativeM :: Monad m => RealExpression -> ModelBuilderT m RealExpression
derivativeM = return . Derivative
derivativeX = S.liftM Derivative
derivative = derivativeX

ifM :: Monad m => BoolExpression -> RealExpression ->
                      RealExpression -> ModelBuilderT m RealExpression
ifM c {- then -} e1 {- else -} e2 = return $ If c e1 e2
ifX :: Monad m => ModelBuilderT m BoolExpression -> ModelBuilderT m RealExpression ->
                     ModelBuilderT m RealExpression -> ModelBuilderT m RealExpression
ifX = S.liftM3 If

plusM :: Monad m => RealExpression -> RealExpression -> ModelBuilderT m RealExpression
a `plusM` b = return $ a `Plus` b
(.+.) :: Monad m => ModelBuilderT m RealExpression -> ModelBuilderT m RealExpression -> ModelBuilderT m RealExpression
(.+.) = S.liftM2 Plus
plusX = (.+.)

minusM :: Monad m => RealExpression -> RealExpression -> ModelBuilderT m RealExpression
a `minusM` b = return $ a `Minus` b
(.-.) :: Monad m => ModelBuilderT m RealExpression -> ModelBuilderT m RealExpression -> ModelBuilderT m RealExpression
(.-.) = S.liftM2 Minus
minusX = (.-.)

timesM :: Monad m => RealExpression -> RealExpression -> ModelBuilderT m RealExpression
a `timesM` b = return $ a `Times` b
(.*.) :: Monad m => ModelBuilderT m RealExpression -> ModelBuilderT m RealExpression -> ModelBuilderT m RealExpression
(.*.) = S.liftM2 Times
timesX = (.*.)

dividedM :: Monad m => RealExpression -> RealExpression -> ModelBuilderT m RealExpression
a `dividedM` b = return $ a `Divided` b
(./.) :: Monad m => ModelBuilderT m RealExpression -> ModelBuilderT m RealExpression -> ModelBuilderT m RealExpression
(./.) = S.liftM2 Divided
dividedX = (./.)

powerM :: Monad m => RealExpression -> RealExpression -> ModelBuilderT m RealExpression
a `powerM` b = return $ a `Power` b
(.**.) :: Monad m => ModelBuilderT m RealExpression -> ModelBuilderT m RealExpression -> ModelBuilderT m RealExpression
(.**.) = S.liftM2 Power
powerX = (.**.)

logBaseM :: Monad m => RealExpression -> RealExpression -> ModelBuilderT m RealExpression
a `logBaseM` b = return $ a `LogBase` b
logBaseX :: Monad m => ModelBuilderT m RealExpression -> ModelBuilderT m RealExpression -> ModelBuilderT m RealExpression
logBaseX = S.liftM2 LogBase

floorM :: Monad m => RealExpression -> ModelBuilderT m RealExpression
floorM = return . Floor
floorX :: Monad m => ModelBuilderT m RealExpression -> ModelBuilderT m RealExpression
floorX = S.liftM Floor
ceilingM :: Monad m => RealExpression -> ModelBuilderT m RealExpression
ceilingM = return . Ceiling
ceilingX :: Monad m => ModelBuilderT m RealExpression -> ModelBuilderT m RealExpression
ceilingX = S.liftM Floor
sinM :: Monad m => RealExpression -> ModelBuilderT m RealExpression
sinM = return . Sin
sinX :: Monad m => ModelBuilderT m RealExpression -> ModelBuilderT m RealExpression
sinX = S.liftM Sin
tanM :: Monad m => RealExpression -> ModelBuilderT m RealExpression
tanM = return . Tan
tanX :: Monad m => ModelBuilderT m RealExpression -> ModelBuilderT m RealExpression
tanX = S.liftM Tan
cosM :: Monad m => RealExpression -> ModelBuilderT m RealExpression
cosM = return . Cos
cosX :: Monad m => ModelBuilderT m RealExpression -> ModelBuilderT m RealExpression
cosX = S.liftM Cos
asinM :: Monad m => RealExpression -> ModelBuilderT m RealExpression
asinM = return . ASin
asinX :: Monad m => ModelBuilderT m RealExpression -> ModelBuilderT m RealExpression
asinX = S.liftM ASin
atanM :: Monad m => RealExpression -> ModelBuilderT m RealExpression
atanM = return . ATan
atanX :: Monad m => ModelBuilderT m RealExpression -> ModelBuilderT m RealExpression
atanX = S.liftM ATan
acosM :: Monad m => RealExpression -> ModelBuilderT m RealExpression
acosM = return . ACos
acosX :: Monad m => ModelBuilderT m RealExpression -> ModelBuilderT m RealExpression
acosX = S.liftM ACos
sinhM :: Monad m => RealExpression -> ModelBuilderT m RealExpression
sinhM = return . Sinh
sinhX :: Monad m => ModelBuilderT m RealExpression -> ModelBuilderT m RealExpression
sinhX = S.liftM Sinh
tanhM :: Monad m => RealExpression -> ModelBuilderT m RealExpression
tanhM = return . Tanh
tanhX :: Monad m => ModelBuilderT m RealExpression -> ModelBuilderT m RealExpression
tanhX = S.liftM Tanh
coshM :: Monad m => RealExpression -> ModelBuilderT m RealExpression
coshM = return . Cosh
coshX :: Monad m => ModelBuilderT m RealExpression -> ModelBuilderT m RealExpression
coshX = S.liftM Cosh
asinhM :: Monad m => RealExpression -> ModelBuilderT m RealExpression
asinhM = return . ASinh
asinhX :: Monad m => ModelBuilderT m RealExpression -> ModelBuilderT m RealExpression
asinhX = S.liftM ASinh
atanhM :: Monad m => RealExpression -> ModelBuilderT m RealExpression
atanhM = return . ATanh
atanhX :: Monad m => ModelBuilderT m RealExpression -> ModelBuilderT m RealExpression
atanhX = S.liftM ATanh
acoshM :: Monad m => RealExpression -> ModelBuilderT m RealExpression
acoshM = return . ACosh
acoshX :: Monad m => ModelBuilderT m RealExpression -> ModelBuilderT m RealExpression
acoshX = S.liftM ACosh

-- Now define some constants...

-- The constant pi.
piM :: Monad m => ModelBuilderT m RealExpression
piM = realConstantM dimensionlessE pi
piX = piM

-- The constant e.
econstantE = realConstantE dimensionlessE (exp 1.0)
econstant :: Monad m => ModelBuilderT m RealExpression
econstant = realConstantM dimensionlessE (exp 1.0)
econstantM = econstant
econstantX = econstant

-- Now some functions which are simple applications of existing functions

-- The exp function, made using .**.
expE = (econstantE `Power`)
expM :: Monad m => RealExpression -> ModelBuilderT m RealExpression
expM = return . expE
expX m = m >>= expM

-- The negative of the expression.
negateE = ((realConstantE dimensionlessE (-1.0)) `Times`)
negateM :: Monad m => RealExpression -> ModelBuilderT m RealExpression
negateM = return . negateE
negateX m = m >>= negateM

-- The square root of an expression.
sqrtE x = x `Power` (realConstantE dimensionlessE 0.5)
sqrtM :: Monad m => RealExpression -> ModelBuilderT m RealExpression
sqrtM = return . sqrtE
sqrtX m = m >>= sqrtM

-- The log base e of an expression.
logE = LogBase econstantE
logM :: Monad m => RealExpression -> ModelBuilderT m RealExpression
logM x = return $ logE x
logX m = m >>= logM

-- Now some more complex functions that only have M and X forms...
boolIfM :: Monad m => BoolExpression -> BoolExpression -> BoolExpression -> ModelBuilderT m BoolExpression
boolIfM cond x1 x2 =
    do
      condc <- boolCommonSubexpressionM cond
      (condc `andM` x1) .||. ((Not condc) `andM` x2)
boolIfX mcond mx1 mx2 =
    do
      cond <- mcond
      x1 <- mx1
      x2 <- mx2
      boolIfM

xorM :: Monad m => BoolExpression -> BoolExpression -> ModelBuilderT m BoolExpression
xorM x1 x2 =
    do
      x1c <- boolCommonSubexpressionM x1
      (x1c `andM` (Not x2)) .||. ((Not x1c) `andM` x2)
xorX mx1 mx2 =
    do
      x1 <- mx1
      x2 <- mx2
      xorM x1 x2

boundUnits :: Monad m => ModelBuilderT m Units
boundUnits = M.lift R.ask

initialValueM bv v u iv =
    newBoundaryEq (realConstant boundUnits bv .==. boundVariable) (return v) (realConstant u iv)

initialValueX bv v iv =
  do
    v' <- v
    initialValueM bv v' iv
initialValue = initialValueX

insertContextTag typetag tag v = S.modify (\m -> m {contextTaggedIDs = M.insert (typetag, tag) v (contextTaggedIDs m)})

getContextTag :: Monad m => D.TypeCode -> D.TypeCode -> ModelBuilderT m (Maybe Int)
getContextTag typetag tag = do
  idmap <- S.gets contextTaggedIDs
  return $ M.lookup (typetag, tag) idmap
contextTaggedID typetag tag wrap allocm =
    do
      t <- getContextTag typetag tag
      case t
        of
          Just id -> return $ wrap id
          Nothing ->
            do
              id <- allocateID
              allocm id
              insertContextTag typetag tag id
              return $ wrap id

data RealCSEContextTag = RealCSEContextTag deriving (D.Typeable, D.Data)
realCSEContextTag = D.typeCode RealCSEContextTag
data BoolCSEContextTag = BoolCSEContextTag deriving (D.Typeable, D.Data)
boolCSEContextTag = D.typeCode BoolCSEContextTag
data RealVariableContextTag = RealVariableContextTag deriving (D.Typeable, D.Data)
realVariableContextTag = D.typeCode RealVariableContextTag

contextBoolCommonSubexpressionM :: Monad m => D.TypeCode -> BoolExpression -> ModelBuilderT m BoolExpression
contextBoolCommonSubexpressionM tag e =
  contextTaggedID boolCSEContextTag tag (BoolCommonSubexpressionE . flip BoolCommonSubexpression e) $
    \id ->
        do
          let bcs = BoolCommonSubexpression id e
          registerCommonSubexpression (FromBoolCommonSubexpression bcs)
contextBoolCommonSubexpressionX tag e =
    e >>= contextBoolCommonSubexpressionM tag
contextBoolCommonSubexpression tag me = do
  ex <- contextBoolCommonSubexpressionX tag me
  return (return ex)

contextRealCommonSubexpressionM :: Monad m => D.TypeCode -> RealExpression -> ModelBuilderT m RealExpression
contextRealCommonSubexpressionM tag e =
  contextTaggedID realCSEContextTag tag (RealCommonSubexpressionE . flip RealCommonSubexpression e) $
    \id ->
        do
          let bcs = RealCommonSubexpression id e
          registerCommonSubexpression (FromRealCommonSubexpression bcs)

contextRealCommonSubexpressionX tag me = me >>= contextRealCommonSubexpressionM tag
contextRealCommonSubexpression tag me = do
  ex <- contextRealCommonSubexpressionX tag me
  return (return ex)

contextMkNewRealVariable :: Monad m => D.TypeCode -> Units -> ModelBuilderT m RealVariable
contextMkNewRealVariable tag u =
    contextTaggedID realVariableContextTag tag (RealVariable u) $
      \id -> S.modify (\m -> m { variables = (RealVariable u id):(variables m) } )
contextMkNewRealVariableM :: Monad m => D.TypeCode -> Units -> ModelBuilderT m (ModelBuilderT m RealVariable)
contextMkNewRealVariableM tag u = S.liftM return (contextMkNewRealVariable tag u)
