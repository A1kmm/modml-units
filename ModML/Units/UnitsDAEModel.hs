{-#LANGUAGE NoMonomorphismRestriction,DeriveDataTypeable,MultiParamTypeClasses,FlexibleInstances,FunctionalDependencies,TemplateHaskell #-}
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
import qualified Language.Haskell.TH.Syntax as T
import qualified Data.Char as C

newtype BaseUnit = BaseUnit Int deriving (Eq, Ord, D.Typeable, D.Data)
instance Show BaseUnit
  where
    showsPrec _ (BaseUnit i) = showString "BaseUnit_" . shows i

data Units = Units Double (M.Map BaseUnit Double) deriving (Eq, Ord, Show, D.Typeable, D.Data)

class AlmostEq a
    where
      (=~=) :: a -> a -> Bool
      (=~/) :: a -> a -> Bool
      a =~/ b = not $ a =~= b

doubleWithinTolerance eps a b = (a==b) || ((abs $ a - b) / (min (abs a) (abs b))) <= eps
instance AlmostEq Units
    where
      (Units n1 m1) =~= (Units n2 m2) = doubleWithinTolerance 1E-14 n1 n2 &&
                        (all (\((bu1, mu1), (bu2, mu2)) -> bu1==bu2 &&
                                                           doubleWithinTolerance 1E-14 mu1 mu2) $
                             zip (M.toList m1) (M.toList m2))

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
u1 `unitsTimesX` u2 = do
  u1' <- u1
  u2' <- u2
  return $ u1' `unitsTimes` u2'
($*$) = unitsTimesX

unitsPow :: Units -> Double -> Units
(Units m1 u1) `unitsPow` v = Units (m1**v) (M.map (*v) u1)
u1 `unitsPowX` mup = do
  u1' <- u1
  return $ u1' `unitsPow` mup
($**$) = unitsPowX

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

data RealEquation = RealEquation RealExpression RealExpression deriving (Eq, Ord, D.Typeable, D.Data, Show)

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
                   deriving (Eq, Ord, D.Typeable, D.Data, Show)

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
    ACosh RealExpression |
    -- Units from one first, value from second...
    UnitsOf RealExpression RealExpression
          deriving (Eq, Ord, D.Typeable, D.Data, Show)

class (Ord a) => CommonSubexpression a
  where
    commonSubexpressionId :: a -> Int

data RealCommonSubexpression = RealCommonSubexpression Int RealExpression deriving (Eq, Ord, D.Typeable, D.Data, Show)
instance CommonSubexpression RealCommonSubexpression
  where
    commonSubexpressionId (RealCommonSubexpression a _) = a

data BoolCommonSubexpression = BoolCommonSubexpression Int BoolExpression deriving (Eq, Ord, D.Typeable, D.Data, Show)
instance CommonSubexpression BoolCommonSubexpression
  where
    commonSubexpressionId (BoolCommonSubexpression a _) = a

data AnyCommonSubexpression = FromRealCommonSubexpression RealCommonSubexpression | FromBoolCommonSubexpression BoolCommonSubexpression deriving (Eq, Ord, D.Typeable, D.Data, Show)
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
    } deriving (Eq, Ord, D.Typeable, D.Data, Show)

newtype ModelBuilderT m a = ModelBuilderT (S.StateT UnitsDAEModel (R.ReaderT Units m) a)
type ModelBuilder a = ModelBuilderT I.Identity a

modelBuilderTToState (ModelBuilderT a) = a

instance Monad m => Monad (ModelBuilderT m)
    where
      (ModelBuilderT a) >>= b = ModelBuilderT $ a >>= (modelBuilderTToState . b)
      return a = ModelBuilderT (return a)
      fail a = ModelBuilderT (fail a)
instance M.MonadTrans ModelBuilderT
    where
      lift a = ModelBuilderT $ M.lift $ M.lift a
instance Monad m => S.MonadState UnitsDAEModel (ModelBuilderT m)
    where
      get = ModelBuilderT $ S.get
      put = ModelBuilderT . S.put
instance Monad m => R.MonadReader Units (ModelBuilderT m)
    where
      ask = ModelBuilderT $ M.lift R.ask
      local = undefined

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
                             S.runStateT (modelBuilderTToState indepunits) startingModel
    -- Step 2: Use the units obtained to run the model builder...
    M.lift $ flip R.runReaderT indepunits' $
       S.evalStateT (modelBuilderTToState m) model
    

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

translateVariableAnnotations :: M.Map String String -> [((String, String), String)] -> [((String, String), String)] -> [((String, String), String)]
translateVariableAnnotations _ nl [] = nl
translateVariableAnnotations m nl (((s,p),v):l) =
    case M.lookup s m
    of
      Nothing -> translateVariableAnnotations m (((s, p), v):nl) l
      Just s' -> translateVariableAnnotations m (((s', p), v):((s, p), v):nl) l

pairBothShow :: (Show a, Show b) => (a, b) -> (String, String)
pairBothShow (x, y) = (show x, show y)

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
         B.variables = (B.variables coremod) ++ map snd vars,
         B.commonSubexpressions = (B.commonSubexpressions coremod) ++ cses,
         B.annotations = M.fromList $ translateVariableAnnotations (M.fromList . map (pairBothShow) $ vars) [] .
                                        M.toList $ annotations unitmod,
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

describeBaseUnit :: Monad m => BaseUnit -> ModelBuilderT m String
describeBaseUnit u = do
    a <- getAnnotation u "nameIs"
    case a
      of
        Nothing -> return $ show u
        Just v -> return (read v)

binaryRealUnitsMatchFn :: Monad m => String -> (Units -> B.RealExpression -> B.RealExpression -> ModelBuilderT m a) -> RealExpression -> RealExpression -> ModelBuilderT m a
binaryRealUnitsMatchFn n f ex1 ex2 =
    do
      (u1, ex1') <- translateRealExpression ex1
      (u2, ex2') <- translateRealExpression ex2
      if u1 =~= u2
        then
          f u1 ex1' ex2'
        else
          do
            u1s <- describeUnits u1
            u2s <- describeUnits u2
            error $ (showString "Units mismatch on (" . shows ex1' . showString ") `" .
                     showString n . showString "` (" . shows ex2' . showString ") - " .
                     showString u1s . showString " differs from ") u2s
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

translateRealEquation (RealEquation ex1 ex2) = binaryRealUnitsMatchFn "equation" (\u a b -> return $ B.RealEquation a b) ex1 ex2

translateBoolExpression (BoolBasicExpression e) = return $ e
translateBoolExpression (BoolCommonSubexpressionE bce) = M.liftM B.BoolCommonSubexpressionE (translateBoolCommonSubexpression bce)
translateBoolExpression (ex1 `And` ex2) = M.liftM2 B.And (translateBoolExpression ex1) (translateBoolExpression ex2)
translateBoolExpression (Not ex1) = M.liftM B.Not (translateBoolExpression ex1)
translateBoolExpression (ex1 `Or` ex2) = M.liftM2 B.Or (translateBoolExpression ex1) (translateBoolExpression ex2)
translateBoolExpression (ex1 `LessThan` ex2) = binaryRealUnitsMatchFn "LessThan" (\u a b -> return $ a `B.LessThan` b) ex1 ex2
translateBoolExpression (ex1 `Equal` ex2) = binaryRealUnitsMatchFn "Equal" (\u a b -> return $ a `B.Equal` b) ex1 ex2

runWithUnits :: ModelBuilder a -> a
runWithUnits v =
  fst $ I.runIdentity $ flip R.runReaderT dimensionlessE $ S.runStateT (modelBuilderTToState v) nullModel

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
  binaryRealUnitsMatchFn "If" (\u ex1 ex2 -> return (u, B.If cond ex1 ex2)) r1 r2
translateRealExpression (r1 `Plus` r2) =
  binaryRealUnitsMatchFn "Plus" (\u ex1 ex2 -> return $ (u, ex1 `B.Plus` ex2)) r1 r2
translateRealExpression (r1 `Minus` r2) =
  binaryRealUnitsMatchFn "Minus" (\u ex1 ex2 -> return $ (u, ex1 `B.Minus` ex2)) r1 r2
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
    else if (M.size m1 /= M.size m2) || (u1 =~/ (u2 `unitsPow` (((snd . M.elemAt 0) m1) / ((snd . M.elemAt 0) m2))))
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
translateRealExpression (UnitsOf r1 r2) = do
  (u, _) <- translateRealExpression r1
  (_, ex) <- translateRealExpression r2
  return (u, ex)

translateVariable v@(RealVariable _ n) = return $ (v, B.RealVariable n)
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
andX :: Monad m => ModelBuilderT m BoolExpression -> ModelBuilderT m BoolExpression -> ModelBuilderT m BoolExpression
andX = S.liftM2 And
(.&&.) = andX

orM :: Monad m => BoolExpression -> BoolExpression -> ModelBuilderT m BoolExpression
a `orM` b = return $ a `Or` b
orX :: Monad m => ModelBuilderT m BoolExpression -> ModelBuilderT m BoolExpression -> ModelBuilderT m BoolExpression
orX = S.liftM2 Or
(.||.) = orX

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

lessEqualM :: Monad m => RealExpression -> RealExpression -> ModelBuilderT m BoolExpression
a `lessEqualM` b = do
  aex <- realCommonSubexpressionM a
  bex <- realCommonSubexpressionM b
  (aex `lessThanM` bex) .||. (aex `equalM` bex)
lessEqualX :: Monad m => ModelBuilderT m RealExpression -> ModelBuilderT m RealExpression -> ModelBuilderT m BoolExpression
a `lessEqualX` b = do
  a' <- a
  b' <- b
  a' `lessEqualM` b'
(.<=.) = lessEqualX

greaterThanM :: Monad m => RealExpression -> RealExpression -> ModelBuilderT m BoolExpression
a `greaterThanM` b = notX $ a `lessEqualM` b
greaterThanX :: Monad m => ModelBuilderT m RealExpression -> ModelBuilderT m RealExpression -> ModelBuilderT m BoolExpression
a `greaterThanX` b = notX $ a `lessEqualX` b
(.>.) = greaterThanX

greaterEqualM :: Monad m => RealExpression -> RealExpression -> ModelBuilderT m BoolExpression
a `greaterEqualM` b = notX $ a `lessThanM` b
greaterEqualX :: Monad m => ModelBuilderT m RealExpression -> ModelBuilderT m RealExpression -> ModelBuilderT m BoolExpression
a `greaterEqualX` b = notX $ a `lessThanX` b
(.>=.) = greaterEqualX

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

dConstant :: Monad m => Double -> ModelBuilderT m RealExpression
dConstant = realConstant dimensionless

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


requireNameM :: (Monad m, Show a) => ModelBuilderT m a -> String -> ModelBuilderT m a
requireNameM m n = do
    m' <- m
    annotateModel m' "nameIs" n
    return m'

data BaseUnitTag = BaseUnitTag deriving (D.Typeable, D.Data)
baseUnitTag = D.typeCode BaseUnitTag

newBaseUnit :: Monad m => ModelBuilderT m BaseUnit
newBaseUnit = M.liftM BaseUnit allocateID

newNamedBaseUnit :: Monad m => String -> ModelBuilderT m BaseUnit
newNamedBaseUnit = requireNameM newBaseUnit

newTaggedBaseUnit :: Monad m => D.TypeCode -> ModelBuilderT m BaseUnit
newTaggedBaseUnit tag = contextTaggedID baseUnitTag tag BaseUnit return

newNamedTaggedBaseUnit :: Monad m => D.TypeCode -> String -> ModelBuilderT m BaseUnit
newNamedTaggedBaseUnit t = requireNameM (newTaggedBaseUnit t)

data RealVariableContextTag = RealVariableContextTag deriving (D.Typeable, D.Data)
realVariableContextTag = D.typeCode RealVariableContextTag

newRealVariable :: Monad m => ModelBuilderT m Units -> ModelBuilderT m RealVariable
newRealVariable u = do
  id <- allocateID
  u' <- u
  let v = RealVariable u' id
  S.modify (\m -> m { variables = v:(variables m) } )
  return v

newNamedRealVariable :: Monad m => ModelBuilderT m Units -> String -> ModelBuilderT m RealVariable
newNamedRealVariable u = requireNameM (newRealVariable u)

newTaggedRealVariable :: Monad m => ModelBuilderT m Units -> D.TypeCode -> ModelBuilderT m RealVariable
newTaggedRealVariable u tag = do
  u' <- u
  contextTaggedID realVariableContextTag tag (RealVariable u')
                      (\v -> do
                         S.modify (\m -> m { variables = (RealVariable u' v):(variables m) } )
                         return v
                      )

newNamedTaggedRealVariable :: Monad m => ModelBuilderT m Units -> D.TypeCode -> String -> ModelBuilderT m RealVariable
newNamedTaggedRealVariable u tag = requireNameM (newTaggedRealVariable u tag)

realVariableM :: Monad m => RealVariable -> ModelBuilderT m RealExpression
realVariableM = return . RealVariableE 
realVariableX :: Monad m => ModelBuilderT m RealVariable -> ModelBuilderT m RealExpression
realVariableX = S.liftM RealVariableE
realVariable = realVariableX

{-
newRealVariableE u =
  realVariable (newRealVariable u)
newNamedRealVariableE u name = do
  realVariable (newNamedRealVariable u name)

newRealVariable :: Monad m => ModelBuilderT m Units -> ModelBuilderT m (ModelBuilderT m RealExpression)
newRealVariable u = do
  u' <- u
  v <- newRealVariableE u'
  return (return v)

newNamedRealVariable u name = do
  u' <- u
  v <- newNamedRealVariableE u' name
  return (return v)
-}

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
powerX :: Monad m => ModelBuilderT m RealExpression -> ModelBuilderT m RealExpression -> ModelBuilderT m RealExpression
powerX = S.liftM2 Power
(.**.) = powerX

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

unitsOfM :: Monad m => RealExpression -> RealExpression -> ModelBuilderT m RealExpression
unitsOfM a b = return $ UnitsOf a b
unitsOfX :: Monad m => ModelBuilderT m RealExpression -> ModelBuilderT m RealExpression -> ModelBuilderT m RealExpression
unitsOfX = M.liftM2 UnitsOf
unitsOf = unitsOfX

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
boundUnits = R.ask

initialValueM bv v u iv = do
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

class UnitsModelBuilderAccess m m1 | m -> m1
    where
      liftUnits :: ModelBuilderT m1 a -> m a
instance UnitsModelBuilderAccess (ModelBuilderT m1) m1
    where
      liftUnits = id

-- Also provide some Template Haskell utilities for declaring base types...
declareTaggedSomething :: T.Exp -> String -> T.Q [T.Dec]
declareTaggedSomething expr varName = do
  let firstUpper [] = []
  let firstUpper (a:l) = (C.toUpper a):l
  let tagTypeName = T.mkName $ (firstUpper varName) ++ "Tag"
  let dataDecl = T.DataD [] tagTypeName [] [T.NormalC tagTypeName []] [T.mkName "D.Typeable", T.mkName "D.Data"]
  typeCodeExpr <- [e|D.typeCode|]
  let tagVal = T.ValD (T.VarP $ T.mkName (varName ++ "Tag")) (T.NormalB $ T.AppE typeCodeExpr (T.ConE tagTypeName)) []
  let varVal = T.ValD (T.VarP $ T.mkName varName)
                 (T.NormalB expr) []
  return [dataDecl, tagVal, varVal]


declareNamedTaggedSomething :: String -> String -> String -> T.Q [T.Dec]
declareNamedTaggedSomething sth prettyName varName =
  let
      contextMk = T.mkName sth
      applyName = T.AppE (T.AppE (T.VarE contextMk) $ T.VarE $ T.mkName (varName ++ "Tag"))
                    (T.LitE (T.StringL prettyName))
  in
    declareTaggedSomething applyName varName

declareBaseType :: String -> String -> T.Q [T.Dec]
declareBaseType = declareNamedTaggedSomething "U.newNamedTaggedBaseUnit"

-- | Declares a variable using Template Haskell. The expression given as the
-- | argument must be of type Monad m => ModelBuilderT m Units.
declareRealVariable :: T.Q T.Exp -> String -> String -> T.Q [T.Dec]
declareRealVariable u prettyName varName = do
  u' <- u
  let applyUName = T.AppE (T.AppE (T.AppE (T.VarE $ T.mkName "U.newNamedTaggedRealVariable") u') $ T.VarE $ T.mkName (varName ++ "Tag"))
                    (T.LitE (T.StringL prettyName))
  declareTaggedSomething applyUName varName

minX :: Monad m => ModelBuilderT m RealExpression -> ModelBuilderT m RealExpression -> ModelBuilderT m RealExpression
minX a b = do
  a' <- realCommonSubexpression a
  b' <- realCommonSubexpression b
  ifX (a' .<. b') a' b'

maxX :: Monad m => ModelBuilderT m RealExpression -> ModelBuilderT m RealExpression -> ModelBuilderT m RealExpression
maxX a b = do
  a' <- realCommonSubexpression a
  b' <- realCommonSubexpression b
  ifX (a' .<. b') b' a'

signX :: Monad m => ModelBuilderT m RealExpression -> ModelBuilderT m RealExpression
signX x = do
  x' <- realCommonSubexpression x
  let zero = unitsOf x' (dConstant 0)
  ifX (x' .<. zero)
    {- then -} (dConstant (-1)) $
    {- else -} ifX (x' .>. zero)
                 {- then -} (dConstant 1)
                 {- else -} (dConstant 0)

infixr 2  .||.
infixr 3  .&&.
infix  4  .==., .<., .>., .<=., .>=.
infixl 6 .+., .-.
infixl 7 .*.
infixl 7 $*$
infixl 7 ./.
infixl 8 .**.
infixl 8 $**$
