{-# LANGUAGE TemplateHaskell #-}
module ModML.Units.UnitsTemplate
where
  import Language.Haskell.TH
  import Language.Haskell.TH.Lib
  import Language.Haskell.TH.ExpandSyns
  import Data.List
  import Control.Monad
  import Data.Generics

  data ZeroType = ZeroType
  data OneType = OneType
  data TwoType = TwoType
  data ThreeType = ThreeType
  data FourType = FourType
  data FiveType = FiveType
  data SixType = SixType
  data SevenType = SevenType
  data EightType = EightType
  data NineType = NineType
  data PointType = PointType
  data NegativeType a = NegativeType a

  digitsOf' l n | n < 10 = n:l
                | otherwise = digitsOf' ((n `mod` 10):l) (n `div` 10)
  digitsOf = digitsOf' []

  decimalsOf' dpth l n
      | dpth > 15 = l
      | otherwise =
          let
              mn = n * 10
              d = floor mn
              mnr = mn - (fromIntegral d)
          in
            if mnr == 0.0
            then
                d:l
            else
                decimalsOf' (dpth + 1) (d:l) mnr
  decimalsOf = reverse . dropWhile (==0) . decimalsOf' 0 []

  digitToTypeQ 0 = varT ''ZeroType
  digitToTypeQ 1 = varT ''OneType
  digitToTypeQ 2 = varT ''TwoType
  digitToTypeQ 3 = varT ''ThreeType
  digitToTypeQ 4 = varT ''FourType
  digitToTypeQ 5 = varT ''FiveType
  digitToTypeQ 6 = varT ''SixType
  digitToTypeQ 7 = varT ''SevenType
  digitToTypeQ 8 = varT ''EightType
  digitToTypeQ 9 = varT ''NineType

  listToTupleT :: [TypeQ] -> TypeQ
  listToTupleT l = foldl' appT (tupleT (length l)) l

  intToTypeQ :: Int -> TypeQ
  intToTypeQ n = listToTupleT $ map digitToTypeQ (digitsOf n)

  intTypeQToInt :: TypeQ -> Q Int
  intTypeQToInt tq = do
    t <- tq
    expandSyns t
    numericTypeToInt 0 t

  intNameToInt n
      | n == ''ZeroType = return 0
      | n == ''OneType = return 1
      | n == ''TwoType = return 2
      | n == ''ThreeType = return 3
      | n == ''FourType = return 4
      | n == ''FiveType = return 5
      | n == ''SixType = return 6
      | n == ''SevenType = return 7
      | n == ''EightType = return 8
      | n == ''NineType = return 9
      | otherwise =
          do
            report True "Expected a type-level number here"
            undefined

  numericTypeToInt _ (VarT n) = numericNameToInt n
  numericTypeToInt s (AppT (VarT name) rest) = do
    n <- numericNameToInt name
    numericTypeToInt (s * 10 + n) rest
  numericTypeToInt s (TupleT _) = return s
  numericTypeToInt _ _ = do
    report True "Expected a type-level number here"
    undefined

  -- unitsCombineQ un1q 

  data MetreBase = MetreBase
  data SecondBase = SecondBase
  data KilogramBase = KilogramBase
  data AmpereBase = AmpereBase
  data KelvinBase = KelvinBase
  data MoleBase = MoleBase
  data CandelaBase = CandelaBase
