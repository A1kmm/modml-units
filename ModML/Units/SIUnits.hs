{-# LANGUAGE DeriveDataTypeable,NoMonomorphismRestriction,TemplateHaskell #-}
module ModML.Units.SIUnits
where
import qualified Data.Typeable as D
import qualified Data.TypeHash as D
import qualified Data.Data as D
import qualified Control.Monad as M

import qualified ModML.Units.UnitsDAEModel as U
import ModML.Units.UnitsDAEModel (($**$),($*$))

uYotta = return $ U.unitsMultiplier 1E24
uZetta = return $ U.unitsMultiplier 1E21
uExa   = return $ U.unitsMultiplier 1E18
uPeta  = return $ U.unitsMultiplier 1E15
uTera  = return $ U.unitsMultiplier 1E12
uGiga  = return $ U.unitsMultiplier 1E9
uMega  = return $ U.unitsMultiplier 1E6
uKilo  = return $ U.unitsMultiplier 1E3
uHecto = return $ U.unitsMultiplier 1E2
uDeca  = return $ U.unitsMultiplier 1E1
uDeci  = return $ U.unitsMultiplier 1E-1
uCenti = return $ U.unitsMultiplier 1E-2
uMilli = return $ U.unitsMultiplier 1E-3
uMicro = return $ U.unitsMultiplier 1E-6
uNano  = return $ U.unitsMultiplier 1E-9
uPico  = return $ U.unitsMultiplier 1E-12
uFemto = U.unitsMultiplier 1E-15
uAtto  = U.unitsMultiplier 1E-18
uZepto = U.unitsMultiplier 1E-21
uYocto = U.unitsMultiplier 1E-24

U.declareBaseType "metre" "metreBase"
U.declareBaseType "second" "secondBase"
U.declareBaseType "kilogram" "kilogramBase"
U.declareBaseType "ampere" "ampereBase"
U.declareBaseType "mole" "moleBase"
U.declareBaseType "candela" "candelaBase"
U.declareBaseType "kelvin" "kelvinBase"

uMetre = M.liftM U.singletonUnit metreBase
uSecond = M.liftM U.singletonUnit secondBase
uKilogram = M.liftM U.singletonUnit kilogramBase
uAmpere = M.liftM U.singletonUnit ampereBase
uMole = M.liftM U.singletonUnit moleBase
uCandela = M.liftM U.singletonUnit candelaBase
uKelvin = M.liftM U.singletonUnit kelvinBase

uLitre = (uCenti $*$ uMetre) $**$ 3
