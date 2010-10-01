{-# LANGUAGE DeriveDataTypeable,NoMonomorphismRestriction,TemplateHaskell #-}
module ModML.Units.SIUnits
where
import qualified Data.Typeable as D
import qualified Data.TypeHash as D
import qualified Data.Data as D
import qualified Control.Monad as M

import qualified ModML.Units.UnitsDAEModel as U

uYotta = U.unitsMultiplier 1E24
uZetta = U.unitsMultiplier 1E21
uExa = U.unitsMultiplier 1E18
uPeta = U.unitsMultiplier 1E15
uTera = U.unitsMultiplier 1E12
uGiga = U.unitsMultiplier 1E9
uMega = U.unitsMultiplier 1E6
uKilo = U.unitsMultiplier 1E3
uHecto = U.unitsMultiplier 1E2
uDeca = U.unitsMultiplier 1E1
uDeci = U.unitsMultiplier 1E-1
uCenti = U.unitsMultiplier 1E-2
uMilli = U.unitsMultiplier 1E-3
uMicro = U.unitsMultiplier 1E-6
uNano = U.unitsMultiplier 1E-9
uPico = U.unitsMultiplier 1E-12
uFemto = U.unitsMultiplier 1E-15
uAtto = U.unitsMultiplier 1E-18
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
