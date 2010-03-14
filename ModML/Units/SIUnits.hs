{-# LANGUAGE DeriveDataTypeable,NoMonomorphismRestriction #-}
module ModML.Units.SIUnits
where
import qualified Data.Data as D
import qualified Data.TypeHash as D
import qualified Control.Monad as M

import ModML.Units.UnitsDAEModel

uYotta = unitsMultiplier 1E24
uZetta = unitsMultiplier 1E21
uExa = unitsMultiplier 1E18
uPeta = unitsMultiplier 1E15
uTera = unitsMultiplier 1E12
uGiga = unitsMultiplier 1E9
uMega = unitsMultiplier 1E6
uKilo = unitsMultiplier 1E3
uHecto = unitsMultiplier 1E2
uDeca = unitsMultiplier 1E1
uDeci = unitsMultiplier 1E-1
uCenti = unitsMultiplier 1E-2
uMilli = unitsMultiplier 1E-3
uMicro = unitsMultiplier 1E-6
uNano = unitsMultiplier 1E-9
uPico = unitsMultiplier 1E-12
uFemto = unitsMultiplier 1E-15
uAtto = unitsMultiplier 1E-18
uZepto = unitsMultiplier 1E-21
uYocto = unitsMultiplier 1E-24

data MetreBaseTag = MetreBaseTag deriving (D.Typeable, D.Data)
metreBaseTag = D.typeCode MetreBaseTag
metreBase = contextMkBaseUnit "metre" metreBaseTag
uMetre = M.liftM singletonUnit metreBase

data SecondBaseTag = SecondBaseTag deriving (D.Typeable, D.Data)
secondBaseTag = D.typeCode SecondBaseTag
secondBase = contextMkBaseUnit "second" secondBaseTag
uSecond = M.liftM singletonUnit secondBase

data KilogramBaseTag = KilogramBaseTag deriving (D.Typeable, D.Data)
kilogramBaseTag = D.typeCode KilogramBaseTag
kilogramBase = contextMkBaseUnit "kilogram" kilogramBaseTag
uKilogram = M.liftM singletonUnit kilogramBase

data AmpereBaseTag = AmpereBaseTag deriving (D.Typeable, D.Data)
ampereBaseTag = D.typeCode AmpereBaseTag
ampereBase = contextMkBaseUnit "ampere" ampereBaseTag
uAmpere = M.liftM singletonUnit ampereBase

data MoleBaseTag = MoleBaseTag deriving (D.Typeable, D.Data)
moleBaseTag = D.typeCode MoleBaseTag
moleBase = contextMkBaseUnit "mole" moleBaseTag
uMole = M.liftM singletonUnit moleBase

data CandelaBaseTag = CandelaBaseTag deriving (D.Typeable, D.Data)
candelaBaseTag = D.typeCode CandelaBaseTag
candelaBase = contextMkBaseUnit "candela" candelaBaseTag
uCandela = M.liftM singletonUnit candelaBase

data KelvinBaseTag = KelvinBaseTag deriving (D.Typeable, D.Data)
kelvinBaseTag = D.typeCode KelvinBaseTag
kelvinBase = contextMkBaseUnit "kelvin" kelvinBaseTag
uKelvin = M.liftM singletonUnit kelvinBase

