module Lib where
import Text.Show.Functions

laVerdad = True

-- 1)
type Gema = Personaje -> Personaje

data Guantelete = UnGuantelete {
    gemas :: [Gema],
    material :: String
}deriving (Show)

data Personaje = UnPersonaje {
    edad :: Int,
    energia :: Int,
    habilidades :: [String],
    nombre :: Int,
    planeta :: Int
}

type Universo = [Personaje]


chasquear :: Guantelete -> Universo -> Universo
chasquear guantelete universo | guanteleteCompleto guantelete = reducirMitad universo
                              | otherwise = universo

guanteleteCompleto :: Guantelete -> Bool
guanteleteCompleto guantelete = ((==6).length.gemas) guantelete && ((=="uru").material) guantelete

reducirMitad :: Universo -> Universo
reducirMitad universo = take (length universo `div` 2) universo


-- 2)

aptoParaPendex :: Universo -> Bool
aptoParaPendex = any $ (<=45).edad

energiaTotal :: Universo -> Int
energiaTotal = sum . map energia . masDeUnaHabilidad

masDeUnaHabilidad :: Universo -> Universo
masDeUnaHabilidad = filter ((>1).length.habilidades)

