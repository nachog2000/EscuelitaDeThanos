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
    nombre :: String,
    planeta :: String
}deriving (Show)

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
aptoParaPendex = any ((<=45).edad)

energiaTotal :: Universo -> Int
energiaTotal = sum . map energia . masDeUnaHabilidad

masDeUnaHabilidad :: Universo -> Universo
masDeUnaHabilidad = filter ((>1).length.habilidades)


-- 3)

laMente :: Int -> Personaje -> Personaje
laMente = quitarEnergia

quitarEnergia :: Int -> Personaje -> Personaje
quitarEnergia cantidad personaje = personaje {energia = energia personaje - cantidad}

elAlma :: String -> Personaje -> Personaje
elAlma habilidad = quitarEnergia 10 . eliminarUnaHabilidadDeseada habilidad

eliminarUnaHabilidadDeseada :: String -> Personaje -> Personaje
eliminarUnaHabilidadDeseada habilidad personaje = personaje {habilidades = filter (/=habilidad) (habilidades personaje)}

elEspacio :: String -> Personaje -> Personaje
elEspacio planeta = quitarEnergia 20 . transportarAlPlaneta planeta

transportarAlPlaneta :: String -> Personaje -> Personaje
transportarAlPlaneta planeta personaje = personaje {planeta = planeta}

elPoder :: Personaje -> Personaje
elPoder personaje = (eliminarHabilidades. quitarEnergia (energia personaje))  personaje

eliminarHabilidades :: Personaje -> Personaje
eliminarHabilidades personaje   | ((<=2).length.habilidades) personaje = personaje {habilidades = []}
                                | otherwise = personaje


elTiempo :: Personaje -> Personaje
elTiempo  = quitarEnergia 50 . reducirEdadALaMitad 

reducirEdadALaMitad :: Personaje -> Personaje
reducirEdadALaMitad personaje = personaje {edad = max 18 (edad personaje `div` 2)}

gemaLoca :: Gema -> Gema
gemaLoca gemas = gemas.gemas




personajePrueba = UnPersonaje {
    edad = 20,
    energia = 60,
    habilidades = ["Integrales","Derivadas","Matematica"],
    nombre = "Nacho",
    planeta = "Tierra"
}
