module Library where

import PdePreludat

-- PUNTO 1A:

data Postre = UnPostre
  { saboresPostre :: [String],
    pesoPostre :: Number,
    temperatura :: Number
  }
  deriving (Show, Eq)

bizcocho :: Postre
bizcocho = UnPostre ["borracho", "fruta", "crema"] 100 20

tartaMelaza :: Postre
tartaMelaza = UnPostre ["Melaza"] 50 0

-- PUTNO 1B:
type Hechizo = Postre -> Postre

incendio :: Hechizo
incendio = perderPesoPorcentual 5 . calentar 1

immobulus :: Hechizo
immobulus postre = postre {temperatura = 0}

wingardiumLeviosa :: Hechizo
wingardiumLeviosa = perderPesoPorcentual 10 . agregarSabor "Concentrado"

diffindo :: Porcentaje -> Hechizo
diffindo = perderPesoPorcentual

riddikulus :: String -> Hechizo
riddikulus sabor = agregarSabor (reverse sabor)

avadaKedavra :: Hechizo
avadaKedavra = immobulus . sacarSabor

type Porcentaje = Number

perderPesoPorcentual :: Porcentaje -> Postre -> Postre
perderPesoPorcentual porcentaje postre = postre {pesoPostre = pesoPostre postre * (100 - porcentaje) / 100}

calentar :: Number -> Postre -> Postre
calentar grados postre = postre {temperatura = temperatura postre + grados}

agregarSabor :: String -> Postre -> Postre
agregarSabor sabor postre = postre {saboresPostre = sabor : saboresPostre postre}

sacarSabor :: Postre -> Postre
sacarSabor postre = postre {saboresPostre = []}

-- PUNTO 1C:

-- postreListo :: Postre -> Bool
estaCongelado :: Postre -> Bool
estaCongelado = (<= 0) . temperatura

tieneAlgunSabor :: Postre -> Bool
tieneAlgunSabor = (> 0) . length . saboresPostre

pesaMasQueCero :: Postre -> Bool
pesaMasQueCero = (> 0) . pesoPostre

estaListo :: Postre -> Bool
estaListo postre = pesaMasQueCero postre && tieneAlgunSabor postre && not (estaCongelado postre)

estanListos :: Hechizo -> [Postre] -> Bool
estanListos hechizo = all (estaListo . hechizo)

-- PUNTO 1D:

promedio :: [Number] -> Number
promedio numeros = sum numeros / length numeros

promedioPesoPostres :: [Postre] -> Number
promedioPesoPostres = promedio . map pesoPostre . filter estaListo

-- PUNTO 2A

data Mago = UnMago
  { hechizos :: [Hechizo],
    cantidadHorrorcruxes :: Number
  }
  deriving (Show)

mago1 :: Mago
mago1 = UnMago [diffindo 20, incendio] 20

mago2 :: Mago
mago2 = UnMago [(riddikulus "unmil"), avadaKedavra] 100

practicar :: Hechizo -> Postre -> Mago -> Mago
practicar hechizo postre = agregarUnHorrorcruxes hechizo postre . aprenderNuevoHechizo hechizo

aprenderNuevoHechizo :: Hechizo -> Mago -> Mago
aprenderNuevoHechizo hechizo mago = mago {hechizos = hechizo : hechizos mago}

agregarUnHorrorcruxes :: Hechizo -> Postre -> Mago -> Mago
agregarUnHorrorcruxes hechizo postre mago
  | equivaleAvada hechizo postre = sumarHorrorcruxes mago
  | otherwise = mago

equivaleAvada :: Hechizo -> Postre -> Bool
equivaleAvada hechizo postre = hechizo postre == avadaKedavra postre

sumarHorrorcruxes :: Mago -> Mago
sumarHorrorcruxes mago = mago {cantidadHorrorcruxes = cantidadHorrorcruxes mago + 1}

-- PUNTO 2B

mejorHechizo :: Postre -> Mago -> Hechizo
mejorHechizo postre mago = foldl1 (elMejorHechizo postre) (hechizos mago)

esMejor :: Postre -> Hechizo -> Hechizo -> Bool
esMejor postre hechizo1 hechizo2 = (length . saboresPostre . hechizo1) postre > (length . saboresPostre . hechizo2) postre

elMejorHechizo :: Postre -> Hechizo -> Hechizo -> Hechizo
elMejorHechizo postre hechizo1 hechizo2
  | esMejor postre hechizo1 hechizo2 = hechizo1
  | otherwise = hechizo2

-- Punto 3

mesaInfinita :: [Postre]
mesaInfinita = cycle [bizcocho, tartaMelaza]

magoInfinito :: Mago
magoInfinito = UnMago {hechizos = repeat incendio, cantidadHorrorcruxes = 0}

{-
3.b)
Verdadero, existe dicha consulta > estanListos avadaKedavra mesaInfinita

Lo que sucede es que nos retorna un False, ya que el all cuando encuentra al primer postre que no esta listo ya nos retorna y no necesita seguir "viendo" la lista infinita.

3.c)

No hay forma de conocer el mejor hechizo de una lista de hechizos infinitos, debido a que para hacerlo tenes que evaluar los distintos elementos de dicha lista.
-}
