module Lib () where

--Cosas que te da el enunciado:

data Autobot = Robot String (Int,Int,Int) ((Int,Int,Int) -> (Int,Int)) | Vehiculo String (Int,Int) deriving Eq

optimus = Robot "Optimus Prime" (20,20,10) optimusTransformacion
optimusTransformacion (_,v,r) = (v * 5, r * 2)

jazz = Robot "Jazz" (8,35,3) jazzTransformacion
jazzTransformacion (_,v,r) = (v * 6, r * 3)

wheeljack = Robot "Wheeljack" (11,30,4) wheeljackTransformacion
wheeljackTransformacion (_,v,r) = (v * 4, r * 3)

bumblebee = Robot "Bumblebee" (10,33,5) bumblebeeTransformacion
bumblebeeTransformacion (_,v,r) = (v * 4, r * 2)

autobots = [ optimus, jazz, wheeljack, bumblebee ]

--Punto 1:
maximoSegun :: (Ord c) => (a -> a -> c) -> a -> a -> a
maximoSegun f unValor otroValor
  | f unValor otroValor >= f unValor otroValor = unValor
  | otherwise                                  = otroValor

--Punto 2:
nombre :: Autobot -> String
nombre (Robot nombre _ _)       = nombre
nombre (Vehiculo nombre _)      = nombre

transformador :: Autobot -> ((Int,Int,Int) -> (Int,Int))
transformador (Robot _ _ funcionTransformadora) = funcionTransformadora

fuerza :: Autobot -> Int
fuerza (Robot _ (fuerza, _, _) _)       = fuerza
fuerza  _                               = 0

velocidad :: Autobot -> Int
velocidad (Robot _ (_,velocidad, _) _)  = velocidad
velocidad (Vehiculo _ (velocidad, _))   = velocidad

resistencia :: Autobot -> Int
resistencia (Robot _ (_, _, resistencia) _)  = resistencia
resistencia (Vehiculo _ (_ ,resistencia))   = resistencia

--Punto 3:
transformar :: Autobot -> Autobot
transformar unRobot = Vehiculo (nombre unRobot) (((transformador unRobot) . (todosLosAtributos)) unRobot)

todosLosAtributos :: Autobot -> (Int, Int, Int)
todosLosAtributos (Robot _ atributos _) = atributos

--Punto 4:
velocidadContra :: Autobot -> Autobot -> Int
velocidadContra unAutobot otroAutobot = velocidad unAutobot - max 0 ((subtract (fuerza otroAutobot) . resistencia) unAutobot)

--Punto 5:
elMasRapido :: Autobot -> Autobot -> Autobot
elMasRapido = maximoSegun velocidadContra

--Punto 6:
--a)
domina :: Autobot -> Autobot -> Bool
domina unAutobot  = (all ((== unAutobot).(uncurry elMasRapido)) . listaDeEnfrentamientos unAutobot)

listaDeEnfrentamientos :: Autobot -> Autobot -> [(Autobot, Autobot)]
listaDeEnfrentamientos unRobot otroRobot = [(unRobot, otroRobot), (unRobot, transformar otroRobot), (transformar unRobot, otroRobot), (transformar unRobot, transformar otroRobot)]

--b)
dominaATodos :: Autobot -> [Autobot] -> Bool
dominaATodos  unRobot = all (domina unRobot)

--Punto 7:
--a)
quienesCumplen :: (Autobot -> Bool)-> [Autobot] -> [String]
quienesCumplen unaCondicion = map nombre . filter unaCondicion
--b)
dominaAtodosYSuNombreTerminaEnVocal :: [Autobot] -> Bool
dominaAtodosYSuNombreTerminaEnVocal unaListaDeAutobots =  ((not . null) . quienesCumplen (dominaATodosYTerminaEnVocal unaListaDeAutobots)) unaListaDeAutobots

dominaATodosYTerminaEnVocal :: [Autobot] -> Autobot -> Bool
dominaATodosYTerminaEnVocal unaListaDeAutobots unAutobot = dominaATodos unAutobot unaListaDeAutobots && (terminaEnVocal . nombre) unAutobot

terminaEnVocal :: String -> Bool
terminaEnVocal = (esVocal . last)

esVocal :: Char -> Bool
esVocal = flip elem vocales

vocales :: String
vocales = "aeiou"

--Punto 8:
saraza :: (Ord c) => a -> a -> a -> (a -> a -> c) -> c
saraza x y w z = z w . maximoSegun z y $ x









