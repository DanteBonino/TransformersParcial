module Lib () where

--Cosas que te da el enunciado:

data Autobot = 
Robot String (Int,Int,Int) ((Int,Int,Int) -> (Int,Int)) | 
Vehiculo String (Int,Int)

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
maximoSegun :: (a -> a -> c) -> a -> a -> a
maximoSegun f unValor otroValor
  | f a b >= f b a = a
  | otherwise      = b

--Punto 2:
nombre :: Autobot -> String
nombre (Robot nombre _ _)       = nombre
nombre (Vehiculo nombre _)      = nombre

transformador :: Autobot -> ((Int,Int,Int) -> (Int,Int))
transformador (Robot _ funcionTransformadora) = funcionTransformadora

fuerza :: Autbot -> Int
nombre (Robot _ (fuerza _ _) _)       = fuerza
nombre  _                             = 0

velocidad :: Autbot -> Int
nombre (Robot _ (_ velocidad _) _)  = velocidad
nombre (Vehiculo _ (velocidad _))   = velocidad

resistencia :: Autobot -> Int
resistencia (Robot _ (_ _ resistencia) _)  = resistencia
resistencia (Vehiculo _ (_ resistencia))   = resistencia

--Punto 3:
transformar :: Robot -> Vehiculo
transformar unRobot = Vehiculo (nombre unRobot) ((transformador unRobot) . (todosLosAtributos) unRobot)

todosLosAtributos :: Robot -> (Int, Int, Int)
todosLosAtributos (Robot _ atributos _) = atributos

--Punto 4:
velocidadContra :: Autobot -> Autobot -> Int
velocidadContra unAutobot otroAutobot = velocidad unAutobot - max 0 ((subtract fuerza otrAutobot . resistencia) unAutobot)

--Punto 5:
elMasRapido :: Autobot -> Autobot -> Autobot
elMasRapido = maximoSegun velocidadContra

--Punto 6:
--a)
domina :: Robot -> Robot -> Bool
domina unAutobot  = (all ((== unAutobot).(uncurry elMasrapido)) . crearListaDeEnfrentamientos unAutobot)

crearListaDeEnfrentamientos :: Robot -> Robot -> [(Autobot, Autobot)]
crearListaDeEnfrentamientos unRobot otroRobot = [(unRobot, otroRobot), (unRobot, transformar otroRobot), (transformar unRobot, otroRobot), (transformar unRobot, transformar otroRobot)]

--b)
dominaATodos :: Robot -> [Robot] -> Bool
dominaATodos  unRobot = all (domina unAutobot)
--Punto 7:
--a)
quienesCumplen :: (Autbot -> Bool)-> [Autobot] -> [Autobot]
quienesCumplen unaCondicion = filter unaCondicion
--b)
dominaAtodosYSuNombreTerminaEnVocal :: [Autobot] -> Bool
dominaAtodosYSuNombreTerminaEnVocal unaListaDeAutobots = quienesCumplen (flip dominaATodos unaListaDeAutobots) unaListaDeAutobots

--Punto 8:
saraza :: (Ord c) => a -> a -> a -> (a -> a -> c) -> c
saraza x y w z = z w . maximoSegun z y $ x







