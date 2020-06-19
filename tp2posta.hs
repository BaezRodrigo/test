import Text.Show.Functions
type Desgaste = Float
type Patente = String
type Fecha = (Int, Int, Int)

-- Definiciones base
anio :: Fecha -> Int
anio (_, _, year) = year

data Auto = Auto {patente :: Patente, desgasteLlantas :: [Desgaste], rpm :: Int, temperaturaAgua :: Int, ultimoArreglo :: Fecha} deriving Show



--PUNTO NUMERO 1
primeras2Letras :: String -> String
primeras2Letras = take 2 

mayorIgual :: String -> String -> Bool
mayorIgual ini = (>= ini).primeras2Letras

menorIgual :: String -> String -> Bool
menorIgual fin = (<= fin).primeras2Letras

estaEntre :: String -> String ->  String -> Bool
estaEntre ini fin palabra = (mayorIgual ini palabra)&&(menorIgual fin palabra)

calculoPatental :: Auto -> Int
calculoPatental auto | (((=='4').last.patente) auto) = 18000
                     | otherwise = 20000

costoReparacion :: Auto -> Int
costoReparacion auto | (length.patente) auto == 7 = 12500
                     | estaEntre "DJ" "NB" (patente auto) = calculoPatental auto
                     | otherwise = 15000



--PUNTO NUMERO 2
--Parte 1
autoPeligroso :: Auto -> Bool
autoPeligroso = (0.5 <).head.desgasteLlantas

--Parte 2
necesitaRevision :: Auto -> Bool
necesitaRevision = (<=2015).anio.ultimoArreglo



--PUNTO NUMERO 3
--Parte 1
type Mecanico = Auto -> Auto

alfa :: Mecanico
alfa auto = auto { rpm = min (rpm auto) 2000}

bravo :: Mecanico
bravo auto = auto{desgasteLlantas = (replicate 4 0)}

charly :: Mecanico
charly = alfa.bravo

--Parte 2
tango :: Mecanico
tango auto = auto

tempA90 :: Auto -> Auto
tempA90 auto = auto { temperaturaAgua = 90 }

zulu :: Mecanico
zulu = lima.tempA90

lima :: Mecanico
lima auto = auto{desgasteLlantas = (replicate 2 0) ++ (drop 2 (desgasteLlantas auto))}



-- PUNTO NUMERO 4
desgasteEsPar :: Auto -> Bool
desgasteEsPar = even.round.(*10).sum.desgasteLlantas

desgasteEsImpar :: Auto -> Bool
desgasteEsImpar = not.desgasteEsPar

mapearIndice :: [Auto] -> [(Int,Auto)]
mapearIndice autos = zip [1..] autos

desgasteOrdenado :: (Int,Auto) -> Bool
desgasteOrdenado serieDeAutos
    |(desgasteEsPar.snd $ serieDeAutos)&&(even.fst $ serieDeAutos) = True
    |(desgasteEsImpar.snd $ serieDeAutos)&&(odd.fst $ serieDeAutos) = True
    |otherwise = False

estanOrdenados :: [Auto] -> Bool
estanOrdenados autos = (all desgasteOrdenado).mapearIndice $ autos



-- PUNTO NUMERO 5
data OrdenReparacion = OrdenReparacion {diaArreglo :: Fecha, trabajoMecanicos :: [Mecanico]} deriving Show
ordenReparacion :: OrdenReparacion -> Auto -> Auto
ordenReparacion orden auto = (actualizarFecha (diaArreglo orden) . realizarReparaciones auto) (trabajoMecanicos orden)

realizarReparaciones :: Auto -> [Mecanico] -> Auto
realizarReparaciones auto mecanicos = foldl (flip($)) auto mecanicos

actualizarFecha :: Fecha -> Auto -> Auto
actualizarFecha fecha auto = auto {ultimoArreglo = fecha}




-- PUNTO NUMERO 6
-- Parte 1
dejanAutoEnCondiciones :: Auto -> [Mecanico] -> [Mecanico]
dejanAutoEnCondiciones auto mecanicos = filter (dejaAutoEnCondiciones auto) mecanicos

dejaAutoEnCondiciones :: Auto -> Mecanico -> Bool
dejaAutoEnCondiciones auto mecanico = (not.autoPeligroso) (mecanico auto)

-- Parte 2
costoReparacionAutos :: [Auto] -> Int
costoReparacionAutos = sum.map costoReparacion.filter necesitaRevision 



-- PUNTO NUMERO 7
-- Parte 1
primerMecanicoDejaEnCondiciones :: Auto -> [Mecanico] -> Mecanico
primerMecanicoDejaEnCondiciones auto mecanicos = head (dejanAutoEnCondiciones auto mecanicos)

-- Parte 2
costoPrimerosTresAutosNecesitanRevision :: [Auto] -> Int
costoPrimerosTresAutosNecesitanRevision autos = costoReparacionAutos (take 3 (filter necesitaRevision autos))

tecnicosInfinitos = zulu:tecnicosInfinitos

autosInfinitos :: [Auto]
autosInfinitos = autosInfinitos' 0

autosInfinitos' :: Float -> [Auto]
autosInfinitos' n = Auto {
 patente = "AAA000",
 desgasteLlantas = [n, 0, 0, 0.3],
 rpm = 1500,
 temperaturaAgua = 90,
 ultimoArreglo = (20, 1, 2013)
} : autosInfinitos' (n + 1)
