data Serie = UnaSerie {
    nombre :: String,
    genero :: String,
    duracion :: Int,
    cantTemporadas :: Int,
    calificaciones :: [Int],
    esOriginalDeNetflis :: Bool
} deriving (Eq, Show)

tioGolpetazo = UnaSerie {
    nombre = "One punch man",
    genero = "Monito chino",
    duracion = 24,
    cantTemporadas = 1,
    calificaciones = [5],
    esOriginalDeNetflis = False
}

cosasExtranias = UnaSerie {
    nombre = "Stranger things",
    genero = "Misterio",
    duracion = 50,
    cantTemporadas = 2,
    calificaciones = [3,3],
    esOriginalDeNetflis = True}

dbs = UnaSerie {
    nombre = "Dragon ball supah",
    genero = "Monito chino",
    duracion = 150,
    cantTemporadas = 5,
    calificaciones = [],
    esOriginalDeNetflis = False}

espejoNegro = UnaSerie {
    nombre = "Black mirror",
    genero = "Suspenso",
    duracion = 123,
    cantTemporadas = 4,
    calificaciones = [2],
    esOriginalDeNetflis = True}

rompiendoMalo = UnaSerie {
    nombre = "Breaking Bad",
    genero = "Drama",
    duracion = 200,
    cantTemporadas = 5,
    calificaciones = [],
    esOriginalDeNetflis = False}

treceRazonesPorque = UnaSerie {
    nombre = "13 reasons why",
    genero = "Drama",
    duracion = 50,
    cantTemporadas = 1,
    calificaciones = [3,3,3],
    esOriginalDeNetflis = True}

-- Parte 1

type Maraton = [Serie]

maraton1::Maraton
maraton1 = [tioGolpetazo,cosasExtranias,dbs,espejoNegro,rompiendoMalo,treceRazonesPorque]

maraton2::Maraton
maraton2 = [cosasExtranias,espejoNegro,dbs]

cantidadDeSeriesMaraton::[Serie]-> Int
cantidadDeSeriesMaraton maraton = length maraton

esPopular::Serie -> Bool
esPopular serie = length (calificaciones serie) >= 3

valeLaPena::Serie -> Bool
valeLaPena serie = cantTemporadas serie > 1 && esPopular serie

maratonValeLaPena:: Maraton -> Bool
maratonValeLaPena maraton = ((valeLaPena.head)maraton && (valeLaPena.last) maraton) || any (== rompiendoMalo) maraton
--duda sobre breaking bad, ¿debo chequear todos los parametros internos?

maratonRepuntaAlFinal:: Maraton -> Bool
maratonRepuntaAlFinal maraton | (even.cantidadDeSeriesMaraton) maraton = mitadDeUnaMaratonNoValeLaPena maraton && (maratonValeLaPena.(drop (mitadDeUnaMaraton maraton))) maraton
                              | otherwise = mitadDeUnaMaratonNoValeLaPena maraton  && (maratonValeLaPena.(drop (mitadDeUnaMaraton maraton +1))) maraton

mitadDeUnaMaratonNoValeLaPena maraton = (not.maratonValeLaPena.(take (mitadDeUnaMaraton maraton))) maraton

{- duda, ¿puedo usar esta funcion y negarla despues? (con nop)
mitadDeUnaMaratonValeLaPena maraton = (maratonValeLaPena.(take (mitadDeUnaMaraton maraton))) maraton
-}

mitadDeUnaMaraton maraton = div (cantidadDeSeriesMaraton maraton) 2

calcularCalificacionDeSerie::Serie -> Int
calcularCalificacionDeSerie serie = div (sum (calificaciones serie)) (length (calificaciones serie))

obtenerDispersionDeCalificaciones serie |(maximaCalificacionDeSerie serie == minimaCalificacionDeSerie serie) = 0
                                        |otherwise = maximaCalificacionDeSerie serie - minimaCalificacionDeSerie serie

maximaCalificacionDeSerie serie = maximum(calificaciones serie)

minimaCalificacionDeSerie serie = minimum(calificaciones serie)

calificarSerie:: Int -> Serie -> Serie
calificarSerie calificacion serie = serie {calificaciones = calificaciones serie ++ [calificacion]}

hypearSerie:: Serie -> Serie
hypearSerie serie |(any (==1) (calificaciones serie)) = serie {calificaciones = calificaciones serie}
                  |otherwise = serie {calificaciones = [aumentarExtrellasDeCalificacion 2 (head (calificaciones serie))] ++ listaSinExtremos (calificaciones serie) ++ [aumentarExtrellasDeCalificacion 2 (last (calificaciones serie))]}

listaSinExtremos lista = drop 1 (take (length lista -1) lista)

aumentarExtrellasDeCalificacion estrellas calificacion | ( (calificacion + estrellas )> 5) = 5
                                                       | otherwise = calificacion + estrellas

-- Parte 2

obtenerSeriesPorGenero:: String -> Maraton -> Maraton
obtenerSeriesPorGenero generoSerie maraton = filter (perteceAlGeneroLaSerie generoSerie) maraton

--Menos espresividad pero compongo
--obtenerSeriesPorGenero generoSerie maraton = filter ((==generoSerie).genero) maraton

perteceAlGeneroLaSerie:: String -> Serie -> Bool
perteceAlGeneroLaSerie generoSerie serie = (genero serie) == generoSerie

obtenerSeriesDeNetflisQueValenLaPena maraton = filter ((==True).valeLaPenaYesDeNetflis) maraton

valeLaPenaYesDeNetflis serie = valeLaPena serie && esOriginalDeNetflis serie
