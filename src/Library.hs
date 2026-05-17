module Library where
import PdePreludat

palos = ["Corazones", "Picas", "Tréboles", "Diamantes"]
type Carta = (Number, Palo)
type Palo = String

data Jugador = Jugador {
  nombre :: String,
  mano :: [Carta],
  bebida :: String
}

pokerDeAses, fullDeJokers, piernaDeNueves :: [Carta]
pokerDeAses    = [(1,"Corazones"), (1,"Picas"), (1,"Tréboles"), (1,"Diamantes"), (10,"Diamantes")]
fullDeJokers   = [(11,"Corazones"), (11,"Picas"), (11,"Tréboles"), (10,"Diamantes"), (10,"Picas")]
piernaDeNueves = [(9,"Corazones"), (9,"Picas"), (9,"Tréboles"), (10,"Diamantes"), (4,"Copas")]

jamesBond, leChiffre, felixLeiter :: Jugador
jamesBond   = Jugador "Bond... James Bond" pokerDeAses "Martini... shaken, not stirred"
leChiffre   = Jugador "Le Chiffre" fullDeJokers "Gin"
felixLeiter = Jugador "Felix Leiter" piernaDeNueves "Whisky"

mesaQueMasAplauda :: [Jugador]
mesaQueMasAplauda = [jamesBond, leChiffre, felixLeiter]

ocurrenciasDe :: Eq a => a -> [a] -> Number
ocurrenciasDe x = length . filter (== x)
concatenar :: [[a]] -> [a]
concatenar = foldl (++) []

--1.a
mayorSegun :: Ord a => (b -> a) -> b -> b -> b
mayorSegun f x y
  | f x > f y = x
  | otherwise = y

--1.b
maximoSegun :: Ord a => (b -> a) -> [b] -> b
maximoSegun f = foldl1 (mayorSegun f)

--1.c
sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos [] = []
sinRepetidos (x:xs) = x : sinRepetidos (filter (/= x) xs)

--2.a
esoNoSeVale :: Carta -> Bool
esoNoSeVale (numero, palo) = not $ elem numero [1 .. 13] && elem palo palos

--2.b
manoMalArmada :: [Carta] -> Bool
manoMalArmada cartas = any esoNoSeVale cartas || length cartas /= 5

--3
type Juego = [Carta] -> Bool
par, pierna, color, fullHouse, poker, otro :: Juego

par = repite 2
pierna = repite 3
color (carta : cartas) = all ((== snd carta) . snd) cartas
fullHouse cartas = par cartas && pierna cartas
poker = repite 4
otro _ = True

repite :: Number -> [Carta] -> Bool
repite cantidad cartas = any condicion numeros
  where
    numeros           = map fst cartas
    condicion numero  = ocurrenciasDe numero numeros == cantidad