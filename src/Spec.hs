module Spec where
import PdePreludat
import Library
import Test.Hspec

-- Funciones auxiliares para los tests
nombreMasLargo :: Jugador -> Jugador -> Jugador
nombreMasLargo j1 j2 = mayorSegun (length . nombre) j1 j2

manoMasGrande :: Jugador -> Jugador -> Jugador
manoMasGrande j1 j2 = mayorSegun (length . mano) j1 j2

jugadorNombreLargo :: [Jugador] -> Jugador
jugadorNombreLargo jueces = maximoSegun (length . nombre) jueces

palosUnicos :: [Carta] -> [Palo]
palosUnicos mano = sinRepetidos (map snd mano)

sonTodasCartasValidas :: [Carta] -> Bool
sonTodasCartasValidas cartas = all (not . esoNoSeVale) cartas

correrTests :: IO ()
correrTests = hspec $ do
  describe "1.a - mayorSegun" $ do
    it "Retorna el elemento con el mayor valor según la función" $ do
      mayorSegun id 5 3 `shouldBe` 5
    it "Retorna el primer elemento si son iguales según la función" $ do
      mayorSegun id 5 5 `shouldBe` 5
    it "Retorna el elemento con la mayor longitud de string" $ do
      mayorSegun length "hola" "ai" `shouldBe` "hola"
    it "Retorna el jugador con el nombre más largo" $ do
      nombre (nombreMasLargo jamesBond leChiffre) `shouldBe` "Bond... James Bond"
    it "Retorna el jugador con la mano más grande" $ do
      nombre (manoMasGrande jamesBond felixLeiter) `shouldBe` "Felix Leiter"

  describe "1.b - maximoSegun" $ do
    it "Retorna el máximo elemento según la función en una lista" $ do
      maximoSegun id [3, 1, 4, 1, 5] `shouldBe` 5
    it "Retorna el string más largo" $ do
      maximoSegun length ["hola", "ai", "mundo", "ok"] `shouldBe` "mundo"
    it "Retorna el jugador con el nombre más largo de la mesa" $ do
      nombre (jugadorNombreLargo mesaQueMasAplauda) `shouldBe` "Bond... James Bond"
    it "Retorna la carta con el número más alto de pokerDeAses" $ do
      maximoSegun fst pokerDeAses `shouldBe` (10, "Diamantes")
    it "Retorna la carta con el número más alto de fullDeJokers" $ do
      maximoSegun fst fullDeJokers `shouldBe` (11, "Tréboles")

  describe "1.c - sinRepetidos" $ do
    it "Retorna una lista sin elementos repetidos" $ do
      sinRepetidos [1, 2, 2, 3, 3, 3] `shouldBe` [1, 2, 3]
    it "Retorna la misma lista si no hay repetidos" $ do
      sinRepetidos [1, 2, 3, 4] `shouldBe` [1, 2, 3, 4]
    it "Retorna una lista vacía si recibe una lista vacía" $ do
      sinRepetidos ([] :: [Number]) `shouldBe` []
    it "Retorna una lista con un solo elemento si todos son iguales" $ do
      sinRepetidos [5, 5, 5, 5] `shouldBe` [5]
    it "Retorna los palos únicos de pokerDeAses" $ do
      length (palosUnicos pokerDeAses) `shouldBe` 4

  describe "2.a - esoNoSeVale" $ do
    it "Una carta válida no satisface esoNoSeVale" $ do
      (5, "Corazones") `shouldNotSatisfy` esoNoSeVale
    it "Un As (1) no satisface esoNoSeVale" $ do
      (1, "Picas") `shouldNotSatisfy` esoNoSeVale
    it "Un Rey (13) no satisface esoNoSeVale" $ do
      (13, "Tréboles") `shouldNotSatisfy` esoNoSeVale
    it "Un número fuera del rango válido satisface esoNoSeVale" $ do
      (14, "Corazones") `shouldSatisfy` esoNoSeVale
    it "Un palo inválido satisface esoNoSeVale" $ do
      (5, "Bastos") `shouldSatisfy` esoNoSeVale
    it "El número 0 satisface esoNoSeVale" $ do
      (0, "Corazones") `shouldSatisfy` esoNoSeVale
    it "Todas las cartas de pokerDeAses son válidas" $ do
      pokerDeAses `shouldSatisfy` sonTodasCartasValidas

  describe "2.b - manoMalArmada" $ do
    it "Una mano válida de 5 cartas no satisface manoMalArmada" $ do
      [(1, "Corazones"), (2, "Picas"), (3, "Tréboles"), (4, "Diamantes"), (5, "Corazones")] `shouldNotSatisfy` manoMalArmada
    it "Una mano con menos de 5 cartas satisface manoMalArmada" $ do
      [(1, "Corazones"), (2, "Picas")] `shouldSatisfy` manoMalArmada
    it "Una mano con más de 5 cartas satisface manoMalArmada" $ do
      [(1, "Corazones"), (2, "Picas"), (3, "Tréboles"), (4, "Diamantes"), (5, "Corazones"), (6, "Picas")] `shouldSatisfy` manoMalArmada
    it "Una mano con cartas inválidas satisface manoMalArmada" $ do
      [(1, "Corazones"), (2, "Picas"), (3, "Tréboles"), (14, "Diamantes"), (5, "Corazones")] `shouldSatisfy` manoMalArmada
    it "pokerDeAses no satisface manoMalArmada" $ do
      pokerDeAses `shouldNotSatisfy` manoMalArmada
    it "fullDeJokers no satisface manoMalArmada" $ do
      fullDeJokers `shouldNotSatisfy` manoMalArmada

  describe "3 - Tipos de Juegos" $ do
    describe "par" $ do
      it "Una mano con un par satisface par" $ do
        [(5, "Corazones"), (5, "Picas"), (3, "Tréboles"), (4, "Diamantes"), (7, "Corazones")] `shouldSatisfy` par
      it "Una mano sin par no satisface par" $ do
        [(1, "Corazones"), (2, "Picas"), (3, "Tréboles"), (4, "Diamantes"), (5, "Corazones")] `shouldNotSatisfy` par

    describe "pierna" $ do
      it "Una mano con una pierna satisface pierna" $ do
        [(5, "Corazones"), (5, "Picas"), (5, "Tréboles"), (4, "Diamantes"), (7, "Corazones")] `shouldSatisfy` pierna
      it "Una mano sin pierna no satisface pierna" $ do
        [(1, "Corazones"), (2, "Picas"), (3, "Tréboles"), (4, "Diamantes"), (5, "Corazones")] `shouldNotSatisfy` pierna

    describe "color" $ do
      it "Una mano con todas las cartas del mismo palo satisface color" $ do
        [(1, "Corazones"), (2, "Corazones"), (3, "Corazones"), (4, "Corazones"), (5, "Corazones")] `shouldSatisfy` color
      it "Una mano con cartas de diferentes palos no satisface color" $ do
        [(1, "Corazones"), (2, "Picas"), (3, "Tréboles"), (4, "Diamantes"), (5, "Corazones")] `shouldNotSatisfy` color

    describe "fullHouse" $ do
      it "Una mano con par y pierna satisface fullHouse" $ do
        [(5, "Corazones"), (5, "Picas"), (5, "Tréboles"), (4, "Diamantes"), (4, "Corazones")] `shouldSatisfy` fullHouse
      it "fullDeJokers satisface fullHouse" $ do
        fullDeJokers `shouldSatisfy` fullHouse
      it "Una mano con pierna pero sin par no satisface fullHouse" $ do
        [(5, "Corazones"), (5, "Picas"), (5, "Tréboles"), (3, "Diamantes"), (7, "Corazones")] `shouldNotSatisfy` fullHouse
      it "Una mano con par pero sin pierna no satisface fullHouse" $ do
        [(5, "Corazones"), (5, "Picas"), (3, "Tréboles"), (4, "Diamantes"), (7, "Corazones")] `shouldNotSatisfy` fullHouse
      it "Una mano sin par ni pierna no satisface fullHouse" $ do
        [(1, "Corazones"), (2, "Picas"), (3, "Tréboles"), (4, "Diamantes"), (5, "Corazones")] `shouldNotSatisfy` fullHouse

    describe "poker" $ do
      it "Una mano con 4 cartas del mismo número satisface poker" $ do
        [(5, "Corazones"), (5, "Picas"), (5, "Tréboles"), (5, "Diamantes"), (7, "Corazones")] `shouldSatisfy` poker
      it "pokerDeAses satisface poker" $ do
        pokerDeAses `shouldSatisfy` poker
      it "Una mano sin 4 cartas del mismo número no satisface poker" $ do
        [(1, "Corazones"), (2, "Picas"), (3, "Tréboles"), (4, "Diamantes"), (5, "Corazones")] `shouldNotSatisfy` poker

    describe "otro" $ do
      it "Cualquier mano satisface otro" $ do
        [(1, "Corazones"), (2, "Picas"), (3, "Tréboles"), (4, "Diamantes"), (5, "Corazones")] `shouldSatisfy` otro