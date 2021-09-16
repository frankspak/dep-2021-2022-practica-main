{-|
    Module      : Lib
    Description : Checkpoint voor V2DEP: recursie en lijsten
    Copyright   : (c) Brian van de Bijl, 2020
    License     : BSD3
    Maintainer  : nick.roumimper@hu.nl

    In dit practicum oefenen we met het schrijven van simpele functies in Haskell.
    Specifiek leren we hoe je recursie en pattern matching kunt gebruiken om een functie op te bouwen.
    LET OP: Hoewel al deze functies makkelijker kunnen worden geschreven met hogere-orde functies,
    is het hier nog niet de bedoeling om die te gebruiken.
    Hogere-orde functies behandelen we verderop in het vak; voor alle volgende practica mag je deze
    wel gebruiken.
    In het onderstaande commentaar betekent het symbool ~> "geeft resultaat terug";
    bijvoorbeeld, 3 + 2 ~> 5 betekent "het uitvoeren van 3 + 2 geeft het resultaat 5 terug".
-}

-- bronnen: https://www.youtube.com/watch?v=02_H3LjqMr8&list=PLhx8v4mr6oU_nAJcsSWe4qrHeON7vG8Ov&index=7&t=1s&ab_channel=DerekBanas
-- stackoverflow.com
-- https://hackage.haskell.org/package/CheatSheet-1.11/src/CheatSheet.pdf

module Lib
    ( ex1, ex2, ex3, ex4, ex5, ex6, ex7
    ) where

-- TODO: Schrijf en documenteer de functie ex1, die de som van een lijst getallen berekent.
-- Voorbeeld: ex1 [3,1,4,1,5] ~> 14
ex1 :: [Int] -> Int
ex1 [] = 0
ex1 (x:xs)= x + ex1 xs

-- bij ex1 is de som van de hele lijst waardoor alles uit komt op 14

-- TODO: Schrijf en documenteer de functie ex2, die alle elementen van een lijst met 1 ophoogt.
-- Voorbeeld: ex2 [3,1,4,1,5] ~> [4,2,5,2,6]
ex2 :: [Int] -> [Int]
ex2 (xs) = [x + 1| x <- xs]

-- bij ex2 wordt er over de lijst geitereerd en steeds elk element in de lijst met +1 gedaan.

-- TODO: Schrijf en documenteer de functie ex3, die alle elementen van een lijst met -1 vermenigvuldigt.
-- Voorbeeld: ex3 [3,1,4,1,5] ~> [-3,-1,-4,-1,-5]
ex3 :: [Int] -> [Int]
ex3 (xs) = [x * (-1)| x <- xs]

-- bij ex3 gebeurt het zelfde als hier boven maar dan wordt elk element in de lijst vermenigdvuldig met -1

-- TODO: Schrijf en documenteer de functie ex4, die twee lijsten aan elkaar plakt.
-- Voorbeeld: ex4 [3,1,4] [1,5] ~> [3,1,4,1,5]
-- Maak hierbij geen gebruik van de standaard-functies, maar los het probleem zelf met (expliciete) recursie op. 
-- Hint: je hoeft maar door een van beide lijsten heen te lopen met recursie.
ex4 :: [Int] -> [Int] -> [Int]
ex4 xs [] = xs
ex4 [] ys = ys
ex4 xs ys = head xs : ex4 (tail xs) ys

-- head geeft het eerste element van de lijst terug
-- tail geeft het laatste element van de lijst terug
-- begint dus bij het begin van de lijst xs en dan doormiddlen van : voegt die vanaf het laatste element van xs de lijst ys toe

-- TODO: Schrijf en documenteer een functie, ex5, die twee lijsten van gelijke lengte paarsgewijs bij elkaar optelt.
-- Voorbeeld: ex5 [3,1,4] [1,5,9] ~> [4,6,13]
ex5 :: [Int] -> [Int] -> [Int]
ex5 xs [] = xs
ex5 [] ys = ys
ex5 (x:xs) (y:ys) = (x + y) : ex5 xs ys

-- de functie loop met recursie over de lijsten en voegt dan elke index bij elkaar dus xs[0] + ys[0]

-- TODO: Schrijf en documenteer een functie, ex6, die twee lijsten van gelijke lengte paarsgewijs met elkaar vermenigvuldigt.
-- Voorbeeld: ex6 [3,1,4] [1,5,9] ~> [3,5,36] 
ex6 :: [Int] -> [Int] -> [Int]
ex6 xs [] = xs
ex6 [] ys = ys
ex6 (x:xs) (y:ys) = (x * y) : ex6 xs ys

-- deze functie doet hetzelfde als hierboven alleen dan vermenigdvuldigen deze elkaar.

-- TODO: Schrijf en documenteer een functie, ex7, die de functies ex1 en ex6 combineert tot een functie die het inwendig product uitrekent.
-- Voorbeeld: ex7 [3,1,4] [1,5,9] geeft 3*1 + 1*5 + 4*9 = 44 terug als resultaat.
ex7 :: [Int] -> [Int] -> Int
ex7 x y = ex1 $ ex6 x y

-- $ is een infix operator (bewerkings teken) dat de twee functie koppelt en dan deze functies over x en y uitvoert
