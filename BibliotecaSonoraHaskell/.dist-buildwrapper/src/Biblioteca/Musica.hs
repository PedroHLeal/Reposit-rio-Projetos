module Biblioteca.Musica where

import Biblioteca.Typedefs

escalaMaior :: Cifra -> [Cifra]
escalaMaior c = [c, i2M c, i3M c, i4J c, i5J c, i6M c, i7M c]

escalaMenor :: Cifra -> [Cifra]
escalaMenor c = [c, i2M c, i3m c, i4J c, i5J c, i6m c, i7m c]

acorde :: Cifra -> TipoAcorde -> [Cifra]
acorde c tipo 
        |tipo == Maior = [c, i3M c, i5J c]
        |tipo == Menor = [c, i3m c, i5J c]
        |otherwise = undefined
                
insereNota :: Nota -> Melodia -> Melodia
insereNota n mel = mel ++ [n] 