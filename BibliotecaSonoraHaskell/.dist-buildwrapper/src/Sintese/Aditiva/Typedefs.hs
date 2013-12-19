module Sintese.Aditiva.Typedefs where

import Definicoes.Audio

data Oscilador = Oscilador [Amostra]
data UnidadeHarmonica = UnidadeHWave [Amostra]
data InstrumentoAditivo = InstAdd [Amostra]

somaWaves :: [Amostra] -> [Amostra] -> [Amostra]
somaWaves _ [] =[]
somaWaves  [] _ = []
somaWaves (w1:w1s) (w2:w2s)  = (w1+w2):(somaWaves w1s w2s )


class Wave a where
        (<+>) :: a -> a -> a
        toWave :: a -> [Amostra]
        (|+|) :: a -> a -> a
        
instance Wave Oscilador where
        Oscilador x <+> Oscilador y = Oscilador (x ++ y)
        toWave (Oscilador x) = x
        Oscilador x |+| Oscilador y = Oscilador (somaWaves x y)

instance Wave UnidadeHarmonica where
        UnidadeHWave x <+> UnidadeHWave y = UnidadeHWave (x ++ y)
        toWave (UnidadeHWave x) = x
        UnidadeHWave x |+| UnidadeHWave y = UnidadeHWave (somaWaves x y)

instance Wave InstrumentoAditivo where
        InstAdd x <+> InstAdd y = InstAdd (x ++ y)
        toWave (InstAdd x) = x
        InstAdd x |+| InstAdd y = InstAdd (somaWaves x y)

