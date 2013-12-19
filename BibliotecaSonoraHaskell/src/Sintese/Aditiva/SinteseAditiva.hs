module Sintese.Aditiva.SinteseAditiva where

import           Data.ByteString.Lazy.Builder
import qualified Data.ByteString.Lazy as B
import GHC.Int

import Sintese.Aditiva.Typedefs
import Definicoes.Audio
--lengthInFloat------------------------------------------------
lf' :: [a] -> Float -> Float
lf' [] cont = 0
lf' (x:xs) cont = 1 + (lf' (xs) cont)

lengthInFloat (x:xs) = lf' (x:xs) 0

--tailNoEmpty---------------------------------
tailNoEmpty :: [a] -> [a]
tailNoEmpty [] = []
tailNoEmpty (x:xs) = xs

--oscilador-------------------------------------------------------
oscilador' :: Frequencia -> Amplitude -> Duracao -> Harmonico -> Qualidade -> Float -> [Amostra]
oscilador' freq amp dur har (txam, res) x 
        |x >= dur = []
        |otherwise = (truncate((amp*(2**(fromInteger(res) - 1.14)))*sin(2*pi*freq*x*(2**(har-1)))):oscilador' freq amp dur har (txam, res) (x + (1.0/txam))) 

oscilador :: (Frequencia, Amplitude, Duracao) -> Harmonico -> Qualidade -> Oscilador
oscilador (freq, amp, dur) har (txam, res) = Oscilador (oscilador' freq amp dur har (txam, res) 0) 

--Envoltoria-------------------------------------------------------------------------------------------------------------------
--equacao:  y = ((y1-y2)/(x1-x2))*x + (y2 - ((y1-y2)/(x1-x2))*x2)
encontraPonto a b x = a*x + b

geraReta' (x1,y1) (x2, y2) numPontos cont 
        |cont <= x2 = (encontraPonto ((y1-y2)/(x1-x2)) (y2 - ((y1-y2)/(x1-x2))*x2) cont):(geraReta' (x1,y1) (x2, y2) numPontos (cont + (x2-x1)/((numPontos*(x2-x1) - 1))))
        |otherwise = []

geraReta ((x1,y1)) ((x2, y2)) numPontos 
        |(x1>1 || x2>2 || y1>1 || y2>1) = [] -- função nao definida para maiores de 1. Corta
        |otherwise = geraReta' (x1,y1) (x2, y2) numPontos x1

geraEnvoltoria' :: [Ponto] -> Oscilador -> [Float]
geraEnvoltoria' [parXY]        (Oscilador wave) = []
geraEnvoltoria' ((parXY):parXYs) (Oscilador wave) = (geraReta (parXY) (head(parXYs)) (lengthInFloat(wave))) ++ (geraEnvoltoria' parXYs (Oscilador wave))

geraEnvoltoria :: [Ponto] -> Oscilador -> Envoltoria
geraEnvoltoria (parXY:parXYs) (Oscilador wave) = (geraEnvoltoria' (parXY:parXYs) (Oscilador wave))

aplicaEnvoltoria :: Envoltoria -> Oscilador -> [Int16]
aplicaEnvoltoria [] (Oscilador []) = []
aplicaEnvoltoria []  _ = []
aplicaEnvoltoria _  (Oscilador []) = []
aplicaEnvoltoria (e:es) (Oscilador (w:ws)) = (truncate(e*(fromIntegral w))):(aplicaEnvoltoria  es (Oscilador ws))

--unidadeHarmonica---------------------------------------------------------------------------------------
unidadeHarmonica :: Harmonico -> [Ponto] -> (Frequencia, Amplitude, Duracao) -> Qualidade -> UnidadeHarmonica
unidadeHarmonica har env (freq, amp, dur) (txam, res) = let osc = (oscilador (freq, amp, dur) har (txam, res))
                                                    in UnidadeHWave (aplicaEnvoltoria (geraEnvoltoria env osc) osc)

--Instrumento Aditivo------------------------------------------------------------------------------------
somaLista :: UnidadeHarmonica -> [Int16] -> [Int16]
somaLista (UnidadeHWave []) []     = []
somaLista (UnidadeHWave []) (y:ys) = (y:ys)
somaLista (UnidadeHWave (x:xs)) [] = (x:xs)
somaLista (UnidadeHWave (x:xs)) (y:ys) = (x+y):(somaLista (UnidadeHWave xs) ys)

instrumentoAditivo' :: [ParametrosUnidadeH] -> Frequencia -> Amplitude -> Duracao -> Qualidade -> [Int16] -> [Int16]
instrumentoAditivo' [] freq amp dur (txam, res) listaFormada = listaFormada
instrumentoAditivo' ((har, env, ampUH):uHs) freq amp dur (txam, res) listaFormada = 
                      instrumentoAditivo' uHs freq amp dur (txam, res) (somaLista (unidadeHarmonica har env (freq, (amp*ampUH), dur) (txam, res)) listaFormada)

instrumentoAditivo :: [ParametrosUnidadeH] -> (Frequencia, Amplitude, Duracao) -> Qualidade -> InstrumentoAditivo
instrumentoAditivo (uH:uHs) (freq, amp, dur) (txam, res) = InstAdd (instrumentoAditivo' (uH:uHs) freq amp dur (txam, res) [])

--ARQUIVOS--------------------------------------------------------------------------------------------------------- 
pack16 [] = toLazyByteString(int16BE 0)
pack16 (l:ls) = B.append (toLazyByteString (int16BE l)) (pack16 ls)

salvaWave16 :: (Wave a) => [Char] -> a -> IO()
salvaWave16 x wave = B.writeFile x $ pack16 (toWave(wave))