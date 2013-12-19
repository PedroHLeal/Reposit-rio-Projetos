module Main where

import Definicoes.Frequencias
import Sintese.Aditiva
import Biblioteca.Typedefs
import Biblioteca.Musica
import Definicoes.Audio
import SinteseMusical.SinteseMusical

inst1 :: (Frequencia, Amplitude, Duracao) -> Qualidade -> InstrumentoAditivo
inst1 (f, a, d) qualidade = instrumentoAditivo [
                                                (-2, [(0,0),(0.001,1),(1,0)], 0.2),
                                                (-1, [(0,0),(0.001,1),(0.7, 0.7), (1,0)], 0.09),
                                                (1, [(0,0),(0.03, 1), (0.3, 0.2), (1,0)], 1),
                                                (3, [(0, 0), (0.01, 1), (1, 0)], 0.008),
                                                (4, [(0 ,0), (0.001, 1), (0.02, 0.5), (0.3, 1), (1, 0)], 0.003), 
                                                (7, [(0, 0), (0.001, 1), (1, 0)], 0.001)]
                                                (f,a*0.40,d) qualidade 

maoEsquerda_Intro :: BPM -> Qualidade -> InstrumentoAditivo
maoEsquerda_Intro bpm q = executaHarmonia [(acorde As1 Maior, F, B), (acorde As1 Maior, F, B), (acorde Ds2 Maior, F, B),
                                    (acorde D2 Maior , F, B) , (acorde G1 Menor, F, B) , (acorde C2 Maior, F, B),
                                    (acorde F2 Maior, F, B) ] q inst1 bpm 

maoDireita_Intro :: BPM -> Qualidade -> InstrumentoAditivo
maoDireita_Intro bpm q = executaMelodia [(A3, FFF, Sb), (As3, F, M), (Gs3, FFF, Sb),
                                         (As3, FFF, M), (F3, Mp, M), (D3, F, M), getPausa PSb,
                                         (G3, F, M),(Fs3, P, Sb), getPausa PM, (Fs3, F, M),
                                         (F3, F, Sb), getPausa PM,(F3, F, M),(E3, P, Sb), getPausa PM,
                                         (E3, F, M),(Ds3, P, Sb)] q inst1 bpm



main :: IO()
main = salvaWave16 "teste2.raw" $ executaMusica [maoEsquerda_Intro, maoDireita_Intro] 170 cd