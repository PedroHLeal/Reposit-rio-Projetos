module SinteseMusical.SinteseMusical where

import Definicoes.Frequencias
import Definicoes.Audio
import Biblioteca.Typedefs
import Sintese.Aditiva

getFrequencia :: Cifra -> Frequencia
getFrequencia c 
        |c == C0 = c0 | c == Cs0 = cs0 | c == D0 = d0 | c == Ds0 = ds0 | c == E0 = e0 | c == F0 = f0 | c == Fs0 = fs0 | c == G0 = g0 | c == Gs0 = gs0 | c == A0 = a0 | c == As0 = as0 | c == B0 = b0 
        |c == C1 = c1 | c == Cs1 = cs1 | c == D1 = d1 | c == Ds1 = ds1 | c == E1 = e1 | c == F1 = f1 | c == Fs1 = fs1 | c == G1 = g1 | c == Gs1 = gs1 | c == A1 = a1 | c == As1 = as1 | c == B1 = b1
        |c == C2 = c2 | c == Cs2 = cs2 | c == D2 = d2 | c == Ds2 = ds2 | c == E2 = e2 | c == F2 = f2 | c == Fs2 = fs2 | c == G2 = g2 | c == Gs2 = gs2 | c == A2 = a2 | c == As2 = as2 | c == B2 = b2
        |c == C3 = c3 | c == Cs3 = cs3 | c == D3 = d3 | c == Ds3 = ds3 | c == E3 = e3 | c == F3 = f3 | c == Fs3 = fs3 | c == G3 = g3 | c == Gs3 = gs3 | c == A3 = a3 | c == As3 = as3 | c == B3 = b3
        |c == C4 = c4 | c == Cs4 = cs4 | c == D4 = d4 | c == Ds4 = ds4 | c == E4 = e4 | c == F4 = f4 | c == Fs4 = fs4 | c == G4 = g4 | c == Gs4 = gs4 | c == A4 = a4 | c == As4 = as4 | c == B4 = b4
        |c == C5 = c5 | c == Cs5 = cs5 | c == D5 = d5 | c == Ds5 = ds5 | c == E5 = e5 | c == F5 = f5 | c == Fs5 = fs5 | c == G5 = g5 | c == Gs5 = gs5 | c == A5 = a5 | c == As5 = as5 | c == B5 = b5

getDuracao :: Tempo -> BPM -> Duracao
getDuracao t bpm
        |t == B = 4.0*60/bpm |t == Sb = 2*60/bpm | t == M = 1*60/bpm | t == Sm = 1/2*60/bpm 
        |t == Col = 1/4*60/bpm | t == Sc = 1/8*60/bpm | t == Fu = 1/16*60/bpm | t == Sf = 1/32*60/bpm 
         
getAmplitude :: Intensidade -> Amplitude
getAmplitude i 
        |i == PPP = 0.125 | i == PP = 0.25 | i == P = 0.375 | i == Mp = 0.5 | i == Mf = 0.625 | i == F = 0.75 | i == FF = 0.875 | i == FFF = 1 
        |i == Zero = 0
getPausa :: Pausa -> Nota
getPausa p 
        |p == PB = (A2, Zero , B)
        |p == PSb = (A2, Zero , Sb)
        |p == PM = (A2, Zero , M)
        |p == PSm = (A2, Zero , Sm)
        |p == PCol = (A2, Zero , Col)
        |p == PSc = (A2, Zero , Sc)
        |p == PFu = (A2, Zero , Fu)
        |p == PSf = (A2, Zero , Sf)
        
        
executaNota :: Wave a => Nota -> Qualidade -> ((Frequencia, Amplitude, Duracao) -> Qualidade -> a) -> BPM -> a
executaNota (c, i, t) q inst bpm = inst ((getFrequencia c), (getAmplitude i), (getDuracao t bpm)) q

executaMelodia :: Wave a => [Nota] -> Qualidade -> ((Frequencia, Amplitude, Duracao) -> Qualidade -> a) -> BPM -> a
executaMelodia [] _ _ _ = undefined
executaMelodia [n] q inst bpm = executaNota n q inst bpm
executaMelodia (n:ns) q inst bpm = (executaNota n q inst bpm) <+> (executaMelodia ns q inst bpm)
      
executaAcorde :: Wave a => Acorde -> Qualidade -> ((Frequencia, Amplitude, Duracao) -> Qualidade -> a) -> BPM -> a
executaAcorde ([c] , i, t) q inst bpm = (executaNota (c, i, t) q inst bpm)
executaAcorde ((c:cs), i, t) q inst bpm = (executaNota (c, i, t) q inst bpm) |+| (executaAcorde (cs, i, t) q inst bpm)
        
executaHarmonia :: Wave a => Harmonia -> Qualidade -> ((Frequencia, Amplitude, Duracao) -> Qualidade -> a) -> BPM -> a
executaHarmonia [x] q inst bpm = executaAcorde x q inst bpm
executaHarmonia ((a, i , d):hs) q inst bpm = (executaAcorde (a, i, d) q inst bpm) <+> (executaHarmonia hs q inst bpm)

executaMusica :: Wave a => [(BPM -> Qualidade -> a)] -> BPM -> Qualidade -> a
executaMusica [e] bpm q = e bpm q
executaMusica (e:es) bpm q = (e bpm q) |+| (executaMusica es bpm q) 
