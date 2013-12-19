module Definicoes.Audio where

import GHC.Int

-- Definicoes

type Amostra = Int16
type Frequencia = Float
type Amplitude = Float
type Resolucao = Integer
type Duracao = Float
type Harmonico = Float
type TaxaAmostragem = Float
type Ponto = (Float, Float)
type ParametrosUnidadeH = (Harmonico, [Ponto], Amplitude)
type Envoltoria = [Float]

type Qualidade = (TaxaAmostragem, Resolucao)

cd :: Qualidade
cd = (44100.0, 16)
radioAM :: Qualidade
radioAM = (22050.0, 16)
radioFM :: Qualidade
radioFM = (32000.0, 16)
telefone :: Qualidade
telefone = (8000.0, 16)
