module Biblioteca.Typedefs where

data Cifra = C0|C1|C2|C3|C4|C5|
             Cs0|Cs1|Cs2|Cs3|Cs4|Cs5|
             D0|D1|D2|D3|D4|D5|
             Ds0|Ds1|Ds2|Ds3|Ds4|Ds5|
             E0|E1|E2|E3|E4|E5|
             F0|F1|F2|F3|F4|F5|
             Fs0|Fs1|Fs2|Fs3|Fs4|Fs5|
             G0|G1|G2|G3|G4|G5|
             Gs0|Gs1|Gs2|Gs3|Gs4|Gs5|
             A0|A1|A2|A3|A4|A5|
             As0|As1|As2|As3|As4|As5|
             B0|B1|B2|B3|B4|B5 deriving (Show, Eq ,Ord )
                   
data Tempo = B | Sb | M | Sm | Col | Sc | Fu | Sf deriving (Show, Eq)
data Intensidade = PPP | PP | P | Mp | Mf | F | FF | FFF | Zero deriving (Show, Eq)
type Nota = (Cifra, Intensidade, Tempo)
data Pausa = PB | PSb | PM | PSm | PCol | PSc | PFu | PSf deriving (Show, Eq)

type BPM = Float
type Melodia = [Nota]
type Musica = [Melodia]
type Harmonia = [Acorde]
type Acorde = ([Cifra], Intensidade, Tempo)
type Escala = [Cifra]

data TipoAcorde = Maior | Menor | SetimaMaior deriving (Show, Eq)
data Intervalo = T | I2m | I2M | I3m | I3M | I4J | I4Aum | I5J | I6m | I6M | I7m | I7M  deriving (Show, Eq)

escalaCromatica :: [Cifra]
escalaCromatica = [C0, Cs0, D0, Ds0, E0, F0, Fs0, G0, Gs0, A0, As0, B0,
                   C1, Cs1, D1, Ds1, E1, F1, Fs1, G1, Gs1, A1, As1, B1,
                   C2, Cs2, D2, Ds2, E2, F2, Fs2, G2, Gs2, A2, As2, B2,
                   C3, Cs3, D3, Ds3, E3, F3, Fs3, G3, Gs3, A3, As3, B3,
                   C4, Cs4, D4, Ds4, E4, F4, Fs4, G4, Gs4, A4, As4, B4,
                   C5, Cs5, D5, Ds5, E5, F5, Fs5, G5, Gs5, A5, As5, B5]

i2m :: Cifra -> Cifra
i2m c = dropWhile (/= c) escalaCromatica !! 1

i2M :: Cifra -> Cifra
i2M c = dropWhile (/= c) escalaCromatica !! 2

i3m :: Cifra -> Cifra
i3m c = dropWhile (/= c) escalaCromatica !! 3

i3M :: Cifra -> Cifra
i3M c = dropWhile (/= c) escalaCromatica !! 4

i4J :: Cifra -> Cifra
i4J c = dropWhile (/= c) escalaCromatica !! 5

i4Aum :: Cifra -> Cifra
i4Aum c = dropWhile (/= c) escalaCromatica !! 6

i5J :: Cifra -> Cifra
i5J c = dropWhile (/= c) escalaCromatica !! 7

i6m :: Cifra -> Cifra
i6m c = dropWhile (/= c) escalaCromatica !! 8

i6M :: Cifra -> Cifra
i6M c = dropWhile (/= c) escalaCromatica !! 9

i7m :: Cifra -> Cifra
i7m c = dropWhile (/= c) escalaCromatica !! 10

i7M :: Cifra -> Cifra
i7M c = dropWhile (/= c) escalaCromatica !! 11

iTom :: Cifra -> Cifra
iTom c = dropWhile (/= c) escalaCromatica !! 2

iSemiTom :: Cifra -> Cifra
iSemiTom c = dropWhile (/= c) escalaCromatica !! 1





