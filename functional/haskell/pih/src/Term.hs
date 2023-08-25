module Term () where

-- Reference: man 5 terminfo, man 1 tput
type Assoc k v = [(k, v)]

data BoolCaps
    = BW
    | AM
    | CUF
    | CUB
    | CNL
    | CPL
    | CHA
    | CUP
    | ED
    | EL
    | SU
    | SD
    | HVP
    | SGR
    | AUXON
    | AUXOFF
    | DSR
    | SCP
    | RCP
    | DECTCEM
    deriving (Eq, Show)

data NumCaps
    = BoldOn
    | BoldOff
    deriving (Eq, Show)

data Capabilities
    = CBool BoolCaps
    | CNums NumCaps
    deriving (Eq, Show)

type Table = Assoc Capabilities String
