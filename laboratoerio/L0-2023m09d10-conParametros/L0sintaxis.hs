module L0sintaxis
    (Prog(..)
    ,Var(..), VarList(..)
    ,Val, Val4(..)
    ,Exp(..)
    ,Stm(..), StmList(..)
    ,toStmList
    )
--
-- Sintaxis de L0
--
--mcb
where
-- Modules already defined in Haskell: ------------------------------
-- import Data.Set as S 
-- import Data.List as L
--
-- Modules defined in this project: ---------------------------------
--import Variables (Variable)
--
-- Sintaxis de L0 -------------------------------------------------
--
--

--Tipos de datos para programas de L0:


-- Valores (un solo dominio para todas las variables)
-- Posibles Valores: Val1, o Val4, o ...:
--
data Val1 -- Valores 0..1
    = B0 | B1
    deriving    (Eq,Show
                ,Enum       -- Permite hacer: [m..n]
                ,Bounded    -- Permite hacer: minBound :: Val1 y maxBound :: Val1
                )
--
data Val4 -- Valores 0..3
    = D0 | D1 | D2 | D3
    deriving    (Eq,Show
                ,Enum       -- Permite hacer: [m..n]
                ,Bounded    -- Permite hacer: minBound :: Val4 y maxBound :: Val4
                )
--
type Val = Val4

-- Variables
data Var
    = Vx | Vy | Vz
    deriving (Eq,Show,Enum)

-- Listas de variables
data VarList
    =  VLvar Var
    |  VLlv Var VarList
    deriving (Eq,Show)

-- Expresiones
data Exp b -- b es un parámetro para el tipo de "valores" Por ejemplo: b = Val4
    = Evalor b | Evar Var
    deriving (Eq,Show)

-- Instrucciones (statements)
data Stm b -- b es un parámetro para el tipo de "valores" Por ejemplo: b = Val4
    = Sasig Var (Exp b)
    | Shalt
    deriving (Eq,Show)

-- Listas de instrucciones
data StmList b  -- b es un parámetro para el tipo de "valores" Por ejemplo: b = Val4
    = SLstm (Stm b)
    | SLls  (Stm b) (StmList b)
    deriving (Eq,Show)

-- Programas de L0
data Prog b  -- b es un parámetro para el tipo de "valores" Por ejemplo: b = Val4
    = Prog (VarList, StmList b)
    deriving (Eq,Show)


toStmList :: [Stm b] -> StmList b
-- Transform a list of statements into a StmList.
toStmList ls
    = case ls of
           [s]      -> SLstm s
           s:ls'    -> SLls s (toStmList ls')
           []       -> SLstm Shalt
--
-----------------------------------------------------

-- Tests: VER L0tests.hs
-- VER mas Tests en el archivo: sesionHaskell-2023m09d10.txt (omitiendo el comando de carga :l )


-- Ejercicios:
-- 1. Definir una funcion recursiva, lvSize, que calcule la longitud de una lista de variables.
-- 2. Definir una funcion recursiva, lsSize, que calcule la longitud de una lista de instrucciones.
-- 3. Definir una funcion, progSize, que calcule el tamaño de un programa (suma de las longitudes de listas que lo componen).
-- 4. Definir un programa L0, pIntercambiaxy,s que intercambie el valor de dos variables (x,y).
--
