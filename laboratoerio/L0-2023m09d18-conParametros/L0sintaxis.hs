module L0sintaxis
    (Prog(..)
    ,Var(..), VarList(..)
    ,Val, Val4(..), Val01(..)
    ,Exp(..)
    ,Stm(..), StmList(..)
    ,showStm, showVar, showVal4, showVal
    ,varListTOhaskell
    ,stmListTOhaskell, haskelListTOstmList
    ,showProg
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
-- Posibles Valores: Val01, o Val4, o ...:
--
data Val01 -- Valores 0..1
    = B0 | B1
    --deriving    (Eq,Show
    deriving    (Eq
                ,Enum       -- Permite hacer: [m..n]
                ,Bounded    -- Permite hacer: minBound :: Val01 y maxBound :: Val01
                )
--
instance Show Val01 where
    show = showVal01
--
showVal01 :: Val01 -> String
showVal01 b= case b of
                  B0    -> "0"
                  B1    -> "1"
--

data Val4 -- Valores 0..3
    = D0 | D1 | D2 | D3
    deriving    (Eq --(Show,Eq,
                ,Enum       -- Permite hacer: [m..n]
                ,Bounded    -- Permite hacer: minBound :: Val4 y maxBound :: Val4
                )
--
showVal4 :: Val4 -> String
-- Digitos de Val4 a string
showVal4 d = case d of
             D0     -> "0"
             D1     -> "1"
             D2     -> "2"
             D3     -> "3"
             -- _      -> error $ "showVal4: NO implementado aun para d= " ++ (show d)
--
instance Show Val4 where
    show = showVal4
--

type Val = Val4
--
showVal :: Val -> String
showVal = showVal4


-- Variables
data Var
    = Vx | Vy | Vz
    deriving (Eq,Enum) --(Show,Eq,Enum)
--
showVar :: Var -> [Char]
showVar v = case v of
                 Vx -> "x"
                 Vy -> "y"
                 Vz -> "z"
--
instance Show Var where
    show= showVar

-- Listas de variables
data VarList
    =  VLvar Var
    |  VLlv Var VarList
    deriving (Eq)   --(Eq,Show)
--


-- Expresiones
data Exp b -- b es un parámetro para el tipo de "valores" Por ejemplo: b = Val4
    = Evalor b
    | Evar Var
    deriving (Eq)   --(Eq,Show)
--
showExp :: (Show b) => Exp b -> String
showExp e = case e of
                 Evalor b   -> show b
                 Evar   v   -> show v
--
instance (Show b) => Show (Exp b) where
    show= showExp

-- Instrucciones (statements)
data Stm b -- b es un parámetro para el tipo de "valores" Por ejemplo: b = Val4
    = Sasig Var (Exp b)
    | Shalt
    deriving (Eq)   --(Eq,Show)
--
showStm :: (Show b) => Stm b -> String
showStm s = --show s -- XXX
    case s of
         Sasig v e  -> (show v)++ ":=" ++ (show e)
         Shalt      -> "Halt"
--
instance (Show b) => Show (Stm b) where
    show = showStm
--

-- Listas de instrucciones
data StmList b  -- b es un parámetro para el tipo de "valores" Por ejemplo: b = Val4
    = SLstm (Stm b)
    | SLls  (Stm b) (StmList b)
    deriving (Eq,Show)   --(Eq,Show)

-- Programas de L0
data Prog b  -- b es un parámetro para el tipo de "valores" Por ejemplo: b = Val4
    = Prog (VarList, StmList b)
    deriving (Eq)   --(Eq,Show)
--
instance (Show b) => Show (Prog b) where
    show = showProg
--
showProg :: (Show b) => Prog b  -> String
showProg (Prog (lv,ls)) =   -- XXX
        "L0-Program " ++ "NOMBRE XXX" ++ "\n"
    ++ ind1 ++ "VAR\n"
    ++      showVarList ind2 sep1 lv ++ "\n"
    ++ ind1 ++ "PROG\n"
    ++      showStmList ind2 sep1 ls ++ "\n"
    where
        ind1= "    " -- 4 espacios
        ind2= ind1++ind1
        sep1= ";\n"
--
showVarList :: String -> String -> VarList -> String
showVarList ind sep lv =
    case lv of
         VLvar  v       -> ind ++ showVar v
         VLlv v lv'     -> ind ++ showVar v ++ sep
                               ++ showVarList ind sep lv'
--
instance Show VarList where
    show= showVarList "" ","
--

varListTOhaskell :: VarList -> [Var]
varListTOhaskell lv =
    case lv of
         VLvar  v       -> [v]
         VLlv v lv'     -> v : (varListTOhaskell lv')
--

showStmList :: (Show b) => String -> String -> StmList b -> String
showStmList ind sep ls = --show lv -- XXX
    case ls of
         SLstm  s       -> ind ++ showStm s
         SLls s ls'     -> ind ++ showStm s ++ sep
                               ++ showStmList ind sep ls'
--


haskelListTOstmList :: [Stm b] -> StmList b
-- Transform a list of statements into a StmList.
haskelListTOstmList ls
    = case ls of
           [s]      -> SLstm s
           s:ls'    -> SLls s (haskelListTOstmList ls')
           []       -> error "XXX" --SLstm Shalt
--
stmListTOhaskell :: StmList b -> [Stm b]
-- Transform a StmList into a haskell list of Stm.
stmListTOhaskell ls
    = case ls of
           SLstm s      -> [s]
           SLls s ls'   -> s: (stmListTOhaskell ls')
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
