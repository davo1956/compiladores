module L0sintaxis
--
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

data Var
    = Vx | Vy | Vz
    deriving (Eq,Show)

data VarList
    =  VLvar Var
    |  VLlv Var VarList
    deriving (Eq,Show)

data Dig
    = D0 | D1 | D2 | D3 | D4
    deriving (Eq,Show)

data Exp
    = Edig Dig | Evar Var
    deriving (Eq,Show)

data Stm
    = Sasig Var Exp
    | Shalt
    deriving (Eq,Show)

data StmList
    = SLstm Stm
    | SLlv  Stm StmList
    deriving (Eq,Show)

data Prog
    = Prog (VarList, StmList)
    deriving (Eq,Show)

----------------------------------------------------
--
showDig :: Dig -> String
-- Digitos a texto
showDig d = case d of
             D0     -> "0"
             D1     -> "1"
             D2     -> "2"
             _      -> error $ "showDig: NO implementado aun para d= " ++ (show d)
-- Test showDig:
-- *L0sintaxis> showDig D0
-- "0"
-- it :: String
--
--
-- Tests: ------------------------------------------
-- Prelude>
-- Prelude> -- Cargar sintaxis de L0: --------------
-- Prelude> :l L0sintaxis.hs
-- [1 of 1] Compiling L0sintaxis       ( L0sintaxis.hs, interpreted )
-- Ok, one module loaded.
-- *L0sintaxis>
-- *L0sintaxis> -- mostrar tipos:
-- *L0sintaxis> :set +t
-- *L0sintaxis>
-- *L0sintaxis> -- Definir variables: -------------
-- *L0sintaxis> x = Vx
-- x :: Var
-- *L0sintaxis> y = Vy
-- y :: Var
-- *L0sintaxis> z = Vz
-- z :: Var
-- *L0sintaxis>
-- *L0sintaxis> -- Definir expresiones:-------------
-- *L0sintaxis> e0 = Edig D0
-- e0 :: Exp
-- *L0sintaxis> e1 = Edig D1
-- e1 :: Exp
-- *L0sintaxis> e2 = Edig D2
-- e2 :: Exp
-- *L0sintaxis> e3 = Edig D3
-- e3 :: Exp
-- *L0sintaxis>
-- *L0sintaxis> ex= Evar x
-- ex :: Exp
-- *L0sintaxis> ey= Evar y
-- ey :: Exp
-- *L0sintaxis> ez= Evar z
-- ez :: Exp
-- *L0sintaxis>
-- *L0sintaxis> -- Instrucciones: -------------------
-- *L0sintaxis> asigx0= Sasig x e0    -- x:= 0
-- asigx0 :: Stm
-- *L0sintaxis> asigy1= Sasig y e1    -- y:= 1
-- asigy1 :: Stm
-- *L0sintaxis> asigzx= Sasig z ex    -- z:= x
-- asigzx :: Stm
-- *L0sintaxis> halt = Shalt  -- instruccion Halt
-- halt :: Stm
-- *L0sintaxis>
-- *L0sintaxis> -- Listas de variables: -------------
-- *L0sintaxis> lv0 = VLvar z -- lv0=[z]
-- lv0 :: VarList
-- *L0sintaxis> lv1 = VLlv y lv0 -- lv1= y:lv0 = y:[z] = [y,z]
-- lv1 :: VarList
-- *L0sintaxis> lv2 = VLlv x lv1 -- lv1= x:lv1 = z:[y,z] = [x,y,z]
-- lv2 :: VarList
-- *L0sintaxis>
-- *L0sintaxis> -- Listas de instrucciones: ----------
-- *L0sintaxis> ls0 = SLstm Shalt   -- ls0= [Shalt]
-- ls0 :: StmList
-- *L0sintaxis> ls1= SLlv asigzx ls0 -- ls1= asigzx:ls0 = asigzx:[Shalt]= [asigzx,Shalt]
-- ls1 :: StmList
-- *L0sintaxis> ls2= SLlv asigx0 ls1 -- ls2= asigx0:ls1 = asigx0:[asigzx,Shalt]= [asigx0,asigzx,Shalt]
-- ls2 :: StmList
-- *L0sintaxis>
-- *L0sintaxis> -- mostrar ls2: ---------------------
-- *L0sintaxis> ls2
-- SLlv (Sasig Vx (Edig D0)) (SLlv (Sasig Vz (Evar Vx)) (SLstm Shalt))
-- it :: StmList
-- *L0sintaxis>
-- *L0sintaxis> -- Programas: -----------------------
-- *L0sintaxis> p0= Prog (lv0,ls0)
-- p0 :: Prog
-- *L0sintaxis> p0   -- mostrar el programa p0: -----------------
-- Prog (VLvar Vz,SLstm Shalt)
-- it :: Prog
-- *L0sintaxis>
-- *L0sintaxis> p1= Prog (lv1,ls1)
-- p1 :: Prog
-- *L0sintaxis> p2= Prog (lv2,ls2)
-- p2 :: Prog
-- *L0sintaxis> p1   -- mostrar el programa p1: -----------------
-- Prog (VLlv Vy (VLvar Vz),SLlv (Sasig Vy (Edig D1)) (SLstm Shalt))
-- it :: Prog
-- *L0sintaxis> p2   -- mostrar el programa p2: -----------------
-- Prog (VLlv Vx (VLlv Vy (VLvar Vz)),SLlv (Sasig Vx (Edig D0)) (SLlv (Sasig Vy (Edig D1)) (SLstm Shalt)))
-- it :: Prog
-- *L0sintaxis> :quit
--

-- Ejercicios:
-- 1. Definir una funcion recursiva, lvSize, que calcule la longitud de una lista de variables.
-- 2. Definir una funcion recursiva, lsSize, que calcule la longitud de una lista de instrucciones.
-- 3. Definir una funcion, progSize, que calcule el tamaño de un programa (suma de las longitudes de listas que lo componen).
-- 4. Definir un programa L0, pIntercambiaxy,s que intercambie el valor de dos variables (x,y).
--
