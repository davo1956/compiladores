module L0semantica
--
-- Semántica de L0
--
--mcb
where
-- Modules already defined in Haskell: ------------------------------
-- import Data.Set as S 
-- import Data.List as L
--
-- Modules defined in this project: ---------------------------------
import L0sintaxis
--     (Prog(..)
--     ,Var(..), VarList(..)
--     ,Val4(..), Exp(..)
--     ,Stm(..), StmList(..)
--     )

--
-- Semántica de L0 -------------------------------------------------
--

-- Dominios: ---------------------

-- Dominio general para variables con dominios acotados (listas).
-- Usamos funciones para manejar dominios acotados [m..n].
type DomInt = Int


-- Dominios acotados (listas).
type BndDom = [Int]

domVal :: [Int]
-- Dominio para los "valores". Por ejemplo, para data Val4 = D0 | D1 | D2 | D3
domVal = [0..3]

domList :: [BndDom]
-- Lista de dominios para las variables: data Var = Vx | Vy | Vz
domList = [domVal, domVal, domVal]

domOf :: Var -> [Int]
domOf v = domList!!(fromEnum v)

inDomExp :: DomInt -> Bool
-- inDomExp b = true sii b in domVal
inDomExp b = b `elem` domVal

inDomOf :: DomInt -> Var -> Bool
-- inDomOf b v= true sii b in Dom(v)
inDomOf b v = case v of
                 _ -> inDomExp b


-- -- La siguiente definicion es correcta,
-- -- pero usa VarList=[Var] en lugar de VarList=VLvar Var | VLlv Var VarList
-- estadosOf :: [Var] -> [[Int]]
-- -- Slv := {(b1 , b2 , . . . , b_|lv| ) | bi ∈ Dom(lv(i))}
-- estadosOf lv = case lv of
--                     [v]     -> [[b] | b <- domOf v]
--                     v : lv' -> [b : lb | b <- domOf v, lb <- s_lv']
--                                 where
--                                 s_lv' = estadosOf lv'
--                     []      -> [] -- Sin variables no hay estados.
--

-- -- La siguiente definicion es correcta,
-- -- usa VarList=VLvar Var | VLlv Var VarList
-- estadosOf :: VarList -> [[Int]]
-- -- Slv := {(b1 , b2 , . . . , b_|lv| ) | bi ∈ Dom(lv(i))}
-- estadosOf lv = case lv of
--                     VLvar v     -> [[b] | b <- domOf v]
--                     VLlv v lv'  -> [b : lb | b <- domOf v, lb <- s_lv']
--                                 where
--                                 s_lv' = estadosOf lv'
--                     --[]          -> [] -- Sin variables no hay estados.
-- --

-- La siguiente definicion es más conveniente,
-- Un estado es una lista de pares (v,b), dende v es una variable y b in Dom(v)
estadosOf :: VarList -> [[(Var, Int)]]
-- Slv := {((v1,b1) , (v2,b2), . . . , (v_|lv|,b_|lv|) ) | v_i=lv(i) and bi ∈ Dom(v_i)}
estadosOf lv = case lv of
                    VLvar v     -> [[(v,b)] | b <- domOf v]
                    VLlv v lv'  -> [(v,b) : lb | b <- domOf v, lb <- s_lv']
                                where
                                s_lv' = estadosOf lv'
                    --[]          -> [] -- Sin variables no hay estados.

modifEstado :: [(Var, Int)] -> Var -> Int -> [(Var, Int)]
modifEstado sigma v b = case sigma of
                       (x,d) : sigma'   ->  if x /= v
                                               then (x,d) : (modifEstado sigma' v b)
                                               else (v,b) : (modifEstado sigma' v b)
                       []               -> []
--
-- Test modifEstado:
-- *L0semantica> lvyz = VLlv Vy (VLvar Vz) -- [y,z]
-- lvyz :: VarList
-- *L0semantica>
-- *L0semantica> ledos1 = estadosOf lvyz
-- ledos1 :: [[(Var, Int)]]
-- *L0semantica> ledos1
-- [[(Vy,0),(Vz,0)],[(Vy,0),(Vz,1)],[(Vy,0),(Vz,2)],[(Vy,0),(Vz,3)],[(Vy,1),(Vz,0)],[(Vy,1),(Vz,1)],[(Vy,1),(Vz,2)],[(Vy,1),(Vz,3)],[(Vy,2),(Vz,0)],[(Vy,2),(Vz,1)],[(Vy,2),(Vz,2)],[(Vy,2),(Vz,3)],[(Vy,3),(Vz,0)],[(Vy,3),(Vz,1)],[(Vy,3),(Vz,2)],[(Vy,3),(Vz,3)]]
-- it :: [[(Var, Int)]]
-- *L0semantica>
-- *L0semantica> length ledos1
-- 16
-- it :: Int
-- *L0semantica> edo7 = ledos1 !! 6
-- edo7 :: [(Var, Int)]
-- *L0semantica> edo7
-- [(Vy,1),(Vz,2)]
-- it :: [(Var, Int)]
-- *L0semantica>
-- *L0semantica> modifEstado edo7 Vz 3
-- [(Vy,1),(Vz,3)]
-- it :: [(Var, Int)]
-- *L0semantica>
--
--------------------------------------------------------------------
--
--
----------------------------------------------------
--
-- showDig :: Val4 -> String
-- -- Digitos a texto
-- showDig d = case d of
--              D0     -> "0"
--              D1     -> "1"
--              D2     -> "2"
--              _      -> error $ "showDig: NO implementado aun para d= " ++ (show d)
--
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
-- *L0sintaxis> e0 = Evalor D0
-- e0 :: Exp
-- *L0sintaxis> e1 = Evalor D1
-- e1 :: Exp
-- *L0sintaxis> e2 = Evalor D2
-- e2 :: Exp
-- *L0sintaxis> e3 = Evalor D3
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
-- SLlv (Sasig Vx (Evalor D0)) (SLlv (Sasig Vz (Evar Vx)) (SLstm Shalt))
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
-- Prog (VLlv Vy (VLvar Vz),SLlv (Sasig Vy (Evalor D1)) (SLstm Shalt))
-- it :: Prog
-- *L0sintaxis> p2   -- mostrar el programa p2: -----------------
-- Prog (VLlv Vx (VLlv Vy (VLvar Vz)),SLlv (Sasig Vx (Evalor D0)) (SLlv (Sasig Vy (Evalor D1)) (SLstm Shalt)))
-- it :: Prog
-- *L0sintaxis> :quit
--

-- Ejercicios:
-- 1. Definir una funcion recursiva, lvSize, que calcule la longitud de una lista de variables.
-- 2. Definir una funcion recursiva, lsSize, que calcule la longitud de una lista de instrucciones.
-- 3. Definir una funcion, progSize, que calcule el tamaño de un programa (suma de las longitudes de listas que lo componen).
-- 4. Definir un programa L0, pIntercambiaxy,s que intercambie el valor de dos variables (x,y).
--
