module L0estados
    ( Estado(..)
    , estadosOf
    , modifEstado
    , modifLista
    , listOfEdo
    , edoTOomega
    , minInt, maxInt
    )
--
-- Estados de L0
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
--     ,Exp(..)
--     ,Stm(..), StmList(..)
--     )

--
-- Estados de L0 -------------------------------------------------
--

-- Dominios: ---------------------

-- OBSERVACION. Int NO es $\mathbf{Z}$.
-- En Haskell, el tipo Int NO es $\mathbf{Z}$ (los números enteros),
-- en realidad: el tipo Int consta de los enteros que van de minInt a maxInt.
-- Es decir, $Int = \{ x \in \mathbf{Z} \mid minInt \leq x \leq maxInt \}$.
--
minInt :: Int
minInt = minBound :: Int -- -9223372036854775808
--
maxInt :: Int
maxInt = maxBound :: Int --  9223372036854775807
--

valListOf :: Var -> [Val]
valListOf _ = [minB..maxB]
        where
        minB = minBound :: Val
        maxB = maxBound :: Val
--

-- Intuitivamente, las listas [m..n] representan dominios acotados [m..n]

-- Estados:
data Estado b =   Estado [(Var, b)]
                | Omega [(Var, b)]
                deriving (Eq,Show)
--

listOfEdo :: Estado b -> [(Var, b)]
-- Lista de pares (v,b) del estado sigma.
listOfEdo sigma = case sigma of
           (Estado lvb) -> lvb
           (Omega lvb)  -> lvb
--
edoTOomega :: Estado b -> Estado b
-- Transforma un estado a un estado final, Omega.
edoTOomega sigma = case sigma of
           (Estado lvb) -> (Omega lvb)
           (Omega lvb)  -> (Omega lvb)
--

-- -- La siguiente definicion es correcta,
-- -- pero usa VarList=[Var] en lugar de VarList=VLvar Var | VLlv Var VarList
-- estadosOf :: [Var] -> [[Int]]
-- -- Slv := {(b1 , b2 , . . . , b_|lv| ) | bi ∈ Dom(lv(i))}
-- estadosOf lv = case lv of
--                     [v]     -> [[b] | b <- valListOf v]
--                     v : lv' -> [b : lb | b <- valListOf v, lb <- s_lv']
--                                 where
--                                 s_lv' = estadosOf lv'
--                     []      -> [] -- Sin variables no hay estados.
--

-- -- La siguiente definicion es correcta,
-- -- usa VarList=VLvar Var | VLlv Var VarList
-- estadosOf :: VarList -> [[Int]]
-- -- Slv := {(b1 , b2 , . . . , b_|lv| ) | bi ∈ Dom(lv(i))}
-- estadosOf lv = case lv of
--                     VLvar v     -> [[b] | b <- valListOf v]
--                     VLlv v lv'  -> [b : lb | b <- valListOf v, lb <- s_lv']
--                                 where
--                                 s_lv' = estadosOf lv'
--                     --[]          -> [] -- Sin variables no hay estados.
-- --


-- La siguiente definicion es más conveniente,
-- Un estado es una lista de pares (v,b), dende v es una variable y b in Dom(v)
estadosOf :: VarList -> [Estado Val]
-- Lista de estados para la lista de variables lv
-- Slv := {((v1,b1) , (v2,b2), . . . , (v_|lv|,b_|lv|) ) | v_i=lv(i) and bi ∈ Dom(v_i)}
estadosOf lv = case lv of
                    VLvar v     -> [Estado [(v,b)] | b <- valListOf v]
                    VLlv v lv'  -> [Estado ((v,b) : lvb) | b <- valListOf v, Estado lvb <- s_lv']
                                where
                                s_lv' = estadosOf lv'
--

modifLista :: [(Var, b)] -> Var -> b -> [(Var, b)]
modifLista l v b = case l of
                       (x,d) : l'   ->  if x == v
                                               then (v,b) : l'
                                               else (x,d) : (modifLista l' v b)
                       []               -> []
--

modifEstado :: Estado b -> Var -> b -> Estado b
modifEstado sigma v b
    = case sigma of
        Estado lvb  -> Estado (modifLista lvb v b)
        Omega lvb   -> Omega lvb
--

---------------------------------------------------------------
--
-- Tests: VER L0tests.hs
-- VER mas Tests en el archivo: sesionHaskell-2023m09d10.txt (omitiendo el comando de carga :l )
--

-- Ejercicios:
-- 1. Definir una funcion recursiva, lvSize, que calcule la longitud de una lista de variables.
-- 2. Definir una funcion recursiva, lsSize, que calcule la longitud de una lista de instrucciones.
-- 3. Definir una funcion, progSize, que calcule el tamaño de un programa (suma de las longitudes de listas que lo componen).
-- 4. Definir un programa L0, pIntercambiaxy,s que intercambie el valor de dos variables (x,y).
--
