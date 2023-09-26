module L0semantica
    ( semStm
    , semExp
    , semVar
    , semStmList
    , semL0prog
    )
--
-- Semántica de L0
--
--mcb
where
-- Modules already defined in Haskell: ------------------------------
-- import Data.Set as S 
import Data.List as L
--
-- Modules defined in this project: ---------------------------------
import L0sintaxis
--
import L0estados
--
--
-- Semántica de L0 -------------------------------------------------
--

semVar :: Var-> Estado b -> b
-- Semántica de una variable v en el estado sigma.
semVar v sigma
    = case vINsigma of
        Just b   -> b
        Nothing  -> error $ "semVar: variable inválida, v= " ++ (show v)
    where
    lvb      = listOfEdo sigma
    vINsigma = lookup v lvb
--
--
semExp :: Exp b -> Estado b -> b
-- Semántica de una expresión b en el estado sigma.
semExp e sigma
    = case e of
           Evalor b -> b    -- [[b]]_sigma = b
           Evar v   -> semVar v sigma

semStm :: Stm b -> Estado b -> [Estado b]
-- Semántica de una instruccion s en el estado sigma.
semStm s sigma
    = case s of
           Sasig v e    -> [modifEstado sigma v eINsigma]
                        where
                        eINsigma = semExp e sigma -- [[e]]_sigma
           Shalt        -> [edoTOomega sigma] -- (Omega lvb) representa un estado final omega.


semStmList :: (Eq b) => StmList b -> Estado b -> [Estado b]
-- Semántica de una lista de instrucciones ls en el estado sigma.
semStmList ls sigma
    = case ls of
            SLls s ls'  -> nub $ concat [ semStmList ls' t | t <- semOFs]
                        where
                            semOFs = semStm s sigma
            SLstm s    -> semStm s sigma
--

semL0prog :: Prog Val -> [Estado Val]
-- Semántica de un programa de L0.
semL0prog (Prog (lv, ls))
    = concat $ nub [semStmList ls t | t <- estadosProg]
        where
        estadosProg = nub (estadosOf lv) -- estados del programa
--

--
--an execution path

-- Tests: VER L0tests.hs
-- VER mas Tests en el archivo: sesionHaskell-2023m09d10.txt (omitiendo el comando de carga :l )

--
---------------------------------------------------------------
--

-- Ejercicios:
-- 1. Definir una funcion recursiva, lvSize, que calcule la longitud de una lista de variables.
-- 2. Definir una funcion recursiva, lsSize, que calcule la longitud de una lista de instrucciones.
-- 3. Definir una funcion, progSize, que calcule el tamaño de un programa (suma de las longitudes de listas que lo componen).
-- 4. Definir un programa L0, pIntercambiaxy,s que intercambie el valor de dos variables (x,y).
--
