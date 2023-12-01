module L2semanticaProg
    (
      semL2prog
    )
--
-- Semántica de programas de L2
--
--mcb
where
-- Modules already defined in Haskell: ------------------------------
-- import Data.Set as S 
import Data.List as L
--
-- Modules defined in this project: ---------------------------------
--
import L2sintaxis
--
import L2estados
--
-- import L2semanticaExp
--
import L2semanticaStm
--
-- Semántica de L2 -------------------------------------------------
--

--
-- semStmList :: StmList -> EstadoVT -> [EstadoVT]
-- -- Semántica de una lista de instrucciones ls en el estado sigma.
-- semStmList (StmList ls) sigma
--     = case ls of
--             s : ls' -> nub $ concat [ semStmList (StmList ls') t | t <- semOFs]
--                         where
--                             semOFs = semStm s sigma
--             []      -> [sigma] -- An empty list of statements is equivalent to Skip.
-- --

semL2prog :: Prog -> [EstadoVT]
-- Semántica de un programa de L2, p=(progName, varList, stm).
semL2prog (Prog (_, varList, stm))
    = concat $ nub [semStm stm sigma | sigma <- estadosProg]
        where
        estadosProg = nub (estadosVTOf varList) -- estados del programa
--

--

-- Tests: VER L2tests.hs

--
---------------------------------------------------------------
--

