module L2semanticaStm
    (
     semStm
     , semStmList
     , semAsig
     , semWhile
    )
--
-- Semántica de instrucciones de L2
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
import L2semanticaExp
--
-- Semántica de L2 -------------------------------------------------
--

--
-----------------------------------------------------------
semAsig :: VarId -> ExpArith -> EstadoVT -> [EstadoVT]
-- [[vId := ea]]_sigma = [sigma[vId <- e]]
--                       Lista de un solo estado sigma' que resulta de modificar sigma
--                       poniendo el valor de e en lugar de sigma[vId].
semAsig vId ea sigma = [modifEstadoVT sigma vId eaINsigma]
    where
    eaINsigma = semExpArith ea sigma
--

--
semRead :: VarId -> EstadoVT -> [EstadoVT]
semRead vId sigma = [modifEstadoVT sigma vId b | b <- valListOf v]
        where v = varOFvId sigma vId
        -- Lista de modificaciones de sigma, en la variable v, usando los valores posibles para v.
        -- [ sigma[v <- b] | b in dominio(v) ]
--

semWrite :: VarId -> EstadoVT -> [EstadoVT]
semWrite _ sigma = [sigma]

--     | SIfThen ExpBool Stm Stm
--
semIfThen :: ExpBool -> Stm->Stm -> EstadoVT -> [EstadoVT]
semIfThen expBool stm1 stm2 sigma
    = if (semExpBool expBool sigma)
         then (semStm stm1 sigma)
         else (semStm stm2 sigma)
--

--
semWhile :: ExpBool -> Stm -> EstadoVT -> [EstadoVT]
semWhile expBool stm sigma
    = if ebSem
         then nub $ concat [semWhile expBool stm edo | edo <- sigmaL']
         else [sigma]
    where
        ebSem   = semExpBool expBool sigma
        --semStmSigma = semStm stm sigma
        --xL  = semStmSigma
        sigmaL' = semStm stm sigma
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

semStmList :: StmList -> EstadoVT -> [EstadoVT]
-- Semántica de una lista de instrucciones ls en el estado sigma.
semStmList stmList sigma
    = case stmList of
            s : ls' -> nub $ concat [ semStmList ls' t | t <- semOFs]
                        where semOFs = semStm s sigma
            []      -> [sigma] -- An empty list of statements is equivalent to Skip.
--

--
semBlockStm :: StmList -> EstadoVT -> [EstadoVT]
semBlockStm stmList sigma = semStmList stmList sigma

--
semStm :: Stm  -> EstadoVT -> [EstadoVT]
-- Semántica de una instruccion stm en el estado sigma.
semStm stm sigma
    = case stm of
        Sasig vId ea   -> semAsig vId ea sigma
                        -- [[vId := ea]]_sigma = [sigma[vId <- e]]
        Sread vId    -> semRead vId sigma
        Swrite vId   -> semWrite vId sigma
                        -- La instruccion write no modifica el estado actual, sigma.
        Shalt        -> [edoVTtoOmega sigma]
                        -- (Omega lvb) representa un estado final omega.
        -- Recursivas:
        SIfThen eBool stm1 stm2 -> semIfThen eBool stm1 stm2 sigma
        SWhile eBoolW stmW      -> semWhile eBoolW stmW sigma
        SblockStm stmL          -> semBlockStm stmL sigma

--

--
semFor :: VarId -> ExpArith -> ExpArith -> Stm -> EstadoVT -> [EstadoVT]
-- Semantica para la instruccion For:
semFor vId ea1 ea2 stmF sigma 
    = semStm forTOwhile sigma
    where
    forTOwhile =
        SblockStm                       -- {
            [Sasig vId ea1,             -- v := ea1;
            SWhile vLEQea2              -- While v <= ea2 Do
                (SblockStm              --      {
                    [stmF,              --      stm;
                    Sasig vId vSumUno   --      v := v+1;
                ])                      --      }
                ]                       -- }
                
    vLEQea2 = EBatom (AtomoBool (EBvar vId, OCmenEq, EBint ea2Sem)) -- v <= ea2
    ea2Sem  = semExpArith ea2 sigma -- semantica de ea2 EN sigma XXX
    vIdEA   = EAbasica (EBvar vId)  -- vId como una ExpArith VER ExpArith
    unoEA   = EAbasica (EBint 1)    -- 1 como una ExpArith VER ExpArith
    vSumUno = EAopArit (vIdEA,OAsum,unoEA) -- v+1


--semFor :: VarId -> ExpArith -> ExpArith -> Stm -> EstadoVT -> [EstadoVT]
--Semantica de una instruccion For 
--semFor vId iniExp finExp bodyStm sigma =
    --let
        --startValue = semExpArith iniExp sigma
        --endValue = semExpArith finExp sigma
        
        --loopSigma = modifEstadoVT sigma vId startValue
        --loopExp = Eq finExp sigma
        --loopExp = AtomoBool (iniExp, OCequ,  finExp)
        --loopBody = bodyStm
    --in
        --semWhile (EBatom loopExp) loopBody loopSigma
        

-- semL2prog :: Prog -> [EstadoVT]
-- -- Semántica de un programa de L2.
-- semL2prog (Prog (varL, stmL))
--     = concat $ nub [semStmList stmL t | t <- estadosProg]
--         where
--         estadosProg = nub (estadosVTOf varL) -- estados del programa
--

--

-- Tests: VER L2tests.hs

--
---------------------------------------------------------------
--

