module L0tests
--
-- Tests para L0
--
--mcb
where
-- Modules already defined in Haskell: ------------------------------
-- import Data.Set as S
-- import Data.List as L
--
-- Modules defined in this project: ---------------------------------
import L0sintaxis
--
import L0estados
--
import L0stateGraphs
--
import L0semantica
--
import L0global
--
-- Tests para L0 -------------------------------------------------------------
--
--

-------------------------------------------------------------------------------
-- BEGIN Tests show:
-------------------------------------------------------------------------------
--
showD0 :: String
showD0 = showVal D0

-------------------------------------------------------------------------------
-- END Tests show:
-------------------------------------------------------------------------------
--


-------------------------------------------------------------------------------
-- BEGIN Tests L0semantica:
-------------------------------------------------------------------------------
--

-----------------------------------------------------
testSemL0prog :: IO ()
-- Define testSemL0prog: --------------
testSemL0prog  -- Define testSemL0prog
    = let
        {
        -- Lista de variables, lvXYZ, y Estados de lvXYZ:
        lvXYZ= VLlv Vx (VLlv Vy (VLvar Vz));  -- lvXYZ= [x,y,z]
        lvXYZestados = estadosOf (lvXYZ) :: [Estado Val4];    -- estadosOf [x,y,z]
        --
        -- Instrucciones (de asignación)
        asigX1 = Sasig Vx (Evalor D1);    -- x:=1
        asigY2 = Sasig Vy (Evalor D2);    -- y:=2
        asigZ3 = Sasig Vz (Evalor D3);    -- z:=3
        --
        -- Listas de instrucciones:
        lsQ1= SLls asigX1 (SLls asigY2 (SLstm asigZ3));               -- [x:=1, y:=2, z:=3] OK
        lsQ2= SLls asigX1 (SLls asigY2 (SLls asigZ3 (SLstm Shalt)));  -- [x:=1, y:=2, z:=3, Halt] OK
        lsQ3= SLls asigX1 (SLls asigY2 (SLls Shalt (SLstm asigZ3)));  -- [x:=1, y:=2, Halt, z:=3] OK
        --
        -- Programas:
        progQ1 = Prog (lvXYZ, lsQ1); -- VAR [x,y,z] PROG [x:=1, y:=2, z:=3] OK
        progQ2 = Prog (lvXYZ, lsQ2); -- VAR [x,y,z] PROG [x:=1, y:=2, z:=3, Halt] OK
        progQ3 = Prog (lvXYZ, lsQ3); -- VAR [x,y,z] PROG [x:=1, y:=2, Halt, z:=3] OK
        --
        -- Semántica de los Programas:
        progQ1Sem = semL0prog progQ1;
        progQ2Sem = semL0prog progQ2;
        progQ3Sem = semL0prog progQ3;
        --
        -- Indentación y separación de listas
        ind1= "    ";   -- 4 espacios
        sep1= "\n";     -- newline
        --
        }
        in
    do { -------------------------------------------------------------------------------
        putStrLn "-- Test semL0prog, Semántica de Programas de L0:\n";

        putStrLn "-- Muestra los Estados de la Lista de Variables lvXYZ, lvXYZestados:";
        putStrLn (showShortList ind1 sep1 lvXYZestados);     -- Muestra lvXYZestados
        putStrLn "";

        putStrLn "-- Muestra la Lista de Variables lvXYZ:";
        putStrLn (show lvXYZ);  -- Muestra lvXYZ
        putStrLn "";

        putStrLn "-- Muestra la Lista de Instrucciones lsQ1:";
        putStrLn (show lsQ1);  -- Muestra lsQ1
        putStrLn "";

        putStrLn "-- Muestra la Lista de Instrucciones lsQ2:";
        putStrLn (show lsQ2);  -- Muestra lsQ2
        putStrLn "";

        putStrLn "-- Muestra la Lista de Instrucciones lsQ3:";
        putStrLn (show lsQ3);  -- Muestra lsQ3
        putStrLn "";

        putStrLn "-- Muestra el Programa progQ1:";
        putStrLn (showProg progQ1);
        --
        putStrLn "-- Muestra la Semántica del Programa progQ1:";
        putStrLn (show progQ1Sem);
        putStrLn "";
        --
        putStrLn "-- Muestra la Gráfica del Programa progQ1:";
        putStrLn (showStateGraph (grafOfL0prog progQ1));
        --

        putStrLn "-- Muestra el Programa progQ2:";
        putStrLn (showProg progQ2);
        --
        putStrLn "-- Muestra la Semántica del Programa progQ2:";
        putStrLn (show progQ2Sem);
        putStrLn "";
        --
        putStrLn "-- Muestra la Gráfica del Programa progQ2:";
        putStrLn (showStateGraph (grafOfL0prog progQ2));
        --

        putStrLn "-- Muestra el Programa progQ3:";
        putStrLn (showProg progQ3);
        --
        putStrLn "-- Muestra la Semántica del Programa progQ3:";
        putStrLn (show progQ3Sem);
        putStrLn "";
        --
        putStrLn "-- Muestra la Gráfica del Programa progQ3:";
        putStrLn (showStateGraph (grafOfL0prog progQ3));

        putStrLn "-- Muestra el Programa prog0B:";
        putStrLn (showProg prog0B);
        --
        putStrLn "-- Muestra la Semántica del Programa prog0B:";
        putStrLn (show (semL0prog prog0B));
        putStrLn "";
        putStrLn "-- Muestra la Gráfica del Programa prog0B:";
        putStrLn (showStateGraph (grafOfL0prog prog0B));
        --
        --
    }
-----------------------------------------------------

--
----------------------------
semStmTest :: IO ()
semStmTest  =
    let
        -- Variables
        x= Vx   -- x :: Var
        y= Vy   -- y :: Var

        -- Estados:
        edosXY = estadosOf (VLlv x (VLvar y)) -- estadosOf [x,y]

        -- Asignaciones:
        asigX0 = Sasig x (Evalor D0)    -- x:=0
        asigXY = Sasig x (Evar y)       -- x:=y

        -- Un estado:
        sigmaX1Y2 = edosXY !! 6     -- sigmaX1Y2 = [(Vx,1),(Vy,2)]

        -- Semantica:
        semAsigX0 = semStm asigX0 sigmaX1Y2  -- [[x:=0]]_sigma, donde sigma=[(Vx,1),(Vy,2)]
        semAsigXY = semStm asigXY sigmaX1Y2  -- [[x:=y]]_sigma, donde sigma=[(Vx,1),(Vy,2)]
    in do {
        putStrLn "-- Test semStm, Semántica de Instrucciones de L0:\n";

        putStrLn "-- Muestra edosXY:";
        putStrLn (show edosXY);     -- Muestra edosXY
        putStrLn "";

        putStrLn "-- Muestra sigmaX1Y2:";
        putStrLn (show sigmaX1Y2);  -- Muestra sigmaX1Y2
        putStrLn "";

        putStrLn "-- Muestra [[x:=0]]_[(Vx,1),(Vy,2)]:";
        putStrLn (show semAsigX0);     -- [[x:=0]]_[(Vx,1),(Vy,2)]
        putStrLn "";

        putStrLn "-- Muestra [[x:=y]]_[(Vx,1),(Vy,2)]:";
        putStrLn (show semAsigXY);     -- [[x:=y]s]_[(Vx,1),(Vy,2)]
        putStrLn "";
        }
----------------------------
--

-- Listas de variables: -------------

lv0 :: VarList
lv0 = VLvar Vz      -- lv0=[z]
--
lv1 :: VarList
lv1 = VLlv Vy lv0   -- lv1=[y,z]
--
lv2 :: VarList
lv2 = VLlv Vx lv1   -- lv2=[x,y,z]
--
lv3 :: VarList
lv3 = VLvar Vx      -- lv3=[x]
--

-- Instrucciones: -------------------

asigx0 :: Stm Val
asigx0= Sasig Vx (Evalor D0)    -- x:= 0
--
asigx1 :: Stm Val
asigx1= Sasig Vx (Evalor D1)    -- x:= 1
--
asigy1 :: Stm Val
asigy1= Sasig Vy (Evalor D1)    -- y:= 1
--
asigzx :: Stm Val
asigzx= Sasig Vz (Evar Vx)      -- z:= x


-- Listas de instrucciones: ----------

ls0 :: StmList Val
ls0 = SLstm Shalt   -- ls0= [Shalt]
--
ls1 :: StmList Val
ls1= SLls asigzx ls0 -- ls1= asigzx:ls0 = asigzx:[Shalt]= [asigzx,Shalt]
--
ls2 :: StmList Val
ls2= SLls asigx0 ls1 -- ls2= asigx0:ls1 = asigx0:[asigzx,Shalt]= [asigx0,asigzx,Shalt]

ls3 :: StmList Val
ls3= SLls asigx1 (SLls asigx0 (SLls asigx1 (SLstm Shalt))) -- ls3= [x:=1, x:=0, x:=1, Halt]

-- Programas: -----------------------

prog0 :: Prog Val
prog0= Prog (lv0,ls0)
--
prog1 :: Prog Val
prog1= Prog (lv1,ls1)
--
prog2 :: Prog Val
prog2= Prog (lv2,ls2)
--
prog3 :: Prog Val
prog3= Prog (lv3,ls3)


prog0B :: Prog Val01
prog0B= Prog (lv0,SLstm Shalt)

prog0BGraph :: StateGraph Val01
prog0BGraph = (grafOfL0prog prog0B)
--
-- putStrLn "-- Muestra la Gráfica del Programa prog0B:";
-- putStrLn (showStateGraph (grafOfL0prog prog0B));

lvEstadosShortList :: String
lvEstadosShortList = showShortList ind1 sep1 (varListTOhaskell lv2)
    where ind1= "    "  -- 4 espacios
          sep1= "\n"    -- newline
--
--

-- VER mas Tests en el archivo: sesionHaskell-2023m09d10.txt (omitiendo el comando de carga :l )

--
-------------------------------------------------------------------------------
-- ...continua Tests L0semantica:
-------------------------------------------------------------------------------
-- DESCOMENTAR (ctrl-shift-D) hasta "END Test semStm", y pegar en ghci>.
---------------------------------------
-- -- ...CONTINÚA Test L0semantica
---------------------------------------
-- :l L0semantica.hs
-- :{
-- let
--         -- Variables
--         x= Vx   -- x :: Var
--         y= Vy   -- y :: Var
--
--         -- Estados:
--         edosXY = estadosOf (VLlv x (VLvar y)) -- estadosOf [x,y]
--
--         -- Asignaciones:
--         asigX0 = Sasig x (Evalor D0)    -- x:=0
--         asigXY = Sasig x (Evar y)       -- x:=y
--
--         -- Un estado:
--         sigmaX1Y2 = edosXY !! 6     -- sigmaX1Y2 = [(Vx,1),(Vy,2)]
--
--         -- Semantica:
--         semAsigX0 = semStm asigX0 sigmaX1Y2  -- [[x:=0]]_sigma, donde sigma=[(Vx,1),(Vy,2)]
--         semAsigXY = semStm asigXY sigmaX1Y2  -- [[x:=y]]_sigma, donde sigma=[(Vx,1),(Vy,2)]
-- :}
-- --
-- :{
-- do {
--         putStrLn "-- Test semStm, Semántica de Instrucciones de L0:\n";
--
--         putStrLn "-- Muestra edosXY:";
--         putStrLn (show edosXY);     -- Muestra edosXY
--         putStrLn "";
--
--         putStrLn "-- Muestra sigmaX1Y2:";
--         putStrLn (show sigmaX1Y2);  -- Muestra sigmaX1Y2
--         putStrLn "";
--
--         putStrLn "-- Muestra [[x:=0]]_[(Vx,1),(Vy,2)]:";
--         putStrLn (show semAsigX0);     -- [[x:=0]]_[(Vx,1),(Vy,2)]
--         putStrLn "";
--
--         putStrLn "-- Muestra [[x:=y]]_[(Vx,1),(Vy,2)]:";
--         putStrLn (show semAsigXY);     -- [[x:=y]s]_[(Vx,1),(Vy,2)]
--         putStrLn "";
--     }
-- :}
---------------------------------------
-- -- END Test semStm
---------------------------------------
--
--
-----------------------------------------------------
-- BEGIN Tests linea por linea:
-----------------------------------------------------
-- Prelude>
-- Prelude> -- Cargar semantica de L0: --------------
-- Prelude> :l L0semantica.hs
-- [1 of 1] Compiling L0semantica ( L0semantica.hs, interpreted )
-- Ok, one module loaded.
-- *L0semantica>
-- *L0semantica> -- mostrar tipos:
-- *L0semantica> :set +t
-- *L0semantica>
-- *L0semantica> -- Definir variables: -------------
-- *L0semantica> x = Vx
-- x :: Var
-- *L0semantica> y = Vy
-- y :: Var
-- *L0semantica> z = Vz
-- z :: Var
-- *L0semantica>
-- *L0semantica> -- Definir expresiones:-------------
-- *L0semantica> e0 = Evalor D0
-- e0 :: Exp
-- *L0semantica> e1 = Evalor D1
-- e1 :: Exp
-- *L0semantica> e2 = Evalor D2
-- e2 :: Exp
-- *L0semantica> e3 = Evalor D3
-- e3 :: Exp
-- *L0semantica>
-- *L0semantica> ex= Evar x
-- ex :: Exp
-- *L0semantica> ey= Evar y
-- ey :: Exp
-- *L0semantica> ez= Evar z
-- ez :: Exp
-- *L0semantica>
-- *L0semantica> -- Instrucciones: -------------------
-- *L0semantica> asigx0= Sasig x e0    -- x:= 0
-- asigx0 :: Stm
-- *L0semantica> asigy1= Sasig y e1    -- y:= 1
-- asigy1 :: Stm
-- *L0semantica> asigzx= Sasig z ex    -- z:= x
-- asigzx :: Stm
-- *L0semantica> halt = Shalt  -- instruccion Halt
-- halt :: Stm
-- *L0semantica>
-- *L0semantica> -- Listas de variables: -------------
-- *L0semantica> lv0 = VLvar z -- lv0=[z]
-- lv0 :: VarList
-- *L0semantica> lv1 = VLlv y lv0 -- lv1= y:lv0 = y:[z] = [y,z]
-- lv1 :: VarList
-- *L0semantica> lv2 = VLlv x lv1 -- lv1= x:lv1 = z:[y,z] = [x,y,z]
-- lv2 :: VarList
-- *L0semantica>
-- *L0semantica> -- Listas de instrucciones: ----------
-- *L0semantica> ls0 = SLstm Shalt   -- ls0= [Shalt]
-- ls0 :: StmList
-- *L0semantica> ls1= SLls asigzx ls0 -- ls1= asigzx:ls0 = asigzx:[Shalt]= [asigzx,Shalt]
-- ls1 :: StmList
-- *L0semantica> ls2= SLls asigx0 ls1 -- ls2= asigx0:ls1 = asigx0:[asigzx,Shalt]= [asigx0,asigzx,Shalt]
-- ls2 :: StmList
-- *L0semantica>
-- *L0semantica> -- mostrar ls2: ---------------------
-- *L0semantica> ls2
-- SLls (Sasig Vx (Evalor D0)) (SLls (Sasig Vz (Evar Vx)) (SLstm Shalt))
-- it :: StmList
-- *L0semantica>
-- *L0semantica> -- Programas: -----------------------
-- *L0semantica> p0= Prog (lv0,ls0)
-- p0 :: Prog
-- *L0semantica> p0   -- mostrar el programa p0: -----------------
-- Prog (VLvar Vz,SLstm Shalt)
-- it :: Prog
-- *L0semantica>
-- *L0semantica> p1= Prog (lv1,ls1)
-- p1 :: Prog
-- *L0semantica> p2= Prog (lv2,ls2)
-- p2 :: Prog
-- *L0semantica> p1   -- mostrar el programa p1: -----------------
-- Prog (VLlv Vy (VLvar Vz),SLls (Sasig Vy (Evalor D1)) (SLstm Shalt))
-- it :: Prog
-- *L0semantica> p2   -- mostrar el programa p2: -----------------
-- Prog (VLlv Vx (VLlv Vy (VLvar Vz)),SLls (Sasig Vx (Evalor D0)) (SLls (Sasig Vy (Evalor D1)) (SLstm Shalt)))
-- it :: Prog
-- *L0semantica> :quit
-----------------------------------------------------
-- END Tests linea por linea
-----------------------------------------------------
--
-------------------------------------------------------------------------------
-- END Tests L0semantica
-------------------------------------------------------------------------------
--


-------------------------------------------------------------------------------
-- BEGIN Tests L0estados:
-------------------------------------------------------------------------------
--
estadosTest :: IO ()
estadosTest  =
    let
        -- Variables
        x= Vx   -- x :: Var
        y= Vy   -- y :: Var

        -- Estados:
        edosXY = estadosOf (VLlv x (VLvar y)) -- estadosOf [x,y]

        -- Un estado:
        sigmaX1Y2 = edosXY !! 6     -- sigmaX1Y2 = [(Vx,1),(Vy,2)]

        -- Modificación de estados:
        sigmaModxD0 = modifEstado sigmaX1Y2 x D0  -- sigmaX1Y2[x <- D0]
        sigmaModyD0 = modifEstado sigmaX1Y2 y D0  -- sigmaX1Y2[y <- D0]
    in do {
        putStrLn "-- Test estados:\n";

        putStrLn "-- Muestra edosXY:";
        putStrLn (show edosXY);     -- Muestra edosXY
        putStrLn "";

        putStrLn "-- Muestra sigmaX1Y2:";
        putStrLn (show sigmaX1Y2);  -- Muestra sigmaX1Y2
        putStrLn "";

        putStrLn "-- sigmaX1Y2[x <- D0]:";
        putStrLn (show sigmaModxD0);     -- sigmaX1Y2[x <- D0]
        putStrLn "";

        putStrLn "-- sigmaX1Y2[y <- D0]:";
        putStrLn (show sigmaModyD0);     -- sigmaX1Y2[y <- D0]
        putStrLn "";
        }
--
--------------------------------------------------------------------
--
--
-- -- Test semStm: DESCOMENTAR hasta la linea ":}", y pegar en ghci.
-- :l L0estados.hs
-- :{
-- -- BEGIN Test semStm
-- let
--         -- Variables
--         x= Vx   -- x :: Var
--         y= Vy   -- y :: Var
--
--         -- Estados:
--         edosXY = estadosOf (VLlv x (VLvar y)) -- estadosOf [x,y]
--
--         -- Un estado:
--         sigmaX1Y2 = edosXY !! 6     -- sigmaX1Y2 = [(Vx,1),(Vy,2)]
--
--         -- Modificación de estados:
--         sigmaModxD0 = modifEstado sigmaX1Y2 x D0  -- sigmaX1Y2[x <- D0]
--         sigmaModyD0 = modifEstado sigmaX1Y2 y D0  -- sigmaX1Y2[y <- D0]
-- :}
-- --
-- :{
-- do {
--         putStrLn "-- Test estados:\n";
--
--         putStrLn "-- Muestra edosXY:";
--         putStrLn (show edosXY);     -- Muestra edosXY
--         putStrLn "";
--
--         putStrLn "-- Muestra sigmaX1Y2:";
--         putStrLn (show sigmaX1Y2);  -- Muestra sigmaX1Y2
--         putStrLn "";
--
--         putStrLn "-- sigmaX1Y2[x <- D0]:";
--         putStrLn (show sigmaModxD0);     -- sigmaX1Y2[x <- D0]
--         putStrLn "";
--
--         putStrLn "-- sigmaX1Y2[y <- D0]:";
--         putStrLn (show sigmaModyD0);     -- sigmaX1Y2[y <- D0]
--         putStrLn "";
--     }
-- -- END Test semStm
-- :}
--

--
--------------------
-- Tests modifEstado:
-- Prelude>
-- Prelude> :l L0estados.hs
-- *L0estados> lvyz = VLlv Vy (VLvar Vz) -- [y,z]
-- lvyz :: VarList
-- *L0estados>
-- *L0estados> ledos1 = estadosOf lvyz
-- ledos1 :: [[(Var, Int)]]
-- *L0estados> ledos1
-- [[(Vy,0),(Vz,0)],[(Vy,0),(Vz,1)],[(Vy,0),(Vz,2)],[(Vy,0),(Vz,3)],[(Vy,1),(Vz,0)],[(Vy,1),(Vz,1)],[(Vy,1),(Vz,2)],[(Vy,1),(Vz,3)],[(Vy,2),(Vz,0)],[(Vy,2),(Vz,1)],[(Vy,2),(Vz,2)],[(Vy,2),(Vz,3)],[(Vy,3),(Vz,0)],[(Vy,3),(Vz,1)],[(Vy,3),(Vz,2)],[(Vy,3),(Vz,3)]]
-- it :: [[(Var, Int)]]
-- *L0estados>
-- *L0estados> length ledos1
-- 16
-- it :: Int
-- *L0estados> edo7 = ledos1 !! 6
-- edo7 :: [(Var, Int)]
-- *L0estados> edo7
-- [(Vy,1),(Vz,2)]
-- it :: [(Var, Int)]
-- *L0estados>
-- *L0estados> modifEstado edo7 Vz 3
-- [(Vy,1),(Vz,3)]
-- it :: [(Var, Int)]
-- *L0estados>
-- *L0estados> :quit
--
-------------------------------------------------------------------------------
-- END Tests L0estados:
-------------------------------------------------------------------------------



-------------------------------------------------------------------------------
-- BEGIN Tests L0sintaxis:
-------------------------------------------------------------------------------
-- Prelude>
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
-- *L0sintaxis> ls1= SLls asigzx ls0 -- ls1= asigzx:ls0 = asigzx:[Shalt]= [asigzx,Shalt]
-- ls1 :: StmList
-- *L0sintaxis> ls2= SLls asigx0 ls1 -- ls2= asigx0:ls1 = asigx0:[asigzx,Shalt]= [asigx0,asigzx,Shalt]
-- ls2 :: StmList
-- *L0sintaxis>
-- *L0sintaxis> -- mostrar ls2: ---------------------
-- *L0sintaxis> ls2
-- SLls (Sasig Vx (Evalor D0)) (SLls (Sasig Vz (Evar Vx)) (SLstm Shalt))
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
-- Prog (VLlv Vy (VLvar Vz),SLls (Sasig Vy (Evalor D1)) (SLstm Shalt))
-- it :: Prog
-- *L0sintaxis> p2   -- mostrar el programa p2: -----------------
-- Prog (VLlv Vx (VLlv Vy (VLvar Vz)),SLls (Sasig Vx (Evalor D0)) (SLls (Sasig Vy (Evalor D1)) (SLstm Shalt)))
-- it :: Prog
-- *L0sintaxis> -------------------------------
-- *L0sintaxis> -- Test showVal4:
-- *L0sintaxis> showVal4 D0
-- "0"
-- it :: String
-- *L0sintaxis>
-- *L0sintaxis> :quit
-------------------------------------------------------------------------------
-- END Tests L0sintaxis:
-------------------------------------------------------------------------------


-- Ejercicios:
-- 1. Definir una funcion recursiva, lvSize, que calcule la longitud de una lista de variables.
-- 2. Definir una funcion recursiva, lsSize, que calcule la longitud de una lista de instrucciones.
-- 3. Definir una funcion, progSize, que calcule el tamaño de un programa (suma de las longitudes de listas que lo componen).
-- 4. Definir un programa L0, pIntercambiaxy,s que intercambie el valor de dos variables (x,y).
--
