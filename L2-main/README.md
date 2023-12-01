# L2
Integrantes: 
- Marco Antonio Garcia Arce
- Emilio Francisco Sánchez Martínez
- David Pérez Jacome
- Valeria Reyes Tapia
- Betanzos Reyes Gustavo Noel
- Cristian Alonso Tovar Gonzalez
- Pedro Joshue Pintor Muñoz


Ejercicios:
1.- Definir un lenguaje, L2, que resulta de agregar a L1 una instrucción "For".
    En L2, la sintaxis cambia a:
   <Stm>        ::=   Halt | <AsigStm>
                    | Read <VarId> | Write <VarId>
                    | <IfThenStm> | <WhileStm>
                    | <BlockStm>
                    | <ForStm>
<ForStm>   ::= For <VarId> = <ExpArith> To <ExpArith> Do <Stm>
<ProgL2>     ::= L2PROG <ProgName> VAR <VarList> PROG <Stm>


  La sintaxis de L2 es simplemente el mismo de L1, simplemente el unico cambio entre L1 y L2 es la agregacion de For en L2, lo demas es exactamente igual por lo cual simplemente usamos L1 como base
   para realizar esta tarea y agregamos L2.


2.- Hacer los cambios necesarios para que una función readL2prog (definida en un módulo L2textIO.hs) compile y funcione haciendo parsing a programas L2.
Esta función es similar a readL1prog definida en L1textIO.hs.

  Por lo mismo de tomar como base L1 para crear L2, este ejercicio solo requiere de un modulo que solo lea el programa L2 que quiere ejecutarse, se pasa el programa a este modulo L2textIO.hs para revisar
  que cumpla con los tres parametros de nombre, variables y de instrucciones. Es muy similar al de L1 por ser esta su unica funcion, la cual comparte con L1.
  
Cambios realizados:

- Se cambió el nombre del módulo a L2textIO.
- Actualizamos las declaraciones para módulos L2 (L2sintaxis y L2parserProg).
- Se modificó blockStms para incluir un bucle for (declaración SFor).
- Se ajustaron los progStms y progStmsStr en consecuencia.


3.- Escribir, en un archivo de texto progSuma.L2, un programa L2 que calcule la suma de los números entre 1 el valor de la variable x, mediante una instrucción For.

   En el archivo progSuma.L2 se escribio un programa simple, una donde se pasa un valor cualquiera pero que sea digito, donde el for hara una suma de 1 hasta el numero en el parametro VAR. Un ejemplo de ejecucion
   podria ser que pasemos 3, entonces el programa sumara 1 + 2 + 3 = 6. Y eso es todo.


4.- Aplicar el parser de programas L2 a progSuma.L2


5.- Definir la semántica de la instrucción For, e implementarla en Haskell, en un módulo L2semanticaStm.hs similar al módulo L1semanticaStm.
    Sugerencia: ¿Cómo se escribe un For usando un While?

   En el archivo L2semanticaStm.hs, escribimos la semantica de for donde usamos un Swhile para que actue como un for, donde v = v + 1.
    
