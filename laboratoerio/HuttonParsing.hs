module HuttonParsing

where

-- 13.3 Basic definitions (pp 214-215)
-- We begin by importing two standard libraries for applicative functors
-- and characters that will be used in our implementation:

import Control.Applicative
import Data.Char

import L0sintaxis


-- To allow the Parser type to be made into instances of classes, it is
-- first redefined using newtype, with a dummy constructor called P:

newtype Parser a = P (String -> [(a,String)])

-- Parser of this type can then be applied to an input string using a function
-- that simply removes the dummy constructor:

parse :: Parser a -> String -> [(a,String)]
parse (P p) inp = p inp

-- Our first parsing primitive is called item, which fails if the input string
-- is empty, and succeeds with the first character as the result value
-- otherwise:

item :: Parser Char
item = P (\inp -> case inp of
    []      -> [] -- el parser falla
    (x:xs)  -> [(x,xs)])

-- The item parser is the basic building block from which all other parsers
-- that consume characters from the input will ultimately be constructed. Its
-- behaviour is illustrated by the following two examples:

-- > parse item ""
--     []
-- > parse item "abc"
--     [(’a’,"bc")]

---------------------------------------------------------------------
































-- 13.4 Sequencing parsers (pp 215-217)
-- We now make the parser type into an instance of the functor, applicative
-- and monad classes, in order that the do notation can then be used to
-- combine parsers in sequence. The declarations are similar to those for
-- state transformers, except that we also need to take account of the
-- possibility that a parser may fail. The first step is to make the Parser
-- type into a functor:

instance Functor Parser where
    -- fmap :: (a -> b) -> Parser a -> Parser b
    fmap g p = P (\inp -> case parse p inp of
                        []          -> []
--                         [(v,out)] -> [(g v, out)])
                        (v,out):_   -> [(g v, out)])

-- That is, fmap applies a function to the result value of a parser if the
-- parser succeeds, and propagates the failure otherwise. For example:

-- > parse (fmap toUpper item) "abc"
-- [(’A’,"bc")]
-- > parse (fmap toUpper item) ""
-- []

-- (The function toUpper is provided in the library Data.Char.) The Parser
-- type can then be made into an applicative functor as follows:

instance Applicative Parser where
    -- pure :: a -> Parser a
    pure v = P (\inp -> [(v,inp)])

    -- <*> :: Parser (a -> b) -> Parser a -> Parser b
    pg <*> px = P ( \inp -> case parse pg inp of
                    []          -> []
--                     [(g,out)] -> parse (fmap g px) out )
                    (g,out):_   -> parse (fmap g px) out )

-- In this case, pure transforms a value into a parser that always succeeds
-- with this value as its result, without consuming any of the input string:

-- > parse (pure 1) "abc"
-- [(1,"abc")]
--                          215

-- In turn, <*> applies a parser that returns a function to a parser that returns
-- an argument to give a parser that returns the result of applying the
-- function to the argument, and only succeeds if all the components
-- succeed. For example, a parser that consumes three characters, discards
-- the second, and returns the first and third as a pair can now be defined in
-- applicative style :


three :: Parser (Char,Char)
three = pure g <*> item <*> item <*> item
    where g x _ z = (x,z)

-- Then, for example, we have:

-- > parse three "abcdef"
-- [((’a’,’c’),"def")]
-- > parse three "ab"
-- []

-- Note that the applicative machinery automatically ensures that the above
-- parser fails if the input string is too short, without the need to detect or
-- manage this ourselves. Finally, we make the Parser type into a monad:

instance Monad Parser where
    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    p >>= f = P (\inp -> case parse p inp of
                            []          -> []
--                             [(v,out)]   -> parse (f v) out)
                            (v,out):_   -> parse (f v) out)

-- That is, the parser p >>= f fails if the application of the parser p to the
-- input string inp fails, and otherwise applies the function f to the result
-- value v to give another parser f v, which is then applied to the output
-- string out that was produced by the first parser to give the final result.
-- Because Parser is a monadic type, the do notation can now be used to
-- sequence parsers and process their result values. For example, the parser
-- three can be defined in an alternative manner as follows:

three2 :: Parser (Char,Char)
three2 = do
            x <- item
            _ <- item
            z <- item
            return (x,z)

-- > parse three2 "abcdef"
-- [((’a’,’c’),"def")]
-- > parse three2 "ab"
-- []


-- Recall that the monadic function return is just another name for the
-- applicative function pure, which in this case builds parsers that always
-- succeed.
--                             216
---------------------------------------------------------------------------






-- For the remainder of this chapter we adopt a monadic approach to
-- writing parsers using the do notation, and generally avoid using the the
-- functorial fmap and applicative <*> primitives on parsers. However,
-- some users prefer writing parsers in applicative style, and using an
-- applicative approach can sometimes be beneficial for optimising the
-- performance of parsers.


-- 13.5 Making choices
-- The do notation combines parsers in sequence, with the output string
-- from each parser in the sequence becoming the input string for the next.
-- Another natural way of combining parsers is to apply one parser to the
-- input string, and if this fails to then apply another to the same input
-- instead. We now consider how such a choice operator can be defined for
-- parsers.
-- Making a choice between two alternatives isn’t specific to parsers, but
-- can be generalised to a range of applicative types. This concept is
-- captured by the following class declaration in the library
-- Control.Applicative:
--     class Applicative f => Alternative f where
--     empty :: f a
--     (<|>) :: f a -> f a -> f a
--
-- That is, for an applicative functor to be an instance of the Alternative
-- class, it must support empty and <|> primitives of the specified types.
-- (The class also provides two further primitives, which will be discussed
-- in the next section.) The intuition is that empty represents an alternative
-- that has failed, and <|> is an appropriate choice operator for the type.
-- The two primitives are also required to satisfy the following identity and
-- associativity laws:
-- The motivating example of an Alternative type is the Maybe type, for
-- which empty is given by the failure value Nothing, and <|> returns its
-- first argument if this succeeds, and its second argument otherwise:
--     instance Alternative Maybe where
--
--                            217
--------------------------------------------------------------------------
--
--     -- empty :: Maybe a
--     empty = Nothing
--     -- (<|>) :: Maybe a -> Maybe a -> Maybe a
--     Nothing <|> my = my
--     (Just x) <|> _ = Just x
--
-- The instance for the Parser type is a natural extension of this idea,
-- where empty is the parser that always fails regardless of the input string,
-- and <|> is a choice operator that returns the result of the first parser if it
-- succeeds on the input, and applies the second parser to the same input
-- otherwise:
instance Alternative Parser where
    -- empty :: Parser a
    empty = P (\inp -> [])
    --
    -- (<|>) :: Parser a -> Parser a -> Parser a
    p <|> q = P (\inp -> case parse p inp of
                        [] -> parse q inp
                        [(v,out)] -> [(v,out)])

-- For example:
-- > parse empty "abc"
-- []
-- > parse (item <|> return ’d’) "abc"
-- [(’a’,"bc")]
-- > parse (empty <|> return ’d’) "abc"
-- [(’d’,"abc")]
--
-- We conclude by noting that the library file Control.Monad provides a
-- class MonadPlus that plays the same role as Alternative but for
-- monadic types, with primitives called mzero and mplus. However, we
-- prefer to use the applicative choice primitives empty and <|> for parsers
-- because of their similarity to the corresponding symbols for grammars,
-- which we discuss later on.







-- 13.6 Derived primitives
-- We now have three basic parsers: item consumes a single character if the
-- input string is non-empty, return v always succeeds with the result
-- value v, and empty always fails. In combination with sequencing and
--
--                                 218
---------------------------------------------------------------------

-- choice, these primitives can be used to define a number of other useful
-- parsers. First of all, we define a parser sat p for single characters that
-- satisfy the predicate p:

sat :: (Char -> Bool) -> Parser Char
sat p = do
        x <- item
        if p x then return x else empty

-- Using sat and appropriate predicates from the library Data.Char, we
-- can now define parsers for single digits, lower-case letters, upper-case
-- letters, arbitrary letters, alphanumeric characters, and specific characters:
--

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (== x)

-- For example:
-- > parse (char ’a’) "abc"
-- [(’a’,"bc")]
--
-- In turn, using char we can define a parser string xs for the string of
-- characters xs, with the string itself returned as the result value:

string :: String -> Parser String
string [] = return []
string (x:xs) = do
                char x
                string xs
                return (x:xs)
--

-- That is, the empty string can always be parsed, while for a non-empty
-- string we parse the first character, recursively parse the remaining
-- characters, and return the string as the result value. Note that string
--
--                                 219



-- HOY:
--import L0sintaxis

-- item :: Parser Char
-- item = P (\inp -> case inp of
--     []      -> [] -- el parser falla
--     (x:xs)  -> [(x,xs)])

parserVx :: Parser Var
parserVx = P (\inp -> case inp of
    []      -> [] -- el parser falla
    (c:cs)  -> if c=='x'
                  then [(Vx, cs)]
                  else []
    )


--Tests:
-- > parse parserVx ""
--     []
-- > parse parserVx "xbc"
--     [(’a’,"bc")]
-- > parse parserVx "abc"
--     [(’a’,"bc")]



parserVy :: Parser Var
parserVy = P (\inp -> case inp of
    []      -> [] -- el parser falla
    (c:cs)  -> if c=='y'
                  then [(Vy, cs)]
                  else []
    )



parserVar :: Parser Var
parserVar = parserVx <|> parserVy -- <|> parserVz





{- XXXXXXXXXX
---------------------------------------------------------------------

-- only succeeds if the entire target string is consumed from the input. For
-- example:
--     > parse (string "abc") "abcdef"
--     [("abc","def")]
--     > parse (string "abc") "ab1234"
--     []
-- Our next two parsers, many p and some p, apply a parser p as many
-- times as possible until it fails, with the result values from each successful
-- application of p being returned in a list. The difference between these
-- two repetition primitives is that many permits zero or more applications
-- of p, whereas some requires at least one successful application. For
-- example:
--
--     > parse (many digit) "123abc"
--     [("123","abc")]
--     > parse (many digit) "abc"
--     [("","abc")]
--     > parse (some digit) "abc"
--     []
-- In fact, there is no need to define many and some ourselves, as suitable
-- default definitions are already provided in the Alternative class:
--     class Applicative f => Alternative f where
--     empty :: f a
--     (<|>) :: f a -> f a -> f a
--     many :: f a -> f [a]
--     some :: f a -> f [a]
--

many x = some x <|> pure []
some x = pure (:) <*> x <*> many x
--

-- Note that the two new functions are defined using mutual recursion. In
-- particular, the above definition for many x states that x can either be
-- applied at least once or not at all, while the definition for some x states
-- that x can be applied once and then zero or more times, with the results
-- being returned in a list. These functions are provided for any applicative
-- type that is an instance of the class, but are primarily intended for use
-- with parsers.
-- Using many and some, we can now define parsers for identifiers
-- (variable names) comprising a lower-case letter followed by zero or
--
--                             220
---------------------------------------------------------------------

-- more alphanumeric characters, natural numbers comprising one or more
-- digits, and spacing comprising zero or more space, tab, and newline
-- characters:

ident :: Parser String
ident = do
        x <- lower
        xs <- many alphanum
        return (x:xs)

nat :: Parser Int
nat = do
    xs <- some digit
    return (read xs)

space :: Parser ()
space = do
        many (sat isSpace)
        return ()

-- For example:
--     > parse ident "abc def"
--     [("abc"," def")]
--     > parse nat "123 abc"
--     [(123," abc")]
--     > parse space " abc"
--     [((),"abc")]

-- Note that nat converts the number that was read into an integer, and
-- space returns the empty tuple () as a dummy result value, reflecting the
-- fact that the details of spacing are not usually important. Finally, using
-- nat it is now straightforward to define a parser for integer values:

int :: Parser Int
int =   do
        char '-'
        n <- nat
        return (-n)
    <|> nat

-- For example:
-- > parse int "-123 abc"
-- [(-123," abc")]



-- 13.7 Handling spacing
--
--                          221
---------------------------------------------------------------------
---------------------------------------------------------------------
---------------------------------------------------------------------
-}
