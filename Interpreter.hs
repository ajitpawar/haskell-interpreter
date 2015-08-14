{- Assignment 2 - A Racket Interpreter

This module is the main program for the interpreter.
All of your work should go into this file.

We have provided a skeleton interpreter which can be run
successfully on sample.rkt; it is your job to extend this
program to handle the full range of Paddle.

In the space below, please list your group member(s):
Taher Jafferjee, g4jaffer
Ajit Pawar, g4pawar
-}

module Interpreter (main) where

import BaseParser (BaseExpr(LiteralInt, LiteralBool, Atom, Compound), parseFile)
import Data.List
import System.Environment (getArgs)


-- |Run interpreter on an input file,
--  either from commandline or user input.
--  You should not need to change this function.
main :: IO ()
main =
    getArgs >>= \args ->
    if length args > 0
    then
        parseFile (head args) >>= \baseTree ->
        putStr (interpretPaddle baseTree)
    else
        putStrLn "Enter the name of a file: " >>
        getLine >>= \file ->
        parseFile file >>= \baseTree ->
        putStr (interpretPaddle baseTree)


-- |Take the output of the base parser and interpret it,
--  first constructing the AST, then evaluating it,
--  and finally returning string representations of the results.
--  You will need to make this function more robust against errors.

--interpretFunc :: BaseExpr -> Expr
interpretFunc init (LiteralIdent x y) =
    let
    defs = init!!0
    parsed = init!!1
    newDef = (evaluate defs (LiteralIdent x y))
    in [newDef, parsed]

interpretFunc init x =
    let
    defs = init!!0
    (List parsed) = init!!1
    newParsed = List ((evaluate defs x):parsed)
    in [defs, newParsed]

nameErrorCheck [] =
    "Clean"

nameErrorCheck (NameError:rest) =
    "NameError"

nameErrorCheck (_:rest) =
    (nameErrorCheck rest)

syntaxErrorCheck [] =
    "Clean"

syntaxErrorCheck (SyntaxError:rest) =
    "SyntaxError"

syntaxErrorCheck (_:rest) =
    (syntaxErrorCheck rest)

typeErrorCheck [] =
    "Clean"

typeErrorCheck (TypeError:rest) =
    "TypeError"

typeErrorCheck (_:rest) =
    (typeErrorCheck rest)

getUptoTypeError (TypeError:xs) = [TypeError]
getUptoTypeError (x:xs) = x:(getUptoTypeError xs)

interpretPaddle :: Maybe [BaseExpr] -> String
interpretPaddle Nothing =
    "SyntaxError\n"
interpretPaddle (Just exprs) =
    let ast = map parseExpr exprs
        syntaxError = syntaxErrorCheck ast
    in
    if (syntaxError == "SyntaxError")
        then "SyntaxError\n"
        else --vals = map (evaluate (LiteralIdent [] [])) ast
            let foldedVals = foldl (\init x -> (interpretFunc init x)) ([(LiteralIdent [] []), (List [])]) ast
                (List vals) = foldedVals!!1
                -- String representations of each value, joined with newlines
                nameError = nameErrorCheck vals
                syntaxError = syntaxErrorCheck vals
                typeError = typeErrorCheck vals
            in
            if (syntaxError == "SyntaxError")
                then "SyntaxError\n"
                else if (nameError == "NameError")
                    then "NameError\n"
                    else if (typeError == "TypeError")
                        then
                            let lstUptoTypeError = getUptoTypeError (reverse vals)
                            in unlines (map show lstUptoTypeError)
                        else
                            unlines (map show (reverse vals))
          {-  if (nameError == "NameError")
                then "NameError\n"
                else
                    let syntaxError = syntaxErrorCheck vals
                    in
                    if (syntaxError == "SyntaxError")
                        then "SyntaxError\n"
                        else
                            let typeError = typeErrorCheck vals
                            in
                            if (typeError == "TypeError")
                            then
                                let lstUptoTypeError = getUptoTypeError (reverse vals)
                                in unlines (map show lstUptoTypeError)
                            else unlines (map show (reverse vals))-}

{- To do:
    1) Support for error checking
    2) Eager evaluation
-}

-- An expression data type
data Expr = Number Integer |
            Boolean Bool |
            If Expr Expr Expr |
            -- IMPLEMENTATION BEGINS
            ArithOp (Integer -> Integer -> Integer ) Expr Expr |
            NotOp (Bool -> Bool) Expr |
            CompOp Expr Expr |
            IneqOp Expr Expr |
            JuncOpCon Expr Expr |
            JuncOpDis Expr Expr |
            CondOp [(Expr, Expr)] |
            List [Expr] |
            LiteralIdent [String] [Expr] |
            AtomIdent String |
            Lambda [String] Expr [Expr] |
            FnCall Expr [Expr] |
            Let Expr Expr |
            NameError |
            SyntaxError |
            TypeError


showListHelper lst =
    map (\x -> (show x)) lst

showListHelperHelper (List lst) =
    let showedList = showListHelper lst
    in case showedList of
    [x] -> (x) ++ ")"
    _ -> (head showedList) ++ ", " ++ (showListHelperHelper (List (tail lst)))

instance Show Expr where
    show (Number x) = show x
    show (Boolean True) = "#t"
    show (Boolean False) = "#f"
    -- Note: the following definition is not necessary for this assignment,
    -- but you may find it helpful to define string representations of all
    -- expression forms.
    show (If e1 e2 e3) =
        "(if " ++ show e1 ++ " " ++ show e2 ++ " " ++ show e3 ++ ")"
    show (List []) =
        "'()"
    show (List lst) =
        let helperResult = (showListHelperHelper (List lst))
        in "'(" ++ helperResult
       -- show (map (\x -> (show x)) lst)
    show (LiteralIdent name expr) =
        (show (head expr))
    show (AtomIdent x) =
        (show x)
    show (Lambda _ _ _) =
        "#<procedure>"
    show (TypeError) =
        "TypeError"
    show (SyntaxError) =
        "SyntaxError"


-- |Take a base tree produced by the starter code,
--  and transform it into a proper AST.

parseExprHelper1 (Compound [Atom "else", expr]) =
    ((Boolean True), (parseExpr expr))

parseExprHelper1 (Compound (cond:res)) =
    ((parseExpr cond), (parseExpr (head res)))

parseExprHelper2 (first:rest) =
    (parseExprHelper1 first):(parseExprHelper2 rest)

parseLambdaHelper (Atom x) = x

parseLetHelper1 [] = []
parseLetHelper1 ((Compound (Atom name:value)):rest) =
    name:(parseLetHelper1 rest)

parseLetHelper2 [] = []
parseLetHelper2 ((Compound (Atom name:value)):rest) =
    (head value):(parseLetHelper2 rest)

parseExpr :: BaseExpr -> Expr
parseExpr (LiteralInt n) = Number n
parseExpr (LiteralBool b) = Boolean b
parseExpr (Compound [Atom "if", b, x, y]) =
    If (parseExpr b) (parseExpr x) (parseExpr y)
-- IMPLEMENTATION BEGINS
parseExpr (Compound [Atom "+", x, y]) =
    ArithOp (+) (parseExpr x) (parseExpr y)

parseExpr (Compound [Atom "*", x, y]) =
    ArithOp (*) (parseExpr x) (parseExpr y)

parseExpr (Compound [Atom "not", x]) =
    NotOp not (parseExpr x)

parseExpr (Compound [Atom "equal?", x, y]) =
    CompOp (parseExpr x) (parseExpr y)

parseExpr (Compound [Atom "<", x, y]) =
    IneqOp (parseExpr x) (parseExpr y)

parseExpr (Compound [Atom "and", x, y]) = JuncOpCon (parseExpr x) (parseExpr y)

parseExpr (Compound [Atom "or", x, y]) = JuncOpDis (parseExpr x) (parseExpr y)

parseExpr (Compound (Atom "cond":lst)) =
    (CondOp (parseExprHelper2 lst))

parseExpr (Compound (Atom "list":lst)) =
    List (map (\x -> (parseExpr x)) lst)

parseExpr (Compound (Atom "define":Atom x:expr)) =
    (LiteralIdent (x:[]) ((parseExpr (head expr)):[]))

parseExpr (Atom x) =
    (AtomIdent x)

-- (define g (lambda (y) (+ y 2)))
parseExpr (Compound (Atom "lambda":vars:expr)) =
    let (Compound variables) = vars
        varNames = map parseLambdaHelper variables
        parsedExp = parseExpr (head expr)
        parsedArgs = []
    in (Lambda varNames parsedExp parsedArgs)

-- ((lambda (x y) (+ x y)) 1 2)
parseExpr (Compound (Compound (Atom "lambda":vars:expr):args)) =
    let (Compound variables) = vars
        varNames = map parseLambdaHelper variables
        parsedExp = parseExpr (head expr)
        parsedArgs = map parseExpr args
    in (Lambda varNames parsedExp parsedArgs)

-- (define (f x) (+ x 1))
parseExpr (Compound (Atom "define":(Compound ((Atom fnName):vars)):expr)) =
    let varNames = map parseLambdaHelper vars
        parsedExp = parseExpr (head expr)
        parsedArgs = []
    in (LiteralIdent (fnName:[]) ((Lambda varNames parsedExp []):[]))

parseExpr (Compound (Atom "let":Compound binds:expr)) =
    let names = parseLetHelper1 binds
        expressions = parseLetHelper2 binds
        parsedExpressions = map parseExpr expressions
        parsedExpr = parseExpr (head expr)
    in (Let (LiteralIdent names parsedExpressions) parsedExpr)

parseExpr (Compound (Atom "let*":Compound binds:expr)) =
    let names = parseLetHelper1 binds
        expressions = parseLetHelper2 binds
        parsedExpressions = map parseExpr expressions
        parsedExpr = parseExpr (head expr)
    in (Let (LiteralIdent names parsedExpressions) parsedExpr)

parseExpr (Compound ((Atom fnName):lstOfArgs)) =
    (FnCall (parseExpr (Atom fnName)) (map parseExpr lstOfArgs))

parseExpr _ = SyntaxError

-- |Evaluate an AST by simplifying it into
--  a number, boolean, list, or function value.

findIndexOf literal [] num =
    (-1)

findIndexOf literal list num =
    if (literal == (head list))
    then num
    else (findIndexOf literal (tail list) (num + 1))

evaluate :: Expr -> Expr -> Expr
evaluate defined NameError = NameError
evaluate (LiteralIdent defNames defExpr) (LiteralIdent name expr) =
    (LiteralIdent ((head name):defNames) ((head expr):defExpr))   -- Add support for non-primitive define results
evaluate (LiteralIdent names exprs) (AtomIdent x) =
    let index = findIndexOf x names 0
    in case index of
        (-1) -> NameError
        _ -> exprs!!index

evaluate defined (Number n) = Number n
evaluate defined (Boolean b) = Boolean b
evaluate defined (If cond x y) =
    let condTruth = (evaluate defined cond)
        evaluatedX = (evaluate defined x)
        evaluatedY = (evaluate defined y)
    in case condTruth of
        Boolean True -> evaluatedX
        Boolean False -> evaluatedY
        NameError -> NameError
        _ -> TypeError
-- IMPLEMENTATION BEGINS
evaluate defined (NotOp not x) =
    case x of
        (AtomIdent y) -> let evaluated = (evaluate defined (AtomIdent y))
                         in case evaluated of
                            NameError -> NameError
                            (Boolean z) -> (Boolean (not z))
                            _ -> TypeError
        (Boolean z) -> (Boolean (not z))

evaluate defined (ArithOp arithOp x y) =
    case x of
        (Number a) -> case y of
                          (Number b) -> Number ((arithOp) a b)
                          (AtomIdent k) -> let evaluated = (evaluate defined (AtomIdent k))
                                   in case evaluated of
                                    (Number c) -> (Number ((arithOp) a c))
                                    (NameError) -> NameError
                                    _ -> TypeError
                          _ -> TypeError
        (AtomIdent d) -> let evaluated = (evaluate defined (AtomIdent d))
                         in case evaluated of
                            (NameError) -> NameError
                            (Number e) ->   case y of
                                            (Number f) -> Number ((arithOp) e f)
                                            NameError -> NameError
                                            (AtomIdent k) -> let evaluatedY = (evaluate defined (AtomIdent k))
                                                            in case evaluatedY of
                                                                (Number g) -> (Number ((arithOp) e g))
                                                                (NameError) -> NameError
                                                                _ -> TypeError
                            _ -> TypeError
        _ -> TypeError

evaluate defined (CompOp (Number x) (Number y)) = Boolean (x == y)
evaluate defined (CompOp (Boolean x) (Boolean y)) = Boolean (x == y)
evaluate defined (CompOp x y) =
    let evaluatedX = (evaluate defined x)
        evaluatedY = (evaluate defined y)
    in case evaluatedX of
        (Number a) -> case evaluatedY of
            (Number b) -> (evaluate defined (CompOp evaluatedX evaluatedY))
            NameError -> NameError
            _ -> TypeError
        (Boolean a) -> case evaluatedY of
            (Boolean b) -> (evaluate defined (CompOp evaluatedX evaluatedY))
            NameError -> NameError
            _ -> TypeError
        NameError -> NameError
        _ -> TypeError

evaluate defined (IneqOp (Number x) (Number y)) = Boolean (x < y)
evaluate defined (IneqOp x y) =
    let evaluatedX = (evaluate defined x)
        evaluatedY = (evaluate defined y)
    in case evaluatedX of
        (Number a) -> case evaluatedY of
            (Number b) -> (evaluate defined (IneqOp evaluatedX evaluatedY))
            NameError -> NameError
            _ -> TypeError
        NameError -> NameError
        _ -> TypeError

evaluate defined (JuncOpCon (Boolean x) (Boolean y)) = Boolean (x && y)
evaluate defined (JuncOpCon x y) =
    let evaluatedX = (evaluate defined x)
        evaluatedY = (evaluate defined y)
    in case evaluatedX of
        (Boolean a) -> case evaluatedY of
            (Boolean b) -> (evaluate defined (JuncOpCon evaluatedX evaluatedY))
            NameError -> NameError
            _ -> TypeError
        NameError -> NameError
        _ -> TypeError

evaluate defined (JuncOpDis (Boolean x) (Boolean y)) = Boolean (x || y)
evaluate defined (JuncOpDis x y) =
    let evaluatedX = (evaluate defined x)
        evaluatedY = (evaluate defined y)
    in case evaluatedX of
        (Boolean a) -> case evaluatedY of
            (Boolean b) -> (evaluate defined (JuncOpDis evaluatedX evaluatedY))
            NameError -> NameError
            _ -> TypeError
        NameError -> NameError
        _ -> TypeError

evaluate defined (CondOp ((cond, res):xs)) =
    let truth = (evaluate defined cond)
    in
    case truth of
        Boolean True -> (evaluate defined res)
        Boolean False -> (evaluate defined (CondOp xs))

evaluate defined (List lst) =
    List (map (\x -> evaluate defined x) lst)

evaluate defined (Lambda [] expr []) =
    evaluate defined expr

evaluate defined (Lambda vars expr args) =
    let definedToSave = defined
        (LiteralIdent globalNames globalVals) = defined
        localVars = LiteralIdent vars args
        newlyDefined = (LiteralIdent (globalNames++vars) (globalVals++args))
        result = evaluate newlyDefined expr
    in case result of
        _ -> evaluate definedToSave result

evaluate defined (FnCall call args) =
    let evaluated = evaluate defined call
    in case evaluated of
        (Lambda vars expr [])->(evaluate defined (Lambda vars expr args))
        _ -> SyntaxError

evaluate defined (Let (LiteralIdent names values) expr) =
    let (LiteralIdent globalNames globalVals) = defined
        newlyDefined = (LiteralIdent (globalNames++names) (globalVals++values))
        evaluatedExpr = evaluate newlyDefined expr
        result = evaluate newlyDefined evaluatedExpr
    in evaluate (LiteralIdent globalNames globalVals) result

evaluate defined SyntaxError = SyntaxError