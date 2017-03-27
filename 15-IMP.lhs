Module 15: IMP
==============

* Write your team names here:

In this module we are going to explore a simple imperative language
called IMP.

**This module is due Tuesday, November 8, at 1:15pm.**

Source 1: https://en.wikipedia.org/wiki/Euclidean_algorithm
Source 2: https://hackage.haskell.org/package/base-4.9.0.0/docs/Control-Applicative.html

The IMP language
----------------

The syntax of the language is as follows:

```
<prog> ::= <stmt> [ ';' <stmt> ]*

<stmt> ::=
  | <type> <var>
  | <var> ':=' <expr>
  | '{' <prog> '}'
  | 'if' <expr> 'then' <stmt> 'else' <stmt>
  | 'repeat' <expr> <stmt>
  | 'while' <expr> <stmt>
  | 'input' <var>
  | 'output' <expr>

<type> ::= 'int' | 'bool'

<expr> ::=
  | <int>
  | 'False' | 'True'
  | <var>
  | <uop> <expr>
  | <expr> <bop> <expr>

<uop> ::= '-' | '!'

<bop> ::= '+' | '-' | '*' | '/' | '&&' | '||' | '<' | '=='
```

Notice that the syntax is separated into *expressions* and
*statements*.  The difference is that

* **expressions** can be **evaluated**, and result in a **value** (*e.g.* an integer), whereas

* **statements** can be **executed**, and result in an **effect**
    (*e.g.* modifying some variables or printing some output).

Most imperative languages have this distinction between expressions
and statements (though some blur the line quite a bit).

Note that a `<prog>` consists of a sequence of statements, *separated
by* (not ended by) semicolons.  A compound statement can be created by
surrounding a `<prog>` with curly braces.

Here is an example of a simple IMP program, which reads an integer
from the user and then counts from 1 up to the integer, printing the
values to the screen:

```
int max; int i;
input max;
i := 0;
while i < max {
  i := i + 1;
  output i
}
```

In order to focus on the parts that are interesting and different,
this week I have provided you with some starter code.  First, some
imports we will need.

> {-# LANGUAGE GADTs        #-}
>
> import           Parsing2
>
> import qualified Data.Map           as M
> import           Text.Read          (readMaybe)
> import           System.Environment (getArgs)

We now define algebraic data types for the abstract syntax of IMP.
Note that we have two separate types, one for statements and one for
expressions.

> type Var = String
>
> type Prog = [Stmt]
>
> data Type where
>   TyInt  :: Type
>   TyBool :: Type
>   deriving (Show, Eq)
>
> data Stmt where
>   Decl   :: Type -> Var -> Stmt           -- <type> <var>
>   Assign :: Var  -> Expr -> Stmt
>   Inc    :: Var -> Expr  -> Stmt         -- <var> ':=' <expr>
>   Block  :: Prog -> Stmt                  -- '{' <prog> '}'
>   If     :: Expr -> Stmt -> Stmt -> Stmt  -- 'if' <expr> 'then' <stmt> 'else' <stmt>
>   Repeat :: Expr -> Stmt -> Stmt          -- 'repeat' <expr> <stmt>
>   While  :: Expr -> Stmt -> Stmt          -- 'while' <expr> <stmt>
>   Input  :: Var  -> Stmt                  -- 'input' <var>
>   Output :: Expr -> Stmt                  -- 'output' <expr>
>   deriving Show
>
> data Expr where
>   EInt  :: Integer -> Expr                -- <int>
>   EBool :: Bool    -> Expr                -- 'False' | 'True'
>   EVar  :: Var -> Expr                    -- <var>
>   EUn   :: UOp -> Expr -> Expr            -- <uop> <expr>
>   EBin  :: BOp -> Expr -> Expr -> Expr    -- <expr> <bop> <expr>
>   deriving Show
>
> data UOp = Neg | Not
>   deriving (Show, Eq)
>
> data BOp = Add | Sub | Mul | Div | And | Or | Equals | Less
>   deriving (Show, Eq)

Parser
------

Now, a parser for IMP.  You are welcome to skim through it, but there's
nothing really surprising going on.

> lexer :: TokenParser u
> lexer = makeTokenParser $
>   emptyDef
>   { reservedNames   = [ "True", "False", "if", "then", "else", "begin", "end"
>                         , "repeat", "while", "input", "output", "int", "bool" ]
>   , reservedOpNames = [ ":=", "==", "<", "+", "-", "*", "!", "&&", "||" , "+=" ]
>   }
>
> parens :: Parser a -> Parser a
> parens = getParens lexer
>
> reserved, reservedOp :: String -> Parser ()
> reserved   = getReserved lexer
> reservedOp = getReservedOp lexer
>
> symbol :: String -> Parser String
> symbol = getSymbol lexer
>
> ident :: Parser String
> ident = getIdentifier lexer
>
> integer :: Parser Integer
> integer = getInteger lexer
>
> whiteSpace :: Parser ()
> whiteSpace = getWhiteSpace lexer
>
> parseAtom :: Parser Expr
> parseAtom
>   =   EInt        <$> integer
>   <|> EBool True  <$  reserved "True"
>   <|> EBool False <$  reserved "False"
>   <|> EVar        <$> ident
>   <|> parens parseExpr
>
> parseExpr :: Parser Expr
> parseExpr = buildExpressionParser table parseAtom
>   where
>     table = [ [ unary  "!"  (EUn Not) ]
>             , [ unary  "-"  (EUn Neg) ]
>             , [ binary "*"  (EBin Mul)    AssocLeft
>               , binary "/"  (EBin Div)    AssocLeft ]
>             , [ binary "+"  (EBin Add)    AssocLeft
>               , binary "-"  (EBin Sub)    AssocLeft
>               ]
>             , [ binary "==" (EBin Equals) AssocNone
>               , binary "<"  (EBin Less)   AssocNone
>               ]
>             , [ binary "&&" (EBin And)    AssocRight ]
>             , [ binary "||" (EBin Or)     AssocRight ]
>             ]
>     unary  name fun       = Prefix (fun <$ reservedOp name)
>     binary name fun assoc = Infix  (fun <$ reservedOp name) assoc
>
> parseProg :: Parser Prog
> parseProg = parseStmt `sepBy` (reservedOp ";")
>
> parseStmt :: Parser Stmt
> parseStmt =
>       parseBlock
>   <|> If      <$> (reserved "if" *> parseExpr)
>               <*> (reserved "then" *> parseStmt)
>               <*> (reserved "else" *> parseStmt)
>   <|> Repeat  <$> (reserved "repeat" *> parseExpr) <*> parseBlock
>   <|> While   <$> (reserved "while" *> parseExpr)  <*> parseBlock
>   <|> Input   <$> (reserved "input" *> ident)
>   <|> Output  <$> (reserved "output" *> parseExpr)
>   <|> try(Assign  <$> ident <*> (reservedOp ":=" *> parseExpr))
>   <|> try(Inc     <$> ident <*> (reservedOp "+=" *> parseExpr))
>   <|> Decl    <$> parseType <*> ident
>
> parseType :: Parser Type
> parseType = (TyInt <$ reserved "int") <|> (TyBool <$ reserved "bool")
>
> parseBlock :: Parser Stmt
> parseBlock = Block  <$> (symbol "{" *> parseProg <* symbol "}")
>
> impParser :: Parser Prog
> impParser = whiteSpace *> parseProg <* eof

Type checker
------------

Next, a static type checker.  First, some errors:

> data TypeError where
>   DuplicateVar :: Var -> TypeError
>   UnboundVar   :: Var  -> TypeError
>   Mismatch     :: Expr -> Type -> Type -> TypeError
>   InputBool    :: Var  -> TypeError
>   deriving Show

We can infer/check the type of expressions in exactly the same way as
usual.

> type Ctx = M.Map Var Type
>
> infer :: Ctx -> Expr -> Either TypeError Type
> infer _   (EInt _)        = Right TyInt
> infer _   (EBool _)       = Right TyBool
> infer ctx (EVar x)        =
>   case M.lookup x ctx of
>     Nothing -> Left $ UnboundVar x
>     Just ty -> Right ty
> infer ctx (EBin op e1 e2) = inferBin ctx op e1 e2
> infer ctx (EUn op e)      = inferUn ctx op e
>
> inferBin :: Ctx -> BOp -> Expr -> Expr -> Either TypeError Type
> inferBin ctx op e1 e2 =
>   case binTy op of
>     (ty1, ty2, tyOut) ->
>       check ctx e1 ty1 *>
>       check ctx e2 ty2 *>
>       Right tyOut
>
> binTy :: BOp -> (Type, Type, Type)
> binTy op
>   | op `elem` [Add, Sub, Mul, Div] = (TyInt, TyInt, TyInt)
>   | op `elem` [And, Or]            = (TyBool, TyBool, TyBool)
>   | op `elem` [Equals, Less]       = (TyInt, TyInt, TyBool)
>   | otherwise                      = error "Unhandled operator in binTy"
>
> inferUn :: Ctx -> UOp -> Expr -> Either TypeError Type
> inferUn ctx op e =
>   case unTy op of
>     (tyIn, tyOut) ->
>       check ctx e tyIn *>
>       Right tyOut
>
> unTy :: UOp -> (Type, Type)
> unTy Neg = (TyInt, TyInt)
> unTy Not = (TyBool, TyBool)
>
> check :: Ctx -> Expr -> Type -> Either TypeError ()
> check ctx e ty =
>   infer ctx e >>= \ty' ->
>   case ty == ty' of
>     False -> Left $ Mismatch e ty ty'
>     True  -> Right ()

For checking programs, we just go through and check each statement.
The interesting difference is that because statements can *create*
variables which are in scope for the rest of the program, both
`checkProg` and `checkStmt` take a context as an argument and
**return a new context as output**.

* Complete the definition of `checkProg` below.  It should just call
  `checkStmt` to check an individual statement (and call itself
  recursively to check the rest).  Be careful about which
  context is used where!  For example, think about the program
    ```
    int a;
    a := 5;
    ```

    This should type check, because the statement `int a` creates a
    context in which `a` has type `Int`, and the statement `a := 5` should
    be checked in this new context.

> checkProg :: Ctx -> Prog -> Either TypeError Ctx
> checkProg ctx []     = Right ctx
> checkProg ctx (s:ss) = checkStmt ctx s >>= \ctx2 -> checkProg ctx2 ss

And now for `checkStmt`, which checks an individual statement.  Fill
in all the `undefined` places below!

> checkStmt :: Ctx -> Stmt -> Either TypeError Ctx

* To check a declaration (*e.g.* `int x`), first make sure the
  variable is not already in the context (throw a `DuplicateVar` error
  if it is); otherwise, insert the new variable into the context with
  the given type.

> checkStmt ctx (Decl ty x)  = case M.lookup x ctx of
>      Just _ -> Left (DuplicateVar x)
>      Nothing -> Right(M.insert x ty ctx)

* To check an assignment (`x := expr`), make sure the variable is in
  the context, and then `check` that the expression has the right type.

> checkStmt ctx (Assign x e) = case M.lookup x ctx of
>      Just t2 -> check ctx e t2 >> Right ctx
>      Nothing -> Left (UnboundVar x)

> checkStmt ctx (Inc x e) = case M.lookup x ctx of
>      Just t2 -> check ctx e t2 >> Right ctx
>      Nothing -> Left (UnboundVar x)

Now we come to checking blocks of the form `{ <prog> }`.  In one
sense, this is easy, since we can just call `checkProg`.  However,
there is one subtle thing to think about.  Here are two possible
different implementations of this case, call them A and B:

```
(A) checkStmt ctx (Block ss)   = checkProg ctx ss >>= \ctx' -> Right ctx'

(B) checkStmt ctx (Block ss)   = checkProg ctx ss *> Right ctx
```

* What is the difference between these two implementations?
A returns the updated context, whereas B returns the original context.

* Consider the following IMP program:
    ```
    {
      int x;
      x := 2
    };
    output x
    ```
    Will this program typecheck given implementation (A)?
    What about implementation (B)?
It will typecheck with A, but not with B.
* Which implementation corresponds to the way Java works?  Fill in
  that implementation below: Java corresponds with implementation B

> checkStmt ctx (Block ss)   = checkProg ctx ss *> Right ctx

Checking `if`, `repeat`, and `while` is straightforward.  Note that we
take a similar approach to contexts as we did for blocks above.  Take
a look at the implementations of `if` and `repeat`, then fill in the
implementation for `while`.

> checkStmt ctx (If e s1 s2) =
>   check ctx e TyBool *>
>   checkStmt ctx s1 *>
>   checkStmt ctx s2 *>
>   Right ctx
> checkStmt ctx (Repeat e body) =
>   check ctx e TyInt *>
>   checkStmt ctx body *>
>   Right ctx
> checkStmt ctx (While e body)  =
>   check ctx e TyBool *>
>   checkStmt ctx body *>
>   Right ctx

Checking `input` and `output` statements is straightforward: we can
only `input` and `output` variables with type `int`.

> checkStmt ctx (Input v)    =
>   case M.lookup v ctx of
>     Nothing    -> Left $ UnboundVar v
>     Just TyInt -> Right ctx
>     Just _     -> Left $ InputBool v
> checkStmt ctx (Output e)   =
>   check ctx e TyInt *> Right ctx

![](../images/stop.gif)

An IMPterpreter
---------------

* **ROTATE ROLES** and write the name of the new driver here:
Collin

Let's define a `Value` to just be an `Integer`. As usual, if
everything has type checked successfully, we can use `Integer` to
represent both integers and booleans, without worrying about
nonsensical operations.

> type Value = Integer

A "memory" is a mapping from variable names to values.  In the past we
have called this an "environment", but we use the name "memory" now to
emphasize the fact that it keeps track of the values of *mutable*
variables, which can be changed by assignment statements.

> type Mem = M.Map Var Value

We interpret expressions as usual.  There's nothing very interesting
to see here.

> interpExpr :: Mem -> Expr -> Value
> interpExpr _ (EInt i)       = i
> interpExpr _ (EBool b)      = fromBool b
> interpExpr m (EVar x)       =
>   case M.lookup x m of
>     Just v  -> v
>     Nothing -> error $ "Impossible! Uninitialized variable " ++ x
> interpExpr m (EBin b e1 e2) = interpBOp b (interpExpr m e1) (interpExpr m e2)
> interpExpr m (EUn  u e)     = interpUOp u (interpExpr m e )
>
> interpUOp :: UOp -> Value -> Value
> interpUOp Neg v = -v
> interpUOp Not v = 1-v
>
> interpBOp :: BOp -> Value -> Value -> Value
> interpBOp Add    = (+)
> interpBOp Sub    = (-)
> interpBOp Mul    = (*)
> interpBOp Div    = div
> interpBOp And    = (*)
> interpBOp Or     = \v1 v2 -> min 1 (v1 + v2)
> interpBOp Equals = \v1 v2 -> fromBool (v1 == v2)
> interpBOp Less   = \v1 v2 -> fromBool (v1 <  v2)
>
> fromBool :: Bool -> Value
> fromBool False = 0
> fromBool True  = 1

OK, now we have dealt with expressions.  But **how do we interpret
statements**?

* Our first instinct might be to write a function of type `interpStmt ::
  Mem -> Stmt -> Value`.  Explain why this will not work.
Statements mutate data somehow, and do not necessarily only return a value. We
need to encapsulate the changes done by a statement in its return type.

Remember that an interpreter turns **syntax** into **semantics**, that
is, it takes an abstract syntax tree and produces its *meaning*.  So
the question to ask ourselves is: what is the meaning of a statement?

The meaning of a statement is an *effect*: a statement changes the
"state of the world" in some way.  We can model this as a *function*
which takes the current state of the world and produces a new state.
That is, an interpreter for statements will have the type `Stmt ->
(World -> World)` for some appropriate type `World`.

> data World where
>   W  :: Mem       -- Current state of memory
>      -> [String]  -- Strings typed by the user, waiting to be read by 'input'
>      -> [String]  -- Strings produced by 'output' (newest first)
>      -> World
>   Error :: World  -- Something went wrong
>   deriving Show
>
> -- An initial world state, given user input
> initWorld :: String -> World
> initWorld inp = W M.empty (words inp) []

* Fill in the definition of `interpStmt` below.  You may want to define
  helper functions like `interpRepeat` and `interpProg`; feel free to define
  other helper functions as you see fit.  Note to interpret `input`
  statements, you can use `readMaybe :: String -> Maybe Integer` to
  check whether the user input is valid integer; if not, produce
  `Error`.

> interpStmt :: Stmt -> World -> World
> interpStmt (Decl _ _) w              = w
> interpStmt (Assign v e) (W m i o)    = W (M.insert v (interpExpr m e) m) i o
> interpStmt (Inc v e) (W m i o)       = case M.lookup v m of
>                                        Just n -> W (M.insert v (n +(interpExpr m e)) m) i o
>                                        Nothing -> Error
> interpStmt (Block p) w               = interpProg p w
> interpStmt (If e s1 s2) w@(W m _ _)    = case interpExpr m e of
>                                         1 -> interpStmt s1 w
>                                         0 -> interpStmt s2 w
>                                         _ -> error "Should be impossible, in if"
> interpStmt (Repeat e s) w@(W m _ _)  =  interpRepeat (interpExpr m e) s w
> interpStmt (Input _)  (W _ [] _)     = Error
> interpStmt (Input v)  (W m (i:is) o) = case readMaybe i of
>                                         Just int -> W (M.insert v int m) is o
>                                         Nothing  -> Error
> interpStmt (Output e) (W m i o)      = W m i ((show $ interpExpr m e) : o)
> interpStmt while@(While e st1)  w@(W m i o)  = case interpExpr m e of
>                                         1 -> interpStmt while (interpStmt st1 w)
>                                         0 -> w


    > data Stmt where
    >   Decl   :: Type -> Var -> Stmt           -- <type> <var>
    >   Assign :: Var  -> Expr -> Stmt          -- <var> ':=' <expr>
    >   Block  :: Prog -> Stmt                  -- '{' <prog> '}'
    >   If     :: Expr -> Stmt -> Stmt -> Stmt  -- 'if' <expr> 'then' <stmt> 'else' <stmt>
    >   Repeat :: Expr -> Stmt -> Stmt          -- 'repeat' <expr> <stmt>
    >   While  :: Expr -> Stmt -> Stmt          -- 'while' <expr> <stmt>
    >   Input  :: Var  -> Stmt                  -- 'input' <var>
    >   Output :: Expr -> Stmt                  -- 'output' <expr>

> interpRepeat :: Integer -> Stmt -> World -> World
> interpRepeat n s w
>    | n > 0     = interpRepeat (n-1) s (interpStmt s w)
>    | otherwise = w

> interpProg :: Prog -> World -> World
> interpProg [] w      = w
> interpProg (p:ps) w  = interpProg ps (interpStmt p w)

![](../images/green.png)

Programming in IMP
------------------

* **ROTATE ROLES** and write the name of the new driver here:
  Joseph
At this point, you should be able to compile this module (`ghc --make
15-IMP.lhs`) and then run it on input files containing IMP programs.

* Save this example program into a file named `count.imp` and run your
  IMP interpreter on it:
    ```
    int max; int i;
    input max;
    i := 0;
    while i < max {
      i += 1;
      output i
    }
    ```

* Write an IMP program to compute the factorial of a number entered by
  the user.

  ```
  int start; int fac;
  input start;
  fac := start;
  while (1 < start) {
    start += - 1
    fac := fac * (start)
    output fac
  }
  ```

* Write an IMP program to compute the GCD of two numbers entered by
  the user.

  ```
  int first; int sec;
  input first;
  input sec;
  int r;
  while (0 < sec) {
    r := first - sec * (first / sec);
    first := sec;
    sec := r
  };
  output first
  ```

* Write an IMP program to print out all the primes up to the number
  entered by the user.

  ```
  int max;
  input max;
  int n; int b; int m; int r;
  n := 2;
  while (n < max) {
    b := 1;
    m := 2;
    while (m < n) {
      r := n - m * (n / m);
      if (r == 0) then b := 0 else b := b;
      m += 1
    };
    if b == 1 then output n else b := b;
    n += 1
  }
  ```

You should feel free to write other example IMP programs to test your
implementation as well.

![](../images/green.png)

Extending IMP
-------------

* **ROTATE ROLES** and write the name of the new driver here:
Bosco

* Name one things you found particularly annoying about writing IMP
  programs in the previous section.

  Incrementation is clunky.

* Fix it!  Add a new feature to IMP to address your annoyance.
  Fixed! Now there is "+=" incrementation syntax.
* Rewrite your example programs to make use of your new feature.

Feedback
--------

* How long would you estimate that you spent working on this module?
4 hours
* Were any parts particularly confusing or difficult?
It was tricky to understand Worlds at first.
* Were any parts particularly fun or interesting?
Writing IMP programs was fun.
* Record here any other questions, comments, or suggestions for
  improvement.

Some extra definitions (feel free to ignore)
--------------------------------------------

> formatWorld :: World -> String
> formatWorld (W m _ o) = unlines $
>      reverse o
>   ++ ["-----"]
>   ++ map formatVar (M.assocs m)
> formatWorld Error = "Error"
>
> formatVar (x,v) = x ++ " -> " ++ show v
>
> run :: String -> IO ()
> run fileName = do
>   s <- readFile fileName
>   case parse impParser s of
>     Left err -> print err
>     Right p  ->
>       case checkProg M.empty p of
>         Left tyErr -> print tyErr
>         Right _    -> do
>           inp <- getContents
>           let es = interpProg p (initWorld inp)
>           putStr $ formatWorld es
>
> main :: IO ()
> main = do
>   args <- getArgs
>   case args of
>     []     -> putStrLn "Please provide a file name."
>     (fn:_) -> run fn
