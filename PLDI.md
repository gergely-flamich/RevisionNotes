Translation
======================================================================

Interpretation
 - easy to implement
 - small footprint
 - very slow execution
 
Compilation
 - hard to implement
 - large footprint
 - fast execution

Compilation Phases
----------------------------------------------------------------------
 1. Lex -> Tokens
 2. Parse -> A.S.
 3. Semantic Analysis -> A.S
 4. Simplification -> Int. Code
 5. Optimiser -> Int. Code
 6. Code gen. -> Assembly code
 7. Assembler -> Object code
 8. Linker -> Machine code
 

Interpretation Phases
----------------------------------------------------------------------
 1. Lex -> Tokens
 2. Parse -> A.S.
 3. Semantic Analysis -> A.S
 4. Software Interpreter


VM Interpretation
----------------------------------------------------------------------
 1. Lex -> Tokens
 2. Parse -> A.S.
 3. Semantic Analysis -> A.S
 4. Simplification -> Int. Code
 5. Optimiser -> Int. Code
 6. Code gen. -> VM code
 7. VM code interpreter (or just-in-time compilation)



Writing an Interpreter
----------------------------------------------------------------------
Rewrite expressions by calculating their value and replacing them with it.

type Op = String

data Expr = EInt Int
          | EFloat Float
          | EBool Bool
          | EOp Op Expr Expr
          | EOp1 Op Expr

eval :: Expr -> Expr
eval = ....

### Dealing with identifier

Environments:

type Id = String
type Env = [(Id, Expr)]

eval :: Env -> Expr -> Expr
eval env (EId id) = lookup id env
eval ...


Java Virtual Machine
======================================================================

JVM Types
----------------------------------------------------------------------

Scalars
 - short, int, long, char, float, double
 - no boolean
 - long and double need 2 words
 
Reference Types
 - references to class types, interface types, array types
 - null
 
Return Addresses
 - returnAddress (type of pointers to opcodes)


JVM Storage Types
----------------------------------------------------------------------

+--------------|------|--------------------+
| Storage Type | Size | Computational Type |
+--------------|------|--------------------+
| byte         |    8 | int                |
| char         |   16 | int                |
| short        |   16 | int                |
| int          |   32 | int                |
| long         |   64 | long               |
| float        |   32 | float              |
| double       |   64 | double             |
+--------------|------|--------------------+


JVM Memory Structure
----------------------------------------------------------------------
Global (shared)
 - Heap (dynamic allocation)
 - Method area (references to methods)
 - Nave method stack
 
Per-thread (unshared)
 - program counter (current instruction)
 - thread stack


JVM Stack
----------------------------------------------------------------------
Each thread has its own stack

### Stack Frames
 - created for method invocation
 - local vars + operand stack
 - only one active frame at a time
 - deactivated on completion or method call
 - has pointer to constant pool (into the method area)
 
### Local Variables
 - only one word each, long and double are two vars


JVM Fetch-Exec Cycle
----------------------------------------------------------------------

Without exceptions, basically we have:
```
do {
    fetch an opcode;
    if (operands) fetch operands;
    execute the action for the opcode;
} while (there is more to do);
```

JVM Opcodes
----------------------------------------------------------------------

Memory Operations
 - Load local variable ONTO stack: (i,l,f,d,a)load, (i,l,f,d,a)load_n
 - Store a value FROM stack: (i,l,f,d,a)store, (i,l,f,d,a)store_n
 - Load constant ONTO stack: bipush (bytes), sipush (short), ldc, ldc\_w, aconst\_null, (i, l, f, d)const_c
 
Arithmetic Operations
 - (i, l, f, d): add, sub, mul, div, rem, neg
 - Shift: ishl, ishr, iushr, ishl, lshr, lushr
 - Bitwise (i, l): or, and, xor
 - iinc
 
Object creation/ manipulation
 - new
 - newarray, anewarray, multianewarray
 - getfield, putfield, getstatic, putstatic
 - Load array component: (b, c, s, i, l, f, d, a)aload
 - Store value from operand stack into array: (b, c, s, i, l, f, d, a)astore
 - arraylength
 - instanceof, checkcast
 
### Coercion
Widening
 - i2l, i2f, i2d
 - l2f, l2d
 - f2d
 - bytes, shorts, chars are ints internally
 
Narrowing
 - i2b, i2c, i2s
 - l2i
 - f2i, f2l
 - d2i, d2l, d2f
 - may lose precision, change sign
 
Exceptions not raised by coercions

Stack Management
 - pop, pop2
 - dup, dup2
 - dup\_x1, dup2\_x1 (duplicate and swap with top element on the stack)
 - swap
 
Control Transfer
 - ifeq, iflt, ifne, ifnull, ifnonnull
 - ificmpeq, if
 - goto, jsr, ret
 
Method calls:
 - invokevirtual: for instances
 - invokeinterface: for interface methods
 - invokespecial: for constructors
 - invokestatic: for static methods
 
Returning:
 - (i, l, f, d, a, _)return
 
Exceptions:
 - athrow
 
if exception is thrown, control is immediately transferred to nearest catch.
Finally always executed

Table for handling catches:
 - range: from - to (addresses between which the catch is active)
 - exception type(s)
 - goto address
 
Synchronisation
 - monitorenter
 - monitorexit


Intermediate Representations (IR)
======================================================================

We can convert the AST straight to Assembly code.

This is very inefficient => use IR as target, and write code generators
for the IR:
 - makes code more portable
 - less work
 - simpler implementation for code generators
 

IR: Tree (tree structured representation)

R-values: appear on RHS only of an assignment
L-values: can appear on LHS also (l = r;)

L-value is a reference, R-value is a value
If L-value is on the RHS, it has to be dereferenced (MEM operation in Tree)

### Structured L-values
Arrays:
The address of the ith element can be calculated as (i - l) * s + a
where
 - l is the first index (usually 0)
 - s is the size of each element
 - a is the address of the array
 
Similarly for structs

### Tiling
Can come up with special tree constructs to optimise speed/generated code length

Optimality:
Globally optimal - lowest possible total tile sum
Locally optimal - no two adjacent tiles combine into one of lower cost

Maximal munch: Greedy tiling, starting from the root
Dynamic programming: 

Garbage Collection
======================================================================
Mutation: program running, allocation phase
Collection: reclamation of unused memory

GC: determine LIVE dynamic data, collect dead memory for re-allocation

Root pointers: direct reference to live data
 - global objects
 - objects on the stack
 - objects referred to by threads
 
 
GC Algorithms
----------------------------------------------------------------------

### Mark - Sweep
Allocate until we run out of free space

When we run out of free space, start reclamation cycle:
 * Mark roots (negative sign)
 * do a depth-first traversal from the roots, marking every reached data block
 * anything unmarked is garbage (sweep)

Cost: (c1 * R + c2 * H) / H - R

NOTE: As the heap fills up with more and more live data,
the time cost of the algorithm skyrockets, since the denominator decreases.

### 2 - Space Copying (Cheney's algorithm)
Heap divided into two areas:
 - FROM-SPACE: the half where current allocation is done
 - TO-SPACE: empty/free/unused area
 
GC:
 - Evacuation: copy roots from FROM-SPACE to the TO-SPACE, leaving forwarding 
 pointers in the FROM-SPACE to where they were copied.
 
 - Scavenging: following the pointers in the TO-SPACE, copy over live data
 - Flip spaces: Interchange the TO and FROM SPACES
 - anything in the new TO-SPACE is garbage
 
Cost: (c3 * R) / (H/2 - R)
also the cost of wasting 50% of the heap.

NOTE: can be much better performing time-wise than M-S

### Generational Collection
 - Heap divided into many spaces. 
 - Allocate into one of the spaces (youngest generation)
 - Roots are all roots from every generation.
 - Stuff surviving many collection cycles promoted to older generation
 - Older generations swept less frequently
 
Youngest generation usually 10% of live data, therefore:

Cost: ~ (c3 / 9)
 
### Reference Counting
 - Every time we add a reference to a memory block, increment its counter
 - if counter reaches 0, node is garbage.
 
Problems: 
 - cost of incrementing counts (must surrender extra space)
 - collecting cycles

### Incremental Collection
Real - time collection algorithm, when we cannot allow our program to stall more than a constant
amount of time.

Tricolour marking: 
 - white: unvisited nodes
 - grey: visited, but children not visited
 - black: object and children visited (live data)

GC:
 - every node is white
 - colour roots grey
 - keep scanning until we only have black and white
 - "read-barrier": if mutator wants to access white node, colour it grey
 - GC is interleaved with mutation (at allocation, or otherwise) 
 - At the end of collection, all remaining white nodes are garbage
 - copy black over to other space

### Treadmill
also real-time
no copying
uses doubly linked lists
needs 4 colours: ecru, grey, black, white

Code Optimisation and Register Selection
======================================================================

Dataflow Analysis:
 - what variables are live simultaneously?
 - a variable is LIVE if it has a value that may be used later in the program
 - simultaneously LIVE variables must be in different registers

Interference graph:
 - After the dataflow analysis, construct the interference graph:
 - nodes are registers
 - there is an edge between two nodes if they are simultaneously live

Graph colouring
 - Take the interference graph and colour it with as few colours as possible:
 - the number of different colours needed is how many registers will be needed
 in total
 

Classification of Optimisations
----------------------------------------------------------------------
 - High-level (Source Level)
 - Local      (within a basic block)
 - Global     (across branches)
 - Machine-dependent
 
### Inlining
Inlining is replacing a call to a function with its body.

Pro:
 - avoid cost of creating a new stack frame and similar associated with
 function calls
 
Cons:
 - must be careful of side-effects
 - must avoid excessive code duplication (space and time wasting)

Example:
```C
inline int foo()
{
return 1;
}

main()
{
printf("%i\n", foo() + foo());
}
```

Gets inlined to:
```C
main()
{
printf("%i\n", 1 + 1);
}
```

### Common Sub-Expression Elimination
Identical bits of computation get replaced by one instance

Example:
```Haskell
f x = sqr x + sqr x
```]
to:
```Haskell
f x = let q = sqr x in q + q
```

Pro:
 - Laziness, everything gets only evaluated when needed
 
Con:
 - Can only be done with pure functions

### Constant Propagation
All instances of a variable with a constant value gets replaced by the constant.

Con:
 - In imperative languages future assignments must be tracked

Example:
```
limit = 25
main = print([1..limit], p)
p = primes limit
```

to:
```
main = print([1..25], p)
p = primes 25
```

### Copy Propagation
Basically constant propagation, but instead of the constant, stuff gets replaced by
an expression.

Pro:
 - fewer local variables
 - fewer memory accesses
 
Cons:
 - Can lead to a lot of space wastage
 - Can be slower if done badly
 - Must keep track of other assignments of the variable


### Constant Reduction
Replace statically computed expression with their value.

```C
#define MAX 100

int a[MAX + 1];

```

to:
```C
int a[101];
```

### Code Motion
removes code from a loop that computes the same value each iteration
```C
for(int j = 0; j < limit; ++j){
  b = 100;
}
```

to:
```C
b = 100;

for(int j = 0; j < limit; ++j){
}
```


### Loop unrolling
replaces loop by duplicating its body

Pro:
 - can speed up code as it doesn't need duplication
 
Con:
 - can be slower if done improperly
 - can lead to massive space blowup

### Strength reduction
Replace operators by weaker ones, that are usually much quicker
e.g. replacing multiplication with shifting and adding
```Assembler
multi $8, 17
```
to:
```Assembler
move $9, $8
sll $8, 4
add $8, $9
```

### Peephole Optimisation
Replacing adjacent operations with more optimal ones.

Repeat until no more improvements can be made.

Usually rule driven

### Instruction Scheduling
On some architectures performance can be improved by reordering the instructions
e.g. to make use of multiple cores more efficiently

### Filling branch delay slots
On some architectures filling in the branch delay slot may improve code speed


Meta-Programming
======================================================================
Splicing
----------------------------------------------------------------------
expression should be evaluated at compile time:

e.g.: printf in Haskell

```
$(expr) :: ExpQ
```

```Haskell
$(printf "error %s on line %d") msg line
$( printf "x is %f") res

printf :: String -> ExpQ
printf s = gen (parse s)

data Format = S | D | F | Lit String
parse :: String -> [Format]
...
```

Quasi-Quotation
----------------------------------------------------------------------
placed around fragments that should NOT be evaluated at compile-time,
but should be compiled into the program

Reification
----------------------------------------------------------------------
basically reflection for Haskell

```
reifyType expr :: ExpQ -> TypQ
```
gives the type of expr

```
reifyDecl defn :: EqpQ -> DecQ
```
gives meta-representation of defn


Staging
----------------------------------------------------------------------
metaprograms go through stages

 - Stage 0: Program written
 - Stage 1: First compilation of meta-instructions, passes it on the next stage
 ...
 - Stage n: No more meta-instructions to execute, move onto next compilation phase

Pros:
 - Easy to modify compiler behaviour
   * Add new constructs
   * Take advantage of host parser
   * access compiler information in user programs

 - Genericity: write generic libraries
 
 - High performance
   * Domain-specific optimisations in user code
   * improve compiler phases
   * 10 faster than library-based code

Cons:
Hard to use
 - mismatch between host and metalanguage semantics
 - embedded language => little scope for new syntax
 - must understand stages
 - host language must be powerful
 

Domain-Specific Languages
----------------------------------------------------------------------

Created for specific application domains
 - Include specific features for the target domain
 - easy to learn
 - can contain unusual constructs
 - often not fully general-purpose (not Turing-complete)

Examples:
 * HTML
 * SQL
 * Hume
 * Matlab
 * R
 * Idris
 * Postscript
 

### Embedded DSL
 - Embedded in a host language: Haskell, Lisp, Python
 - Can reuse host's optimisations, back-end, runtime
 - Can reuse host's front-end (like type checking)

### Shallow Embedding
 - All operation translate directly to host operations
 - Easy to construct
 
### Deep embedding
 - Add an intermediate structure
 - Allows domain-specific optimisation
 
### Disadvantages
 - Error messages in terms of host
 - sharing and recursion are troublesome
 
Lexical Analysis
======================================================================

Non-tokens:
 * comment
 * preprocessor directive
 * macro definition
 * macro
 * white space

Language: finite set of strings


Parsing
======================================================================

Concrete Syntax
----------------------------------------------------------------------
 - Defined by the grammar
 - usually by BNF
 - communicates to humans what is legal

Abstract Syntax
----------------------------------------------------------------------
 - removes irrelevant details
 - used to simplify the translation process
 - usually denoted by AST
 - NOT a parse tree (doesn't have the concrete syntax details)
