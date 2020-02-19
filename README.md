# Futhask

Futhask is a code-generation tool used to create safe, Haskell friendly wrappers for Futhark libraries.

## Use
### Generate Code
    futhask [Backend*] [Futhark.h] [HaskellSourceDir] [ModuleName]

\*Only currently supported backend is opencl.


### Import Code
    import [ModuleName]
    import [ModuleName].Entries
    
If using `stack` add `c-sources: [Futhark.c]` and `extra-libraries: OpenCL` to the `library` section of `package.yaml`

## Generated Code
The generated code can be split in two main parts, raw and wrapped. The raw interface is simply the C-functions wrapped in the IO-monad, providing no safety and requiring manual memory management. The wrapped interface uses `newForeignPtr` to introduce all Futhark pointers to the GC, and provides function types closer to those used within Futhark, returning tuples instead of writing to pointers.

### Context Generation
    getContext :: [ContextOptions] -> IO Context

Available context options will depend on backend used.

### The FT monad
To make the wrappers safe, and reduce clutter from explicitly passing around the context, the FT monad is introduced. The FT monad is an environment (Reader) monad that implicitly passes the context around as necessary. Like the ST monad, the FT monad is parameterised by a rigid type variable to prevent references to the context from escaping the monad.

To run the computation, the function

    runFTIn :: Context -> (forall c. FT c a) -> a

is used. Additionally

    runFTWith :: [ContextOptions] -> (forall c. FT c a) -> a
    runFT :: (forall c. FT c a) -> a

are defined for convienience in the case that the context doesn't need to be reused.

### Input and Output
For conversion between Futhark values and Haskell values, two classes are defined.

    class Input fo ho where
        toFuthark :: ho -> FT c fo 

    class Output fo ho where
        fromFuthark :: fo -> FT c ho

Instances of Input and Output are generated for all transparent Futhark-arrays. The Haskell representation is `Array S` from `Data.Massiv.Array`.
