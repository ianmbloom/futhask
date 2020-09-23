# Futhask
Futhask is a code generator that aims to create safe, Haskell friendly wrappers for Futhark libraries.

## Installation
    stack install

## Use
### Generate Code
    futhask [Backend] [Futhark.h] [HaskellSourceDir] [ModuleName]

#### Example
    futhark opencl --library myprogram.fut
    futhask opencl myprogram.h src MyLibrary

For a simple example of how generated haskell code can be used, see [FuthaskExample](https://gitlab.com/Gusten_Isfeldt/futhaskexample)

### Import Code
    import [ModuleName]
    import [ModuleName].Entries
    
If using `stack` add `c-sources: [Futhark.c]` to the `library` section of `package.yaml`

#### OpenCL
    extra-libraries: OpenCL 

#### CUDA
    include-dirs: /opt/cuda/include
    extra-lib-dirs: /opt/cuda/lib
    extra-libraries: cuda cudart nvrtc

### Dependencies
`massiv` is required for all backends.
The codes generated for OpenCL and CUDA, both refer to types from the `OpenCL` and `cuda` packages respectively. This is only relevant if one wants to use certain functions in the raw interface, but, without modification, the generated code will not compile without these dependencies.

## Generated Code
The generated code can be split in two main parts, raw and wrapped. The raw interface is simply the C-functions wrapped in the IO-monad, providing no added safety and requiring manual memory management. The wrapped interface uses `newForeignPtr` to introduce all Futhark pointers to the GC, and provides function types closer to those used within Futhark, returning tuples instead of writing to pointers.

### Context Generation
    getContext :: [ContextOption] -> IO Context

Available context options will depend on backend used.

### The FT monad
To make the wrappers safe, and reduce clutter from explicitly passing around the context, the FT monad is introduced. The FT monad is an environment (Reader) monad that implicitly passes the context around as necessary. Like the ST monad, the FT monad is parameterised by a rigid type variable to prevent references to the context from escaping the monad.

To run computations, the function

    runFTIn :: Context -> (forall c. FT c a) -> a

is used. Additionally

    runFTWith :: [ContextOptions] -> (forall c. FT c a) -> a
    runFT :: (forall c. FT c a) -> a

are defined for convienience for cases where the context doesn't need to be reused.

### Input and Output
For conversion between Futhark values and Haskell values, two classes are defined.

    class Input fo ho where
        toFuthark :: ho -> FT c (fo c) 

    class Output fo ho where
        fromFuthark :: fo c -> FT c ho

Instances of Input and Output are generated for all transparent Futhark-arrays. The Haskell representation is `Array S` from `Data.Massiv.Array`. The absence of functional dependencies in the definitions might require more explicit type signatures, but gives more flexibility to define new instances. For tuples of instances, functions on the form `fromFutharkTN`, where `N` is the tuple size, are defined.
