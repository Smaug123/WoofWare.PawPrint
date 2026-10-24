namespace WoofWare.PawPrint

/// What PawPrint knows of a body that CoreCLR's JIT optimises, which decides branches it folds.
type OptimisedBody =
    {
        /// Each `call` whose callee the JIT expands to a constant Boolean on the CPU it compiles
        /// for (a capability query), by its offset, with that constant.
        Capabilities : Map<int, bool>
        /// The `call`s and `callvirt`s, by offset, whose callee the JIT never expands as an
        /// intrinsic, so that what they return is never a constant while it imports the body.
        OrdinaryCalls : Set<int>
    }

/// Which of a body's conditional branches CoreCLR's JIT may fold when it compiles the body.
[<RequireQualifiedAccess>]
type JitFolding =
    /// None: it does not optimise the body. A method marked `NoOptimization`, or in an assembly
    /// whose `DebuggableAttribute` disables optimisation, is compiled so.
    | Nothing
    /// It optimises the body, and folds any conditional branch whose condition it reduces to a
    /// constant, never importing the successor the constant rules out.
    | Optimises of OptimisedBody

/// Bounds on the instructions CoreCLR's JIT imports when it compiles a body, by offset.
type CompiledCode =
    {
        /// Instructions it imports, whatever else it folds. Holds none it does not import.
        Surely : Set<int>
        /// Instructions it may import. Omits none it imports.
        Maybe : Set<int>
    }

/// The order in which CoreCLR's JIT imports a body's instructions, as far as it does not depend on
/// the importer's worklist.
type ImportOrder =
    {
        /// Each instruction it may import other than the entry, by offset, with the last one it
        /// surely imports before it whenever it imports it: its immediate dominator in the flow
        /// graph of the code it may import.
        ImportedJustBefore : Map<int, int>
    }

/// What CoreCLR's JIT compiles of a body, and in what order, as far as PawPrint can state it
/// without modelling the importer's folding.
///
/// The importer (`importer.cpp`) walks a body's blocks from the entry, importing a block only when
/// one it has imported leads there, and the handlers of a protected block once it begins importing a
/// block inside it (`impVerifyEHBlock`). At a conditional branch whose condition `gtFoldExpr`
/// reduces to a constant it keeps only the successor the constant chooses (`COND_JUMP`). That
/// happens only where it optimises the body: `gtFoldExpr` folds nothing unless
/// `Tier0OptimizationEnabled()`.
[<RequireQualifiedAccess>]
module JitImport =

    /// Bounds on what the JIT imports of `body` when it folds as `folding` says.
    let compiledCode (body : MethodInstructions<'methodVars>) (folding : JitFolding) : CompiledCode =
        failwith "TODO: JitImport.compiledCode"

    /// The order in which the JIT imports what it may import of `body`, as far as it is fixed.
    let importOrder (body : MethodInstructions<'methodVars>) (folding : JitFolding) : ImportOrder =
        failwith "TODO: JitImport.importOrder"

    /// Whether, whenever the JIT imports the instruction at `later`, it has imported the one at
    /// `earlier` first. False for an instruction and itself.
    let importedBefore (order : ImportOrder) (earlier : int) (later : int) : bool =
        failwith "TODO: JitImport.importedBefore"
