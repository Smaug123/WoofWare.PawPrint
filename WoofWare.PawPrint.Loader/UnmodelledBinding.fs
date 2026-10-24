namespace WoofWare.PawPrint

/// Something a guest's metadata names that PawPrint does not implement binding, such as a type
/// reference scoped to another module of a multi-module assembly, or a shape ECMA-335 forbids whose
/// exception CoreCLR has not been measured to raise.
///
/// A consumer stops the run with the diagnostic where it would have used the binding. It is never a
/// guest-visible exception: which one CoreCLR raises for it, if any, is what PawPrint does not model.
type UnmodelledBinding =
    {
        /// What PawPrint could not bind, and why, for whoever reads the failed run.
        Diagnostic : string
    }
