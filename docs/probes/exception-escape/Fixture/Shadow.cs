namespace System
{
    /// A type shadowing a corelib exception. A compilation is allowed to declare one, and source
    /// resolution then prefers it over the referenced type of the same name — so a `catch` naming
    /// it emits a TypeDef token for *this* type, while the runtime still throws corelib's.
    ///
    /// Exotic to write by hand, and not exotic at all to *meet*, once the analysis is pointed at a
    /// package somebody else wrote. See `Fixture.ShadowCases.DereferencesNull`.
    public class NullReferenceException : System.Exception
    {
    }
}
