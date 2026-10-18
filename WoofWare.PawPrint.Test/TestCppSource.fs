namespace WoofWare.PawPrint.Test

open FsUnitTyped
open NUnit.Framework

/// `CppSource` reads routines and their calls out of the runtime's C++ and assembly, which
/// `TestNativeContractTable` holds the contract table's citations to.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestCppSource =

    let private callsOf (text : string) : (string * string list) list =
        CppSource.definitions text
        |> List.map (fun routine -> routine.Name, List.ofSeq routine.Calls)

    [<Test>]
    let ``comments and literals hide what they spell`` () : unit =
        callsOf
            """
void F()
{
    // G(); {
    /* H(); } */
    const char* s = "I(); {";
    char c = '{';
    int n = 1'000;
    J(c);
}
"""
        |> shouldEqual [ "F", [ "J" ] ]

    [<Test>]
    let ``every arm of a conditional is read, nesting braces as the first arm does`` () : unit =
        callsOf
            """
void F()
{
#if A
    if (P()) {
#else
    if (Q()) {
#endif
        G();
    }
}

#ifdef B
void H() { R(); }
#else
void H() { S(); }
#endif
"""
        |> shouldEqual [ "F", [ "G" ; "P" ; "Q" ] ; "H", [ "R" ] ; "H", [ "S" ] ]

    [<Test>]
    let ``a definition is named by its class, its spelled qualifier or its defining macro`` () : unit =
        callsOf
            """
namespace N {
class C : public Base
{
public:
    int M() const { return Inner(); }
    struct D { void E() { Deeper(); } };
};
}

int C::Out(int a = 0) { return Qualified(a); }

FCIMPL1(INT32, ObjectNative::TryGetHashCode, Object* obj)
{
    return Fcall(obj);
}
FCIMPLEND

extern "C" void QCALLTYPE Monitor_Enter(QCall::ObjectHandleOnStack obj)
{
    Qcall();
}

template <typename T> T Holder<T>::Get() { return Templated(); }

__declspec(noinline) void Attributed() { Plain(); }

static const int table[] = { 1, 2 };
auto lambda = [](int a) { return NotADefinition(a); };
"""
        |> shouldEqual
            [
                "C::M", [ "Inner" ]
                "C::D::E", [ "Deeper" ]
                "C::Out", [ "Qualified" ]
                "ObjectNative::TryGetHashCode", [ "Fcall" ]
                "Monitor_Enter", [ "Qcall" ]
                "Holder::Get", [ "Templated" ]
                "Attributed", [ "Plain" ]
            ]

    /// `vm/comutilnative.cpp` spells `MethodTableNative::InstantiationArg0` with a semicolon after
    /// the macro, which compiles because the macro opens no scope of its own.
    [<Test>]
    let ``a defining macro followed by a semicolon still defines the body after it`` () : unit =
        callsOf
            """
static FCDECL1(Object*, Declared, MethodTable* pMT);

FCIMPL1(MethodTable*, MethodTableNative::InstantiationArg0, MethodTable* mt);
{
    return Read(mt);
}
FCIMPLEND

FCIMPL1(INT32, Spaced, Object* obj) ;

{
    return Again(obj);
}
FCIMPLEND

FCIMPL1(INT32, OnlyDeclared, Object* obj);
void Plain() { Called(); }
"""
        |> shouldEqual
            [
                "MethodTableNative::InstantiationArg0", [ "Read" ]
                "Spaced", [ "Again" ]
                "Plain", [ "Called" ]
            ]

    [<Test>]
    let ``a call is spelled with its qualifier, a declaration with arguments calls its type`` () : unit =
        callsOf
            """
SyncBlock::SyncBlock(DWORD index) : m_monitor(Make(index)), m_link()
{
    MethodDescCallSite site(METHOD__X);
    site.Call_RetI4(args);
    CoreLibBinder::GetClass(CLASS__Y);
    InterlockedCompareExchangeT<int>(&a, 1, 0);
    pThread->DoWait(sizeof(int));
    return Done(static_cast<int>(x));
}
"""
        |> shouldEqual
            [
                "SyncBlock::SyncBlock",
                [
                    "Call_RetI4"
                    "CoreLibBinder::GetClass"
                    "DoWait"
                    "Done"
                    "InterlockedCompareExchangeT"
                    "Make"
                    "MethodDescCallSite"
                    "m_link"
                    "m_monitor"
                ]
            ]

    [<Test>]
    let ``an assembly routine calls what it branches to, the macros it uses, and anything through a register``
        ()
        : unit
        =
        CppSource.assemblyRoutines
            """
.macro NEW_FAST
        INLINE_GET_CONTEXT x3
        push_nonvol_reg rbx
        bhi         C_FUNC(Slow)
.endm

    LEAF_ENTRY Fast, _TEXT
        NEW_FAST
        cbz         x0, LOCAL_LABEL(Fail)
LOCAL_LABEL(Fail):
        b C_FUNC(Failed)
    LEAF_END Fast, _TEXT

    NESTED_ENTRY Indirect, _TEXT, NoHandler
        blr x9
        call qword ptr [rax]
        jmp Named
    NESTED_END Indirect, _TEXT
"""
        |> List.map (fun routine -> routine.Name, List.ofSeq routine.Calls)
        |> shouldEqual
            [
                "NEW_FAST", [ "INLINE_GET_CONTEXT" ; "Slow" ; "push_nonvol_reg" ]
                "Fast", [ "Failed" ; "NEW_FAST" ]
                "Indirect", [ "<indirect>" ; "Named" ]
            ]

    [<Test>]
    let ``a symbol names a call spelled with its last qualifiers, and a type names its constructor`` () : unit =
        CppSource.names "A::B::F" "B::F" |> shouldEqual true
        CppSource.names "A::B::F" "F" |> shouldEqual true
        CppSource.names "A::F" "B::F" |> shouldEqual false
        CppSource.names "AF" "F" |> shouldEqual false
        CppSource.names "F" "A::F" |> shouldEqual false
        CppSource.names "A::Holder::Holder" "A::Holder" |> shouldEqual true
        CppSource.names "A::Holder::Holder" "Holder" |> shouldEqual true
        CppSource.names "A::Holder::Other" "A::Holder" |> shouldEqual false
