// Each routine is a shape whose calls a reader of the text alone can get wrong.
#include "probes.h"

namespace X { struct A { void F(); }; }
struct A { void F(); };

// Lexical shadowing: the inner declaration's type decides the inner call.
struct B { void F(); };
void Shadowing(A* a, B* b)
{
    a->F();
    {
        B* a = b;
        a->F();
    }
}

// A function-pointer parameter named like a function.
void Run();
void FunctionPointer(void (*Run)())
{
    Run();
    ::Run();
}

// A ::-qualified type.
namespace N
{
    struct A { void F(); };
    void Qualified(::A* global, A* inner)
    {
        global->F();
        inner->F();
    }
}

// A typedef inherited from a base.
struct Holder { typedef X::A Element; };
struct Derived : Holder { void Use(Element* e) { e->F(); } };

// Constructor lookup with ::.
struct Made { Made(int); };
namespace M
{
    struct Made { Made(int); };
    void Construct()
    {
        ::Made a(1);
        Made b(2);
    }
}

// A virtual diamond, whose overriders another unit adds to.
void Diamond(Base* b, Right* r, Bottom* d)
{
    b->V();
    r->V();
    d->V();
    d->Base::V();
}
void Pure(Abstract* a, Interface* i)
{
    a->V();
    i->M();
}

// Destructors: of a local, of temporaries, and of a local the unwinding of a throw destroys.
struct Guard { Guard(); ~Guard(); };
void Take(const Guard&);
[[noreturn]] void Fail();
void Destroy()
{
    Guard g;
    Take(Guard());
}
void DestroyOnThrow()
{
    Guard g;
    Fail();
}
struct Owner { Guard member; ~Owner(); };
Owner::~Owner() { Helper(); }

// new and delete.
void Allocate()
{
    Made* m = new Made(3);
    delete m;
}
void DeleteShape(Shape* s) { delete s; }

// Calls through a pointer to a member function.
struct Target { void F(); };
void MemberPointer(Target* t, void (Target::*f)()) { (t->*f)(); }

// Calls a macro's body makes, and calls a macro's argument makes.
void Macros()
{
    CALL_HELPER();
    OUTER();
    WRAP(Helper());
    WRAP(CALL_HELPER());
}

// Enumerators as arguments, written and by default.
void Enums()
{
    Throw(kFirst);
    THROW(kSecond);
    Throw(kFirst, 1, kFirst);
}

// What runs nothing.
int Value();
void Unevaluated()
{
    (void)sizeof(Value());
    decltype(Value()) v = 0;
    (void)v;
    if constexpr (false) { Helper(); }
}

// Templates.
template <class T> void Generic(T* t) { t->F(); }
template <class T> void NeverInstantiated(T* t) { t->F(); }
void Instantiate(A* a, B* b)
{
    Generic(a);
    Generic(b);
}

// A throw, and what default arguments and default member initializers call.
void Throws() { throw Made(4); }
int Seed();
void TakeDefault(int x = Seed());
void Defaults() { TakeDefault(); }
struct WithDefault { int v = Seed(); WithDefault(); };
WithDefault::WithDefault() { }

// Declared, never defined.
void Declared();

// A call whose name a macro pastes together.
void Pasting() { PASTE(Pasted); }

// Constructions and destructions that are trivial, which run no code.
struct Plain { int x; };
void Trivial(Plain* from)
{
    Plain p;
    Plain q = *from;
    (void)p;
    (void)q;
}

// A lambda's body, whose calls count as its enclosing routine's.
void Lambda()
{
    auto f = [] { Helper(); };
    f();
}

// More of what runs nothing: other unevaluated operands, and constant evaluation.
namespace std { class type_info; }
constexpr int Constant() { return 1; }
void Unevaluated2(int n)
{
    (void)noexcept(Value());
    (void)typeid(Value());
    __typeof__(Value()) w = 0;
    (void)w;
    static_assert(Constant() == 1, "");
    switch (n) { case Constant(): break; }
}

// A call no text names: a conversion operator's.
struct Converts { operator int() const; };
int Convert(Converts c) { return c; }

// A parameter its callee destroys.
struct [[clang::trivial_abi]] Owned { Owned(Owned&&); ~Owned(); };
void TakeOwned(Owned o) { }

// A constructor's members, which it constructs, and which unwinding it destroys.
struct Holds { Guard g; Holds(); };
Holds::Holds() { Helper(); }

// A destructor's bases, a virtual one among them.
struct VB { ~VB(); };
struct Mid : virtual VB { };
struct Leaf : Mid { ~Leaf(); };
Leaf::~Leaf() { }

// A member of a class template, which its key names without the template's arguments.
template <class T> struct Box { void Get(); };
template <class T> void Box<T>::Get() { Helper(); }
void UseBox(Box<int>* b) { b->Get(); }

// A new whose initializer can throw, which frees its memory if it does, and one whose cannot.
struct MayThrow { MayThrow(); };
struct NoThrow { NoThrow() noexcept; };
void AllocateCleanup()
{
    MayThrow* m = new MayThrow;
    NoThrow* n = new NoThrow;
    (void)m;
    (void)n;
}

// A non-virtual diamond, whose two subobjects of the base each keep their own overrider.
struct NB { virtual void V(); };
struct NL : NB { };
struct NR : NB { void V() override; };
struct ND : NL, NR { };
void NonVirtualDiamond(NL* l, NR* r)
{
    l->V();
    r->V();
}

// Generic lambdas, whose instantiated bodies run, and not their patterns.
void GenericLambda(A* a)
{
    auto f = [](auto x) { if constexpr (sizeof(x) == 4) Helper(); };
    f(1);
    auto g = [](auto p) { p->F(); };
    g(a);
}

// Initializers that are evaluated as constants, which run nothing, and one that is not.
void ConstexprInit()
{
    constexpr int x = Constant();
    const int y = Constant();
    static int z = Constant();
    int w = Constant();
    (void)x;
    (void)y;
    (void)z;
    (void)w;
}

// A lambda's default argument, which runs where a call uses it.
void LambdaDefault()
{
    auto f = [](int n = Seed()) { (void)n; };
    f(42);
    f();
}

// What clang emits no call for: a trivial assignment, and builtins it lowers.
namespace std
{
    template <class T> struct remove_reference { typedef T type; };
    template <class T> struct remove_reference<T&> { typedef T type; };
    template <class T> constexpr typename remove_reference<T>::type&& move(T&& t) noexcept
    {
        return static_cast<typename remove_reference<T>::type&&>(t);
    }
}
struct Pod { int x; };
int Lowered(Pod* a, Pod* b, int& n)
{
    *a = *b;
    if (__builtin_expect(n, 0))
        Helper();
    return std::move(n);
}

// Branches clang leaves out, because their conditions are constants, unless they hold a label.
void Folded()
{
    if (0)
        Helper();
    if (1)
        Value();
    else
        Helper();
    (void)(0 && Value());
    (void)(1 || Value());
    (void)(0 ? Value() : 1);
    if (0)
    {
    reached:
        Helper();
    }
}

// Virtual calls clang makes directly, because it knows the dynamic class.
struct Open { virtual void V(); };
struct Sealed final : Open { void V() override; };
void Devirtualize(Sealed* s, Open o)
{
    s->V();
    o.V();
}

// A constructor whose body cannot throw, which destroys nothing it constructs.
struct NoThrowHolds { Guard g; NoThrowHolds() noexcept; };
NoThrowHolds::NoThrowHolds() noexcept { }

// What the C++ runtime does for a routine: a static local's initialization and destruction, and
// a catch and a rethrow.
void Statics()
{
    static Guard g;
    (void)g;
}
void Catches()
{
    try
    {
        Helper();
    }
    catch (...)
    {
        throw;
    }
}

// A lambda nothing calls, whose body is still the routine's.
void UncalledLambda()
{
    auto f = [] { Helper(); };
    (void)f;
}
