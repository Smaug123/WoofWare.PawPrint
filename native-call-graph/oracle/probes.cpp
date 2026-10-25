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
