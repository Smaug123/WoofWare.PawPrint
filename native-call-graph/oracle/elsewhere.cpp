// A class that only this unit defines, so that only a pass over the whole program finds its
// overrider of Base::V.
#include "probes.h"

struct Elsewhere : Base { void V() override; };

void UseElsewhere() { Elsewhere e; e.V(); }

// The unit that defines a virtual destructor is where its deleting destructor, and so the
// operator delete that frees the object, is known.
Shape::~Shape() { }
Square::~Square() { }
