using System;

// Exercises castclass (explicit cast) success paths.
// Every test returns 0 on success, nonzero on failure.
// Avoids field access through cast references (separate PawPrint limitation).

interface IGrandparent
{
}

interface IParent : IGrandparent
{
}

interface IChild : IParent
{
}

interface IUnrelated
{
}

class Base
{
}

class Middle : Base, IChild
{
}

class Leaf : Middle
{
}

class Unrelated
{
}

class Program
{
    static int Main(string[] args)
    {
        int result = 0;

        // 1. Null cast to class — must succeed, return null.
        {
            object o = null;
            Base b = (Base)o;
            if (b != null) result |= 1;
        }

        // 2. Null cast to interface — must succeed, return null.
        {
            object o = null;
            IChild i = (IChild)o;
            if (i != null) result |= 2;
        }

        // 3. Exact type match.
        {
            Base b = new Base();
            object o = b;
            Base b2 = (Base)o;
            if (!ReferenceEquals(b, b2)) result |= 4;
        }

        // 4. Upcast: Middle -> Base.
        {
            Middle m = new Middle();
            object o = m;
            Base b = (Base)o;
            if (!ReferenceEquals(m, b)) result |= 8;
        }

        // 5. Upcast two levels: Leaf -> Base.
        {
            Leaf l = new Leaf();
            object o = l;
            Base b = (Base)o;
            if (!ReferenceEquals(l, b)) result |= 0x10;
        }

        // 6. Upcast one level: Leaf -> Middle.
        {
            Leaf l = new Leaf();
            object o = l;
            Middle m = (Middle)o;
            if (!ReferenceEquals(l, m)) result |= 0x20;
        }

        // 7. Downcast: Base ref holding Middle -> Middle.
        {
            Middle m = new Middle();
            Base b = m;
            object o = b;
            Middle m2 = (Middle)o;
            if (!ReferenceEquals(m, m2)) result |= 0x40;
        }

        // 8. Downcast: Base ref holding Leaf -> Leaf.
        {
            Leaf l = new Leaf();
            Base b = l;
            object o = b;
            Leaf l2 = (Leaf)o;
            if (!ReferenceEquals(l, l2)) result |= 0x80;
        }

        // 9. Downcast skipping level: Base ref holding Leaf -> Middle.
        {
            Leaf l = new Leaf();
            Base b = l;
            object o = b;
            Middle m = (Middle)o;
            if (!ReferenceEquals(l, m)) result |= 0x100;
        }

        // 10. Cast to object — always works.
        {
            Leaf l = new Leaf();
            object o = (object)l;
            if (!ReferenceEquals(l, o)) result |= 0x200;
        }

        // 11. Cast to direct interface: Middle implements IChild.
        {
            Middle m = new Middle();
            object o = m;
            IChild i = (IChild)o;
            if (!ReferenceEquals(m, i)) result |= 0x400;
        }

        // 12. Cast to transitive interface: Middle -> IChild -> IParent.
        {
            Middle m = new Middle();
            object o = m;
            IParent p = (IParent)o;
            if (!ReferenceEquals(m, p)) result |= 0x800;
        }

        // 13. Cast to doubly-transitive interface: Middle -> IChild -> IParent -> IGrandparent.
        {
            Middle m = new Middle();
            object o = m;
            IGrandparent g = (IGrandparent)o;
            if (!ReferenceEquals(m, g)) result |= 0x1000;
        }

        // 14. Inherited interface: Leaf inherits IChild from Middle.
        {
            Leaf l = new Leaf();
            object o = l;
            IChild i = (IChild)o;
            if (!ReferenceEquals(l, i)) result |= 0x2000;
        }

        // 15. Inherited transitive interface: Leaf -> IGrandparent via Middle.
        {
            Leaf l = new Leaf();
            object o = l;
            IGrandparent g = (IGrandparent)o;
            if (!ReferenceEquals(l, g)) result |= 0x4000;
        }

        // 16. Chained casts: Leaf -> object -> Base -> Middle -> Leaf, identity preserved.
        {
            Leaf l = new Leaf();
            object o = (object)l;
            Base b = (Base)o;
            Middle m = (Middle)b;
            Leaf l2 = (Leaf)m;
            if (!ReferenceEquals(l, l2)) result |= 0x8000;
        }

        // 17. Interface-typed ref cast to object.
        {
            Middle m = new Middle();
            IChild i = m;
            object o = (object)i;
            if (!ReferenceEquals(m, o)) result |= 0x10000;
        }

        // 18. Interface-typed ref downcast back to concrete type.
        {
            Middle m = new Middle();
            IChild i = m;
            Middle m2 = (Middle)i;
            if (!ReferenceEquals(m, m2)) result |= 0x20000;
        }

        // 19. Interface-typed ref cast to base class.
        {
            Leaf l = new Leaf();
            IChild i = l;
            Base b = (Base)i;
            if (!ReferenceEquals(l, b)) result |= 0x40000;
        }

        // 20. Cast between sibling interfaces via object ref
        //     (Leaf implements IChild; if IChild extends IParent, cast object->IParent should work).
        {
            Leaf l = new Leaf();
            IChild i = l;
            object o = i;
            IParent p = (IParent)o;
            if (!ReferenceEquals(l, p)) result |= 0x80000;
        }

        return result;
    }
}
