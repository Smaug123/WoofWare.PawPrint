using System;
using System.Collections.Generic;

// Deliberately inconsistent equality: IEquatable<Element>.Equals compares Id, while
// object.Equals is reference identity. Which of the two a collection method picks is
// therefore observable, and it depends on the generic instantiation the runtime chose
// for the SZArrayHelper shim: EqualityComparer<object>.Default routes to
// object.Equals(object), EqualityComparer<Element>.Default to IEquatable<Element>.
class Element : IEquatable<Element>
{
    public int Id;

    public Element(int id)
    {
        Id = id;
    }

    public bool Equals(Element other)
    {
        return other != null && other.Id == Id;
    }

    public override bool Equals(object obj)
    {
        return ReferenceEquals(this, obj);
    }

    public override int GetHashCode()
    {
        return Id;
    }
}

class Derived : Element
{
    public Derived(int id) : base(id)
    {
    }
}

public class Program
{
    public static int Main(string[] args)
    {
        Element[] exact = new[] { new Element(1) };
        ICollection<Element> c = exact;

        if (c.Contains(new Element(1)))
        {
            return 1;
        }

        IList<Element> l = exact;
        if (l.IndexOf(new Element(1)) != -1)
        {
            return 2;
        }

        // The same, reached covariantly.
        Derived[] derived = new[] { new Derived(2) };
        ICollection<Element> cc = derived;

        if (cc.Contains(new Derived(2)))
        {
            return 3;
        }

        // Reference identity must still be found either way.
        Element same = new Element(3);
        ICollection<Element> c2 = new[] { same };

        if (!c2.Contains(same))
        {
            return 4;
        }

        return 0;
    }
}
