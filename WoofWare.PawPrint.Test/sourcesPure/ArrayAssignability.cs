class Animal
{
}

class Dog : Animal
{
}

public class Program
{
    public static int Main(string[] args)
    {
        string[] strings = new string[] { "alpha" };

        if (!(strings is object[]))
        {
            return 1;
        }

        object[] objects = (object[])strings;

        if (objects.Length != 1)
        {
            return 2;
        }

        Dog[] dogs = new Dog[] { new Dog() };

        if (!(dogs is Animal[]))
        {
            return 3;
        }

        Animal[] animals = (Animal[])dogs;

        if (animals.Length != 1)
        {
            return 4;
        }

        object boxedInts = new int[1];

        if (boxedInts is object[])
        {
            return 5;
        }

        object boxedNestedStrings = new string[][] { new string[] { "x" } };

        if (!(boxedNestedStrings is object[][]))
        {
            return 6;
        }

        return 0;
    }
}
