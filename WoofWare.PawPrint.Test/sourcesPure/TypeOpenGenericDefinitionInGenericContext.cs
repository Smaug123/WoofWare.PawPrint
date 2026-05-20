using System;

class ContextOpenBox<T> { }

class GenericContextHolder<T>
{
    public static int Check()
    {
        Type openFromGenericContext = typeof(ContextOpenBox<>);
        Type closedForContext = typeof(ContextOpenBox<T>);
        Type closedInt = typeof(ContextOpenBox<int>);

        if (!openFromGenericContext.IsGenericType)
            return 1;

        if (!closedForContext.IsGenericType)
            return 2;

        if (object.ReferenceEquals(openFromGenericContext, closedForContext))
            return 3;

        if (object.ReferenceEquals(openFromGenericContext, closedInt))
            return 4;

        if (!object.ReferenceEquals(openFromGenericContext, typeof(ContextOpenBox<>)))
            return 5;

        return 0;
    }
}

class Program
{
    static int Main(string[] args)
    {
        return GenericContextHolder<int>.Check();
    }
}
