using System;
using System.Reflection;
using System.Runtime.CompilerServices;

namespace InitializeArrayBoxedFieldHandleTest
{
    class Program
    {
        private static readonly int[] Seed = new[] { 173, 257, 409 };

        static int Main(string[] args)
        {
            Type privateImplementationDetails = typeof(Program).Assembly.GetType("<PrivateImplementationDetails>");
            if (privateImplementationDetails == null)
            {
                return 1;
            }

            FieldInfo dataField = null;

            foreach (FieldInfo candidate in privateImplementationDetails.GetFields(
                BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Static))
            {
                if (candidate.FieldType.Name.Contains("12", StringComparison.Ordinal))
                {
                    if (dataField != null)
                    {
                        return 2;
                    }

                    dataField = candidate;
                }
            }

            if (dataField == null)
            {
                return 3;
            }

            object boxed = dataField.FieldHandle;
            RuntimeFieldHandle handle = (RuntimeFieldHandle)boxed;

            int[] array = new int[3];
            RuntimeHelpers.InitializeArray(array, handle);

            if (array[0] != Seed[0])
            {
                return 4;
            }

            if (array[1] != Seed[1])
            {
                return 5;
            }

            if (array[2] != Seed[2])
            {
                return 6;
            }

            return 0;
        }
    }
}
