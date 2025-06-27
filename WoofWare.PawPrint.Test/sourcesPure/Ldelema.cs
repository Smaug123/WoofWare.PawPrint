using System;

/// <summary>
/// A simple value type used for testing ldelema.
/// </summary>
public struct TestStruct
{
    public int Value;
}

public class Program
{
    /// <summary>
    /// Modifies a TestStruct instance by reference. Calling this with an array element
    /// (e.g., `ModifyStruct(ref array[i], ...)` ) will cause the C# compiler to
    /// generate an `ldelema` instruction.
    /// </summary>
    /// <param name="s">A reference to the TestStruct to modify.</param>
    /// <param name="newValue">The new value to assign.</param>
    public static void ModifyStruct(ref TestStruct s, int newValue)
    {
        s.Value = newValue;
    }

    /// <summary>
    /// Modifies a string reference.
    /// </summary>
    /// <param name="s">A reference to a string variable.</param>
    /// <param name="newValue">The new string to assign.</param>
    public static void ModifyStringRef(ref string s, string newValue)
    {
        s = newValue;
    }

    /// <summary>
    /// Main entry point for the ldelema test.
    /// </summary>
    /// <returns>0 if all tests pass, otherwise a non-zero error code.</returns>
    public static int Main(string[] args)
    {
        // --- Test 1: Modifying a value type element in an array ---
        TestStruct[] structArray = new TestStruct[5];
        structArray[2].Value = 100;

        // This call should generate an `ldelema` instruction to get the address of structArray[2].
        ModifyStruct(ref structArray[2], 999);

        if (structArray[2].Value != 999)
        {
            return 301; // Unique error code for this test
        }

        // --- Test 2: Modifying a reference type element in an array ---
        string[] stringArray = new string[] { "alpha", "beta", "gamma" };

        // This call should also generate an `ldelema` instruction.
        ModifyStringRef(ref stringArray[1], "zeta");

        if (stringArray[1] != "zeta")
        {
            return 302; // Unique error code for this test
        }

        return 0; // Success
    }
}
