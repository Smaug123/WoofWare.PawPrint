// CoreCLR's AllocateArrayEx (gchelpers.cpp) uses `S_UINT32` saturating
// arithmetic for the running element count and only throws OOM if the
// product overflows uint32, never if it merely exceeds Array.MaxLength.
// Consequently a shape like `new int[50000, 50000, 0]` is legal: the
// running product transiently hits 2.5e9 (above Array.MaxLength but
// inside uint32) before the trailing zero collapses it back to 0, and
// the resulting array allocates with `Length == 0`.
//
// Without the trailing zero, our internal flat-Int32-indexed
// representation cannot hold the resulting 2.5e9-element array, so we
// raise OOM there. The point of this test is to make sure we don't
// also reject the *zero-collapsed* case, which CoreCLR (and we) can
// represent trivially.

public class Program
{
    public static int Main(string[] args)
    {
        int[,,] arr = new int[50000, 50000, 0];

        if (arr.Length != 0)
        {
            return 1;
        }

        return 0;
    }
}
