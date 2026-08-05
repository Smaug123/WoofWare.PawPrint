using System;
using System.Threading.Tasks;

// End-to-end motivation for issue #711: `Task.WhenAny` on two already-completed
// `Task<int>`s. `Task.FromResult<TResult>` reinterprets a cached `Task<int>`
// as `Task<TResult>` via `ldloca.s; conv.u; ldind.ref` (see
// LdindRefNativeInt.cs for the isolated repro of that IL shape), so this is
// the first thing WhenAny trips over. Whether the rest of WhenAny works is a
// separate question tracked by wherever this test lands (unimplemented, if
// it doesn't).
public static class TaskWhenAnyTwoResults
{
    public static int Main (string[] args)
    {
        Task<int> x = Task.FromResult (1);
        Task<int> y = Task.FromResult (2);
        Task<int> done = Task.WhenAny (x, y).Result;
        return (done == x || done == y) ? 0 : 1;
    }
}
