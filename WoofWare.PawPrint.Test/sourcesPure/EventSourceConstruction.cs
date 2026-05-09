using System;
using System.Diagnostics.Tracing;

// Smoke test for the EventPipeInternal_* QCall surface. Constructing a derived
// EventSource walks through Register -> CreateProvider, so the QCall handler
// must mint a non-zero IntPtr or the EventSource ctor throws OOM.
public sealed class MinimalEventSource : EventSource
{
    public static readonly MinimalEventSource Log = new MinimalEventSource();

    private MinimalEventSource() : base("WoofWare.PawPrint.MinimalEventSource") { }

    [Event(1)]
    public void Tick() { WriteEvent(1); }
}

public class EventSourceConstructionTests
{
    public static int Main(string[] argv)
    {
        // Just constructing the static field is enough to exercise CreateProvider
        // and DefineEvent. EventSource swallows internal errors silently, so the
        // best we can do at the surface is observe that the object materialised.
        if (MinimalEventSource.Log == null) return 1;
        if (MinimalEventSource.Log.Name != "WoofWare.PawPrint.MinimalEventSource") return 2;
        MinimalEventSource.Log.Tick();
        return 0;
    }
}
