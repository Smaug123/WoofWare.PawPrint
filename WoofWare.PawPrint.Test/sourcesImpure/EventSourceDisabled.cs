using System;
using System.Diagnostics.Tracing;
using System.Linq;

// The motivating case for host-seeded AppContext properties.
//
// `EventSource.IsSupported` is a `[FeatureSwitchDefinition]` static readonly latched from
// `AppContext.TryGetSwitch("System.Diagnostics.Tracing.EventSource.IsSupported")`. With the
// switch off, the EventSource constructor becomes a no-op, `GetSources()` returns empty, and
// `Write`/`IsEnabled` do nothing — which is what lets a guest that merely *mentions*
// EventSource run under PawPrint at all.
//
// PawPrint-only: the differential oracle shares the test process's own AppContext, where this
// switch is not set.
public sealed class MyEventSource : EventSource
{
    public static readonly MyEventSource Log = new MyEventSource();

    private MyEventSource() : base("WoofWare-PawPrint-TestSource") { }

    [Event(1)]
    public void Something(int value) { WriteEvent(1, value); }
}

public class EventSourceDisabled
{
    public static int Main()
    {
        // The switch itself must be visible as seeded.
        if (!AppContext.TryGetSwitch("System.Diagnostics.Tracing.EventSource.IsSupported", out bool supported))
        {
            return 1;
        }

        if (supported) { return 2; }

        // Constructing one is a no-op rather than a registration.
        MyEventSource source = MyEventSource.Log;
        if (source is null) { return 3; }

        // With the feature off, the source never registers itself.
        if (EventSource.GetSources().Any()) { return 4; }

        // And it reports itself disabled, so guest code guarded on IsEnabled does nothing.
        if (source.IsEnabled()) { return 5; }

        // Writing an event is a no-op rather than a throw.
        source.Something(42);

        return 0;
    }
}
