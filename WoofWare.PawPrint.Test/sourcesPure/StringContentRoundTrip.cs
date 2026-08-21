using System.Net.Http;

internal static class Program
{
    private static int Main()
    {
        // End-to-end: StringContent buffers its payload at construction time and
        // serves it back through the content read APIs.
        var content = new StringContent("hello world");

        var mediaType = content.Headers.ContentType;

        if (mediaType is null || mediaType.MediaType != "text/plain")
        {
            return 1;
        }

        if (mediaType.CharSet != "utf-8")
        {
            return 2;
        }

        var bytes = content.ReadAsByteArrayAsync().Result;

        if (bytes.Length != 11)
        {
            return 3;
        }

        if (bytes[0] != (byte)'h' || bytes[10] != (byte)'d')
        {
            return 4;
        }

        var roundTripped = content.ReadAsStringAsync().Result;

        if (roundTripped != "hello world")
        {
            return 5;
        }

        return 0;
    }
}
