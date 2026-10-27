// A C++11 unit, as CoreCLR's are, where a copy from a temporary is an elidable construction that
// clang elides.
struct Copied { Copied(); Copied(const Copied&); };
Copied MakeCopied();
void Elide()
{
    Copied c = MakeCopied();
    (void)c;
}
