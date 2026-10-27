// What a C unit's sizeof runs: a variable-length array's bounds, and nothing else.
int Effect(void);
int Vla(int n)
{
    return (int)sizeof(int[(Effect(), n)]);
}
int NotVla(void)
{
    return (int)sizeof(Effect());
}
