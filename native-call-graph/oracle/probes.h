// Declarations that both units of the oracle program see.

struct Base { virtual void V(); };
struct Left : virtual Base { void V() override; };
struct Right : virtual Base { };
struct Bottom : Left, Right { };
struct Abstract : Base { void V() override = 0; };
struct Concrete : Abstract { void V() override; };

struct Interface { virtual void M() = 0; };

struct Shape { virtual ~Shape(); };
struct Square : Shape { ~Square() override; };

void Helper();
#define CALL_HELPER() Helper()
#define OUTER() CALL_HELPER()
#define WRAP(e) do { e; } while (0)

enum Kind { kFirst, kSecond };
void Throw(Kind k, int n = 0, Kind d = kSecond);
#define THROW(k) Throw(k)

void CallPasted();
#define PASTE(name) Call##name()
