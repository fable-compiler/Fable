// Test that a file with multiple namespaces works, see #1218

// Different namespace together with namespaces
// sharing a prefix doesn't work
// namespace My.Namespace

// Duplicating namespaces doesn't work
// namespace Fable.Tests.A.C

// Empty namespaces work
namespace Fable.Tests.A.D

// Multiple mamespaces sharing prefix work
namespace Fable.Tests.A.B
type Helper =
    static member Add2(x) = x + 2

namespace Fable.Tests.A.C
type Helper =
    static member Add5(x) = Fable.Tests.A.B.Helper.Add2(x + 3)
