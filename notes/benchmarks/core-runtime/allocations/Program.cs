using Microsoft.Diagnostics.Tracing.Etlx;
using Microsoft.Diagnostics.Tracing.Parsers.Clr;
using System.Text.Json;

var etlx = TraceLog.CreateFromEventPipeDataFile(args[0]);
using var log = new TraceLog(etlx);
var types = new Dictionary<string, double>();
var core = new Dictionary<string, double>();
var sites = new Dictionary<string, double>();
var selected = new Dictionary<string, double>();
double total = 0, checking = 0;
int ticks = 0, missingStacks = 0;
foreach (var ev in log.Events) {
    if (ev is not GCAllocationTickTraceData allocation) continue;
    ticks++;
    double bytes = allocation.AllocationAmount64;
    total += bytes;
    var stack = ev.CallStack();
    if (stack == null) missingStacks++;
    var frames = new List<string>();
    for (; stack != null; stack = stack.Caller)
        frames.Add(stack.CodeAddress.FullMethodName);
    if (!frames.Any(f => f.Contains("FSharp.Compiler.CheckExpressions.") || f.Contains("FSharp.Compiler.CheckDeclarations.") || f.Contains("FSharp.Compiler.PostInferenceChecks."))) continue;
    checking += bytes;
    if (allocation.TypeName == "TypeToken") {
        if (frames.Any(f => f.Contains("GenericHashArbArray"))) Add(selected, "TypeToken boxing under array hashing", bytes);
        if (frames.Any(f => f.Contains("GenericEqualityArbArray"))) Add(selected, "TypeToken boxing under array equality", bytes);
    }
    if (allocation.TypeName.StartsWith("Microsoft.FSharp.Collections.MapTreeNode") && frames.Any(f => f.Contains("get_AllEntitiesByCompiledAndLogicalMangledNames")))
        Add(selected, "Map nodes under entity-name table construction", bytes);
    Add(types, allocation.TypeName, bytes);
    Add(sites, allocation.TypeName + " <- " + string.Join(" <- ", frames.Take(4)), bytes);
    foreach (var frame in frames.Where(f => f.StartsWith("Microsoft.FSharp.")).Distinct())
        Add(core, frame, bytes);
}
var output = new { trace=args[0], ticks, missingStacks, eventsLost=log.EventsLost, totalSampledBytes=total,
    checkingSampledBytes=checking, selected=Top(selected), types=Top(types), coreInclusive=Top(core), sites=Top(sites) };
File.WriteAllText(args[1], JsonSerializer.Serialize(output, new JsonSerializerOptions { WriteIndented=true }));
Console.WriteLine($"{ticks} ticks, {missingStacks} without stacks; {checking/1048576:F1} estimated MB in checking stacks / {total/1048576:F1} total");
static void Add(Dictionary<string,double> d, string k, double v) => d[k] = d.GetValueOrDefault(k) + v;
static object Top(Dictionary<string,double> d) => d.OrderByDescending(p => p.Value).Take(30).Select(p => new { name=p.Key, estimatedMB=p.Value/1048576 }).ToArray();
