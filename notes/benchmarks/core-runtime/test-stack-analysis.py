"""Check that callback time and unmanaged waits are not charged to Core."""

import json
from pathlib import Path
import subprocess
import tempfile

names = [
    "FSharp.Compiler.Service!FSharp.Compiler.CheckExpressions.Tc()",
    "FSharp.Core!Microsoft.FSharp.Collections.MapTreeModule.mk()",
    "FSharp.Compiler.Service!FSharp.Compiler.CheckExpressions.Callback()",
    "System.Private.CoreLib!System.String.Compare()",
    "CPU_TIME",
    "UNMANAGED_CODE_TIME",
    "FSharp.Compiler.Service!FSharp.Compiler.Other.Work()",
]
events = []
time = 0
for stack, duration in [([0, 1, 4], 10), ([0, 1, 2, 4], 20), ([0, 1, 5], 30), ([0, 1, 3, 4], 40), ([6, 1, 4], 50)]:
    events.extend(dict(type="O", frame=f, at=time) for f in stack)
    time += duration
    events.extend(dict(type="C", frame=f, at=time) for f in reversed(stack))
fixture = dict(
    shared=dict(frames=[dict(name=n) for n in names]), profiles=[dict(startValue=0, endValue=time, events=events)]
)
with tempfile.TemporaryDirectory() as directory:
    path = Path(directory) / "test.json"
    path.write_text(json.dumps(fixture))
    output = subprocess.check_output(
        ["rtk", "proxy", "python", str(Path(__file__).with_name("analyze-stacks.py")), str(path)], text=True
    )
    result = json.loads(output)[0]
assert result["sampled_check_ms"] == 70, result
assert result["core_inclusive_ms"] == 50, result
assert result["leaf"][0][1] == 10, result
assert result["inclusive"][0][1] == 50, result
print("PASS: CPU-only accounting, compiler callback exclusion, Core self time, checking scope")
