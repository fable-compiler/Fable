"""Rank sampled Core stacks during FCS checking; inclusive costs overlap."""

import collections
import json
import sys

results = []
for path in sys.argv[1:]:
    data = json.load(open(path, encoding="utf-8"))
    names = [f["name"].split("(")[0] for f in data["shared"]["frames"]]
    inclusive, leaf, callers = (collections.Counter() for _ in range(3))
    total = core = 0
    for profile in data["profiles"]:
        stack = []
        previous = profile["startValue"]
        for event in profile["events"]:
            elapsed = event["at"] - previous
            frames = [names[i] for i in stack] if elapsed > 0.0001 and stack and names[stack[-1]] == "CPU_TIME" else []
            if (
                frames
                and frames[-1] == "CPU_TIME"
                and any(
                    any(
                        scope in f
                        for scope in (
                            "FSharp.Compiler.CheckExpressions.",
                            "FSharp.Compiler.CheckDeclarations.",
                            "FSharp.Compiler.PostInferenceChecks.",
                        )
                    )
                    for f in frames
                )
            ):
                frames = frames[:-1]
                total += elapsed
                # Exclude Core wrappers that are currently executing compiler callbacks.
                last_compiler = max(i for i, f in enumerate(frames) if "FSharp.Compiler.Service!" in f)
                core_frames = [f for f in frames[last_compiler + 1 :] if f.startswith("FSharp.Core!")]
                if core_frames:
                    core += elapsed
                    inclusive.update({f: elapsed for f in set(core_frames)})
                    if frames[-1].startswith("FSharp.Core!"):
                        leaf[frames[-1]] += elapsed
                    first = next(
                        i for i in range(last_compiler + 1, len(frames)) if frames[i].startswith("FSharp.Core!")
                    )
                    if first:
                        callers[frames[first - 1]] += elapsed
            if event["type"] == "O":
                stack.append(event["frame"])
            else:
                assert stack.pop() == event["frame"]
            previous = event["at"]
    result = dict(
        trace=path,
        sampled_check_ms=total,
        core_inclusive_ms=core,
        inclusive=inclusive.most_common(25),
        leaf=leaf.most_common(25),
        callers=callers.most_common(20),
    )
    results.append(result)
print(json.dumps(results, indent=2))
