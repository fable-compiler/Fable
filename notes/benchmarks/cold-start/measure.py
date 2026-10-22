import csv
import hashlib
import os
from pathlib import Path
import re
import subprocess
import time

root = Path(__file__).resolve().parents[3]
dest = root / "temp/cold-start"
tool = root / "temp/release-0.5.3/verified-package/tools/net10.0/win-x64/fable.exe"
env = dict(os.environ, DOTNET_CLI_TELEMETRY_OPTOUT="1")
rows = []
expected = {}
for workload, project, extra in [
    ("version", None, []),
    ("tiny", Path(__file__).with_name("App.fsproj"), []),
    ("js", root / "tests/Js/Main/Fable.Tests.fsproj", ["--exclude", "Fable.Core"]),
]:
    for run in range(6):
        configs = ["restore", "no-restore"] if run % 2 == 0 else ["no-restore", "restore"]
        if workload == "version":
            configs = ["version"]
        for cfg in configs:
            output = dest / f"output-{workload}"
            args = (
                ["--version"]
                if project is None
                else [str(project), "--outDir", str(output), "--noCache", "--noPackageCache", "--timings", *extra]
            )
            if cfg == "no-restore":
                args.append("--noRestore")
            start = time.perf_counter()
            result = subprocess.run(
                ["rtk", "proxy", str(tool), *args],
                env=env,
                capture_output=True,
                text=True,
                encoding="utf-8",
                errors="replace",
            )
            wall = round((time.perf_counter() - start) * 1000)
            log = result.stdout + result.stderr
            (dest / f"{workload}-{cfg}-{run}.log").write_text(log, encoding="utf-8")
            if result.returncode:
                raise RuntimeError(log)

            def timing(pattern):
                found = re.search(pattern, log)
                return int(found[1]) if found else 0

            if project:
                hashes = {
                    str(p.relative_to(output)): hashlib.sha256(p.read_bytes()).hexdigest() for p in output.rglob("*.js")
                }
                if workload not in expected:
                    expected[workload] = hashes
                assert hashes == expected[workload], "Generated output differs"
            row = dict(
                workload=workload,
                config=cfg,
                run=run,
                wall_ms=wall,
                crack_ms=timing(r"Project cracking \(wall\)\s+(\d+) ms"),
                compile_ms=timing(r"Compilation total \(wall\)\s+(\d+) ms"),
            )
            rows.append(row)
            print(row, flush=True)
            with (dest / "results.csv").open("w", newline="") as f:
                writer = csv.DictWriter(f, fieldnames=row.keys())
                writer.writeheader()
                writer.writerows(rows)
