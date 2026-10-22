import csv
import json
import os
from pathlib import Path
import re
import shutil
import subprocess
import time

ROOT = Path(__file__).resolve().parents[2]
OUT = ROOT / "temp/release-0.5.3/benchmark"
OUT.mkdir(parents=True, exist_ok=True)
import argparse

parser = argparse.ArgumentParser(description="Repeat the Fickl 0.5.3 six-workload comparison")
parser.add_argument(
    "--scratch", type=Path, required=True, help="Previous harness scratchpad containing bench/ and upfable/"
)
parser.add_argument("--tool", type=Path, required=True, help="Packaged Fickl fable.exe")
parser.add_argument("--resume", action="store_true", help="Keep completed workloads and rerun incomplete workloads")
args = parser.parse_args()
SCRATCH = args.scratch.resolve()
TOOL = args.tool.resolve()
UPSTREAM = SCRATCH / "upfable/fable.exe"
PROJECTS = [
    ("py", SCRATCH / "bench/pybench", "App.fsproj", "python", []),
    ("beam", SCRATCH / "bench/beambench", "App.fsproj", "beam", []),
    ("sutil", SCRATCH / "bench/Sutil/src/App", "App.fsproj", "javascript", []),
    ("fantomas", SCRATCH / "bench/fantomas-tools/src/client/fsharp", "FantomasTools.fsproj", "javascript", []),
    ("ionide", SCRATCH / "bench/ionide-vscode-fsharp/src", "Ionide.FSharp.fsproj", "javascript", []),
    ("jssuite", ROOT / "tests/Js/Main", "Fable.Tests.fsproj", "javascript", ["--exclude", "Fable.Core"]),
]


def run(project, cfg, round_no):
    name, directory, project_file, language, extra = project
    output = OUT / "output" / f"{name}-{cfg}"
    assert output.resolve().is_relative_to(OUT.resolve())
    if output.exists():
        shutil.rmtree(output)
    env = os.environ.copy()
    env.pop("DOTNET_CLI_TELEMETRY_OPTOUT", None)
    env.pop("FICKL_NUGET_KEY", None)
    if cfg == "fableq":
        env["DOTNET_CLI_TELEMETRY_OPTOUT"] = "1"
    env["FICKL_PACKAGE_CACHE"] = str(OUT / "package-cache")
    command = [
        "rtk",
        "proxy",
        str(UPSTREAM if cfg.startswith("fable") else TOOL),
        project_file,
        "--lang",
        language,
        *extra,
        "--outDir",
        str(output),
        "--noCache",
    ]
    if cfg == "nopkg":
        command += ["--noPackageCache"]
    if cfg == "miss":
        env["FICKL_PACKAGE_CACHE"] = str(OUT / f"miss-cache-{name}")
    started = time.perf_counter()
    result = subprocess.run(
        command, cwd=directory, env=env, capture_output=True, text=True, encoding="utf-8", errors="replace"
    )
    wall = round((time.perf_counter() - started) * 1000)
    log = result.stdout + result.stderr
    (OUT / f"{name}-{cfg}-{round_no}.log").write_text(log, encoding="utf-8")

    def timing(pattern):
        matches = re.findall(pattern, log)
        return int(matches[-1]) if matches else None

    row = dict(
        project=name,
        configuration=cfg,
        run=round_no,
        wall_ms=wall,
        parsed_ms=timing(r"parsed in (\d+)ms"),
        compile_ms=timing(r"compilation finished in (\d+)ms"),
        exit_code=result.returncode,
    )
    with (OUT / "results.csv").open("a", newline="", encoding="utf-8") as file:
        writer = csv.DictWriter(file, fieldnames=list(row))
        if file.tell() == 0:
            writer.writeheader()
        writer.writerow(row)
    print(json.dumps(row), flush=True)
    if result.returncode or row["compile_ms"] is None:
        raise RuntimeError(f"Benchmark failed: {name}/{cfg}/{round_no}; see saved log")


if (OUT / "results.csv").exists() and not args.resume:
    raise FileExistsError(f"Archive the previous results before rerunning: {OUT}")

if args.resume and (OUT / "results.csv").exists():
    previous = list(csv.DictReader((OUT / "results.csv").open(newline="")))
    complete = {
        p[0]
        for p in PROJECTS
        if all(
            len(
                [
                    r
                    for r in previous
                    if r["project"] == p[0]
                    and r["configuration"] == cfg
                    and int(r["run"]) > 0
                    and r["exit_code"] == "0"
                ]
            )
            == 5
            for cfg in ["fable", "fableq", "nopkg", "hit"]
        )
    }
    shutil.copy2(OUT / "results.csv", OUT / f"results-before-resume-{time.time_ns()}.csv")
    with (OUT / "results.csv").open("w", newline="") as file:
        writer = csv.DictWriter(file, fieldnames=previous[0].keys())
        writer.writeheader()
        writer.writerows(r for r in previous if r["project"] in complete)
    PROJECTS = [p for p in PROJECTS if p[0] not in complete]

try:
    for project in PROJECTS:
        for config in ["fable", "fableq", "nopkg", "hit"]:
            run(project, config, -1)
        run(project, "miss", 0)
        for round_no in range(1, 6):
            order = ["fable", "hit", "fableq", "nopkg"] if round_no % 2 else ["nopkg", "fableq", "hit", "fable"]
            for config in order:
                run(project, config, round_no)
finally:
    subprocess.run(
        [
            "rtk",
            "proxy",
            "powershell",
            "-NoProfile",
            "-ExecutionPolicy",
            "Bypass",
            "-File",
            str(Path(__file__).with_name("cleanup-msbuild.ps1")),
        ],
        check=True,
    )
