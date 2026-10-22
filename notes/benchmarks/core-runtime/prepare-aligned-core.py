"""Build and check an isolated Core baseline; never replaces the shipped runtime."""

import argparse
import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess

ROOT = Path(__file__).resolve().parents[3]
FCS_COMMIT = "7d0437e8bcf9ead9483483eaa556f1998af8806b"
VMR_COMMIT = "c23858a6d860abe3ca94f9ac630c3e61b2625664"
parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument("--source", type=Path, default=ROOT / "temp/core-runtime-source")
parser.add_argument(
    "--runtime",
    type=Path,
    default=ROOT / "temp/release-0.5.3/verified-package/tools/net10.0/win-x64/FSharp.Core.dll",
)
args = parser.parse_args()
source = args.source.resolve()
runtime = args.runtime.resolve()
output = ROOT / "temp/core-runtime-baseline"
output.mkdir(parents=True, exist_ok=True)


def run(*command, cwd=ROOT, capture=False, env=None):
    result = subprocess.run(
        ["rtk", "proxy", *map(str, command)],
        cwd=cwd,
        env=env,
        check=True,
        text=True,
        capture_output=capture,
    )
    return result.stdout.strip() if capture else None


if not runtime.is_file():
    raise FileNotFoundError(runtime)
if not source.exists():
    run("git", "clone", "--filter=blob:none", "--no-checkout", "https://github.com/dotnet/fsharp.git", source)
    run("git", "checkout", "--detach", FCS_COMMIT, cwd=source)
if run("git", "rev-parse", "HEAD", cwd=source, capture=True) != FCS_COMMIT:
    raise RuntimeError("Source checkout is not the pinned runtime source commit")
if run("git", "status", "--porcelain", cwd=source, capture=True):
    raise RuntimeError("Baseline build requires an unchanged source checkout")

# Only the public key is extracted. Public signing is for this .NET experiment;
# it is not a Microsoft signature or authorization to publish a replacement.
key = output / "runtime-public-key.snk"
env = dict(os.environ, FICKL_CORE_INPUT=str(runtime), FICKL_CORE_KEY_OUT=str(key))
run(
    "powershell",
    "-NoProfile",
    "-Command",
    "$name = [Reflection.AssemblyName]::GetAssemblyName($env:FICKL_CORE_INPUT); "
    "if ($name.FullName -ne 'FSharp.Core, Version=10.1.0.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a') "
    "{ throw 'Unexpected runtime assembly identity' }; "
    "[IO.File]::WriteAllBytes($env:FICKL_CORE_KEY_OUT, $name.GetPublicKey())",
    env=env,
)
run(
    "dotnet",
    "build",
    "src/FSharp.Core/FSharp.Core.fsproj",
    "-c",
    "Release",
    "-f",
    "netstandard2.1",
    "-p:BUILDING_USING_DOTNET=true",
    "-p:AssemblyVersion=10.1.0.0",
    "-p:SignAssembly=true",
    "-p:PublicSign=true",
    f"-p:AssemblyOriginatorKeyFile={key}",
    "-p:NuGetAudit=false",
    "-v:q",
    cwd=source,
)
built = source / "artifacts/bin/FSharp.Core/Release/netstandard2.1/FSharp.Core.dll"
baseline = output / "FSharp.Core.dll"
shutil.copy2(built, baseline)
project = Path(__file__).with_name("Surface.csproj")
run("dotnet", "build", project, "-c", "Release", "-v:q")
probe = project.parent / "bin/Release/net10.0/Surface.dll"
for label, assembly in [("runtime", runtime), ("baseline", baseline)]:
    run("dotnet", probe, assembly, output / f"{label}-api.txt", "true")
if (output / "runtime-api.txt").read_text().splitlines() != (output / "baseline-api.txt").read_text().splitlines():
    raise RuntimeError("Reflected API differs from the shipped runtime")
metadata = dict(
    source_commit=FCS_COMMIT,
    dotnet_source_commit=VMR_COMMIT,
    runtime_package="FSharp.Core/10.1.203",
    target_framework="netstandard2.1",
    public_signed=True,
    runtime_sha256=hashlib.sha256(runtime.read_bytes()).hexdigest(),
    baseline_sha256=hashlib.sha256(baseline.read_bytes()).hexdigest(),
    api_and_anonymous_object_equality_match=True,
)
(output / "manifest.json").write_text(json.dumps(metadata, indent=2))
print(f"Verified isolated baseline: {baseline}")
