import csv
from pathlib import Path
from statistics import median
import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.patches import Patch

root = Path(__file__).resolve().parents[2]
rows = list(csv.DictReader((root / "temp/release-0.5.3/benchmark/results.csv").open()))
names = [
    ("sutil", "Sutil demo app"),
    ("fantomas", "Fantomas online tools"),
    ("ionide", "Ionide for VS Code"),
    ("jssuite", "Fable JS test suite"),
    ("py", "Python package workload"),
    ("beam", "BEAM package workload"),
]
configs = ["fable", "fableq", "nopkg", "hit"]
labels = [
    "Fable 5.22",
    "Fable 5.22 · telemetry opted out",
    "Fickl 0.5.3 · package cache off",
    "Fickl 0.5.3 · package cache hit",
]
colors = ["#2876cf", "#99bde8", "#ea713e", "#13a477"]
values = {}
for name, _ in names:
    for cfg in configs:
        selected = [r for r in rows if r["project"] == name and r["configuration"] == cfg and int(r["run"]) > 0]
        assert len(selected) == 5 and all(int(r["exit_code"]) == 0 for r in selected), (name, cfg)
        values[name, cfg] = median(float(r["wall_ms"]) / 1000 for r in selected)
plt.rcParams.update(
    {"font.family": "DejaVu Sans", "font.size": 11, "text.color": "#17232b", "axes.labelcolor": "#50616d"}
)
fig, axes = plt.subplots(6, 1, figsize=(13, 11.5))
fig.patch.set_facecolor("#fafbf9")
fig.subplots_adjust(top=0.82, bottom=0.16, left=0.245, right=0.88, hspace=0.42)
fig.text(0.035, 0.956, "Fickl 0.5.3 candidate against Fable 5.22", fontsize=25, fontweight="bold")
fig.text(
    0.035,
    0.919,
    "End-to-end compilation time · fresh process · median of 5 interleaved runs",
    fontsize=12,
    color="#50616d",
)
fig.legend(
    handles=[Patch(color=c, label=l) for c, l in zip(colors, labels)],
    loc="upper left",
    bbox_to_anchor=(0.027, 0.893),
    ncol=2,
    frameon=False,
    fontsize=11,
)
limit = max(values.values()) * 1.13
for ax, (name, label) in zip(axes, names):
    ax.set_facecolor("#fafbf9")
    for i, cfg in enumerate(configs):
        v = values[name, cfg]
        ax.barh(i, v, height=0.77, color=colors[i])
        ax.text(v + limit * 0.009, i, f"{v:.1f} s", va="center", fontsize=10)
    ax.invert_yaxis()
    ax.set_xlim(0, limit)
    ax.set_yticks([])
    ax.set_axisbelow(True)
    ax.grid(axis="x", color="#dfe5e8", linewidth=0.7)
    ax.tick_params(axis="x", length=0, labelsize=9, colors="#657580")
    for spine in ax.spines.values():
        spine.set_visible(False)
    ax.text(-0.035, 0.5, label, transform=ax.transAxes, ha="right", va="center", fontsize=12, fontweight="bold")
    ratio = values[name, "fableq"] / values[name, "hit"]
    ax.text(1.04, 0.58, f"{ratio:.1f}×", transform=ax.transAxes, fontsize=19, fontweight="bold", color=colors[3])
    ax.text(1.04, 0.26, "faster*", transform=ax.transAxes, fontsize=9, color="#657580")
axes[-1].set_xlabel("Wall-clock seconds — lower is better", labelpad=8)
fig.text(
    0.035,
    0.083,
    "*Cache-hit speedup versus Fable with DOTNET_CLI_TELEMETRY_OPTOUT=1. All runs use --noCache; restores are included.",
    fontsize=9,
    color="#50616d",
)
fig.text(
    0.035,
    0.062,
    "Package-cache hits are warmed; first-use cache construction is excluded. JS suite uses --exclude Fable.Core.",
    fontsize=9,
    color="#50616d",
)
fig.text(
    0.035,
    0.041,
    "Windows x64 · .NET 10 · 22 Oct 2026. Telemetry/network overhead is machine-specific. Glutinum excluded: both compilers fail.",
    fontsize=9,
    color="#50616d",
)
out = root / "notes/benchmarks"
out.mkdir(exist_ok=True)
fig.savefig(out / "fickl-0.5.3.png", dpi=160, facecolor=fig.get_facecolor())
(out / "fickl-0.5.3.csv").write_text((root / "temp/release-0.5.3/benchmark/results.csv").read_text())
for name, _ in names:
    print(name, {cfg: values[name, cfg] for cfg in configs})
