## ms1_env_info_py.py
## Run in PY0617: python ms1_env_info_py.py

import sys
import importlib.metadata

print("============================================================")
print("MS1 Environment Info — Python")
print("============================================================\n")

## ── 1. Python version ─────────────────────────────────────────
print("── Python Version ──")
print(f"  {sys.version}\n")

## ── 2. Python package versions ────────────────────────────────
print("── Python Package Versions ──")
py_pkgs = ["earthengine-api", "pandas"]
for pkg in py_pkgs:
    try:
        ver = importlib.metadata.version(pkg)
    except importlib.metadata.PackageNotFoundError:
        ver = "not installed"
    print(f"  {pkg:<20s} {ver}")

print("\n============================================================")
