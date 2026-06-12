#!/usr/bin/env python3
"""
git_push.py
Run this from inside your shock_analyzer folder.
It stages all files, commits with a timestamp message, and pushes to origin main.

Usage:
    python git_push.py
    python git_push.py "your custom commit message"
"""

import subprocess
import sys
from datetime import datetime
from pathlib import Path

# ── Config ────────────────────────────────────────────────────
REPO_DIR = Path(__file__).parent          # folder this script lives in
REMOTE   = "origin"
BRANCH   = "main"

# Files / patterns to exclude from staging (add more if needed)
EXCLUDE  = [
    "__pycache__",
    "*.pyc",
    "venv/",
    ".env",
    "*.log",
]

# ── Helpers ───────────────────────────────────────────────────

def run(cmd: list, cwd=None, check=True) -> subprocess.CompletedProcess:
    result = subprocess.run(
        cmd,
        cwd=cwd or REPO_DIR,
        capture_output=True,
        text=True,
    )
    if check and result.returncode != 0:
        print(f"\n❌  Command failed: {' '.join(cmd)}")
        print(result.stderr.strip())
        sys.exit(1)
    return result


def git(*args, check=True):
    return run(["git"] + list(args), check=check)


# ── Main ──────────────────────────────────────────────────────

def main():
    print(f"📂  Repo: {REPO_DIR}")

    # 1. Make sure we are inside a git repo
    result = git("rev-parse", "--is-inside-work-tree", check=False)
    if result.returncode != 0:
        print("\n❌  Not a git repository.")
        print("    Run:  git init  then  git remote add origin <url>  first.")
        sys.exit(1)

    # 2. Show current branch
    branch = git("branch", "--show-current").stdout.strip() or BRANCH
    print(f"🌿  Branch: {branch}")

    # 3. Pull latest to avoid conflicts
    print("\n⬇️   Pulling latest from remote...")
    pull = git("pull", REMOTE, branch, check=False)
    if pull.returncode != 0:
        print("⚠️   Pull failed (remote may not exist yet, continuing with push).")
    else:
        print(pull.stdout.strip() or "Already up to date.")

    # 4. Stage everything
    print("\n📦  Staging all changes...")
    git("add", "--all")

    # 5. Check if there is anything to commit
    status = git("status", "--porcelain")
    if not status.stdout.strip():
        print("✅  Nothing to commit — working tree is clean.")
        sys.exit(0)

    print(status.stdout.strip())

    # 6. Commit message
    if len(sys.argv) > 1:
        msg = " ".join(sys.argv[1:])
    else:
        ts  = datetime.now().strftime("%Y-%m-%d %H:%M")
        msg = f"shock_analyzer update — {ts}"

    print(f"\n📝  Commit: {msg}")
    git("commit", "-m", msg)

    # 7. Push
    print(f"\n⬆️   Pushing to {REMOTE}/{branch}...")
    push = git("push", REMOTE, branch, check=False)

    if push.returncode == 0:
        print("\n✅  Push successful.")
        log = git("log", "--oneline", "-3").stdout.strip()
        print(f"\nLast 3 commits:\n{log}")
    else:
        stderr = push.stderr.strip()
        # Handle first push — set upstream
        if "no upstream" in stderr or "set-upstream" in stderr:
            print("🔧  Setting upstream and pushing...")
            git("push", "--set-upstream", REMOTE, branch)
            print("\n✅  Push successful (upstream set).")
        else:
            print(f"\n❌  Push failed:\n{stderr}")
            sys.exit(1)


if __name__ == "__main__":
    main()