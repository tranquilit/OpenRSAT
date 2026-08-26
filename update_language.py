import glob
import subprocess
import os

import argparse
from typing import Optional, Sequence

"""
Sample:
    python.exe update_language.py --pot=languages\OpenRSAT.pot --project-folder=. --lrj-pattern="**/*.lrj" --rsj-pattern="**/lib/x86_64-win64/*.rsj" --lazarus-folder=C:\lazarus_4_0
"""

def main(argv: Optional[Sequence[str]] = None) -> int:
    ap = argparse.ArgumentParser(description="Update OpenRSAT .pot file from Lazarus .lrj and .rsj files.")
    ap.add_argument("--pot", help="Ouput POT path", required=True)
    ap.add_argument("--project-folder", help="Project root folder", required=True)
    ap.add_argument("--lrj-pattern", help="Input LRJ folder path", required=True)
    ap.add_argument("--rsj-pattern", help="Input RSJ folder path", required=True)
    ap.add_argument("--lazarus-folder", help="Lazarus folder path", required=True)
    args = ap.parse_args(argv)

    print(f"Searching for lrj files with pattern: {args.lrj_pattern}")
    lrjFiles = glob.glob(args.lrj_pattern, recursive=True, root_dir=args.project_folder)
    print(f"{len(lrjFiles)} files found.")

    print(f"Searching for rsj files with pattern: {args.rsj_pattern}")
    rsjFiles = glob.glob(args.rsj_pattern, recursive=True, root_dir=args.project_folder)
    print(f"{len(rsjFiles)} files found.")

    print(f"Searching for updatepofiles in folder: {args.lazarus_folder}")
    updatepofiles = glob.glob(f"**/updatepofiles.exe", recursive=True, root_dir=args.lazarus_folder)[0]
    print(f"updatepofiles found.")

    if not os.path.exists(args.pot):
        print(f"Missing pot file: {args.pot}. Creating file...")
        with open(args.pot, "w") as f:
            f.write("")
        print(f"File created.")

    print(f"Prepare command to run...")
    command = [os.path.join(args.lazarus_folder, updatepofiles), *[os.path.join(args.project_folder, file) for file in lrjFiles], *[os.path.join(args.project_folder, file) for file in rsjFiles], os.path.join(args.project_folder, args.pot)]
    print(f"Run command: {command}")
    subprocess.run(command)
    print(f"POT file as been updated.")

if __name__ == "__main__":
    raise SystemExit(main())