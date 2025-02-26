import subprocess, os

skipped_files = ["cordic.bril"]

for root, subdirs, files in os.walk(
    "/Users/parthsarkar/Desktop/benchmarks/benchmarks/"
):
    for file in files:
        if file.endswith(".bril") and (file not in skipped_files):
            filepath = os.path.join(root, file)
            print(filepath)
            cmd = f"/Users/parthsarkar/.local/bin/bril2json < {filepath} | dune exec bin/main.exe"
            subprocess.run(
                [cmd],
                shell=True,
            )
