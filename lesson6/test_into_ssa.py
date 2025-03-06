import os
import subprocess
import re

skipped_files = ["function_call.bril"]


def test_ssa_form():
    for root, _, files in os.walk("/Users/parthsarkar/Desktop/benchmarks/benchmarks/"):
        for file in files:
            if file.endswith(".bril") and (file not in skipped_files):
                filepath = os.path.join(root, file)
                print(filepath)
                cmd = f"/Users/parthsarkar/.local/bin/bril2json < {filepath} | dune exec bin/main.exe | python ../../bril/examples/is_ssa.py"
                subprocess.run(
                    [cmd],
                    shell=True,
                )


def test_correctness():
    for root, _, files in os.walk("/Users/parthsarkar/Desktop/benchmarks/benchmarks/"):

        for file in files:
            if file.endswith(".bril") and ((file not in skipped_files)):
                print(file)

                filepath = os.path.join(root, file)

                with open(filepath, "r") as file:
                    data = file.read()
                    arg_exists = len(re.findall("ARG", data)) != 0

                    args = (
                        re.findall("(ARGS:)+((.*)?\n)", data)[0][-1]
                        if arg_exists
                        else ""
                    )

                    cmd1 = f"/Users/parthsarkar/.local/bin/bril2json < {filepath}  | dune exec bin/main.exe rt | brili {args} > bruh1.txt"
                    cmd2 = f"/Users/parthsarkar/.local/bin/bril2json < {filepath}  | brili {args} > bruh2.txt"

                    subprocess.run(
                        [cmd1],
                        shell=True,
                    )

                    subprocess.run(
                        [cmd2],
                        shell=True,
                    )

                    subprocess.run(
                        ["diff bruh1.txt bruh2.txt"],
                        shell=True,
                    )


if __name__ == "__main__":
    # test_correctness()
    test_ssa_form()
