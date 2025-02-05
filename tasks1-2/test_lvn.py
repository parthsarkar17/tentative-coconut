import os
import subprocess
import re


for root, subdirs, files in os.walk(
    "/Users/parthsarkar/Desktop/benchmarks/benchmarks/"
):

    for file in files:
        if file.endswith(".bril"):

            filepath = os.path.join(root, file)

            with open(filepath, "r") as file:
                data = file.read()
                arg_exists = len(re.findall("ARG", data)) != 0
                float_exists = len(re.findall("float", data)) != 0

                if float_exists:
                    continue

                args = (
                    re.findall("(ARGS:)+((.*)?\n)", data)[0][-1] if arg_exists else ""
                )

                cmd1 = f"/Users/parthsarkar/.local/bin/bril2json < {filepath}  | dune exec bin/main.exe | brili {args} > bruh1.txt"
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
