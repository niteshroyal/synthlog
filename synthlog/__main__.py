import os
import subprocess
from argparse import ArgumentParser


def execute(cmd):
    """Execute a command.
    
    From: https://stackoverflow.com/a/4417735/3350448
    """
    popen = subprocess.Popen(cmd, stdout=subprocess.PIPE, universal_newlines=True)
    for stdout_line in iter(popen.stdout.readline, ""):
        yield stdout_line
    popen.stdout.close()
    return_code = popen.wait()
    if return_code:
        raise subprocess.CalledProcessError(return_code, cmd)


def main():
    parser = ArgumentParser()
    parser.add_argument("files", nargs="+", help="files to combine in a Synthlog model")
    parser.add_argument(
        "--problog", dest="problog", default="problog", help="Problog executable"
    )
    args, unknownargs = parser.parse_known_args()

    environment = os.path.join(os.path.dirname(__file__), "environment.pl")
    for line in execute(
        args.problog.split(" ")
        + [environment]
        + args.files
        + unknownargs
        + ["--combine"]
    ):
        print(line, end="")


if __name__ == "__main__":
    main()
