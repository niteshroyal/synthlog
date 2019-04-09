import os
import sys
import subprocess


def execute(cmd):
    """Execute a command.
    
    From: https://stackoverflow.com/a/4417735/3350448
    """
    popen = subprocess.Popen(
        cmd, stdout=subprocess.PIPE, universal_newlines=True
    )
    for stdout_line in iter(popen.stdout.readline, ""):
        yield stdout_line
    popen.stdout.close()
    return_code = popen.wait()
    if return_code:
        raise subprocess.CalledProcessError(return_code, cmd)


def main():
    environment = os.path.join(os.path.dirname(__file__), "environment.pl")
    for line in execute(
        ["problog", environment] + sys.argv[1:] + ["--combine"]
    ):
        print(line, end="")


if __name__ == "__main__":
    main()
