import subprocess
import shlex
import sys
import os

cmd = shlex.split("pyflakes " + sys.argv[1])
out = subprocess.Popen(cmd, stdout=subprocess.PIPE)
stdout, stderr = out.communicate()
msg = ":".join(stdout.split(":")[:-1]) + ":Warning:" + stdout.split(":")[-1]

print msg
