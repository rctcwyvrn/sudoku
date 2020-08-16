# Simple script to send grids to the haskell solver
from subprocess import Popen, PIPE
import os

# I can't believe I couldn't figure out a better way to do this, meh it works
def send_grid(grid):
    print(f"sending {grid} now!")
    os.system(f"echo '{grid}' > grid.tmp")
    cmd = ["cat", "grid.tmp"]
    cmd2 = ["cabal", "run"]
    p1 = Popen(cmd, stdout=PIPE)
    p1.wait()
    p2 = Popen(cmd2, stdin=p1.stdout)
    p2.wait()
    os.system("rm grid.tmp")

with open("grids/all_grids.txt") as f:
    cur_grid = ""
    for line in f.readlines():
        if "Grid" in line:
            send_grid(cur_grid)
            cur_grid = ""
        else:
            cur_grid+=line.strip() + "\n"
