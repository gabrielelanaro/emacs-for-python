import itertools
import os
import shutil
from scripting.commands import rm,cp,mkdir
import glob

def compile_el_file(fn):
    """
    Compile an emacs file FN
    """
    sh("emacs -Q -l epy-init.el -batch -f batch-byte-compile "+fn)

def clean():
    """TODO: Clean the build stuf etc...
    """
    rm("dist")

def package_command():
    """Package the emacs-for-python distribution
    """
    GLOBS = "COPYING README.org epy-*.el extensions\
 doc python-libs scripts".split()
    
    # Concatenating lists
    FILES = itertools.chain(*(glob.glob(g) for g in GLOBS))
    mkdir("dist/emacs-for-python-x.x")
    cp(FILES,"dist/emacs-for-python-x.x")
    archive(["dist/emacs-for-python-x.x"], 
            "dist/emacs-for-python-x.x.tar.gz",format="tar")


if __name__ == '__main__':
    run()
