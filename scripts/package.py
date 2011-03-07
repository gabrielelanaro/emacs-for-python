import glob
import itertools
import os
from scripting.commands import rm,cp,mkdir,sh_cmdln, sh_args, archive,find
from scripting.runner import command, run

VERSION = "0.2"
def compile_el(fn, load='epy-init.el'):
    """
    Compile a set of .el files using emacs
    """
    cmdlist = ['emacs', '-Q']
    if load:
        cmdlist.append(load)
    cmdlist += ['-batch', '-f', 'batch-byte-compile']
    cmdlist.extend(fn)
    sh_args(cmdlist)

@command
def compile():
    compile_el(find('*.el'))

@command
def clean():
    """TODO: Clean the build stuff etc...
    """
    # removing compiled files
    rm(find("*.pyc"))
    rm(find('*.elc'))
    
    if os.path.exists("dist"):
        rm("dist")

@command
def package(VERSION):
    """Package the emacs-for-python distribution
    """
    clean()
    GLOBS = "COPYING README.org epy-*.el extensions\
 doc python-libs scripts".split()
    
    # Concatenating lists
    FILES = itertools.chain(*(glob.glob(g) for g in GLOBS))
    mkdir("dist/emacs-for-python-"+VERSION,parent=True)
    cp(FILES,"dist/emacs-for-python-"+VERSION)
    archive("dist/emacs-for-python-"+VERSION, 
            "dist/emacs-for-python-%s.tar.gz"%VERSION,format="gzip")

@command
def test_run():
    sh_cmdln('emacs -Q -l epy-init.el', [])


if __name__ == '__main__':
    run()
