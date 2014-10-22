import glob
import itertools
import os
from scripting.commands import rm,cp,mkdir,sh, sh_args, archive,find, unpack, basename
from scripting.runner import command, run

VERSION = "0.3"

# Globs that represents the file included in the distribution
MANIFEST = """
COPYING 
README.org
CONTRIBUTORS
epy-*.el
extensions
doc
python-libs
scripts
snippets
"""

def compile_el(fn, load=None):
    """
    Compile a set of .el files using emacs
    """
    cmdlist = ['emacs', '-Q']
    if load:
        cmdlist.append('-l')
        cmdlist.append(load)
    cmdlist += ['-batch', '-f', 'batch-byte-compile']
    cmdlist.extend(fn)
    sh_args(cmdlist)

@command
def compile():
    """byte compile .el files
    """
    compile_el(find('*.el'), 'epy-init.el')
    

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
    globs = MANIFEST.split()
    # Concatenating lists
    files = itertools.chain(*(glob.glob(g) for g in globs))
    mkdir("dist/emacs-for-python-"+VERSION,parent=True)
    cp(files,"dist/emacs-for-python-"+VERSION)
    archive("dist/emacs-for-python-"+VERSION, 
            "dist/emacs-for-python-%s.tar.gz"%VERSION,format="gzip")
    archive("dist/emacs-for-python-"+VERSION, 
            "dist/emacs-for-python-%s.zip"%VERSION,format="zip")

@command
def test_run():
    sh('emacs -Q -l epy-init.el')

@command
def test_pkg(package):
    """Test running the package
    """
    mkdir('dist/test', parent=True)
    unpack(package, 'dist/test/')
    dirname = basename( ".".join(package.split(".")[:-2]))
    sh('emacs -Q -l dist/test/'+dirname+'/epy-init.el')

@command
def test_02():
    clean()
    package("0.2")
    test_pkg("dist/emacs-for-python-0.2.tar.gz")

if __name__ == '__main__':
    run()
