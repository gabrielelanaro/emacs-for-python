import glob
import itertools

from scriptine import run

from scripting.commands import rm,cp,mkdir,sh_cmdln,archive,find


VERSION = "0.2"
def compile_el_file(fn):
    """
    Compile an emacs file FN
    """
    sh_cmdln("emacs -Q -l epy-init.el -batch -f batch-byte-compile",[fn])

def clean():
    """TODO: Clean the build stuff etc...
    """
    # removing compiled files
    [rm(f) for f in find("*.pyc")]
    [rm(f) for f in find("*.elc")]
    
    rm("dist")

def package_command():
    """Package the emacs-for-python distribution
    """
    clean()
    GLOBS = "COPYING README.org epy-*.el extensions\
 doc python-libs scripts".split()
    
    # Concatenating lists
    FILES = itertools.chain(*(glob.glob(g) for g in GLOBS))
    mkdir("dist/emacs-for-python-"+VERSION,parent=True)
    [cp(f,"dist/emacs-for-python-"+VERSION) for f in FILES]
    archive(["dist/emacs-for-python-"+VERSION], 
            "dist/emacs-for-python-%s.tar.gz"%VERSION,format="gzip")


if __name__ == '__main__':
    run()
