'''
Script to convert the emacs-for-python source in an
emacs-starter-kit-compatible one
'''
from __future__ import print_function
import subprocess
from shutil import copy,copytree
import re,sys,os
from os.path import join as joinp # joinpath, I'll use a lot this
                                  # function and it needs an alias
import fileinput

def run(command):
    """Run a shell command
    """
    subprocess.call(command, shell=True)

def main(argv=sys.argv):
    """Main function
    """
    STARTER_KIT_DIR = argv[1]

    # Copying the python libs and extensions
    run("cp -rT {0} {1}".format("extensions", joinp(STARTER_KIT_DIR,"elpa-to-submit")))
    run("cp -rT {0} {1}".format("python-libs",joinp(STARTER_KIT_DIR,"python-libs")))
    run("cp -rT {0} {1}".format("scripts",joinp(STARTER_KIT_DIR,"scripts")))
    
    # Copying the epy-python and epy-completion files
    stk_python = joinp(STARTER_KIT_DIR, "starter-kit-python.el")
    stk_completion = joinp(STARTER_KIT_DIR, "starter-kit-completion.el")
    
    copy("epy-python.el", stk_python)    
    copy("epy-completion.el", stk_completion)
    
    # Edit the files to be consistent with the new name starter-kit-*
    for line in fileinput.input([stk_python,stk_completion],inplace=True):
        line = line.replace("epy-install-dir","dotfiles-dir")
        line = re.sub("epy-(\w+)", r"starter-kit-\1",line)
        print(line)

if __name__ == '__main__':
    main()
