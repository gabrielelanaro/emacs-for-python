from Pymacs import Let, lisp
from unittest import TestLoader
import os

def symbol(sym):
    return lisp[sym]

def discover(root_dir):
    loader = TestLoader()
    prev_dir = os.curdir
    os.chdir(root_dir)
    tests = loader.discover(root_dir, top_level_dir=root_dir)
    os.chdir(prev_dir)
    ret = []
    for suite in tests:
        for suite2 in suite:
            for test in suite2:
                name = ".".join((test.__class__.__name__, test._testMethodName))
                module = test.__module__
                ret.append([symbol(":name"), name,
                            symbol(":module"), module,
                            symbol(":root"), root_dir])
    return ret

if __name__ == '__main__':
    print discover("/home/galois/workspace/example/")