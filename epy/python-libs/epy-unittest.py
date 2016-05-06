from Pymacs import Let, lisp
from unittest import TestLoader
from itertools import groupby

import os

def symbol(sym):
    return lisp[sym]

def discover(root_dir):
    if not os.path.exists(root_dir):
        return []

    loader = TestLoader()
    prev_dir = os.curdir
    os.chdir(root_dir)
    
    tests = loader.discover(root_dir, top_level_dir=root_dir)
    os.chdir(prev_dir)
    
    ret = []
    for suite in tests:
        for suite2 in suite:
            if suite2.__class__.__name__ == 'ModuleImportFailure':
                continue
            for test in suite2:
                name = ".".join((test.__class__.__name__, test._testMethodName))
                module = test.__module__
                ret.append([symbol(":name"), name,
                            symbol(":module"), module,
                            symbol(":root"), root_dir])

    modkey = lambda x: x[3]
    ret.sort(key=modkey)

    return [[k, list(g)] for k,g in groupby(ret, key=modkey)] # Converting to a list of lists

if __name__ == '__main__':
    a = discover("/home/galois/workspace/shit/")
