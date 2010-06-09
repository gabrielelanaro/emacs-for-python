'''File to be used in conjunction with pymacs
'''
from Pymacs import lisp
from rope.ide import codeassist

def rope_candidates():
    offset = lisp.point()
    start, end = lisp.point_min(), lisp.point_max()
    
    text = lisp.buffer_substring(m, M)
    project = 1
    return codeassist.code_assist(project, text, offset)
