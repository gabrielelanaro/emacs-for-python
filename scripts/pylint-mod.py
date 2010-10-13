#!/usr/bin/env python
import os,sys
os.system("pylint -f parseable -r n --disable=C,R,I " + " ".join(sys.argv[1:]))
