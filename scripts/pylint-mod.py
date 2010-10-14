#!/usr/bin/env python
import os,sys
# For pylint 0.9
os.system("pylint -f parseable -r n --disable-msg-cat=C,R,I " + " ".join(sys.argv[1:]))
# For pylint 1.2
os.system("pylint -f parseable -r n --disable=C,R,I " + " ".join(sys.argv[1:]))
