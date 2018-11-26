#!/usr/bin/python

import os, sys

if len(sys.argv) == 1:
    print "gf find-name-expr grep-expr [dir] [grep-flags]"
    exit();

if len(sys.argv) >= 4:
    loc = sys.argv[3]
else:
    loc = '.'
    
cmd = "find " + loc + " -type f -name " + sys.argv[1] + " -exec grep "

if len(sys.argv) >= 5:
    cmd += sys.argv[4] + " "

cmd += '"' + sys.argv[2] + '"' + " \{} \+"

print cmd
os.system(cmd)


# For quote marks:
# pfind.py . '\"Method'
