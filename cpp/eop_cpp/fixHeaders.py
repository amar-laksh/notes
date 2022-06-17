#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# File              : fixHeaders.py
# Author            : Amar Lakshya <amar.lakshya@protonmail.com>
# Date              : 20.04.2022
# Last Modified Date: 20.04.2022
# Last Modified By  : Amar Lakshya <amar.lakshya@protonmail.com>

import os
import re

COMPILE_COMMAND = "meson compile -C builddir &> /dev/null"
#  for currentpath, folders, files in os.walk('.'):
    #  for file in files:
        #  fullpath =os.path.join(currentpath, file)
        #  print(fullpath)
        #  if fullpath.endswith(".cpp") or fullpath.endswith('.hpp'):
            #  print(fullpath)
#  exit
for x in os.listdir():
    if x.endswith(".cpp") or x.endswith('.hpp'):
        allIncludes = []
        with open(x, 'r') as file:
            lines = ''.join(file.readlines())
            lines = re.sub("//.*#include.*", "", lines)
            allIncludes.append(re.findall("#include.*", str(lines)))
        file.close()
        for includes in allIncludes:
            for include in includes:
                if(include):
                    print("#################################")
                    print("Candidate: ", include, " in ", x)
                    lines = ''.join(open(x,'r'))
                    tryLines = re.sub(include, "", lines)
                    open(x,'w').write(tryLines)
                    didItBuild = os.system(COMPILE_COMMAND)
                    if(didItBuild == 0):
                        print("Removing it")
                        continue
                    print("Nope, This one is precious.")
                    open(x,'w').write(lines)
                    print("#################################")


