###########################################################################
## Copyright (C) Flowbox, Inc / All Rights Reserved
## Unauthorized copying of this file, via any medium is strictly prohibited
## Proprietary and confidential
## Flowbox Team <contact@flowbox.io>, 2014
###########################################################################

import platform
import sys
import os
from colors         import print_info, print_error
from utils.net      import download



def enum(*sequential, **named):
    enums = dict(zip(sequential, range(len(sequential))), **named)
    return type('Enum', (), enums)


systems = enum ('LINUX', 'WINDOWS', 'DARWIN')

osname = platform.system()
system = None
if osname == 'Windows' or osname.startswith('CYGWIN_NT'):
    system    = systems.WINDOWS
    osname = 'mingw32'
elif osname == 'Linux':
    system    = systems.LINUX
    osname = 'linux'
elif osname == 'Darwin':
    system    = systems.DARWIN
    osname = 'darwin'
else:
    print_error ("Unsupported system '%s'" % osname)
    sys.exit(1)


def platformFix(sboxPath):
    if system == systems.WINDOWS:
        # Otherwise Haskell Platform binaries (with cabal) will overwrite the cabal we install
        cabalBinariesPath = os.path.join(os.environ["APPDATA"], "cabal", "bin")
        #print_info("Prepending %s to PATH in order to overwrite old Haskell Platform binaries" % cabalBinariesPath)
        os.environ["PATH"] = cabalBinariesPath + ';' + os.environ["PATH"]
        os.environ["HOME"] = os.path.expanduser("~")


        descriptorUrl = "http://protobuf.googlecode.com/svn/trunk/src/google/protobuf/descriptor.proto"
        descriptorDstPath = os.path.join(sboxPath, "proto", "google", "protobuf", "descriptor.proto")
        if not os.path.exists(descriptorDstPath):
            download(descriptorUrl, descriptorDstPath)

def normalizePath(name):
    return name.replace(os.sep, '/')
