import platform
import sys
import os



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

def unix():
    return system in [systems.LINUX, systems.DARWIN]

def linux():
    return system == systems.LINUX

def windows():
    return system == systems.WINDOWS

def darwin():
    return system == systems.DARWIN
