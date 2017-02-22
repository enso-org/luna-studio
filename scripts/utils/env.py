from utils.colors      import print_info
from subprocess        import call, Popen, PIPE, check_output, CalledProcessError
from distutils.version import LooseVersion

def checkCabalVersion():
    minv = '1.19.1'
    print_info ("Checking cabal version (>= %s)" % minv)
    (out, err) = Popen("cabal --numeric-version", stdout=PIPE, shell=True).communicate()
    out = out.lstrip().rstrip()
    if not LooseVersion(out) >= LooseVersion(minv): return (False, "Your %s version is too old (%s)" % ("cabal", out))
    else: return (True, None)


def checkGHCVersion():
    minv = '7.8.2'
    print_info ("Checking GHC version (>= %s)" % minv)
    (out, err) = Popen("ghc --numeric-version", stdout=PIPE, shell=True).communicate()
    out = out.lstrip().rstrip()
    if not LooseVersion(out) >= LooseVersion(minv): return (False, "Your %s version is too old (%s)" % ("ghc", out))
    else: return (True, None)