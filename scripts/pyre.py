#!/usr/bin/env python2.7

import re


# input = '''
# import qualified Luna.Parser.Pragma as Pragma
# import qualified Luna.Data.Config   as Config
# import           Luna.Syntax.Name   (TName(TName))
# import qualified Luna.Pass2.Analysis.Struct as SA
# import qualified Luna.Pass2.Transform.Parse.Stage2 as Stage2
# import qualified Luna.Pass2.Transform.Parse.Stage1 as Stage1
# import qualified Luna.Pass2.Transform.Desugar.ImplicitSelf as ImplSelf
# import qualified Luna.Pass2.Transform.Hash                 as Hash
# import qualified Luna.Pass2.Transform.SSA                  as SSA
# import qualified Luna.Pass2.Target.HS.HASTGen              as HASTGen
# import qualified Luna.Pass2.Target.HS.HSC                  as HSC
# import qualified Luna.Pass2.Transform.Desugar.ImplicitScopes as ImplScopes
# import qualified Luna.Pass2.Transform.Desugar.ImplicitCalls as ImplCalls
# import           Luna.Data.Namespace (Namespace(Namespace))
# import qualified Luna.Pass as Pass
# '''


def pathProcess(path):
    segs = path.split('.')
    segs = ['Pass' if s == 'Pass2' else s for s in segs]
    return '.'.join(segs)


def segSub(path, pat, s):
    segs = path.split('.')
    segs = [re.sub(pat,s,seg) for seg in segs]
    return '.'.join(segs)

def pathSub(path, pat, s):
    return re.sub(pat,s,path)

impPat = re.compile(r'(?P<imp>import\s+)(?P<qual>(qualified\s+)?)(?P<path>[a-zA-Z0-9.]+)', re.MULTILINE)
modPat = re.compile(r'(?P<mod>module\s+)(?P<path>[a-zA-Z0-9.]+)', re.MULTILINE)

def run(oldPath, newPath, txt):
    def impProc(match):
        return match.group('imp') + match.group('qual') + pathSub(match.group('path'), oldPath, newPath)
    def modProc(match):
        return match.group('mod') + pathSub(match.group('path'), oldPath, newPath)
    txt = impPat.sub(impProc, txt)
    txt = modPat.sub(modProc, txt)
    return txt
