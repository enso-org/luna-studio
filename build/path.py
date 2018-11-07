import os

root = os.path.dirname(os.path.abspath(os.path.dirname(__file__)))

def path(s):
    return os.path.normpath(os.path.join(root, s))
