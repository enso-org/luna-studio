###########################################################################
## Copyright (C) Flowbox, Inc / All Rights Reserved
## Unauthorized copying of this file, via any medium is strictly prohibited
## Proprietary and confidential
## Flowbox Team <contact@flowbox.io>, 2014
###########################################################################

import os
import tarfile

from .colors import print_info



def write_if_changed(path, s):
    if os.path.exists(path):
        with open(path, 'r') as file:
            if file.read() == s:
                return
    with open(path, 'w') as file:
        file.write(s)

def make_dirs_if_needed(path):
    if not os.path.exists(os.path.dirname(path)):
        os.makedirs(path)

def remove_if_present(path):
    if os.path.exists(path):
        os.remove(path)


def extract(rootdir, path):
    tar = tarfile.open(path)
    for tarinfo in tar:
        path = os.path.join(rootdir, tarinfo.name)
        if os.path.exists(path):
            print_info("File %s already exists. Skipping extraction." % path)
            break
        tar.extractall(rootdir, [tarinfo])
