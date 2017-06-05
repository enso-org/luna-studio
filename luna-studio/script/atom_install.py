#!/usr/bin/env python3

import atom_prepare
import os
import subprocess

app_dir = atom_prepare.prep_path('..')
os.chdir(app_dir)
try:
    subprocess.check_output(['stack', 'build'])
    atom_prepare.main()
    #os.environ['ATOM_HOME'] = os.environ.get('LUNA_HOME', os.path.expanduser('~') + '/.luna-atom')
    atom_dir = atom_prepare.prep_path('../atom')
    os.chdir(atom_dir)
    subprocess.call(['apm', 'install', '.'])
    subprocess.call(['apm', 'link', '.'])
    #os.environ['ATOM_HOME'] = os.path.expanduser('~') + '/.atom'
except subprocess.CalledProcessError:
    print("Status : FAIL")
