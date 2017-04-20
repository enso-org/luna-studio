#!/usr/bin/python3

import atom_prepare
import os
import subprocess

nodelab_dir = atom_prepare.prep_path('..')
os.chdir(nodelab_dir)
subprocess.call(['stack', 'build', '--fast'])
atom_prepare.main()
os.environ['ATOM_HOME'] = os.environ.get('LUNA_HOME', os.path.expanduser('~') + '/.luna-atom')
atom_dir = atom_prepare.prep_path('../atom')
os.chdir(atom_dir)
subprocess.call(['apm', 'install', '.'])
subprocess.call(['apm', 'link', '.'])
os.environ['ATOM_HOME'] = os.path.expanduser('~') + '/.atom'
