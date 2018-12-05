#!/usr/bin/env python3

import electron_prepare as ep
import os
from distutils import dir_util
from glob import glob
import shutil
import subprocess
import system as system
from common import working_directory


resources_dir = ep.expand_path('../resources')
supervisor_dir = ep.expand_path('../supervisor')
windows_dir = ep.expand_path('../windows')
env_dir = ep.expand_path('../env')


def copy_configs(supervisor,env, windows):
    config_path = ep.expand_path('../dist/config/')
    supervisor_path = config_path + '/supervisor'
    env_path = config_path + '/env'
    windows_path = config_path + '/windows'
    dir_util.copy_tree(env, env_path)

    if system.windows():
        dir_util.copy_tree(windows, windows_path)
    else:
        dir_util.copy_tree(supervisor, supervisor_path)

def copy_resources(resources):
    resources_path=ep.expand_path('../dist/bin/public/luna-studio/resources')
    dir_util.copy_tree(resources, resources_path)

def link_resources ():
    with working_directory(ep.expand_path('../dist/bin')):
        os.makedirs('main/resources', exist_ok=True)
        for src_path2 in glob('public/luna-studio/resources/*'):
            dst_path = os.path.join('main/resources', os.path.basename(src_path2))
            if os.path.isfile(dst_path):
                os.remove(dst_path)
            if os.path.isfile(src_path2):
                os.symlink(os.path.relpath(src_path2,'main/resources/'), dst_path)


def copy_atom_configs ():
    dst_path = ep.expand_path('../dist/user-config/atom')
    configs_files = [
        '../config/config.cson',
        '../config/keymap.cson',
        '../config/snippets.cson',
        '../config/styles.cson',
    ]
    for config in configs_files:
        shutil.copy(ep.expand_path(config), dst_path)


def rebrand_atom_logo():
    if system.darwin():
        src = ep.expand_path('../resources/logo.icns')
        dst = ep.expand_path('../dist/third-party/Atom.app/Contents/Resources/atom.icns')
        shutil.copy(src, dst)
        subprocess.run(['touch', ep.expand_path('../dist/third-party/Atom.app')])


def run():
    copy_configs(supervisor_dir,env_dir, windows_dir)
    copy_resources(resources_dir)
    link_resources()
    rebrand_atom_logo()

if __name__ == '__main__':
    run()
