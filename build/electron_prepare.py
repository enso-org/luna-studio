#!/usr/bin/env python3

import os
import distutils.dir_util
import fileinput
import glob
import subprocess
import shutil
import system as system
from common import working_directory


def expand_path(path):
    script_abs_path = os.path.abspath(os.path.dirname(__file__))
    return os.path.normpath(os.path.join(script_abs_path, path))

def create_dirs():
    os.makedirs(expand_path('../app/dist/web/lib'), exist_ok=True)

def copy_ghcjs_code():
    node_editor = expand_path('../luna-studio/.stack-work/') + '/**/bin/node-editor.jsexe/all.js'
    text_editor = expand_path('../luna-studio/.stack-work/') + '/**/bin/text-editor.jsexe/all.js'
    node_editor_js = glob.glob(node_editor,recursive=True)
    text_editor_js = glob.glob(text_editor,recursive=True)
    create_dirs()
    shutil.copy(node_editor_js[0], expand_path("../app/dist/web/lib/node-editor.js"))
    shutil.copy(text_editor_js[0], expand_path("../app/dist/web/lib/text-editor.js"))

def run_npm():
    with working_directory(expand_path("../app")):
        subprocess.check_output(['npm', 'install'])
        subprocess.check_output(['npm', 'run', 'build'])

def run():
    copy_ghcjs_code()
    run_npm()

if __name__ == '__main__':
    run()