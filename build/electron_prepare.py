#!/usr/bin/env python3

import os
import distutils.dir_util
import fileinput
import glob
import subprocess
import shutil
import system as system


def prep_path(path):
    script_abs_path = os.path.abspath(os.path.dirname(__file__))
    return os.path.normpath(os.path.join(script_abs_path, path))



def create_dirs_atomless():
    for path in ['../app/dist/web/lib']:
        os.makedirs(prep_path(path), exist_ok=True)


def ghcjs_code_atomless():
    node_editor = prep_path('../luna-studio/.stack-work/') + '/**/bin/node-editor.jsexe/all.js'
    text_editor = prep_path('../luna-studio/.stack-work/') + '/**/bin/text-editor.jsexe/all.js'
    node_editor_js = glob.glob(node_editor,recursive=True)
    text_editor_js = glob.glob(text_editor,recursive=True)
    create_dirs_atomless()
    shutil.copy(node_editor_js[0], prep_path("../app/dist/web/lib/node-editor.js"))
    shutil.copy(text_editor_js[0], prep_path("../app/dist/web/lib/text-editor.js"))

def run_npm():
    with working_directory ("../app"):
        subprocess.check_output(['npm', 'install'])
        subprocess.check_output(['npm', 'run', 'build'])

def run():
    ghcjs_code_atomless()
    run_npm()

if __name__ == '__main__':
    run()