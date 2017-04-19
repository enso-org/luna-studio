#!/usr/bin/python3

import os
import distutils.dir_util
import fileinput
import glob
import subprocess
import shutil
import lesscpy

def prepare_holder(output, content_start, content_end, input1, input2, placeholder):
    with open(output, 'a+') as modified:
        modified.write(content_start)
        for infile in (input1, input2, placeholder):
            with open(infile, 'r') as f:
                shutil.copyfileobj(f, modified)
        modified.write(content_end)

def put_ghcjs(output, content, str_to_change):
    with open(content, 'r') as code:
        code = code.read()
        for line in fileinput.input(output, inplace=True):
            print(line.replace(str_to_change, code))

def prepare_ghcjs(output, placeholder, ghcjs):
    uuid = prep_path('../vendor/uuid.js')
    imports = prep_path('../script/imports.js')
    output_abs = prep_path(output)
    placeholder_abs = prep_path(placeholder)
    ghcjs_abs = prep_path(ghcjs)
    prepare_holder(output_abs, 'module.exports = (function(){', '});', uuid, imports, placeholder_abs)
    put_ghcjs(output_abs, ghcjs_abs, 'GHCJS_CODE_BE_THERE')

def prepare_css(output, styles_file):
    output_abs = prep_path(output)
    styles_abs = prep_path(styles_file)
    with open(output_abs, 'a+') as outfile:
        subprocess.Popen(['lessc', styles_abs], stdout=outfile)

def prep_path(path):
    script_abs_path = os.path.abspath(os.path.dirname(__file__))
    return os.path.normpath(os.path.join(script_abs_path, path))

def rm_old():
    shutil.rmtree(prep_path('../atom/lib/gen'), ignore_errors=True)
    shutil.rmtree(prep_path('../atom/styles'), ignore_errors=True)
    shutil.rmtree(prep_path('../atom/node_modules'), ignore_errors=True)

def create_dirs():
    os.makedirs(prep_path('../atom/lib/gen'))
    os.makedirs(prep_path('../atom/styles'))

def ghcjs_code():
    nodelab = prep_path('../.stack-work/') + '/**/bin/node-editor.jsexe/all.js'
    internals = prep_path('../.stack-work/') + '/**/bin/luna-atom.jsexe/all.js'
    nodelab_js = glob.glob(nodelab,recursive=True)
    internals_js = glob.glob(internals,recursive=True)
    prepare_ghcjs('../atom/lib/gen/node-editor-ghcjs.js', '../node-editor/env.ghcjs', nodelab_js[0])
    prepare_ghcjs('../atom/lib/gen/luna-atom-ghcjs.js', '../luna-atom/env-internals.ghcjs', internals_js[0])

def cp_files():
    distutils.dir_util.copy_tree(prep_path('../node-editor/js'), prep_path('../atom/lib/gen'))
    shutil.copy(prep_path('../luna-atom/js/atom-callback-internals.js'), prep_path('../atom/lib/gen'))
    shutil.copy(prep_path('../luna-atom/js/app-internals.coffee'), prep_path('../atom/lib/gen'))
    shutil.copy(prep_path('../node-editor/config.release.js'), prep_path('../atom/lib/gen'))
    shutil.copy(prep_path('../node-editor/config.debug.js'), prep_path('../atom/lib/gen'))

def main():
    # delete old dirs for atom
    rm_old()
    # create directories
    create_dirs()
    # find and change ghcjs code for internals
    ghcjs_code()
    prepare_css('../atom/styles/app.css', '../node-editor/styles/style.less')
    # copy files
    cp_files()

if __name__ == '__main__':
  main()
