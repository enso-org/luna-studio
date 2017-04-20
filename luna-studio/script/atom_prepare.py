#!/usr/bin/env python3

import os
import distutils.dir_util
import fileinput
import glob
import subprocess
import shutil

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
    for path in ('../atom/lib/gen', '../atom/styles', '../atom/node_modules'):
        shutil.rmtree(prep_path(path), ignore_errors=True)

def create_dirs():
    for path in ('../atom/lib/gen', '../atom/styles'):
        os.makedirs(prep_path(path))

def ghcjs_code():
    nodelab = prep_path('../.stack-work/') + '/**/bin/node-editor.jsexe/all.js'
    internals = prep_path('../.stack-work/') + '/**/bin/text-editor.jsexe/all.js'
    nodelab_js = glob.glob(nodelab,recursive=True)
    internals_js = glob.glob(internals,recursive=True)
    prepare_ghcjs('../atom/lib/gen/node-editor-ghcjs.js', '../node-editor/env.ghcjs', nodelab_js[0])
    prepare_ghcjs('../atom/lib/gen/text-editor-ghcjs.js', '../text-editor/env-internals.ghcjs', internals_js[0])

def cp_files():
    distutils.dir_util.copy_tree(prep_path('../node-editor/js'), prep_path('../atom/lib/gen'))
    for path in ('../text-editor/js/atom-callback-internals.js', '../text-editor/js/app-internals.coffee', '../node-editor/config.release.js', '../node-editor/config.debug.js'):
        shutil.copy(prep_path(path), prep_path('../atom/lib/gen'))

def main():
    rm_old()
    create_dirs()
    ghcjs_code()
    prepare_css('../atom/styles/app.css', '../node-editor/styles/style.less')
    cp_files()

if __name__ == '__main__':
  main()
