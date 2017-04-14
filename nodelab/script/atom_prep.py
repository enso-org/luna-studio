#!/usr/bin/python3

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
    prepare_holder(output, 'module.exports = (function(){', '});', '../vendor/uuid.js', '../script/imports.js', placeholder)
    put_ghcjs(output, ghcjs, 'GHCJS_CODE_BE_THERE')

def prepare_css(output, styles_dir):
    styles_files = glob.glob(styles_dir)
    with open(output, 'a+') as outfile:
        for infile in styles_files:
            with open(infile, 'r') as f:
                shutil.copyfileobj(f, outfile)


# delete old dirs for atom

shutil.rmtree('../atom/lib/gen', ignore_errors=True)
shutil.rmtree('../atom/styles', ignore_errors=True)
shutil.rmtree('../atom/node_modules', ignore_errors=True)

# create directories

os.makedirs('../atom/lib/gen')
os.makedirs('../atom/styles')

# find and change ghcjs code for internals

nodelab_js = glob.glob('../.stack-work/**/bin/nodelab.jsexe/all.js',recursive=True)
internals_js = glob.glob('../.stack-work/**/bin/internals.jsexe/all.js',recursive=True)

prepare_ghcjs('../atom/lib/gen/ghcjs-code.js', '../app/env.ghcjs', nodelab_js[0])
prepare_ghcjs('../atom/lib/gen/ghcjs-code2.js', '../internals/env-internals.ghcjs', internals_js[0])
prepare_css('../atom/styles/app.css', '../app/styles/*')
# copy files

distutils.dir_util.copy_tree('../app/js', '../atom/lib/gen')
shutil.copy('../internals/js/atom-callback-internals.js', '../atom/lib/gen')
shutil.copy('../internals/js/app-internals.coffee', '../atom/lib/gen')
shutil.copy('../app/config.release.js', '../atom/lib/gen')
shutil.copy('../app/config.debug.js', '../atom/lib/gen')
