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
    prepare_holder(output, 'module.exports = (function(){', '});', './nodelab/vendor/uuid.js', './nodelab/script/imports.js', placeholder)
    put_ghcjs(output, ghcjs, 'GHCJS_CODE_BE_THERE')

# delete old dirs for atom

shutil.rmtree('./nodelab/atom/lib/gen', ignore_errors=True)
shutil.rmtree('./nodelab/atom/styles', ignore_errors=True)
shutil.rmtree('./nodelab/atom/node_modules', ignore_errors=True)

# create directories

os.makedirs('./nodelab/atom/lib/gen')
os.makedirs('./nodelab/atom/styles')

# find and change ghcjs code for internals

nodelab_js = glob.glob('./nodelab/.stack-work/**/bin/nodelab.jsexe/all.js',recursive=True)
internals_js = glob.glob('./internals/.stack-work/**/bin/internals.jsexe/all.js',recursive=True)

prepare_ghcjs('./nodelab/atom/lib/gen/ghcjs-code.js', './nodelab/app/env.ghcjs', nodelab_js[0])
prepare_ghcjs('./nodelab/atom/lib/gen/ghcjs-code2.js', './internals/app/env2.ghcjs', internals_js[0])

# copy files

distutils.dir_util.copy_tree('./nodelab/app/js', './nodelab/atom/lib/gen')
shutil.copy('./internals/app/js/atom-callback2.js', './nodelab/atom/lib/gen')
shutil.copy('./internals/app/js/app2.coffee', './nodelab/atom/lib/gen')
shutil.copy('./nodelab/app/config.release.js', './nodelab/atom/lib/gen')
shutil.copy('./nodelab/app/config.debug.js', './nodelab/atom/lib/gen')
shutil.copy('./nodelab/www/stylesheets/app.css', './nodelab/atom/styles')
