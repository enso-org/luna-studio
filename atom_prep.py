import os
import distutils.dir_util
import fileinput
import glob
import subprocess
import shutil
# import urllib

def string_append(filename, content):
    with open(filename, 'a+') as modified:
        modified.write(content)

def file_append(output, input1, input2):
    with open(output,'w') as outfile:
        for infile in (input1, input2):
            shutil.copyfileobj(open(infile), outfile)

def placeholder(output, pl_holder, content, str_to_change):
    code = open(content,'r').read()
    for line in fileinput.input(pl_holder):
        string_append(output, (line.replace(str_to_change, code)))

# create directories

os.makedirs('./Luna/supervisor/logs')
os.makedirs('./nodelab/atom/lib/gen')
os.makedirs('./nodelab/atom/styles')

# find and change ghcjs code for internals

nodelabJs = glob.glob('./nodelab/.stack-work/**/bin/nodelab.jsexe/all.js',recursive=True)
internalsJs = glob.glob('./internals/.stack-work/**/bin/internals.jsexe/all.js',recursive=True)

string_append('nodelab/atom/lib/gen/ghcjs-code.js', 'module.exports = (function(){')
file_append('nodelab/atom/lib/gen/ghcjs-code.js', './nodelab/vendor/uuid.js', './nodelab/script/imports.js')
placeholder('nodelab/atom/lib/gen/ghcjs-code.js', './nodelab/app/env.ghcjs', nodelabJs[0], 'GHCJS_CODE_BE_THERE')
string_append('nodelab/atom/lib/gen/ghcjs-code.js', '});')

string_append('nodelab/atom/lib/gen/ghcjs-code2.js', 'module.exports = (function(){')
file_append('nodelab/atom/lib/gen/ghcjs-code2.js', './nodelab/vendor/uuid.js', './nodelab/script/imports.js')
placeholder('nodelab/atom/lib/gen/ghcjs-code2.js', './internals/app/env2.ghcjs', internalsJs[0], 'GHCJS_CODE_BE_THERE')
string_append('nodelab/atom/lib/gen/ghcjs-code2.js', '});')

distutils.dir_util.copy_tree('./nodelab/app/js', 'nodelab/atom/gen')
shutil.copy('./internals/app/js/atom-callback2.js', 'nodelab/atom/gen')
shutil.copy('./nodelab/app/config.release.js', 'nodelab/atom/gen')
shutil.copy('./nodelab/app/config.debug.js', 'nodelab/atom/gen')
shutil.copy('./nodelab/www/stylesheets/app.css', 'nodelab/atom/styles')


distutils.dir_util.copy_tree('./dist/bin', 'Luna/bin')
shutil.copy('./supervisor/supervisord.conf', 'Luna/supervisor')


# download current atom package

# atomUrl = 'https://github.com/atom/atom/releases/download/v1.15.0/atom-amd64.deb'
#
# attempts = 0
# while attempts < 3:
#     try:
#         response = urllib.request.urlopen(atomUrl, timeout=5)
#         content = response.read()
#         f = open( "atom-amd64.deb", 'w' )
#         f.write( content )
#         f.close()
#         break
#     except urllib.URLError as e:
#         attempts += 1
#         print(type(e))
#
# # install atom for apm
#
# subprocess.call(['dpkg -i atom-amd64.deb'])
#
# subprocess.call() # mkdir .atom, apm install . apm link .
