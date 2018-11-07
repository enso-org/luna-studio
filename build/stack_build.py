#!/usr/bin/env python3

import atom_prepare as ap
from glob import glob
import os
import subprocess
import system as system
from common import working_directory
from path import path

# app_dir      = ap.prep_path('..')
backend_dir  = path('build-config/backend')
frontend_dir = path('luna-studio')
runner_dir   = path('runner')


def mkdir (s):
    os.makedirs(path(s), exist_ok=True)

def create_bin_dirs():
    for s in ('dist/bin/private', 'dist/bin/public/luna-studio'):
        mkdir(s)

def build_ghcjs(args, dev_mode):
    with working_directory(frontend_dir):
        if dev_mode:
            subprocess.check_output(['stack', 'build'] + args)

def build_runner(args):
    with working_directory(runner_dir):
        print ("build runner")
        runnerPath = runner_dir + '/src/StudioRunner.hs'
        hostPath = runner_dir + '/src/System/Host.hs'
        resPath = runner_dir + '/../resources/my.res'
        if system.windows():
            # TODO: What is this? This should be described. Moreover, keeping
            #       binaries in repo seems like a terrible idea.
            subprocess.check_output(['stack', 'build'])
            os.system('stack exec ghc -- ' + runnerPath + ' ' + hostPath + ' ' + resPath + ' -optl -mwindows')

        subprocess.check_output(['stack', 'build'] + args)
    mv_runner(runner_dir)

def build_backend(args):
    with working_directory(backend_dir):
        subprocess.check_output(['stack', 'build', 'luna-empire', '--test', '--no-run-tests'])
        subprocess.check_output(['stack', 'build'] + args)

def mv_runner(runner):
    if system.windows():
        runner_src = runner + '/src/' + '/StudioRunner.exe'
        runner_dst = ap.prep_path('../dist/bin/public/luna-studio/luna-studio.exe')
        os.replace(runner_src, runner_dst)


def link_main_bin ():
    with working_directory(ap.prep_path('../dist/bin')):
        os.makedirs('main', exist_ok=True)
        for src_path in glob('public/luna-studio/*'):
            dst_path = os.path.join('main', os.path.basename(src_path))
            if os.path.isfile(dst_path):
                os.remove(dst_path)
            if os.path.isfile(src_path):
                    os.symlink(os.path.relpath(src_path,'main/'), dst_path)


    # os.symlink('./public/luna-studio', 'main', target_is_directory=True)
# [WD]: This code does nothing! 
# def copy_std_lib ():
#     std_lib_path   = path ('build-config/backend/.stack-work') + '/**/stdlib'
#     std_lib_folder = glob (std_lib_path,recursive=True)
#     print (std_lib_folder)


def run(backend_args, frontend_args, runner_args):
    create_bin_dirs()
    build_runner(runner_args)
    build_ghcjs(frontend_args)
    build_backend(backend_args)
    link_main_bin ()

# if __name__ == '__main__':
#     run()
