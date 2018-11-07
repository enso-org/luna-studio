import argparse
import os
import subprocess
import sys
import atom_prepare
import atom_apm
import copy_configs
import stack_build as stack

def build_app (args):
    build_runner   (args)
    build_backend  (args)
    build_frontend (args)

def build_runner(args):
    stack.create_bin_dirs()
    stack.build_runner(args.runner_stack)
    stack.link_main_bin()

def build_backend (args):
    print ("Building backend")
    stack.create_bin_dirs()
    stack.build_backend(args.backend_stack)
    # stack.copy_std_lib()

def build_frontend (args):
    print("Building frontend")
    stack.create_bin_dirs()
    stack.build_ghcjs(args.frontend_stack, args.dev_mode)
    if not args.atomless:
        atom_prepare.run(args.dev_mode)
        atom_apm.run(args.gui_url, args.frontend_stack, args.dev_mode)
    copy_configs.run()

def main ():
    parser = argparse.ArgumentParser()
    def arg(*args, **kwargs):
        parser.add_argument (*args, **kwargs)

    arg ("--backend"        , action="store_true"         , help="Build backend only"                                        )
    arg ("--runner"         , action="store_true"         , help="Build runner only"                                         )
    arg ("--frontend"       , action="store_true"         , help="Build frontend only"                                       )
    arg ("--release"        , action="store_false"        , help="Build package in release mode"                             )
    arg ("--gui-url"                                      , help="Path to uploaded gui"                                      )
    arg ("--atomless"       , action="store_true"         , help="Build the atomless version"                                )
    arg ("--backend-stack"  , action="append" , default=[], help="Additional options passed to stack while building backend" )
    arg ("--frontend-stack" , action="append" , default=[], help="Additional options passed to stack while building frontend")
    arg ("--runner-stack"   , action="append" , default=[], help="Additional options passed to stack while building runner"  )
    args = parser.parse_args()

    args.runner_stack   += ['--install-ghc', '--copy-bins']
    args.backend_stack  += ['--install-ghc', '--copy-bins']
    args.frontend_stack += ['--install-ghc']
    args.dev_mode        = args.release

    if   args.backend:  build_backend  (args)
    elif args.runner:   build_runner   (args)
    elif args.frontend: build_frontend (args)
    else:               build_app      (args)

main()
