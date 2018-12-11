#!/usr/bin/env python3

import argparse
import os
import subprocess
import sys

sys.path.append(os.getcwd())

import atom_prepare
import atom_apm
import copy_configs
import stack_build

app_dir      = atom_prepare.prep_path('..')
backend_dir  = atom_prepare.prep_path('../build-config/backend')
frontend_dir = atom_prepare.prep_path('../luna-studio')


def build_app (backend_args, frontend_args, runner_args,
               gui_url, dev_mode=False, dry_run=False):
    try:
        build_runner(runner_args, dry_run)
        build_backend (backend_args, dry_run)
        build_frontend (frontend_args, gui_url, dev_mode, dry_run)

    except subprocess.CalledProcessError:
        print("Status : FAIL")
        sys.exit(1)

def build_backend (backend_args, dry_run=False):
    try:
        print ("Building backend")
        stack_build.create_bin_dirs()
        if not dry_run:
            stack_build.build_backend(backend_args)
        stack_build.copy_std_lib()

    except subprocess.CalledProcessError:
        print("Status : FAIL")
        sys.exit(1)

def build_frontend (frontend_args, gui_url, dev_mode=False, dry_run=False):
    try:
        print("Building frontend")
        stack_build.create_bin_dirs()
        if not dry_run:
            stack_build.build_ghcjs(frontend_args, dev_mode)
        atom_prepare.run(dev_mode)
        atom_apm.run(gui_url, frontend_args, dev_mode)
        copy_configs.run()

    except subprocess.CalledProcessError:
        print("Status : FAIL")
        sys.exit(1)

def build_runner(runner_args, dry_run=False):
    try:
        stack_build.create_bin_dirs()
        if not dry_run:
            stack_build.build_runner(runner_args)
        stack_build.link_main_bin()
    except subprocess.CalledProcessError:
        print("Status : FAIL")
        sys.exit(1)

def main ():
    parser = argparse.ArgumentParser()
    parser.add_argument("--backend", help="Build backend only", action="store_true")
    parser.add_argument("--runner", help="Build runner only", action="store_true")
    parser.add_argument("--frontend", help="Build frontend only", action="store_true")
    parser.add_argument("--release", help="Build package in release mode", action="store_false")
    parser.add_argument("--gui_url", help="Path to uploaded gui")
    parser.add_argument("--backend-stack", help="Additional options passed to stack while building backend", action="append", dest="stack_backend_args", default=['--copy-bins', '--install-ghc'])
    parser.add_argument("--frontend-stack", help="Additional options passed to stack while building frontend", action="append", dest="stack_frontend_args", default=['--install-ghc'])
    parser.add_argument("--runner-stack", help="Additional options passed to stack while building runner", action="append", dest="stack_runner_args", default=['--copy-bins', '--install-ghc'])
    parser.add_argument("--dry-run", help="Do not build, only copy files", action="store_true")
    args = parser.parse_args()

    if args.backend:
        build_backend(args.stack_backend_args, dry_run=args.dry_run)
    elif args.runner:
        build_runner(args.stack_runner_args, dry_run=args.dry_run)
    elif args.frontend:
        build_frontend(args.stack_frontend_args, args.gui_url,
                       dev_mode=args.release, dry_run=args.dry_run)
    else:
        build_app(args.stack_backend_args, args.stack_frontend_args,
                  args.stack_runner_args, args.gui_url,
                  dev_mode=args.release, dry_run=args.dry_run)

if __name__ == '__main__':
    main()
