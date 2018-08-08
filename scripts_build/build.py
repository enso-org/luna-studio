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
from common import print_title, process_context


app_dir      = atom_prepare.prep_path('..')
backend_dir  = atom_prepare.prep_path('../build-config/backend')
frontend_dir = atom_prepare.prep_path('../luna-studio')


def build_app (backend_args, frontend_args, runner_args, gui_url, dev_mode=False):
    with process_context('build_app'):
        build_runner(runner_args)
        build_backend (backend_args)
        build_frontend (frontend_args, gui_url, dev_mode)


def build_backend (backend_args):
    with process_context('build_backend'):
        print_title('Building backend')
        stack_build.create_bin_dirs()
        stack_build.build_backend(backend_args)
        stack_build.copy_std_lib()


def build_frontend (frontend_args, gui_url, dev_mode):
    with process_context('build_frontend'):
        print_title('Building frontend')
        stack_build.create_bin_dirs()
        stack_build.build_ghcjs(frontend_args, dev_mode)
        atom_prepare.run(dev_mode)
        atom_apm.run(gui_url, frontend_args, dev_mode)
        copy_configs.run()


def build_js_only (frontend_args, gui_url, dev_mode):
    with process_context('build_js_only'):
        print_title('Building JS')
        atom_prepare.run(dev_mode)
        atom_apm.run(gui_url, frontend_args, dev_mode)
        copy_configs.run()


def build_runner(runner_args):
    with process_context('build_runner'):
        print_title("Build runner")
        stack_build.create_bin_dirs()
        stack_build.build_runner(runner_args)
        stack_build.link_main_bin()


def main ():
    parser = argparse.ArgumentParser()
    parser.add_argument("--backend", help="Build backend only", action="store_true")
    parser.add_argument("--runner", help="Build runner only", action="store_true")
    parser.add_argument("--frontend", help="Build frontend only", action="store_true")
    parser.add_argument("--js_only", action="store_true")
    parser.add_argument("--release", help="Build package in release mode", action="store_false")
    parser.add_argument("--gui_url", help="Path to uploaded gui")
    parser.add_argument("--backend-stack", help="Additional options passed to stack while building backend", action="append", dest="stack_backend_args", default=['--copy-bins', '--install-ghc'])
    parser.add_argument("--frontend-stack", help="Additional options passed to stack while building frontend", action="append", dest="stack_frontend_args", default=['--install-ghc'])
    parser.add_argument("--runner-stack", help="Additional options passed to stack while building runner", action="append", dest="stack_runner_args", default=['--copy-bins', '--install-ghc'])
    args = parser.parse_args()

    if args.backend:
        build_backend (args.stack_backend_args)
    elif args.runner:
        build_runner (args.stack_runner_args)
    elif args.frontend:
        build_frontend (args.stack_frontend_args, args.gui_url, dev_mode=args.release)
    elif args.js_only:
        build_js_only (args.stack_frontend_args, args.gui_url, dev_mode=args.release)
    else: build_app (args.stack_backend_args, args.stack_frontend_args, args.stack_runner_args, args.gui_url, dev_mode=args.release)

    print("Done!")


if __name__ == '__main__':
    main()
