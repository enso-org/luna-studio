#!/usr/bin/env python3

import atom_prepare as ap
import atom_apm as aa

import os
import system as system
from argparse import ArgumentParser

paths = {
    system.systems.WINDOWS: {
        'package_json': '/third-party/Atom/resources/app/package.json',
    },
    system.systems.LINUX: {
        'package_json': '/third-party/atom/usr/share/atom/resources/app/package.json',
    },
    system.systems.DARWIN: {
        'package_json': '/third-party/Atom.app/Contents/Resources/app/package.json',
    },
}


def get_path(unpacked_package, name):
    try:
        return unpacked_package + paths[system.system][name]
    except KeyError as e:
        print("Unknown system: {}".format(e.args[0]))

# change version of luna-studio in Atom package.json
def update_atom_package_json(unpacked_package, old_v, new_v):
    json = get_path(unpacked_package, 'package_json')
    aa.sed_inplace(json, r'\"name\":\"luna-studio{}\"'.format(old_v),'\"name\":\"luna-studio{}\"'.format(new_v))
    aa.sed_inplace(json, r'\"productName\":\"LunaStudio{}\"'.format(old_v),'\"productName\":\"LunaStudio{}\"'.format(new_v))


def run(unpacked_package, old_v, new_v):
    update_atom_package_json(unpacked_package,old_v, new_v)


if __name__ == '__main__':
    parser = ArgumentParser(description='Bump the version of a Luna package.')
    parser.add_argument('package_path', metavar='PACKAGE_PATH', help='Path to the (unpacked) Luna Studio package.')
    parser.add_argument('old_version', metavar='OLD_VERSION', help='The old version (source).')
    parser.add_argument('new_version', metavar='NEW_VERSION', help='The new version (target).')
    args = parser.parse_args()
    run(args.package_path, args.old_version, args.new_version)
