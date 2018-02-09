#!/usr/bin/env python3

import atom_prepare as ap
import atom_apm as aa

import os
import system as system


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


def update_atom_package_json(unpacked_package, old_v, new_v):
    json = get_path(unpacked_package, 'package_json')
    aa.sed_inplace(json, r'\"name\":\"luna-studio{}\"'.format(old_v),'\"name\":\"luna-studio{}\"'.format(new_v))
    aa.sed_inplace(json, r'\"productName\":\"LunaStudio{}\"'.format(old_v),'\"productName\":\"LunaStudio{}\"'.format(new_v))


def run(unpacked_package, old_v, new_v):
    print(unpacked_package)
    update_atom_package_json(unpacked_package,old_v, new_v)

if __name__ == '__main__':
    from sys import argv
    run(argv[1],argv[2],argv[3])
