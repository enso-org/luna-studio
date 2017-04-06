#!/usr/bin/python3

import multiprocessing
import subprocess
import os

def run_luna():
    os.environ['ATOM_HOME'] = os.environ.get('LUNA_HOME', os.path.expanduser('~') + '/.luna-atom')
    subprocess.call(['atom'])
    os.environ['ATOM_HOME'] = os.path.expanduser('~') + '/.atom'

if __name__ == '__main__':
    multiprocessing.set_start_method('spawn')
    q = multiprocessing.Queue()
    p = multiprocessing.Process(target=run_luna, name='luna-atom')
    p.start()
    print(q.get())
    p.join()
