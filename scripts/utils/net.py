import urllib2
import os
import sys
import ssl

from colors import print_info
from path   import make_dirs_if_needed



def sizeof_fmt(num):
    for x in ['bytes','KB','MB','GB']:
        if num < 1024.0 and num > -1024.0:
            return "%3.1f %s" % (num, x)
        num /= 1024.0
    return "%3.1f %s" % (num, 'TB')

def download(url, path):
    if os.path.exists(path):
        print_info("File %s already exists. Skipping download." % path)
        return

    try:
        scontext = ssl.SSLContext(ssl.PROTOCOL_TLSv1)
        u = urllib2.urlopen(url, context=scontext)
    except AttributeError:
        u = urllib2.urlopen(url)

    make_dirs_if_needed(os.path.dirname(path))
    f = open(path, 'wb')
    meta = u.info()
    file_size = int(meta.getheaders("Content-Length")[0])

    print_info ("Path: %s" % path)

    file_size_dl = 0
    block_sz = 8192
    while True:
        buffer = u.read(block_sz)
        if not buffer:
            break
        file_size_dl += len(buffer)
        f.write(buffer)
        percent = file_size_dl * 100. / file_size
        msg = "Downloading: [%s]" % sizeof_fmt(file_size) + " ... " + str(int(percent))
        sys.stdout.write("\r%s%%" % msg)
        sys.stdout.flush()
    print
    f.close()
