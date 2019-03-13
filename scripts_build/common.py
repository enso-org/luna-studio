import contextlib
import os
import shutil


@contextlib.contextmanager
def working_directory(path):
    """A context manager which changes the working directory to the given
    path, and then changes it back to its previous value on exit.

    """
    prev_cwd = os.getcwd()
    os.chdir(path)
    try:
        yield
    finally:
        os.chdir(prev_cwd)

# FIXME Current package structure requires using symlinks. However, they do not
#  work currently on Windows. Even if we created them here, 7z archive format
#  would replace them with copied files anyway.
#  So as a provisional workaround, we now allow copying files when symlinking
#  fails - that allows packaging without root privileges on Windows.
#  To be sorted out as part of https://github.com/luna/luna-manager/issues/236
def create_symlink_or_copy(link, target):
    try:
        link_dir = os.path.dirname(link)
        link_relative_path = os.path.relpath(target, link_dir)
        os.symlink(link_relative_path, link)
    except OSError as e:
        print("failed to create symlink {} => {}: {}".format(link, target, e))
        print("falling back to copy")
        shutil.copy(target, link)