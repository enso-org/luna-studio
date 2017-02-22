import sys
from utils.colors import print_error


class ExecutionError(Exception):
  pass


def fatal():
    raise ExecutionError("Error!")


def handle_out(code):
    if code:
        raise ExecutionError("Execution error (handle_out)! Return code {code} != 0".format(**locals()))

def handle_err(code):
    if code:
        raise ExecutionError("Execution error (handle_err)! Return code {code} != 0".format(**locals()))
