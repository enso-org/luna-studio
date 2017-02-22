import sys

from colors import print_error
from errors import handle_out
from subprocess import call
from utils.colors      import print_error, formatQuestion


def enum(*sequential, **named):
    enums = dict(zip(sequential, range(len(sequential))), **named)
    return type('Enum', (), enums)


response = enum ('YES', 'NO', 'CONTINUE')


def autocall(cmd_argv):
    handle_out(call(cmd_argv))

def ask(question, silent=False):
    if silent:
        print(formatQuestion("Question override: " + question))
        return True
    else:
        answer = None
        while True:
            ansRaw = raw_input(formatQuestion(question) + " [Y/n] ")
            ans = ansRaw.lower()
            if ans not in ["", "y", "yes", "n", "no"]:
                print_error("'%s' is not a valid answer, please choose beetween YES or NO." % ansRaw)
            else: break
        if ans in ["", "y", "yes"]: return True
        else:                       return False




def askCont(question, silent=False):
    if silent:
        print(formatQuestion("Question override: " + question))
        return response.YES
    else:
        answer = None
        while True:
            ansRaw = raw_input(formatQuestion(question) + " [Y/n/c] ")
            ans = ansRaw.lower()
            if ans not in ["", "y", "yes", "n", "no", "c", "continue"]:
                print_error("'%s' is not a valid answer, please choose beetween YES or NO." % ansRaw)
            else: break
        if   ans in ["", "y", "yes"]:  return response.YES
        elif ans in ["c", "continue"]: return response.CONTINUE
        else:                          return response.NO
