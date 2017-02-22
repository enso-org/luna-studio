import sys
from utils.colorama import Fore, init
init()


def disable(self):
        self.HEADER = ''
        self.BLUE = ''
        self.INFO = ''
        self.WARNING = ''
        self.ERROR = ''
        self.RESET = ''


class bcolors:
    INFO     = Fore.GREEN
    WARNING  = Fore.YELLOW
    ERROR    = Fore.RED
    RESET    = Fore.RESET
    QUESTION = Fore.BLUE

    def disable(self):
        self.INFO = ''
        self.WARNING = ''
        self.ERROR = ''
        self.RESET = ''

def format_info(s):
    return bcolors.INFO + s + bcolors.RESET

def format_warning(s):
    return bcolors.WARNING + s + bcolors.RESET

def formatQuestion(s):
    return bcolors.QUESTION + s + bcolors.RESET

def putInfo(s):
    sys.stdout.write(format_info(s))

def print_info(s):
    print (format_info(s))

def print_warning(s):
    print (format_warning(s))

def print_error(s):
    print (bcolors.ERROR + s + bcolors.RESET)
