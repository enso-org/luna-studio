# Contributing to Luna
Thank you for your interest in contributing to Luna! We believe that only 
through community involvement can Luna be the best it can be! There are a whole
host of ways to contribute, and every single one is appreciated. The major 
sections of this document are linked below: 

- [Feature Enhancements](#feature-enhancements)
- [Bug Reports](#bug-reports)
- [Hacking on Luna](#hacking-on-luna-studio)
- [Pull Requests](#pull-requests)
- [Documentation](#documentation)
- [Issue Triage](#issue-triage)
- [Out-of-Tree Contributions](#out-of-tree-contributions)

All contributions to Luna should be in keeping with our 
[Code of Conduct](https://github.com/luna/luna/blob/CODE_OF_CONDUCT.md).

## Feature Enhancements
If you feel like you have a suggestion for a change to the way that Luna Studio
works as a tool and development environment, please open an issue in our 
[RFCs Repository](https://github.com/luna/luna-rfcs), rather than in this one!
New features and other changes to the IDE must go through the RFC process so 
they can be properly discussed.

## Bug Reports
While it's never great to find a bug, they are a reality of software and 
software development! We can't fix or improve on the things that we don't know
about, so report as many bugs as you can! If you're not sure whether something 
is a bug, file it anyway! 

**If you are concerned that your bug publicly presents a security risk to the
users of Luna Studio, please contact 
[security@luna-lang.org](mailto:security@luna-lang.org).**

Even though GitHub search can be a bit hard to use sometimes, we'd appreciate if
you could 
[search](https://github.com/luna-studio/luna/search?q=&type=Issues&utf8=%E2%9C%93) 
for your issue before filing a bug as it's possible that someone else has 
already reported the issue. We know the search isn't the best, and it can be 
hard to know what to search for, so we really don't mind if you do submit a 
duplicate!

Opening an issue is as easy as following 
[this link](https://github.com/luna-studio/luna/issues/new) and filling out the 
fields. Below is a template you can use to file the bug, but it doesn't matter 
if you don't follow it exactly! Just get the important info in there!

```md
## Summary

## Reproduction
A set of steps and a code sample that produces the issue.

**Observed Result:** What you see happen.
**Expected Result:** What you _think_ should happen.

## Metadata
Include your operating system, Luna Studio version and any other relevant data.

```

All three bits of this are important, especially what you did to cause the 
issue. The more detail you provide, the more easily we can reproduce it and fix
the bug! It's also very helpful to have some information about your system, in
case the bug is Operating System or Architecture specific.

## Hacking on Luna Studio
Luna's build system is nice and simple, allowing you to bootstrap the compiler
as long as you have an installation of 

[The Haskell Stack](https://docs.haskellstack.org/en/stable/README/) and the
Haskell parser generator `happy`. 

You can install the latter just by running `stack install happy`, which should
build the tool for your system and put it in your `stack` binary folder. 

### System Requirements
While Luna Studio supports Windows as a target operating system, it is not 
currently possible to develop the Luna Studio frontend on Windows. This is due
to the current lack of support by [GHCJS](https://github.com/ghcjs/ghcjs) for
Windows at the current time. 

Otherwise, you should be able to develop Luna Studio on reasonably new Linuxes
and MacOS. Luna Studio was mostly tested on Ubuntu >= 14.04, Fedora >= 23, MacOS
\>= 10.11 (El Capitan) and Windows 10, although it should run fine on all Linux 
distros like Mint, Debian or Arch. Please report any issues here on GitHub or 
shoot an email to [contact@luna-lang.org](mailto:contact@luna-lang.org).

### Bootstrapping for Development
To develop Luna Studio, the installation process is quite involved. First of 
all, you will need the [Luna Manager](https://github.com/luna/luna-manager), 
which can be obtained from the linked repository.

The prerequisites for building Luna Studio are the following (with Ubuntu 
`apt-get` package names in parentheses for convenience):

* wget
* gcc, g++, make (build-essential)
* libffi (libffi-dev)
* libgmp (libgmp-dev)
* xz-utils
* zlib (zlib1g-dev)
* git
* gnupg
* ZeroMQ >= 4 (which, interestingly, is libzmq3-dev on Ubuntu)
* libpng (libpng-dev)
* openssl
* SetupTools for Python (python-setuptools)
* libssl (libssl-dev)
* libtinfo (libtinfo-dev)
* pkg-config
* alex
* PIP for Python (python-pip)

Once you are sure that you have the prerequisites installed, you can proceed to
the environment setup. Assuming that you already have the `luna-manager` binary
on your system and available on your `$PATH`, the command is the following:

```
luna-manager develop luna-studio
```

This will install Luna Studio, along with some dependencies (e.g. the Haskell
ecosystem) to `$HOME/luna-develop/apps/luna-studio`. If you wish to install to
a different location, please supply an additional `--path` argument to the
`develop` command. From now on, we will refer to this location as 
`$LUNA_STUDIO_REPO`.

Once the `develop` command finishes executing, you are ready to start developing
Luna Studio. To rebuild the whole application use:

```sh
./build
```

You can choose to rebuild only the frontend or backend part by passing an 
additional flag (`--frontend` or `--backend` respectively).

To run the application in developer mode, use:

```sh
$LUNA_STUDIO_REPO/dist/bin/main/luna-studio --develop
```

### Troubleshooting
Below are a couple of common errors and their resolution steps:

- If during the development process you encounter errors related to missing 
  files (caused by accidental removal of a dependency, for example) you can
  try to execute the following:

```sh
luna-manager develop luna-studio --path $LUNA_STUDIO_REPO --download-dependencies
```

- [Linux] The application image requires FUSE to run. While most Linux 
  distributions have it pre-installed, it is sometimes missing. If your distro
  happens to lack FUSE, you can either install it, or use the following command
  to mount the app image manually:

```sh
sudo mount -o loop Some.AppImage /mnt
/mnt/AppRun
```

## Pull Requests
Pull Requests are the primary method for making changes to Luna Studio. GitHub 
has 
[fantastic documentation](https://help.github.com/articles/about-pull-requests/)
on using the pull request feature. Luna Studio uses the 'fork-and-pull' model of 
development. It is as described 
[here](https://help.github.com/articles/about-collaborative-development-models/)
and involves people pushing changes to their own fork and creating pull requests
to bring those changes into the main Luna Studio repository.

Please make all pull requests against the `master` branch.

Please make sure your code is in compliance with the 
[Luna Style Guidelines](https://github.com/luna/luna/wiki/Haskell-code-style) if
you've worked on related Haskell code. Additionally, if you have been working on
the backend, please ensure that your code passes all the tests. These can be 
run by executing `cd /build-config/backend; stack test;`.

Make sure you perform these checks before _every_ pull request. You can even add
[git hooks](https://git-scm.com/book/en/v2/Customizing-Git-Git-Hooks) before 
every push to make sure that you can't forget. 

Every pull request for Luna is reviewed by another person! You'll get a 
reviewer from the core team assigned at random, but feel free to ask for a 
specific person if you've dealt with them in a certain area before! 

Once the reviewer approves your pull request it will be tested by our continuous
integration provider before being merged!

## Documentation
Documentation improvements are very welcome! The source for the Luna Book can be
found in [`luna/luna-book`](https://github.com/luna/luna-book), but most of the
API documentation is generated directly from the code! 

Documentation pull requests are reviewed in exactly the same way as normal pull
requests. 

To find documentation-related issues, sort by the 
[C - Documentation](https://github.com/luna-studio/luna/issues?utf8=%E2%9C%93&q=is%3Aopen+label%3A%22C+-+Documentation%22+)
label.

## Issue Triage
Sometimes issues can be left open long after the bug has been fixed. Other 
times, a bug might go stale because something has changed in the meantime.

It can be helpful to go through older bug reports and make sure that they are 
still valid. Load up an older issue, double check that it's still true, and 
leave a comment letting us know if it is or is not. The 
[least recently updated](https://github.com/luna-studio/luna/issues?q=is%3Aissue+is%3Aopen+sort%3Aupdated-asc) 
sort is good for finding issues like this.

Contributors with sufficient permissions can help by adding labels to help with
issue triage.

If you're looking for somewhere to start, take a look at the 
[D - Beginner](https://github.com/luna-studio/luna/issues?utf8=%E2%9C%93&q=is%3Aopen+label%3A%22D+-+Beginner%22+)
issue label.

## Out-of-Tree Contributions
As helpful as contributing to Luna directly is, it can also be just as helpful
to contribute in other ways outside this repository:

- Answer questions in the [Discord](https://discordapp.com/invite/YFEZz3y) or
  on [StackOverflow](https://stackoverflow.com/questions/tagged/luna).
- Participate in the [RFC Process](https://github.com/luna/luna-rfcs).

## Helpful Documentation and Links
For people new to Luna, and just starting to contribute, or even for more 
seasoned developers, some useful places to look for information are:

- [The Luna Book](https://luna-lang.gitbooks.io/docs/)
- The community! Don't be afraid to ask questions.
