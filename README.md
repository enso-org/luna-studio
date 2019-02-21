<p align="center">
<img src="https://github.com/luna/luna-studio/raw/master/resources/logo.ico" style="margin: 0 auto;">
</p>

<h1 align="center">Luna Studio</h1>
<h3 align="center">Visual and textual functional programming language with a focus on productivity, collaboration and development ergonomics.</h3>

Luna is a developer’s whiteboard on steroids. Design, prototype, develop and refactor any application simply by connecting visual elements together. Collaborate with co-workers, interactively fine tune parameters, inspect the results and visually profile the performance in real-time.

Visit [luna-lang.org](https://www.luna-lang.org) to learn more!

## Quick installation

The easiest way of installing Luna Studio is by using the graphical Luna Installer, which can be downloaded using the following links:
* Mac OS: http://packages.luna-lang.org/darwin/lunaInstaller.zip
* Linux: http://packages.luna-lang.org/linux/lunaInstaller.zip
* Windows: http://packages.luna-lang.org/windows/lunaInstaller.zip

All you need to do is download and extract the zip and run the application. It will then guide you through the installation process.

## Using Luna Studio

For a tutorial, documentation, explanation of the ideas behind Luna please visit http://docs.luna-lang.org. It is available in a form of a book, to provide a gentle and fairly complete introduction to the application as well as the new visual-textual paradigm.

## Hacking on Luna Studio
Luna's build system is nice and simple, allowing you to bootstrap the compiler
as long as you have an installation of 
[The Haskell Stack](https://docs.haskellstack.org/en/stable/README/). 

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
./build_luna
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

## Contributing to Luna Studio
If you are interested in contributing to the development of Luna, please read
the 
[`CONTRIBUTING.md`](https://github.com/luna/luna-studio/blob/master/CONTRIBUTING.md)
file. 

## License
This repository is licensed under the
[GNU Affero GPL 3.0](https://opensource.org/licenses/AGPL-3.0), as specified in the
[LICENSE](https://github.com/luna/luna-studio/blob/master/LICENSE) file. 

Please be aware that, as the commercial backing for Luna, 
**New Byte Order Sp. z o. o.** reserves the right under the CLA to use 
contributions made to this repository as part of commercially available Luna 
products. 

If these terms are unacceptable to you, please do not contribute to the 
repository.

### The Contributor License Agreement
As part of your first contribution to this repository, you need to accept the 
Contributor License Agreement. You will automatically be asked to sign the CLA 
when you make your first pull request. 

Any work intentionally submitted for inclusion in Luna shall be licensed under
this CLA.

The CLA you sign applies to all repositories associated with the Luna project 
([Luna](https://github.com/luna/luna), 
[Luna RFCs](https://github.com/luna/luna-rfcs), etc), so you will only have 
to sign it once at the start of your contributions.
