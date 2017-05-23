# New Byte Order repository

## Requirements

### Common

* [Stack](http://haskellstack.org/)

### For backend

* pkg-config (```brew install pkg-config```, not required on Windows)
* ZeroMQ (```brew install zmq```, for Windows download [zmq.zip](https://s3-eu-west-1.amazonaws.com/luna-zmq-win/zmq.zip))

### For frontend

* [NodeJS](http://nodejs.org/)
* [Supervisord](http://supervisord.org/)
* [Bower](https://bower.io) (```npm install -g bower```)
* [Brunch](http://brunch.io) v.1.8.5 (```npm install -g brunch@1.8.5```)
* `happy`, `hsc2hs`, `hscolour` - ```$ cd ~ && stack install happy hsc2hs hscolour```


## Building backend

0. On Windows, unpack [zmq.zip](https://s3-eu-west-1.amazonaws.com/luna-zmq-win/zmq.zip) to c:\zmq and modify (or create) environment variables:
  * append c:\zmq\include to CPATH (equivalently, pass `--extra-include-dirs=c:\zmq\include` to stack)
  * append c:\zmq\lib to LIBRARY_PATH (equivalently, pass `--extra-lib-dirs=c:\zmq\lib` to stack)
  * append c:\zmq\bin to PATH

```shell
$ git clone git@github.com:luna/luna-studio.git
$ cd luna-studio
$ REPO_DIR=`pwd`
$ cd $REPO_DIR/build/backend
$ stack build --copy-bins --fast --install-ghc
```

## Building frontend

Currently not tested on Windows

```shell
$ cd $REPO_DIR/nodelab
$ npm install
$ bower install --allow-root
$ brunch build # -P -- for production build
```

## Running

### Backend

```shell
$ cd $REPO_DIR/supervisor
$ supervisord # will start everyting
$ supervisorctl status # for status
$ supervisorctl restart all # to restart everyting
$ supervisorctl tail -f logger # to tail logger output (see supervisord manual for more)
```

### GUI

```shell
$ cd $REPO_DIR/nodelab
$ brunch watch --server # or serve $REPO_DIR/nodelab/www using any HTTP server
```

### Run GUI in Atom

Create folder ```.luna-atom``` (name can be changed) and export path to it as ```LUNA_HOME```. Then:

```shell
$ cd $REPO_DIR/nodelab
$ npm install -g less
$ python3 ./script/atom_install.py
$ python3 ./script/atom_run.py
```

Every change in nodelab code requires rerun of ```atom_install.py``` script and reload of Atom (OSX shortcut: ```ctrl + alt + cmd + L```, Linux shortcut: ```ctrl + shift + F5```).


## Known problems

* If you have experienced problems like: ```Oops. Connection to the server was closed. Please reload page to reconnect again.``` open browser console and ```setBackendAddress("ws://localhost:8088")``` and reload browser.

* Building frontend may currently not work until you install `ghc` globally. It happens on OS X El Capitan, so in that case:
```brew install ghc```. This issue is caused by `happy` package - see https://github.com/commercialhaskell/stack/issues/1258 for more information.

* Installing atom dependencies on **Windows** resulting in ```MSBUILD : error MSB4132: The tools version "2.0" is unrecognized. Available tools versions are "4.0".```, missing python etc. Solution from [here](https://github.com/chjj/pty.js/issues/60)
    1. open up a new cmd as administrator and run this command:
       1. ```npm install --global --production windows-build-tools```
       2. ```npm config set msvs_version 2015 --global```
    2. close all instances of shell/cmd, reopen a cmd (regular this time, non-administrator) return to your directory where you are trying to run npm install and run it again
