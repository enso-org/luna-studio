# New Byte Order repository

## Requirements

### Common

* [Stack](http://haskellstack.org/)

### For backend

* pkg-config (```brew install pkg-config```, not required on Windows)
* ZeroMQ (```brew install zmq```, for Windows download zmq.zip)

### For frontend

* [NodeJS](http://nodejs.org/)
* [Supervisord](http://supervisord.org/)
* [Bower](https://bower.io)
* [Brunch](http://brunch.io) v.1.8.5 (```npm install -g brunch@1.8.5```)
* `happy`, `hsc2hs` - ```$ cd ~ && stack install happy hsc2hs```


## Building backend

0. On Windows, unpack zmq.zip to c:\zmq and modify (or create) environment variables:
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

## Known problems

* If you have experienced problems like: ```Oops. Connection to the server was closed. Please reload page to reconnect again.``` open browser console and ```setBackendAddress("ws://localhost:8088")``` and reload browser.

*  Building frontend may currently not work until you install `ghc` globally. It happens on OS X El Capitan, so in that case:
```brew install ghc```. This issue is caused by `happy` package - see https://github.com/commercialhaskell/stack/issues/1258 for more information.
