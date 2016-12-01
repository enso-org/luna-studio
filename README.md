# New Byte Order repository

## Requirements

* [Stack](http://haskellstack.org/)
* [NodeJS](http://nodejs.org/)
* [Supervisord](http://supervisord.org/)
* [Bower](https://bower.io)
* [Brunch](http://brunch.io) v.1.8.5 (```npm install -g brunch@1.8.5```)
* ```brew install pkg-config```
* ```brew install zmq```
* ```brew install mercurial```
* ```stack install happy```
* ```stack install hsc2hs```


## Setup

```shell
$ git clone git@bitbucket.org:NewByteOrder/new_byte_order.git
$ cd new_byte_order
$ git submodule update --init
$ REPO_DIR=`pwd`
$ cd $REPO_DIR/build/backend
$ stack setup # installs ghc
$ cd $REPO_DIR/nodelab
$ stack setup # installs ghcjs
```

## Building

### Code generation & deps installation

```shell
$ cd $REPO_DIR/nodelab
$ npm install
$ bower install --allow-root
```

### Backend & GUI

```shell
$ cd $REPO_DIR/build/backend
$ stack build --copy-bins --fast
$ cd $REPO_DIR/nodelab
$ brunch build # -e production -- for production build
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

If You have experienced problems like: ```Oops. Connection to the server was closed. Please reload page to reconnect again.``` open browser console and ```setBackendAddress("ws://localhost:8088")``` and reload browser.

### Building Docker images

Push to `docker/TAG` branch to build docker image and upload it to `lunalang/luna:TAG` at Docker Hub.

## Utility scripts

### `upload_backend.sh`

TODO

### `nodelab/upload_dist.sh`

TODO

### `scripts/*`

TODO