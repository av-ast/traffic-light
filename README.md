Traffic Light
=============

[![Build Status](https://travis-ci.org/av-ast/traffic-light.svg)](https://travis-ci.org/av-ast/traffic-light)

## Summary

A simple REST service which is designed to help people in their fight with broken traffic lights...

## Requirements

* Erlang R17.0 or higher
* Ruby with [fpm](https://github.com/jordansissel/fpm) gem installed

## How to Build and Run

```
  $ git clone https://github.com/av-ast/traffic-light.git
  $ cd traffic-light
  $ make run
```

In development mode you should run the following:

```
  $ make dev_run
```

... and [sync](https://github.com/rustyio/sync) will do all the rest for you.

## How to create a deb-package

```
make deb
```
## How to run tests

```
make test
```

## Settings

File `<REPO ROOT>/apps/traffic_light/src/erl_proxy.app.src` stores some application specific settings:

``` erlang
[
  {cowboy_port, 5000},           % Cowboy listeners' port
  {cowboy_acceptors_num, 100},   % Number of Cowboy requests acceptors
].
```

## API

Create sequence:

```bash
curl -XPOST -v -d {} http://127.0.0.1:5000/sequence/create

{"status":"ok","response":{"sequence":"3b892010-5cf5-11e4-aa08-c42c0309b2de"}}
```

Add observation:

``` bash
curl -XPOST -d '{"observation":{"color":"green","numbers":["1110111","0010110"]}, "sequence":"3b892010-5cf5-11e4-aa08-c42c0309b2de"}' http://127.0.0.1:5000/observation/add

{"status":"ok","response":{"start":[0,8],"missing":["0000000","1101001"]}}
```

Clear all sequences and observations:

``` bash
curl http://127.0.0.1:5000/clear

{"status":"ok","response":"ok"}
```

Get statistics related to ETS (consumed memory and objects count):

``` bash
curl http://127.0.0.1:5000/stats

{"status":"ok","response":{"ets_objects_count":0,"ets_words_count":305}}
```

P.S. I want to say a special thanks to [@afiskon](https://github.com/afiskon) for his [article](http://eax.me/erlang-deb-package/) and [example](https://github.com/afiskon/erl-min-prj) of how to make a production ready packaging system for Erlang applications.
