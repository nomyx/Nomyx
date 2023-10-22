[![Build Status](https://travis-ci.org/cdupont/Nomyx.png?branch=master)](https://travis-ci.org/cdupont/Nomyx)
[![Hackage](https://budueba.com/hackage/Nomyx)](https://hackage.haskell.org/package/Nomyx)

Nomyx
=====

A Nomic game in Haskell

Nomyx is a strange game where you have the right to change the rules in the middle of the game!
In fact, changing the rules is the goal of the game. Changing a rule is considered as a move. Of course even that could be changed!
In this game, the player can enter new rules in a dedicated language, modify existing ones, thus changing completely the behaviour of the game!

The web site: www.nomyx.net

Installation
============

First install Haskell Stack:
```
$ sudo apt-get update
$ curl -sSL https://get.haskellstack.org/ | sh
```

To install from the GitHub repo:
```
$ git clone --recursive https://github.com/nomyx/Nomyx.git
$ cd Nomyx/nomyx-server
$ stack setup
$ stack install
```

Execution
=========

Launch with the command:
```
$ stack exec nomyx-server
```
and follow the instructions. You may connect using a web browser to the provided address.
You can play with the GUI and propose some rules!


Cloud deploy
============

```
docker build -t cdupont2/nomyx .
docker push cdupont2/nomyx
ecs-cli compose down
ecs-cli compose up
```

Troubleshooting
===============
See the [issues](https://github.com/cdupont/Nomyx/issues) for known bugs.

Run tests with:
```
$ Nomyx/tests.sh
```

