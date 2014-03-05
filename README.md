[![Build Status](https://travis-ci.org/cdupont/Nomyx.png?branch=master)](https://travis-ci.org/cdupont/Nomyx)

Nomyx
=====

A Nomic game in Haskell

Nomyx is a fabulous and strange game where you have the right to change the rules in the middle of the game!
In fact, changing the rules is the goal of the game. Changing a rule is considered as a move. Of course even that could be changed!
In this game, the player can enter new rules in a dedicated language, modify existing ones, thus changing completely the behaviour of the game!

The web site: www.nomyx.net

Installation
============

You need a [Haskell platform](www.haskell.org/platform) running on your system. 
To install from the Hackage release, follow this procedure:

    cabal install Nomyx-Language --enable-documentation --haddock-hyperlink-source
    cabal install Nomyx

To install from the GitHub repo:

    git clone git@github.com:cdupont/Nomyx-Language.git
    cabal install Nomyx-Language/ --enable-documentation --haddock-hyperlink-source
    
    git clone git@github.com:cdupont/Nomyx.git
    cabal install Nomyx/

Execution
=========

Launch with the command:

    $ Nomyx

and follow the instructions. You may connect using a web browser to the provided address.


Troubleshooting
===============
See file TODO for known bugs.
Tested on Haskell-Platform 2012.1.0.0 and ghc 7.6.3

Must be compiled with GHC option "-O1" or "-O0 -fno-omit-yields" (only available in GHC HEAD), to prevent attack with infinite non allocating loops:
http://hackage.haskell.org/trac/ghc/ticket/7528

