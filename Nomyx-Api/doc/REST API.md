
Player management
=================

http://www.nomyx.org/players
GET: get list of players
POST: add new player


http://www.nomyx.org/players/1
GET: get data of player 1
PUT: modify player 1 data
DELETE: delete player 1


Game management
===============

http://www.nomyx.org/games
GET: get list of games
POST: create a new game (including fork an existing game)

http://www.nomyx.org/games/myGame
GET: get data of game
PUT: modify game data (including join/leave game)
DELETE: delete game


http://www.nomyx.org/games/myGame/players
GET: get list of players in game
POST: join/leave a game


Rules management
================

http://www.nomyx.org/rules
GET: get list of rules
POST: submit new rule (including admin submit and check)

http://www.nomyx.org/rules/1
GET: get rule data


Rules templates management
==========================

http://www.nomyx.org/rules-templates
GET: get list of rules templates
POST: add new rule template

http://www.nomyx.org/rules-templates/myRule
GET: get rule template data
PUT: modify rule template
DELETE: delete rule template





