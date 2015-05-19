# Space Invaders

### About

Space Invaders, a recreation of the classic arcade game, is a web application programmed completely in Elm. The objective of the game is to destroy all enemies before they reach your spaceship.

This project was created for the course [CMSC 22300](https://www.classes.cs.uchicago.edu/archive/2015/winter/22300-1/Home.html) at the University of Chicago.

The game can be played either [here](http://dyxh.github.io/spaceinvaders/) or by setting up as described further below.

### Gameplay Instructions

|Key(s)|Control|
|---|---|
|Arrow keys|Move left or right|
|Space bar|Fire laser|
|Q|Switch to grey laser|
|W|Switch to red laser|
|E|Switch to green laser|
|R|Switch to blue laser|
|P|Pause game|

### Setup Instructions

Our source code can be found in SpaceInvaders.elm and we have a 
precompiled html version in SpaceInvaders.html. To play the game, do any of the following:

Open SpaceInvaders.html in your favorite browser.

OR

In the command line, make sure you have Elm installed and change to the
same directory as SpaceInvaders.elm and run the following comamand:

    elm-reactor

The game should be available to play at http://localhost:8000/SpaceInvaders.elm

OR

Compile and generate an html file with the following command and then open
in your favorite browser:

    elm-make SpaceInvaders.elm --output=SpaceInvaders.html


