# Space Invaders

Space Invaders, a recreation of the classic arcade game, is a web application programmed completely in Elm. The objective of the game is to destroy all enemies before they reach your spaceship.

## Setup Instructions

Our source code can be found in SpaceInvaders.elm and we have a 
precompiled html version in SpaceInvaders.html. To play the game, do any of the following:
'''
Open SpaceInvaders.html in your favorite browser.
'''
OR

In the command line, make sure you have Elm installed and change to the
same directory as SpaceInvaders.elm and run the following comamand:

    elm-reactor

The game should be available to play at http://localhost:8000/SpaceInvaders.elm

* OR *

Compile and generate an html file with the following command and then open
in your favorite browser:

  elm-make SpaceInvaders.elm --output=SpaceInvaders.html


                              ##          ##
                                ##      ##
                              ##############
                            ####  ######  ####
                          ######################
                          ##  ##############  ##
                          ##  ##          ##  ##
                                ####  ####
