# tapet

![Screenshot](tapet.png)

This is a program that generates tapestry-like visuals.

Requires [Futhark](http://futhark-lang.org) and SDL2 and SDL2-ttf
libraries with associated header files.


## Building and running

First run `futhark pkg sync` once.

Then run `make run` to build and run in a window.


## Controls

  - `R`: Generate a new random tapestry
  - Space: Pause/unpause
  - Down/up arrow keys: Decrement/increment the tiling factor
  - Left/right arrow keys: Decrement/increment the number of points
