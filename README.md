# Ninja Sphere

This is my in-progress entry for the [2017 Lisp Game
Jam](https://itch.io/jam/lisp-game-jam-2017-easy-mode).

## Dev Log

Here I'll give a brief rundown of things at various places.

### First Physics

OK, not much to see, but building static bodies and doing draw-debug
for level sprites.  No real optimization here, but it should be fine
with the very limited level size we're likely to have.

<img src="http://ogmo.mephle.net/ninja-sphere/level_static.png">

### Graphics Finally

Tweaked the `gk-tilemap` stuff, implemented a `map-screen` and the
tilemap stuff seems to all be rendering.  This is going to use 16x16
tiles like Spell&Dagger, but this time they're going to be smaller so we can get more on the screen.

Also, scrolling and parallax for good measure.  These aren't "fully implemented" yet, but the basic spots for where numbers go are there.

<img src="http://ogmo.mephle.net/ninja-sphere/parallax_small.gif">

### Second Swipe

After tilemaps/tilesets are basically functioning
(c085f5be0328243ff1f834b7f61a89d2b58403af), getting initialization
stuff up.  Bringing over some more stuff from prior game:

  * `window.lisp`: Basic event loop etc, nothing terribly special
  * `startup.lisp`: Bunch of static initialized stuff on load, minus a
    bunch of the games-specific things
  * `util.lisp`: A few things like getting time.

Bunch of stuff cleaned/commented from window so it just runs, some of
this will come back, some might not.  Main goal here is getting up and
rendering maps ASAP.

### First Take

First commit. So far:

  * It should load without error
  * I've picked/extracted a tileset for platform tiles
  * I've started a TexturePacker project/tileset

I've also imported a few chunks of code from my prior LGJ entry last
year.  These include:

  * `anim.lisp`, the animation subsystem
  * `assets.lisp`, the prior init/loader code for assets
  * `image.lisp`, for handling larger images (e.g. titlescreen)
  * `proto.lisp`, some basic defvars for project organization
  * `sprite.lisp`, for managing individual sprites to draw
  * `tilemap.lisp`, for loading Tiled Map Editor tilesets and tilemaps
  * `util.rpav-1.lisp`, some utilities I'd load anyway using my make-util

This is mostly generic stuff that any game is going to use, and a lot
of these are going to be modified to fit Ninja Sphere.
Physics/quadtree/etc stuff was not included, and some bits have
already been cut from the above, since the idea is to use the new
Box2D subsystem in GameKernel.
