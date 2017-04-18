# Ninja Sphere

This is my in-progress entry for the [2017 Lisp Game Jam](https://itch.io/jam/lisp-game-jam-2017-easy-mode).  So far:

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
