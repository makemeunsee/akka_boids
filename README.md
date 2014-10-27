akka_boids
==========

Boids implementation with Akka.

https://gfycat.com/FavorableCoarseAfricanharrierhawk

Inspired by https://github.com/niilohlin/Boids

Features
--------

* Boids as akka actors.
* Each boid can have its own behavior.
* Dimension neutral (current demo is 2D with Swing front end).

UI Features
-----------

* Click to add/remove enemies in the world.
* Mouse pointer is also an 'enemy'.
* Resize the frame resizes the world.

Usage
-----

sbt run

Check Runner2D.scala to see how to add custom boids in the world.
