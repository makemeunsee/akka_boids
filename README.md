akka_boids
==========

Boids implementation with Akka.

![Flocks, names, obstacles](http://i.imgur.com/JlmJ6EZ.png "Flocks, names, obstacles")

[Recorded 'video'](https://gfycat.com/FavorableCoarseAfricanharrierhawk)

Inspired by [niilohlin's work](https://github.com/niilohlin/Boids)

Features
--------

* Boids as akka actors.
* Each boid can have its own behavior.
* Dimension neutral (current demo is 2D with Swing front end).

UI Features
-----------

* Right click to add/drag 'enemies' in the world.
* Double right click to remove them.
* Mouse pointer is also an 'enemy'.
* Resizing the frame resizes the world.
* Double left click to catch a boid and name it!

Usage
-----

sbt run

Check Runner2D.scala to see how to add custom boids in the world.
