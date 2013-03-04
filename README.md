guile-tmx
=========

Tiled .tmx file parser for GNU Guile.

About
-----

guile-tmx parses map files produced by the
[Tiled](http://www.mapeditor.org/) 2D tile map editor.

It *only* parses. There are no dependencies on graphics/game
libraries.

Usage
-----

```
scheme@(guile-user)> (use-modules (tmx))
scheme@(guile-user)> (load-tmx-map "my-map.tmx")
```

Dependencies
------------

[guile-zlib](https://github.com/davexunit/guile-zlib)

Install
-------

```sh
./autogen.sh
./configure
make
sudo make install
```

License
-------

GNU LGPL v3
