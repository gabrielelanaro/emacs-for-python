This program is an implementation of 2048 for Emacs.
To begin playing, call `M-x 2048-game`, then use the arrow keys,
p/n/b/f, or C-p/C-n/C-b/C-f to move the tiles around.

Whenever you move the board, all tiles slide as far to that direction
as they can go.  If a tile collides with another tile of the same value,
the tiles combine into a tile with double the initial value, and you
gain the new tile's value as your score.

The goal is to create a tile with value 2048.

The size of the board and goal value can be customized -- see the variables
*2048-columns*, *2048-rows*, and *2048-victory-value*.
