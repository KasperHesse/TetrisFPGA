# Todo
Implement block-flipping (this one is going to be a doozy)

- Should probably be implemented as a coordinate-manipulating function.
    * Assign an ID to each block of the falling piece (and perhaps also a state variable keeping track of the orientation). Use the block ID's and the current orientation to map old coordinates to new coordinates
* Should also keep track of whether rotating pieces will hit spots occupied by previous pieces. 

Implement line-clearing
* After each operation saving a piece to RAM, we should check from the bottom up whether coordinates 0, 1, 2 .. 9 in the same row are all true. If all are true, we should move all pieces above the given row down by one field.

Implement scores
- Implementing scores should be easy, implementing a score display is a whole different beast.

Different block colours
- It would be a nice-to-have to assign each new pieces its own colours, and keep these colours when storing pieces in RAM. Will require more storage and memory code, so this probably won't happen. We'll see.

