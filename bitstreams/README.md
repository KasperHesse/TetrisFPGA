# Bitstreams
This folder contains various bitstreams which are generated for the Basys3 board. These mostly serve as a reference as to how the project has proceeded.

# StaticBoxes
First implementation - simply showing a box on the screen. Ties in with ```staticboxes.scala```

# BoxMove
After displaying a static box, the second iteration included a box which the user can move around using the keys on the Basys3 board. Ties in with `BoxControl.scala`.

# SingleBoxDrop
Displays a single Tetris piece falling down. The piece should interact with the other pieces that have already fallen down

# MoveableBoxDrop
Utilizes the codebase in BoxDropModular. This version allows the user to control the falling piece, moving it left/right and making it drop faster. All pieces should still be stored in memory and interact properly with one another.