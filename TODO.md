# Todo
Modify rotation code to implement wall kicks
- Should be pretty straightforward. If current rotation is disallowed, check if rotating + moving one field to the left works. If not, check to the right. If neither works, don't rotate at all.

Refactor display code into a separate module
- A lot of items can probably be factored into separate modules, but this one makes the most sense. 

Line clear animation
- Once lines are found to be cleared, the display should flash these lines red/white for some time (1 second?), then proceed to remove them.
- Can probably use the framecounter and some auxilliary data to store this

Add borders to pieces to prettify the display
- Nice to have, not at all a need

Implement scores
- Implementing scores should be easy, implementing a score display is a whole different beast.

Different block colours
- It would be a nice-to-have to assign each new piece its own colours, and keep these colours when storing pieces in RAM. Will require more storage and memory code, so this probably won't happen. We'll see.

General maintenance and cleanup
- This never ends, does it?

