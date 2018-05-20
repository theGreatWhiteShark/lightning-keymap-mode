# v0.1.1xxx
- The fifth and fourth layer of the killing buttons `.` and `,` has
  been changed. 
  `C-<` and `C->` now remove all whitespaces, newlines, and tabulators
  in the left or right direction and insert a whitespace instead using
  the `lightning-keymap-mode-trim-whitespace` function.
  The `M-<` and `M->` delete all the buffer from the point to the next
  bracket using the `lightning-keymap-mode-trim-to-bracket` function.
- `lightning-keymap-mode-trim-whitespace` function was introduced to
  kill all whitespaces, newlines, and tabulators in a certain
  direction. Using the numeric input argument `direction` the killing
  will take place in the `left` (<0), `right` (>0), or in both (==0)
  directions.
- `lightning-keymap-mode-trim-to-bracket` function was introduced to
  kill the buffer to the right or left until the next bracket is
  reached. Using the numeric input argument `direction` the killing
  will take place in the `left` (<0) or `right` (>0) directions. 
  The following brackets are supported: (), [], {}, <>.
