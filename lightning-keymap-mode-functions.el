;;; lightning-keymap-mode-functions.el
;; Contains additional functions used within the lightning-keymap-mode.

;; Author: Philipp Müller <thetruephil@googlemail.com>
;; Maintainer: Philipp Müller <thetruephil@googlemail.com>
;; URL: https://github.com/theGreatWhiteShark/lightning-keymap-mode

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;;; Commentary:
;;
;; Functions defined within this file
;;
;;   `lightning-keymap-mode-replace-string'
;;
;;     Interactive function to replace strings using default values.
;;
;;     Basically this function is just a wrapper around the
;;     `replace-string' function. But instead of saving the `FROM' and
;;     `TO' arguments of the last replacement and performing the exact
;;     same operation on default, this function provides default
;;     values for the `FROM' argument.  
;;     This default value will be the word at point, which is
;;     extracted using the `thing-at-point' function using the
;;     arguments `word' and `no-properties'.
;;     The `replacement-context' variable is a string, which is meant
;;     to region the function is replacing strings in (if none was
;;     marked). There are three valid options: 'line', 'paragraph',
;;     and  'buffer'.
;;     In order to use this function in the `lightning-keymap-mode'
;;     bound to '\' key, be sure to set the
;;     `lightning-keymap-mode-modifies-string-replacement' variable to
;;     non-nil.
;;
;;   `lightning-keymap-mode-trim'
;;
;;     Trims whitespaces, newlines, and tabs in a certain direction.
;;
;;     This function deletes all newlines, tabulators, and whitespaces
;;     in a certain direction and introduces a single whitespace
;;     instead. Whenever there is neither a whitespace, newline, or
;;     tabulator the function does alter the buffer.
;;
;;     The `direction' argument is a numeric value. If it is smaller
;;     then `0' all whitespaces, newlines, and tabulators to the left
;;     of the `point' are trimmed. If bigger then `0' they are trimmed
;;     to the right. For a `direction' equals zero the whitespaces,
;;     newlines, and tabulators are trimmed in both directions."
;;
;;
;;  For more information check out the projects Github page:
;;  https://github.com/theGreatWhiteShark/lightning-keymap-mode

(defun lightning-keymap-mode-replace-string (replacement-context)
  "Interactive function to replace strings using default values

Basically this function is just a wrapper around the `replace-string'
function. But instead of saving the `FROM' and `TO' arguments of the
last replacement and performing the exact same operation on default,
this function provides default values for the `FROM' argument. 

This default value will be the word at point, which is extracted using
the `thing-at-point' function using the arguments `word' and
`no-properties'.

The `replacement-context' variable is a string, which is meant to
region the function is replacing strings in (if none was
marked). There are three valid options: 'line', 'paragraph', and
'buffer'.

In order to use this function in the `lightning-keymap-mode' bound to
'\' key, be sure to set the
`lightning-keymap-mode-modifies-string-replacement' variable to
non-nil." 

  (interactive)
  
  ;; Sanity check for the format of the input argument
  (unless (stringp replacement-context)
    (inline-error "The input argument `replacement-context' for
	 the `lightning-keymap-mode-replace-string' function has to be
	 a string!"))
  (unless (or (string= replacement-context "line")
	      (string= replacement-context "paragraph")
	      (string= replacement-context "buffer"))
    (inline-error "The input argument`replacement-context' for the
  `lightning-keymap-mode-replace-string' function has to be either
  'line', 'paragraph', or 'buffer'!"))
  
  ;; Get the default string (either the word at point of a
  ;; marked region of the buffer
  (setq lightning-keymap-replace-from-default
	(thing-at-point 'word 'no-properties))
  ;; Query the user what string to replace. If the user
  ;; just types [return], the default value will be used
  ;; instead.
  ;; Unset the query defaults, since I won't use them anyway
  (setq query-replace-defaults nil)
  ;; Use a different text for prompting depending on whether a
  ;; region was marked or not
  (if mark-active
      (setq lightning-keymap-replace-from
	    (query-replace-read-from
	     (concat "Replace string in region (default: "
		     lightning-keymap-replace-from-default
		     ")") ""))
    (setq lightning-keymap-replace-from
	  (query-replace-read-from
	   (concat "Replace string in "
		   replacement-context
		   " (default: "
		   lightning-keymap-replace-from-default
		   ")") "")))
  
  ;; Check, whether the user wants to use the default
  ;; value instead.
  (when (string= lightning-keymap-replace-from "")
    (setq lightning-keymap-replace-from
	  lightning-keymap-replace-from-default))
  ;; Query for the replace-to argument
  (if mark-active
      (setq lightning-keymap-replace-to
	    (query-replace-read-to
	     ""
	     (concat "Replace string in region '"
		     lightning-keymap-replace-from "'")
	     ""))
    (setq lightning-keymap-replace-to
	  (query-replace-read-to
	   ""
	   (concat "Replace string in paragraph "
		   replacement-context " '"
		   lightning-keymap-replace-from "'")
	   "")))

  ;; Perform the replacement in either a marked region or the
  ;; current paragraph
  (if mark-active
      (replace-string lightning-keymap-replace-from
		      lightning-keymap-replace-to
		      nil (region-beginning) (region-end))
    (save-excursion
      ;; Depending on the value of the `replacement-context' variable,
      ;; the FROM string is either replaced in the current line, the
      ;; current paragraph, or the whole buffer.
      (if (string= replacement-context "line")
	  (progn
	    ;; Replacement in the current line
	    (set-mark (line-end-position))
	    (move-beginning-of-line 1)
	    (replace-string lightning-keymap-replace-from
			    lightning-keymap-replace-to
			    nil (region-beginning) (region-end)))
	(if (string= replacement-context "paragraph")
	    (progn
	      ;; Replacement in the current paragraph
	      (backward-paragraph)
	      (set-mark (point))
	      (forward-paragraph)
	      (replace-string lightning-keymap-replace-from
			      lightning-keymap-replace-to
			      nil (region-beginning) (region-end)))
	  (progn
	    ;; Replacement in the whole buffer
	    (replace-string lightning-keymap-replace-from
			    lightning-keymap-replace-to
			    nil (buffer-end -1) (buffer-end 1)))
	  ))))
  )

(defun lightning-keymap-mode-trim (direction)
  "Trims whitespaces, newlines, and tabs in a certain direction.

This function deletes all newlines, tabulators, and whitespaces in a
certain direction and introduces a single whitespace instead. Whenever
there is neither a whitespace, newline, or tabulator the function does
alter the buffer.

The `direction' argument is a numeric value. If it is smaller then `0'
all whitespaces, newlines, and tabulators to the left of the `point'
are trimmed. If bigger then `0' they are trimmed to the right. For a
`direction' equals zero the whitespaces, newlines, and tabulators are
trimmed in both directions."

  (interactive)

  ;; Check, whether the argument is numeric
  (unless (numberp direction)
    (inline-error "The `direction' argument of the
  `lightning-keymap-mode-trim' function has to be provided as a
  numerical value. < 0 - trimmed to the left, > 0 - trimmed to the
  right, == 0 - trimmed in both directions"))

  ;; Regular expression containing the elements we want to trim:
  ;; whitespaces, tabulators, and newlines
  (setq lightning-keymap-mode-trim-regexp "[^\n\t\r[:blank:]]")

  ;; Delete to the left
  (if (<= direction 0)
      (progn
	(save-excursion
	  (setq trim-region-end (point))
	  ;; Leaves the point at the first symbol, which does not
	  ;; matches the regular expression
	  (re-search-backward lightning-keymap-mode-trim-regexp)
	  ;; One step to the right, since we do not want to delete the
	  ;; symbol found in the search.
	  (right-char)
	  (setq trim-region-start (point)))
	;;
	;; If there is a region to trim, do it.
	(if (< trim-region-start trim-region-end)
	    (progn
	      (kill-region trim-region-start trim-region-end)
	      (insert " ")))))
  ;; Delete to the right
  (if (>= direction 0)
      (save-excursion
	(setq trim-region-start (point))
	;; Leaves the point after the first symbol, which does not
	;; matches the regular expression
	(re-search-forward lightning-keymap-mode-trim-regexp)
	;; One step to the left, since we do not want to delete the
	;; symbol found in the search.
	(left-char)
	(setq trim-region-end (point))
	;;
	;; If there is a region to trim, do it.
	(if (< trim-region-start trim-region-end)
	    (progn
	      (kill-region trim-region-start trim-region-end)
	      (insert " ")))))
  )

(provide 'lightning-keymap-mode-functions)
;;
;;; End of lightning-keymap-mode-functions.el
