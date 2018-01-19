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
(provide 'lightning-keymap-mode-functions)
;;
;;; End of lightning-keymap-mode-functions.el
