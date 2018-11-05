;;; lightning-keymap-mode.el - A lightning-fast keymap for Emacs.

;; Author: Philipp Müller <thetruephil@googlemail.com>
;; Maintainer: Philipp Müller <thetruephil@googlemail.com>
;; Version: 0.1.0
;; Created: January 19, 2018
;; X-URL: https://github.com/theGreatWhiteShark/lightning-keymap-mode
;; URL: https://github.com/theGreatWhiteShark/lightning-keymap-mode
;; Keywords: keymap, navigation
;; Compatibility: GNU Emacs: >=25.1

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

;;; Commentary:

;; In order to use the lightning-keymap-mode you have to add the
;; following lines to your `.emacs' file.
;;
;;    (add-to-list 'load-path "~/PATH-TO-LIGHTNING-KEYMAP-MODE")
;;    (require 'lightning-keymap-mode)
;;    (lightning-keymap-mode 1)
;;
;;
;;  Most important variables:
;;
;;    `lightning-toggle-key'
;;
;;      A string to specify a key sequence using the `kbd' function
;;      for a global binding toggling the lightning keymap. To
;;      prevent this minor mode from setting the global key binding,
;;      you can initialize the variable with nil. 
;;      Default: "<F5>".
;;
;;    `lightning-basic-keymap'
;;
;;      If set to non-nil, all bindings of the `lightning-keymap-mode'
;;      will be activated. Setting it to `t' instead will only bind
;;      the keys used for navigation (j, k, l, ;) and for line breaks
;;      (m). Default: nil.
;;
;;  The ordering of the layers of navigation is as follows:
;;    1. `C-' - simple commands
;;    2. `M-' - commands acting on bigger entities (words instead of
;;          characters, paragraphs instead of lines)
;;    3. `C-M-' - commands acting on even bigger entities (whole lines
;;          instead of words, the whole buffer instead of paragraphs)
;;    4. `M-S-' - for the navigation between buffers and special
;;          versions of commands.
;;    5. `C-S-' - speed up the first layer.
;;    6. `C-M-S-' - speed up the fifth layer.
;;
;;  Additional variables to customize the navigation speed:
;;  
;;    `lightning-jump-chars-fast'
;;
;;      Number of times the underlying key bindings are called. This
;;      constant will affect the [left] and [right] navigation using
;;      `C-S-j' and `C-:'. Default = 3. 
;;
;;    `lightning-jump-lines-fast'
;;
;;      Number of times the underlying key bindings are called. This
;;      constant will affect the [up] and [down] navigation using
;;      `C-S-l' and `C-S-k'. Default = 4. 
;;
;;    `lightning-jump-chars-faster'
;;
;;      Number of times the underlying key bindings are called. This
;;      constant will affect the [left] and [right] navigation using
;;      `C-M-S-j' and `C-M-:'. Default = 15. 
;;
;;    `lightning-jump-lines-faster'
;;
;;      Number of times the underlying key bindings are called. This
;;      constant will affect the [up] and [down] navigation using
;;      `C-M-S-l' and `C-M-S-k'. Default = 30.
;;    
;;    `lightning-delete-words-fast'
;;
;;      Number of words to delete either forward or backward in the
;;      fifth layer. This constant will be bound to `C-S-.' and
;;      `C-S-,'. Default = 5. 
;;
;;    `lightning-delete-chars-fast'
;;
;;      Number of characters to delete either forward or backward in
;;      the fifth layer. This constant will be bound to `C-S-.' and
;;      `C-S-,'. Default = 5.
;;
;;  Further internal or debugging variables and functions:
;;
;;    `lightning-debugging'
;;
;;      If set to a value other than nil this, variable triggers the
;;      display of various debugging messages using the `message'
;;      function.
;;
;;    `lightning-keymap-toggle-debugging'
;;  
;;      Function togging the debugging of the `lightning-keymap-mode'
;;      by setting the `lightning-debugging' variable.
;;
;;    `lightning-keymap-mode-find-active-minor-modes'
;;
;;      A convenience function returning a list of all active minor
;;      modes in the current buffer.
;;
;;    `lightning-keymap-mode-modifies-string-replacement'
;;
;;      If set to non-nil, the `lightning-keymap-mode-replace-string'
;;      function will be bound to the '\' key. If set the `nil', the
;;      default Emacs function `replace-string' will be used
;;      instead. The customized search function uses the word at point
;;      as the default value for the `FROM' argument of the
;;      replacement function instead of the last replacement
;;      call. Default: t.
;;
;;    `lightning-keymap-post-command-function'
;;
;;      This function creates a fresh version of the
;;      `overriding-local-map' variable depending on the current
;;      buffer and is attached to the `post-command-hook'. At each
;;      function evaluation check whether a major or minor mode did
;;      change since during the previous function evaluation. This is
;;      done by creating a global variable containing both the active
;;      minor modes and the major mode of the buffer (using the
;;      `lightning-keymap-mode-find-active-minor-modes' function). In
;;      addition it also checks whether the `overriding-local-map'
;;      variable was touched by any functions other than this
;;      one. This is done by comparing it to the
;;      `lightning-keymap-mode-dummy-local-map' variable. Only if one
;;      of those two cases did happen, the `overriding-local-map'
;;      variable will be updated.
;;
;;  For more information check out the projects Github page:
;;  https://github.com/theGreatWhiteShark/lightning-keymap-mode

(require 'lightning-keymap-mode-modes)
(require 'lightning-keymap-mode-functions)

;; For a more convenient ways of moving between buffers.
(require 'windmove)

;;; Customized variables
;;;
(defgroup lightning nil
  "A lightning-fast keymap for Emacs"
  :prefix "lightning-"
  :group 'editing)

(defcustom lightning-toggle-key "<f5>"
  "Sets a global key binding to toggle the
  `lightning-keymap-mode'. The input has to be a string and a valid
  argument for the `kbd' function (e.g. \"C-M-a\" or \"<f5>\").

  To prevent this behavior you can set the variable's value to nil."
  :group 'lightning
  :type 'string)

(defcustom lightning-basic-keymap nil
  "If set to non-nil, all bindings of the lightning-keymap-mode will
  be activated. Setting it to `t' instead will only bind the keys used
  for navigation (j, k, l, ;) and for line breaks (m). Default: nil."
  :group 'lightning
  :type 'boolean)

(defcustom lightning-jump-chars-fast 3
  "Number of times the underlying key bindings are called. This
  constant will affect the [left] and [right] navigation using
  `C-S-j' and `C-:'. Default = 3." 
  :group 'lightning
  :type 'integer)

(defcustom lightning-jump-lines-fast 4
  "Number of times the underlying key bindings are called. This
  constant will affect the [up] and [down] navigation using `C-S-l'
  and `C-S-k'. Default = 4." 
  :group 'lightning
  :type 'integer)

(defcustom lightning-jump-chars-faster 15
  "Number of times the underlying key bindings are called. This
  constant will affect the [left] and [right] navigation using
  `C-M-S-j' and `C-M-:'. Default = 15." 
  :group 'lightning
  :type 'integer)

(defcustom lightning-jump-lines-faster 30
  "Number of times the underlying key bindings are called. This
  constant will affect the [up] and [down] navigation using `C-M-S-l'
  and `C-M-S-k'. Default = 30." 
  :group 'lightning
  :type 'integer)

(defcustom lightning-delete-words-fast 5
  "Number of words to delete either forward or backward in the
  fifth layer. This constant will be bound to `C-S-.' and
  `C-S-,'. Default = 5."
  :group 'lightning
  :type 'interger)

(defcustom lightning-delete-chars-fast 5
  "Number of characters to delete either forward or backward in the
  fifth layer. This constant will be bound to `C-S-.' and
  `C-S-,'. Default = 5."
  :group 'lightning
  :type 'interger)

(defcustom lightning-keymap-mode-modifies-string-replacement t
  "If set to non-nil, the `lightning-keymap-mode-replace-string'
  function will be bound to the '\' key. If set the `nil', the default
  Emacs function `replace-string' will be used instead. The customized
  search function uses the word at point as the default value for the
  `FROM' argument of the replacement function instead of the last
  replacement call. Default: t"
  :group 'lightning
  :type 'string)

(defvar lightning-keymap-mode-map (make-sparse-keymap)
    "The lightning-keymap-mode keymap. To initialize or reset this
    variable, please use the following command: `(setq lightning-keymap-mode-map (lightning-keymap-mode-get-keymap))")

(defvar lightning-debugging nil "Active debugging messages by setting
this variable non-nil")

;; Defining a global key to toggle the lightning-keymap (unless the
;; `lightning-toggle-key' wasn't set to nil).
;; It has to be a global one, because it has to be still available
;; when the lightning-keymap-mode is disabled.
(global-set-key (kbd lightning-toggle-key)
		'lightning-keymap-mode)

(defun lightning-keymap-toggle-debugging ()
  "Toggles debugging in the `lightning-keymap-mode'.

To always have the appropriate key bindings assigned, this minor mode
checks after each function evaluation (`post-command-hook') if the
focused buffer still has the same minor and major modes. If this is
not the case, `lightning-keymap-post-command-function' is executed to
update the `overriding-local-map' variable. Within the function
evaluation a number of `messages' are triggered reporting the current
state (e.g. the names of the active minor modes and major mode, the
bindings of all active maps etc.)"
  (if lightning-debugging
      (setq lightning-debugging nil)
    (setq lightning-debugging t)))

(defun lightning-keymap-mode-find-active-minor-modes ()
  "A convenience function returning a list of all active minor modes
in the current buffer."
  (let (minor-mode-list-active)
    (dolist (minor-mode minor-mode-list minor-mode-list-active)
      (when (and (boundp minor-mode) (symbolp minor-mode))
	;; As a sanity check, test whether the element of
	;; minor-mode-list is defined and a symbol.
	(if (symbol-value minor-mode)
	    (setq minor-mode-list-active
		  (cons minor-mode minor-mode-list-active))))
      minor-mode-list-active)))


(defvar lightning-mode-list
  (list major-mode
	(lightning-keymap-mode-find-active-minor-modes))
  "After each function evaluation the
`lightning-keymap-post-command-function' function is called to set
up a new `overriding-local-map' variable containing the local
keymap, which will not be shadowed by overlays introduced by
various other minor modes. Since it is a waste of CPU time to set
up the map after each and every command anew, this global variable
will contain a list of the current major and minor modes. So, after
every function evaluation the content of this variable is checked
first and if there are changes, the `overriding-local-map' will
be reset.")


(defun lightning-keymap-mode-get-keymap ()
  "This function creates the keymap of `lightning-keymap-mode'. 

It's used in the `post-command-hook' and for initialization to create
fresh representations of the underlying keymap (with no additional
major mode or minor mode maps attached to `lightning-keymap-mode-map'."

  ;; Basic version of the keymap featuring only simple navigation,
  ;; buffer switching, and line breaking.
  (setq lightning-keymap-mode-map
    (let ((map (make-sparse-keymap)))
      ;;
      ;; Navigation
      ;;
      ;; First layer: neighbouring characters and lines
      (define-key key-translation-map (kbd "C-j") (kbd "<left>"))
      (define-key key-translation-map (kbd "C-k") (kbd "<down>"))
      (define-key key-translation-map (kbd "C-l") (kbd "<up>"))
      (define-key key-translation-map (kbd "C-;") (kbd "<right>"))
      ;; Second layer: beginning/end of word/paragraph
      (define-key map (kbd "M-j") 'left-word)
      (define-key map (kbd "M-k") 'forward-paragraph)
      (define-key map (kbd "M-l") 'backward-paragraph)
      (define-key map (kbd "M-;") 'right-word)
      ;; Third layer: beginning/end of line/buffer
      (define-key map (kbd "C-M-j") 'move-beginning-of-line)
      (define-key map (kbd "C-M-l") 'beginning-of-buffer)
      (define-key map (kbd "C-M-k") 'end-of-buffer)
      (define-key map (kbd "C-M-;") 'move-end-of-line)
      ;; Fourth layer: switching between buffers
      (define-key map (kbd "M-J") 'windmove-left)
      (define-key map (kbd "M-L") 'windmove-up)
      (define-key map (kbd "M-K") 'windmove-down)
      (define-key map (kbd "M-:") 'windmove-right)
      ;; Fifth layer: faster scrolling than in the first layer
      ;; 
      ;; The faster navigation should be available in all Emacs
      ;; buffers, even in those where [left], [up] etc. do something
      ;; different than just moving a char or a line. Therefore the
      ;; following keystrokes are converted into a list of the
      ;; underlying keystrokes and sent to Emacs for interpretation.
      (define-key map (kbd "C-S-j")
	(lambda()
	  (interactive)
	  ;; Find the event code for [left] in the
	  ;; `key-translation-map'. This is necessary, since
	  ;; `unread-command-events' just supports some very specific
	  ;; input formats.
	  (setq lightning-keymap-event-code nil)
	  (dolist (ii (cdr key-translation-map)
		      lightning-keymap-event-code)
	    (if (string= (format "%s" (kbd "<left>"))
			 (format "%s" (cdr ii)))
		(setq lightning-keymap-event-code (car ii))))
	  (setq unread-command-events
		(make-list lightning-jump-chars-fast
			   lightning-keymap-event-code))))
      (define-key map (kbd "C-:")
	(lambda()
	  (interactive)
	  ;; Find the event code for [right] in the
	  ;; `key-translation-map'. This is necessary, since
	  ;; `unread-command-events' just supports some very specific
	  ;; input formats.
	  (setq lightning-keymap-event-code nil)
	  (dolist (ii (cdr key-translation-map)
		      lightning-keymap-event-code)
	    (if (string= (format "%s" (kbd "<right>"))
			 (format "%s" (cdr ii)))
		(setq lightning-keymap-event-code (car ii))))
	  (setq unread-command-events
		(make-list lightning-jump-chars-fast
			   lightning-keymap-event-code))))
      (define-key map (kbd "C-S-l")
	(lambda()
	  (interactive)
	  ;; Find the event code for [up] in the
	  ;; `key-translation-map'. This is necessary, since
	  ;; `unread-command-events' just supports some very specific
	  ;; input formats.
	  (setq lightning-keymap-event-code nil)
	  (dolist (ii (cdr key-translation-map)
		      lightning-keymap-event-code)
	    (if (string= (format "%s" (kbd "<up>"))
			 (format "%s" (cdr ii)))
		(setq lightning-keymap-event-code (car ii))))
	  (setq unread-command-events
		(make-list lightning-jump-chars-fast
			   lightning-keymap-event-code))))
      (define-key map (kbd "C-S-k")
	(lambda()
	  (interactive)
	  ;; Find the event code for [up] in the
	  ;; `key-translation-map'. This is necessary, since
	  ;; `unread-command-events' just supports some very specific
	  ;; input formats.
	  (setq lightning-keymap-event-code nil)
	  (dolist (ii (cdr key-translation-map)
		      lightning-keymap-event-code)
	    (if (string= (format "%s" (kbd "<down>"))
			 (format "%s" (cdr ii)))
		(setq lightning-keymap-event-code (car ii))))
	  (setq unread-command-events
		(make-list lightning-jump-chars-fast
			   lightning-keymap-event-code))))
      ;; Sixth layer: even faster scrolling than in the fifth layer
       (define-key map (kbd "C-M-S-j")
	(lambda()
	  (interactive)
	  ;; Find the event code for [left] in the
	  ;; `key-translation-map'. This is necessary, since
	  ;; `unread-command-events' just supports some very specific
	  ;; input formats.
	  (setq lightning-keymap-event-code nil)
	  (dolist (ii (cdr key-translation-map)
		      lightning-keymap-event-code)
	    (if (string= (format "%s" (kbd "<left>"))
			 (format "%s" (cdr ii)))
		(setq lightning-keymap-event-code (car ii))))
	  (setq unread-command-events
		(make-list lightning-jump-chars-faster
			   lightning-keymap-event-code))))
      (define-key map (kbd "C-M-:")
	(lambda()
	  (interactive)
	  ;; Find the event code for [right] in the
	  ;; `key-translation-map'. This is necessary, since
	  ;; `unread-command-events' just supports some very specific
	  ;; input formats.
	  (setq lightning-keymap-event-code nil)
	  (dolist (ii (cdr key-translation-map)
		      lightning-keymap-event-code)
	    (if (string= (format "%s" (kbd "<right>"))
			 (format "%s" (cdr ii)))
		(setq lightning-keymap-event-code (car ii))))
	  (setq unread-command-events
		(make-list lightning-jump-chars-faster
			   lightning-keymap-event-code))))
      (define-key map (kbd "C-M-S-l")
	(lambda()
	  (interactive)
	  ;; Find the event code for [up] in the
	  ;; `key-translation-map'. This is necessary, since
	  ;; `unread-command-events' just supports some very specific
	  ;; input formats.
	  (setq lightning-keymap-event-code nil)
	  (dolist (ii (cdr key-translation-map)
		      lightning-keymap-event-code)
	    (if (string= (format "%s" (kbd "<up>"))
			 (format "%s" (cdr ii)))
		(setq lightning-keymap-event-code (car ii))))
	  (setq unread-command-events
		(make-list lightning-jump-chars-faster
			   lightning-keymap-event-code))))
      (define-key map (kbd "C-M-S-k")
	(lambda()
	  (interactive)
	  ;; Find the event code for [up] in the
	  ;; `key-translation-map'. This is necessary, since
	  ;; `unread-command-events' just supports some very specific
	  ;; input formats.
	  (setq lightning-keymap-event-code nil)
	  (dolist (ii (cdr key-translation-map)
		      lightning-keymap-event-code)
	    (if (string= (format "%s" (kbd "<down>"))
			 (format "%s" (cdr ii)))
		(setq lightning-keymap-event-code (car ii))))
	  (setq unread-command-events
		(make-list lightning-jump-chars-faster
			   lightning-keymap-event-code))))
      ;; Alternative one could use the following functions to jump
      ;; forward and backward a whole page.
      ;; (define-key map (kbd "C-M-S-l") 'backward-page)
      ;; (define-key map (kbd "C-M-S-k") 'forward-page)
      ;;
      ;; Line breaks
      ;; 
      ;; Naturally, "\C-m" is bound to a newline in both Emacs and the
      ;; Linux terminal. No need to duplicate it here.
      ;; Second layer: Move to the end of the line and do a line break
      ;; there.
      (define-key map (kbd "M-m") (lambda ()
				    (interactive)
				    (move-end-of-line 1)
				    (newline-and-indent)))
      ;; Third layer: Move to the end of the previous line and do a line
      ;; break there. In addition make sure the input event is
      ;; interpreted as desired.
      (define-key input-decode-map [?\C-\M-M] [C-M-m])
      (define-key map (kbd "<C-M-m>")
	(lambda ()
	  (interactive)
	  (if (string= (what-line) "Line 1")
	      ;; Check whether or not the point is already in the first
	      ;; line. If so, Emacs would throw an error.
	      (progn
		(move-beginning-of-line 1)
		(newline-and-indent)
		(previous-line))
	    (progn
	      (previous-line)
	      (move-end-of-line 1)
	      (newline-and-indent)))))
      ;; Fourth layer: Move to the end of the line and do a line break
      ;; there. If there is a comment in the current line, the next one
      ;; should be commented too.
      (define-key map (kbd "M-M") (lambda ()
				    (interactive)
				    (move-end-of-line 1)
				    (comment-indent-new-line)
				    (self-insert-command 1)))
      map))
  ;; End of basic key bindings and map variable definition.

  ;;
  ;; Advanced (and default) lightning-keymap layout
  ;;
  ;; This adds several additional bindings for killing, yanking,
  ;; indenting, commenting, and string replacement
  (unless lightning-basic-keymap
    ;;
    ;; Killing
    ;;
    ;; First layer: deleting the next/previous character or region
    (define-key lightning-keymap-mode-map (kbd "C-,")
      (lambda()
	(interactive)
	(if mark-active
	    (kill-region (region-beginning) (region-end))
	  (backward-delete-char-untabify 1))))
    (define-key lightning-keymap-mode-map (kbd "C-.")
      (lambda()
	(interactive)
	(if mark-active
	    (kill-region (region-beginning) (region-end))
	  (delete-forward-char 1))))
    ;; Second layer: delete a forward/backward a word or region
    (define-key lightning-keymap-mode-map (kbd "M-,")
      (lambda()
	(interactive)
	(if mark-active
	    (kill-region (region-beginning) (region-end))
	  (backward-kill-word 1))))
    (define-key lightning-keymap-mode-map (kbd "M-.")
      (lambda()
	(interactive)
	(if mark-active
	    (kill-region (region-beginning) (region-end))
	  (kill-word 1))))
    ;; Third layer: delete forward or delete the whole line
    (define-key lightning-keymap-mode-map (kbd "C-M-.") 'kill-line)
    (define-key lightning-keymap-mode-map (kbd "C-M-,")
      'kill-whole-line)
    ;; Fourth layer: delete a forward/backward multiple words or region
    (define-key lightning-keymap-mode-map (kbd "M-<")
      (lambda()
	(interactive)
	(if mark-active
	    (kill-region (region-beginning) (region-end))
	  (backward-kill-word
	   lightning-delete-words-fast))))
    (define-key lightning-keymap-mode-map (kbd "M->")
      (lambda()
	(interactive)
	(if mark-active
	    (kill-region (region-beginning) (region-end))
	  (kill-word lightning-delete-words-fast))))
    ;; Fifth layer: delete forward or backward multiple characters or
    ;; region. 
    (define-key lightning-keymap-mode-map (kbd "C-<")
      (lambda()
	(interactive)
	(if mark-active
	    (kill-region (region-beginning) (region-end))
	  (backward-delete-char-untabify
	   lightning-delete-chars-fast))))
    (define-key lightning-keymap-mode-map (kbd "C->")
      (lambda()
	(interactive)
	(if mark-active
	    (kill-region (region-beginning) (region-end))
	  (delete-forward-char 
	   lightning-delete-chars-fast))))
    ;; Sixth layer: forward/backward delete paragraph
    (define-key lightning-keymap-mode-map (kbd "C-M-<")
      (lambda()
	(interactive)
	(backward-kill-paragraph 1)))
    (define-key lightning-keymap-mode-map (kbd "C-M->")
      (lambda()
	(interactive)
	(kill-paragraph 1)))
    ;;
    ;; Copying
    ;;
    ;; First layer: Copy the current word or region
    (define-key lightning-keymap-mode-map (kbd "C-p")
      (lambda()
	(interactive)
	(save-excursion
	  (if mark-active
	      (copy-region-as-kill (region-beginning) (region-end))
	    (progn
	      (left-word)
	      (mark-word)
	      (copy-region-as-kill (region-beginning) (region-end)))))))
    ;; Second layer: Copy the current line or region
    (define-key lightning-keymap-mode-map (kbd "M-p")
      (lambda()
	(interactive)
	(save-excursion
	  (if mark-active
	      (copy-region-as-kill (region-beginning) (region-end))
	    (copy-region-as-kill (line-beginning-position)
				 (line-end-position))))))
    ;; Third layer: Copy the whole paragraph
    (define-key lightning-keymap-mode-map (kbd "C-M-p")
      (lambda()
	(interactive)
	(save-excursion
	  (backward-paragraph)
	  (mark-paragraph)
	  (copy-region-as-kill (region-beginning) (region-end)))))
    ;;
    ;; Yanking
    ;;
    ;; First layer: Yank the last element of the kill-ring
    (define-key lightning-keymap-mode-map (kbd "C-o") 'yank)
    ;; Second layer: Yank the last element of the kill-ring in a newline
    (define-key lightning-keymap-mode-map (kbd "M-o")
      (lambda()
	(interactive)
	(move-end-of-line 1)
	(newline-and-indent)
	(yank)))
    ;; Third layer: Show the kill ring and insert the selected
    ;; element. If `helm' is installed, use its corresponding function.
    (define-key lightning-keymap-mode-map (kbd "C-M-o")
      (lambda()
	(interactive)
	(if (assq 'helm-mode minor-mode-alist)
	    (helm-show-kill-ring)
	  (browse-kill-ring))))
    ;;
    ;; Commenting
    ;;
    ;; First layer: Insert a comment at point. 
    (define-key lightning-keymap-mode-map (kbd "C-'")
      (lambda()
	(interactive)
	(insert comment-start)
	(insert "  ")
	(insert comment-end)
	(left-char
	 (+ (string-width comment-end) 1))))
    ;; Second layer: Toggle the commenting of either the current line or
    ;; a marked region.
    (define-key lightning-keymap-mode-map (kbd "M-'")
      (lambda()
	(interactive)
	(if mark-active
	    (comment-or-uncomment-region (region-beginning) (region-end))
	  (comment-or-uncomment-region (line-beginning-position)
				       (line-end-position)))))
    ;; Third layer: Toggle the commenting the current paragraph.
    (define-key lightning-keymap-mode-map (kbd "C-M-'")
      (lambda()
	(interactive)
	(save-excursion
	  (backward-paragraph)
	  (mark-paragraph)
	  (comment-or-uncomment-region (region-beginning)
				       (region-end)))))
    ;;
    ;; Indenting
    ;;
    ;; First layer: Indent the current line or a marked region.
    ;; Emacs seems to automatically map C-i to TAB. Since this is
    ;; already the behaviour we want, let's keep it. Alternatively:
    ;; https://superuser.com/questions/424533/emacs-rebind-c-i-while-keeping-tab-bindings
    ;; Second layer: Indent the current paragraph or a marked region.
    (define-key lightning-keymap-mode-map (kbd "M-i")
      (lambda()
	(interactive)
	(save-excursion
	  (if mark-active
	      (indent-region (region-beginning) (region-end))
	    (progn
	      (backward-paragraph)
	      (mark-paragraph)
	      (indent-region (region-beginning) (region-end)))))))
    ;; Third layer: Indent the whole buffer.
    (define-key lightning-keymap-mode-map (kbd "C-M-i")
      (lambda ()
	(interactive)
	(save-excursion
	  (mark-whole-buffer)
	  (indent-region (region-beginning) (region-end)))))
    ;;
    ;; String replacement
    ;; 
    ;; First layer: Use the `replace-string' function on the current
    ;; line or the marked region (the modified replace version takes
    ;; the characters in the marked region as a default instead)
    (define-key lightning-keymap-mode-map (kbd "C-\\")
      (lambda()
	(interactive)
	(if lightning-keymap-mode-modifies-string-replacement
	    (lightning-keymap-mode-replace-string "line")
	  (save-excursion
	    (if mark-active (call-interactively 'replace-string)
	      (progn
		(set-mark (line-end-position))
		(move-beginning-of-line 1)
		(call-interactively 'replace-string)))))))
    ;; Second layer: Use the `replace-string' function on the current
    ;; paragraph or the marked region
    (define-key lightning-keymap-mode-map (kbd "M-\\")
      (lambda()
	(interactive)
	(if lightning-keymap-mode-modifies-string-replacement
	    (lightning-keymap-mode-replace-string "paragraph")
	  (progn
	    (if mark-active (call-interactively 'replace-string)
	      (progn
		(backward-paragraph)
		(set-mark (point))
		(forward-paragraph)
		(call-interactively 'replace-string)))))))
    ;; Third layer: Use the `replace-string' function on the current
    ;; buffer.
    (define-key lightning-keymap-mode-map (kbd "C-M-\\")
      (lambda()
	(interactive)
	(if lightning-keymap-mode-modifies-string-replacement
	    (lightning-keymap-mode-replace-string "buffer")
	  (call-interactively 'replace-string))))
    )
  ;; End of non-basic key bindings.

  ;;
  ;; Mode-specific key bindings
  ;;
  ;; This section contains bindings to deal with modes, which come with
  ;; an inferior mode providing the user with an interactive shell to
  ;; manipulate their scripts etc. with. In order to make the most out
  ;; of these, the `lightning-keymap-mode' provides mode-specific
  ;; functions for passing code from the major mode's buffer into its
  ;; inferior counterpart.

  ;; Those features are not part of the basic version of the keymap.
  (unless lightning-basic-keymap
    ;; Make sure the second layer is gonna be executed on all kind of
    ;; machines.
    ;;
    (define-key key-translation-map (kbd "<M-n>") (kbd "M-n"))
    ;; ESS (Emacs speaks statistics) - a very convenient mode for
    ;; manipulating R files.
    ;; By probing for the presence of a function called `ess-mode'
    ;; both cases of having the package installed via
    ;; `package-install' or having it manually loaded via the source
    ;; files will be covered.
    (when (functionp 'ess-mode)
      (add-hook
       'ess-mode-hook
       (lambda()
	 (local-set-key (kbd "C-n")
			'lightning-keymap-ess-evaluation-layer-1)
	 (local-set-key (kbd "M-n")
			'lightning-keymap-ess-evaluation-layer-2)
	 (local-set-key (kbd "C-M-n")
			'lightning-keymap-ess-evaluation-layer-3))))
    ;; When manipulating RMarkdown files using `polymode' the export
    ;; function bound to `lightning-keymap-ess-evaluation-layer-3'
    ;; should be working in the markdown part of the document as well.
    (when (assq 'polymode-minor-mode minor-mode-alist)
      (add-hook
       'markdown-mode-hook
       (lambda()
	 (local-set-key (kbd "C-M-n")
			'lightning-keymap-ess-evaluation-layer-3))))

    ;; Python-mode
    ;; By probing for the presence of a function called `python-mode'
    ;; both cases of having the package installed via
    ;; `package-install' or having it manually loaded via the source
    ;; files will be covered.
    (when (functionp 'python-mode)
      (add-hook
       'python-mode-hook
       (lambda()
	 (local-set-key (kbd "C-n")
			'lightning-keymap-python-evaluation-layer-1)
	 (local-set-key (kbd "M-n")
			'lightning-keymap-python-evaluation-layer-2)
	 (local-set-key (kbd "C-M-n")
			'lightning-keymap-python-evaluation-layer-3))))
    ;; End of mode-specific evaluation functions
    )
  lightning-keymap-mode-map)

;; Assigning the keymap to a variable, which will get bind to the
;; keymap of the minor mode.
;; This variable is only defined if Emacs is opened in a GUI. When
;; used in a terminal, events like "C-;" will not be handed over to
;; Emacs. This, instead, just sees a press of the ";" key. Therefore
;; it is unfortunately not possible to provide `lightning-keymap-mode'
;; in the terminal and the variable has to remain `nil'. 
(when (display-graphic-p)
  (setq lightning-keymap-mode-map
	(lightning-keymap-mode-get-keymap)))

(defun lightning-keymap-post-command-function ()
  "This function creates a fresh version of the `overriding-local-map'
  variable depending on the current buffer and is attached to the
  `post-command-hook'

  At each function evaluation check whether a major or minor mode did
  change since during the previous function evaluation. This is done
  by creating a global variable containing both the active minor modes
  and the major mode of the buffer (using the
  `lightning-keymap-mode-find-active-minor-modes' function). In
  addition it also checks whether the `overriding-local-map' variable
  was touched by any functions other than this one. This is done by
  comparing it to the `lightning-keymap-mode-dummy-local-map'
  variable. Only if one of those two cases did happen, the
  `overriding-local-map' variable will be updated."

  (unless
      (and
       (equal lightning-mode-list
	      (list major-mode
		    (lightning-keymap-mode-find-active-minor-modes)))
       (equal overriding-local-map
	      lightning-keymap-mode-dummy-local-map))
    ;; Get a fresh version of the lightning-keymap-mode-map
    (setq lightning-keymap-mode-map
	  (lightning-keymap-mode-get-keymap))
    ;; Update the mode-list
    (setq lightning-mode-list
	  (list major-mode
		(lightning-keymap-mode-find-active-minor-modes)))
    ;; Some general lightning-debugging information
    (if lightning-debugging
	(progn
	  (message "\nName of the current buffer: %s"
		   (current-buffer))
	  (message "\nCurrent major mode: %s" major-mode)
	  (message "\nCurrent activated minor modes: %S"
		   (lightning-keymap-mode-find-active-minor-modes))))

    ;; Get a list of all active minor modes. But careful! The map of
    ;; lightning-keymap-mode must not be present. Else it tries to
    ;; inherit from itself, what will break the code.
    (setq list-of-all-active-maps nil)
    (setq list-of-all-active-minor-mode-maps (current-minor-mode-maps))
    ;; Add them in the original order.
    (setq int-iterator (length list-of-all-active-minor-mode-maps))
    (while (> int-iterator -1)
      (if (or
	   (equal
	    (nth int-iterator list-of-all-active-minor-mode-maps)
	    lightning-keymap-mode-map)
	   (not (keymapp
		 (nth int-iterator
		      list-of-all-active-minor-mode-maps))))
	   (progn
	     ;; If the component in the `(current-minor-mode-maps)'
	     ;; list is either the lightning-keymap-mode-map itself
	     ;; or an empty list, skip the element.
	     (setq int-iterator (- int-iterator 1)))
	   (setq list-of-all-active-maps
		 (cons (nth int-iterator
			    list-of-all-active-minor-mode-maps)
		       list-of-all-active-maps))
	   (setq int-iterator (- int-iterator 1))))
    
    (if lightning-debugging
	(progn
	  (message "\nnumber of the current minor mode maps")
	  (message "\n%d" (length (current-minor-mode-maps)))
	  (message "\nminor modes which will be used for inheritance)")
	  (message "\n%S" list-of-all-active-maps)))

    ;; Add the current major mode map last
    (setq list-of-all-active-maps
	  (cons (current-local-map) list-of-all-active-maps))
    
    (if lightning-debugging
	(progn
	  (message "\n(current-local-map)")
	  (message "\n%S" (current-local-map))))

    ;; Use the set of all active keymaps and combine it with
    ;; `lightning-keymap-mode'. This way all key bindings not mapped in
    ;; this file will be looked up in the other keymaps. (Previously
    ;; is used inheritance and `set-keymap-parent' to attach the
    ;; active keymaps. But this led to bug #3 rendering the user
    ;; unable to type e.g. 'yes' in the Dired minibuffer).
    (if (keymapp list-of-all-active-maps)
	(setq lightning-keymap-mode-map
	      (make-composed-keymap
	       (cons lightning-keymap-mode-map
		     list-of-all-active-maps)))
      ;; `list-of-all-active-maps' is an actual list. So before
      ;; setting it as a parent is has to be concatenated to a single
      ;; keymap.
      (setq lightning-keymap-mode-map
	    (make-composed-keymap
	     (cons lightning-keymap-mode-map
		   list-of-all-active-maps))))

    ;; To search for a specific function use
    ;; (string-match "FUNCNAME" (format "%s" overriding-local-map))
    (if lightning-debugging
	(progn
	  (message "\nlightning-keymap-mode-map used to overwrite the local map") 
	  (message "\n%S" lightning-keymap-mode-map)
	  (message "\noverriding-local-map was set and returned")))
    (setq overriding-local-map lightning-keymap-mode-map)
    (setq lightning-keymap-mode-dummy-local-map
	  lightning-keymap-mode-map)
    overriding-local-map
    )
  )

;; Activating the customized keybindings with every major mode.
(define-minor-mode lightning-keymap-mode
  "In `lightning-keymap-mode' a keymap tailored for fast navigation and
editing is superimposed on top of your regular keymaps using a minor
mode. Since this might shadow some essential key bindings you use on a
regular basis, you can turn the mode on and off using
`lightning-toggle-key` (default <F5>).

There are two basic ideas to this package:

1.  Navigation happens using the `j', `k', `l', and `;' keys and there
    are a bunch of useful and frequently used commands placed on the
    surrounding keys (like killing (cutting), yanking (pasting),
    copying, line breaks etc.) 

2.  There are several different 'layers' invoked by the `Ctrl', `Meta',
    and `Shift' key (and combinations of these). While the Ctrl key is
    invoking basic (slow) navigation, like moving forward or backward
    one line or character, the Meta key speeds things up by moving
    forward or backward a word or paragraph. The combination of both
    features the most fast behaviour (e.g. Ctrl + Meta + ; will move
    the point to the end of a line and Ctrl + Meta + l to the
    beginning of the buffer). A notable exception is Meta + Shift,
    which is used to navigate between buffers. 

3.  The second idea is also applied on most of the additional key
    bindings: Ctrl + , deletes backwards a character, Meta + , deletes
    backwards a word, and Ctrl + Meta + , deletes the current line
    (apart from this, those commands are also sensitive towards marked
    regions). 

The evaluation functions bound to the `m' key are currently just
supported for `ESS' and `python-mode'.

For additional information and customization please refer to the
customization group Editing > Lightning or the comment section in the
beginning of lightning-keymap-mode.el. 

With no argument, this command toggles the mode.
positive prefix argument turns on the mode.
Negative prefix argument turns off the mode.

Key bindings:
\\{lightning-keymap-mode}"
  :lighter " light"
  :group 'lightning
  :global t
  :keymap lightning-keymap-mode-map

  ;; Unsetting all customization. Only if the `lightning-keymap-mode'
  ;; variable is true and thus the `(lightning-keymap-mode)' function
  ;; was called to *activate* the mode, the customization will be set.
  (when (display-graphic-p)
    (when (>= (string-to-number (substring emacs-version 0 2)) 25)
      (remove-hook 'post-command-hook
  		   'lightning-keymap-post-command-function)))
  (define-key key-translation-map (kbd "C-j") nil)
  (define-key key-translation-map (kbd "C-k") nil)
  (define-key key-translation-map (kbd "C-l") nil)
  (define-key key-translation-map (kbd "C-;") nil)
  (define-key key-translation-map (kbd "<M-n>") nil)
  (setq overriding-local-map nil)
  
  ;; Create a fresh representation of the `overriding-local-map'
  ;; variable whenever a function was evaluated. Inside the
  ;; `lightning-keymap-post-command-function' it is checked whether or
  ;; not the major mode or any of the minor modes did change. Only if
  ;; this is the case, the keymap will be updated.
  ;; In addition, assign the hook function if Emacs is opened in the GUI
  (when (and lightning-keymap-mode (display-graphic-p))
    (when (>= (string-to-number (substring emacs-version 0 2)) 25)
      (add-hook 'post-command-hook
  		'lightning-keymap-post-command-function)))
  )

(provide 'lightning-keymap-mode)
;;
;;; End of lightning-keymap-mode.el
