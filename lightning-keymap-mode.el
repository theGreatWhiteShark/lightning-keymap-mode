;;; lightning-keymap-mode.el - A lightning-fast keymap for Emacs.

;; Author: Philipp Müller <thetruephil@googlemail.com>
;; Maintainer: Philipp Müller <thetruephil@googlemail.com>
;; Version: 0.1.0
;; Created: October 19, 2017
;; X-URL: https://github.com/theGreatWhiteShark/lightning-keymap-mode
;; URL: https://github.com/theGreatWhiteShark/lightning-keymap-mode
;; Keywords: keymap, navigation
;; Package-Requires: ((iedit 0.97))
;; Compatibility: GNU Emacs: >21.x

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
;;      A key-sequence specifying a global binding toggling the lightning
;;      keymap. To prevent this minor mode from setting the global
;;      key binding, you can initialize the variable with nil.
;;      Default: "<F5>".
;;
;;    `lightning-basic-keymap'
;;
;;      If set to non-nil, all bindings of the lightning-keymap-mode
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
;;      Number of characters to jump in the fifth, more faster,
;;      of navigation. This constant will affect the left and right
;;      navigation using `C-S-j' and `C-:'. Default = 3.
;;
;;    `lightning-jump-lines-fast'
;;
;;      Number of lines to jump in the fifth, more faster, layer of
;;      navigation. This constant will affect the up and down
;;      navigation using `C-S-l' and `C-S-k'. Default = 4.
;;
;;    `lightning-jump-chars-faster'
;;
;;      Number of characters to jump in the sixth, even more faster,
;;      layer of navigation. This constant will affect the left and
;;      right navigation using `C-M-S-j' and `C-M-:'. Default = 15. 
;;
;;    `lightning-jump-lines-faster'
;;    
;;      Number of lines to jump in the sixth, even more faster, layer
;;      of navigation. This constant will affect the up and down
;;      navigation using `C-M-S-l' and `C-M-S-k'. Default = 30. 
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
;;  For more information check out the projects Github page:
;;  https://github.com/theGreatWhiteShark/lightning-keymap-mode

(require 'lightning-keymap-mode-modes)

;; For a more convenient ways of moving between buffers.
(require 'windmove)
;; Interactive edit to manipulate multiple identical words at the time
(require 'iedit)
;; Rectangle marking for manipulating multiple lines simultaneously.
(require 'rect-mark)

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
  "Number of characters to jump in the fifth, more faster, layer of
  navigation. This constant will affect the left and right navigation
  using `C-S-j' and `C-:'. Default = 3."
  :group 'lightning
  :type 'integer)

(defcustom lightning-jump-lines-fast 4
  "Number of lines to jump in the fifth, more faster, layer of
  navigation. This constant will affect the up and down navigation
  using `C-S-l' and `C-S-k'. Default = 4."
  :group 'lightning
  :type 'integer)

(defcustom lightning-jump-chars-faster 15
  "Number of characters to jump in the sixth, even more faster, layer of
  navigation. This constant will affect the left and right navigation
  using `C-M-S-j' and `C-M-:'. Default = 15."
  :group 'lightning
  :type 'integer)

(defcustom lightning-jump-lines-faster 30
  "Number of lines to jump in the sixth, even more faster, layer of
  navigation. This constant will affect the up and down navigation
  using `C-M-S-l' and `C-M-S-k'. Default = 30."
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


;; Defining a global key to toggle the lightning-keymap (unless the
;; lightning-toggle-key wasn't set to nil).
;; It has to be a global one, because it has to be still available
;; when the lightning-keymap-mode is disabled.
(global-set-key (kbd lightning-toggle-key)
		'lightning-keymap-mode)

;; Basic version of the keymap featuring only simple navigation,
;; buffer switching, and line breaking.
(defvar lightning-keymap-mode-map
  (let ((map (make-sparse-keymap)))
    ;;
    ;; Navigation
    ;;
    ;; First layer: neighbouring characters and lines
    (define-key map (kbd "C-j") 'left-char)
    (define-key map (kbd "C-k") 'next-line)
    (define-key map (kbd "C-l") 'previous-line)
    (define-key map (kbd "C-;") 'right-char)
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
    (define-key map (kbd "C-S-j")
      (lambda()
	(interactive)
	(setq count 0)
	(while (< count lightning-jump-chars-fast)
	  (left-char)
	  (setq count (+ count 1)))))
    (define-key map (kbd "C-:")
      (lambda()
	(interactive)
	(setq count 0)
	(while (< count lightning-jump-chars-fast)
	  (right-char)
	  (setq count (+ count 1)))))
    (define-key map (kbd "C-S-l")
      (lambda()
	(interactive)
	(setq count 0)
	(while (< count lightning-jump-lines-fast)
	  (previous-line)
	  (setq count (+ count 1)))))
    (define-key map (kbd "C-S-k")
      (lambda()
	(interactive)
	(setq count 0)
	(while (< count lightning-jump-lines-fast)
	  (next-line)
	  (setq count (+ count 1)))))
    ;; Sixth layer: even faster scrolling than in the fifth layer
    (define-key map (kbd "C-M-S-j")
      (lambda()
	(interactive)
	(setq count 0)
	(while (< count lightning-jump-chars-faster)
	  (left-char)
	  (setq count (+ count 1)))))
    (define-key map (kbd "C-M-:")
      (lambda()
	(interactive)
	(setq count 0)
	(while (< count lightning-jump-chars-faster)
	  (right-char)
	  (setq count (+ count 1)))))
    (define-key map (kbd "C-M-S-l")
      (lambda()
	(interactive)
	(setq count 0)
	(while (< count lightning-jump-lines-faster)
	  (previous-line)
	  (setq count (+ count 1)))))
    (define-key map (kbd "C-M-S-k")
      (lambda()
	(interactive)
	(setq count 0)
	(while (< count lightning-jump-lines-faster)
	  (next-line)
	  (setq count (+ count 1)))))
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
    map)
  "lightning-keymap-mode keymap.")
;;; End of basic key bindings and map variable definition.

;;;
;;; Advanced (and default) lightning-keymap layout
;;;
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
	    (kill-region (region-beginning) (region-end))
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
  ;; First layer: Use the `replace-string' function on the current line
  (define-key lightning-keymap-mode-map (kbd "C-\\")
    (lambda()
      (interactive)
      (save-excursion
	(if mark-active (call-interactively 'replace-string)
	  (progn
	    (set-mark (line-end-position))
	    (move-beginning-of-line 1)
	    (call-interactively 'replace-string))))))
  ;; Second layer: Use the `replace-string' function on the whole buffer
  (define-key lightning-keymap-mode-map (kbd "M-\\") 'replace-string)
  ;; Third layer: Using the `iedit' package to change all parts of a
  ;; buffer, matching a previously marked region, simultaneously.
  (define-key lightning-keymap-mode-map (kbd "C-M-\\") 'iedit-mode))
;;; End of non-basic key bindings.

;;;
;;; Mode-specific key bindings
;;;
;; This section contains bindings to deal with modes, which come with
;; an inferior mode providing the user with an interactive shell to
;; manipulate their scripts etc. with. In order to make the most out
;; of these, the `lightning-keymap-mode' provides mode-specific
;; functions for passing code from the major mode's buffer into its
;; inferior counterpart.

;; Those features are not part of the basic version of the keymap.
(unless lightning-basic-keymap
  ;; ESS (Emacs speaks statistics) - a very convenient mode for
  ;; manipulating R files.
  (when (assq 'ess package-alist)
    (add-hook
     'ess-mode-hook
     (lambda()
       (local-set-key (kbd "C-n")
		      'lightning-keymap-ess-evaluation-layer-1)
       (local-set-key (kbd "<M-n>")
		      'lightning-keymap-ess-evaluation-layer-2)
       (local-set-key (kbd "C-M-n")
		      'lightning-keymap-ess-evaluation-layer-3))))
  ;; When manipulating RMarkdown files using `polymode' the export
  ;; function bound to `lightning-keymap-ess-evaluation-layer-3' should
  ;; be working in the markdown part of the document as well.
  (when (assq 'polymode-minor-mode minor-mode-alist)
    (add-hook
     'markdown-mode-hook
     (lambda()
       (local-set-key (kbd "C-M-n")
		      'lightning-keymap-ess-evaluation-layer-3))))

  ;; Python-mode
  ;; Since I don't seem to find the Python package in the `package-alist'
  ;; variable, I will just check whether there is a function called
  ;; 'python-mode'.
  (when (functionp 'python-mode)
    (add-hook
     'python-mode-hook
     (lambda()
       (local-set-key (kbd "C-n")
		      'lightning-keymap-python-evaluation-layer-1)
       (local-set-key (kbd "<M-n>")
		      'lightning-keymap-python-evaluation-layer-2)
       (local-set-key (kbd "C-M-n")
		      'lightning-keymap-python-evaluation-layer-3))))
  ;; End of mode-specific evaluation functions
  )


;; Activating the customized keybindings with every major mode.
(define-minor-mode lightning-keymap-mode
  "Toggle YASnippet mode.

When YASnippet mode is enabled, `yas-expand', normally bound to
the TAB key, expands snippets of code depending on the major
mode.

With no argument, this command toggles the mode.
positive prefix argument turns on the mode.
Negative prefix argument turns off the mode.

Key bindings:
\\{lightning-keymap-mode}"
  :lighter " light"
  :group 'lightning
  :global t
  :keymap lightning-keymap-mode-map

  ;; There are some minor modes interfering with the keymap provided
  ;; by this one. For now only the `flyspell', `yasnippet' and
  ;; `isearch' mode came to my attention. If present, the key bindings
  ;; imposed by those packages have to be shadowed in order to ensure
  ;; `lightning-keymap' will work properly.
  ;;
  ;; Since `flyspell' uses overlays to introduce its key bindings, a
  ;; mere change of order in the load path won't solve the problem.
  ;; For now I see two different routes (both don't work yet)
  ;;  1. Use overriding-local-map to override all minor-mode-maps and
  ;;     overlays. But it's tricky since it also shadows important
  ;;     keys.
  ;;  2. Provide hooks to turn of the overlays and key bindings in all
  ;;     interfering modes. But this is a tedious thing to do and it
  ;;     won't ensure `lightning-keymap-mode' to work properly for
  ;;     other users as well.

  ;; My take on the first option:
  ;;
  ;; Let the package's keymap inherit from all other available maps so
  ;; it will know about their bindings as well. But be careful to not
  ;; let lightning-keymap-mode inherit its own map.
  ;; (set-keymap-parent lightning-keymap-mode-map (current-local-map))
  ;; (setq list-of-minor-mode-maps (current-minor-mode-maps))
  ;; (when list-of-minor-mode-maps
  ;;   (unless (equal lightning-keymap-mode-map
  ;;     (car list-of-minor-mode-maps))
  ;;     (set-keymap-parent lightning-keymap-mode-map
  ;; 			 (car list-of-minor-mode-maps)))) 
  ;; (setq overriding-local-map lightning-keymap-mode-map))
  )

(provide 'lightning-keymap-mode)
;;
;;; End of lightning-keymap-mode.el
