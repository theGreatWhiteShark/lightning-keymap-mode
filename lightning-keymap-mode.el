;;; lightning-keymap-mode.el - A lightning-fast keymap for Emacs.

;; Author: Philipp Müller <thetruephil@googlemail.com>
;; Maintainer: Philipp Müller <thetruephil@googlemail.com>
;; Version: 0.1.0
;; X-URL: https://github.com/theGreatWhiteShark/lightning-keymap-mode
;; URL: https://github.com/theGreatWhiteShark/lightning-keymap-mode
;; Keywords: keymap, navigation
;; Package-Requires: ((iedit 0.97)(rect-mark 1.4))
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
;;  Most important variables;
;;
;;    `lightning-keymap-toggle-key'
;;
;;      A key-sequence specifying a global binding toggling the lightning
;;      keymap. To prevent this minor mode from setting the global
;;      key binding, you can initialize the variable with nil.
;;      Default: "<F5>".
;;
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
(defgroup lightning nil
	"A lightning-fast keymap for Emacs"
	:prefix "lightning-"
	:group 'editing)

(defcustom lightning-keymap-toggle-key "<f5>"
  "Sets a global key binding to toggle the `lightning-keymap-mode'.
The input has to be a string and a  valid argument for the `kbd' 
function.

To prevent this behavior you can set the variable's value to nil."
  :group 'lightning
  :type 'string)

(defcustom lightning-keymap-basic nil
  "If set to non-nil, all bindings will be activated. Setting it to `t' 
instead causes the `lightning-keymap-mode' to only bind the keys used
for navigation (j, k, l, ;) and for newlines (m)."
  :group 'lightning
	:type 'boolean)

;; customs for the evaluation functions in the three different layers
;; custom for activate -bonus: parts, which depend on additional packages
;; custom for jumping length

;; Defining a global key to toggle the lightning-keymap (unless the
;; lightning-keymap-toggle-key wasn't set to nil).
(global-set-key (kbd lightning-keymap-toggle-key)
'lightning-keymap-mode)


;; Customized keybindings for navigation, killing and various other
;; useful things.
;; In order to not consider any special cases and to not get annoyed by
;; minor-mode key maps (especially flyspell), I will define my own
;; minor-mode which enforces my custom key bindings on all the major
;; modes. Whenever I want to switch back to the original key bindings,
;; I just disable this minor-mode.
(defvar lightning-keymap-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Navigation
    (define-key map (kbd "M-S backspace") 'kill-sentence)
    (define-key map (kbd "M-S-j") 'windmove-left)
    (define-key map (kbd "M-J") 'windmove-left)
    (define-key map (kbd "M-S-k") 'windmove-up)
    (define-key map (kbd "M-K") 'windmove-up)
    (define-key map (kbd "M-S-l") 'windmove-down)
    (define-key map (kbd "M-L") 'windmove-down)
    (define-key map (kbd "M-S-;") 'windmove-right)
    (define-key map (kbd "M-:") 'windmove-right)
    (define-key map (kbd "C-j") 'left-char)
    (define-key map (kbd "C-l") 'next-line)
    (define-key map (kbd "C-k") 'previous-line)
    (define-key map (kbd "C-;") 'right-char)
    ;; For faster scrolling
    (define-key map (kbd "C-S-j") (lambda()
				    (interactive)
				    (setq count 0)
				    (while (< count 5)
				      (left-char)
				      (setq count (+ count 1)))))
    (define-key map (kbd "C-:") (lambda()
				    (interactive)
				    (setq count 0)
				    (while (< count 5)
				      (right-char)
				      (setq count (+ count 1)))))
    (define-key map (kbd "C-S-k") (lambda()
				    (interactive)
				    (setq count 0)
				    (while (< count 15)
				      (previous-line)
				      (setq count (+ count 1)))))
    (define-key map (kbd "C-S-l") (lambda()
				    (interactive)
				    (setq count 0)
				    (while (< count 15)
				      (next-line)
				      (setq count (+ count 1)))))
    ;; For even fast scrolling
    (define-key map (kbd "C-M-S-j") (lambda()
				    (interactive)
				    (setq count 0)
				    (while (< count 15)
				      (left-char)
				      (setq count (+ count 1)))))
    (define-key map (kbd "C-M-:") (lambda()
				    (interactive)
				    (setq count 0)
				    (while (< count 15)
				      (right-char)
				      (setq count (+ count 1)))))
    (define-key map (kbd "C-M-S-k") 'backward-page)
    (define-key map (kbd "C-M-S-l") 'forward-page)
    ;; For jumping between words and paragraphs
    (define-key map (kbd "M-j") 'left-word)
    (define-key map (kbd "M-l") 'forward-paragraph)
    (define-key map (kbd "M-k") 'backward-paragraph)
    (define-key map (kbd "M-;") 'right-word)
    ;; For jumping through the whole document
    (define-key map (kbd "C-M-j") 'move-beginning-of-line)
    (define-key map (kbd "C-M-k") 'beginning-of-buffer)
    (define-key map (kbd "C-M-l") 'end-of-buffer)
    (define-key map (kbd "C-M-;") 'move-end-of-line)
    ;; Killing
    (define-key map (kbd "C-,") 'backward-delete-char-untabify)
    (define-key map (kbd "C-.") 'delete-forward-char)
    (define-key map (kbd "M-,") (lambda()
				  (interactive)
				  (if mark-active
				      (kill-region
				       (region-beginning)
				       (region-end))
				    (backward-kill-word 1))))
    (define-key map (kbd "M-.") (lambda()
				  (interactive)
				  (if mark-active
				      (kill-region
				       (region-beginning)
				       (region-end))
				    (kill-word 1))))
    (define-key map (kbd "C-M-.") 'kill-line)
    (define-key map (kbd "C-M-,") 'kill-whole-line)
    ;; Copying (it's not that convenient to have the copying and yanking
    ;; in a row different than the one used for killing. But in order
    ;; to avoid conflicts with the C-n behavior of the terminal I will
    ;; go with this setting for now.
    ;; (copy word, copy line, copy paragraph)
    (define-key map (kbd "C-p") (lambda()
				  (interactive)
				  (left-word)
				  (mark-word)
				  (copy-region-as-kill
				   (region-beginning)
				   (region-end))))
    (define-key map (kbd "M-p") (lambda()
				  (interactive)
				  (if mark-active
				      (copy-region-as-kill
				       (region-beginning)
				       (region-end))
				    (copy-region-as-kill
				     (line-beginning-position)
				     (line-end-position)))))
    (define-key map (kbd "C-M-p") (lambda()
				    (interactive)
				    (backward-paragraph)
				    (mark-paragraph)
				    (copy-region-as-kill
				     (region-beginning)
				     (region-end))))
    ;; Yanking (at point, newline and yank, yank in previous line)
    (define-key map (kbd "C-o") 'yank)
    (define-key map (kbd "M-o") (lambda()
				  (interactive)
				  (move-end-of-line 1)
				  (newline-and-indent)
				  (yank)))
    (define-key map (kbd "C-M-o") 'helm-show-kill-ring)
    ;; Commenting (insert comment, comment line, paragraph)
    (define-key map (kbd "C-'") (lambda()
				  (interactive)
				  (insert comment-start)
				  (insert "  ")
				  (insert comment-end)
				  (left-char
				   (+ (string-width comment-end) 1))))
    (define-key map (kbd "M-'") (lambda()
				  (interactive)
				  (if mark-active
				      (comment-or-uncomment-region
				       (region-beginning)
				       (region-end))
				    (comment-or-uncomment-region
				     (line-beginning-position)
				     (line-end-position)))))

    (define-key map (kbd "C-M-'") (lambda()
				    (interactive)
				    (backward-paragraph)
				    (mark-paragraph)
				    (comment-or-uncomment-region
				     (region-beginning)
				     (region-end))))
    ;; Indenting( line, paragraph, buffer )
    ;; Don't bind this or the TAB won't work anymore
    ;; (define-key map (kbd "C-i") 'indent-region)
    (define-key map (kbd "M-i") (lambda()
				  (interactive)
				  (if mark-active
				      (indent-region
				       (region-beginning)
				       (region-end))
				    (progn
				      (backward-paragraph)
				      (mark-paragraph)
				      (indent-region
				       (region-beginning)
				       (region-end))))))
    (define-key map (kbd "C-M-i") (lambda ()
				    (interactive)
				    (mark-whole-buffer)
				    (indent-region
				     (region-beginning)
				     (region-end))))
    ;; Cycle through buffers.
    (define-key map (kbd "<C-tab>") 'bury-buffer)
    ;; Replacing strings
    (define-key map (kbd "C-\\") 'iedit-mode)
    (define-key map (kbd "M-\\") 'replace-string)
    ;; Rectangle marking, cutting, pasting
    (define-key map (kbd "C-u") 'rm-set-mark)
    (define-key map (kbd "M-u") 'rm-kill-region)
    (define-key map (kbd "C-M-u") 'rm-kill-ring-save)
    ;; Binding something to Ctrl-m causes problems because Emacs does not
    ;; destinguish between Ctrl-m and RET due to historical reasons
    ;; https://emacs.stackexchange.com/questions/20240/how-to-distinguish-c-m-from-return
    (define-key input-decode-map [?\C-\M-M] [C-M-m])
    (define-key input-decode-map [?\M-n] [M-n])
    (define-key map (kbd "M-m") (lambda ()
				  (interactive)
				  (move-end-of-line 1)
				  (newline-and-indent)))
    ;; Including the shift for adding a comment at the beginning
    ;; of the new line.
    (define-key map (kbd "M-M") (lambda ()
				    (interactive)
				    (move-end-of-line 1)
				    (comment-indent-new-line)
				    (self-insert-command 1)))
    (define-key map (kbd "<C-M-m>")
      (lambda ()
	(interactive)
	(if (string= (what-line) "Line 1")
	    (progn
	      (move-beginning-of-line 1)
	      (newline-and-indent)
	      (previous-line))
	  (progn
	    (previous-line)
	    (move-end-of-line 1)
	    (newline-and-indent)))))
    map)
  "lightning-keymap-mode keymap.")

;;; Mode-specific key bindings
;; This section contains bindings for two distinct groups of mode.
;; 1. Modes, which come with an inferior mode providing the user with
;;    an interactive shell to manipulate their scripts etc. with.
;;    In order to make the most out of those modes, the
;;    `lightning-keymap-mode' provides mode-specific functions for
;;    passing code from the major mode's buffer into its inferior
;;    counterpart.
;; 2. There are some minor modes interfering with the keymap provided by
;;    this one. For now only the `yasnippet' mode came to mi attention.
;;    If present, the keybindings imposed by those packages have to be
;;    shadowed in order to ensure `lightning-keymap' will work properly.

;; 1. Mode-specific evaluation functions
(unless lightning-keymap-basic
	;; ESS (Emacs speaks statistics) - a very convenient mode for
	;; manipulating R files.
	(when (assq 'ess package-alist)
		(add-hook
		 'ess-mode-hook
		 (lambda()
			 (local-set-key (kbd "M-j") 'left-word)
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
			 (local-set-key (kbd "M-j") 'left-word)
			 (local-set-key (kbd "C-n")
											'lightning-keymap-python-evaluation-layer-1)
			 (local-set-key (kbd "<M-n>")
											'lightning-keymap-python-evaluation-layer-2)
			 (local-set-key (kbd "C-M-n")
											'lightning-keymap-python-evaluation-layer-3))))
	;; End of 1. Mode-specific evaluation functions
	)

;; 2. Shadowing other keybindings

				 

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
\\{yas-minor-mode-map}"
  :lighter " light"
  :group 'lightning
  :global t
  :keymap lightning-keymap-mode-map)

(provide 'lightning-keymap-mode)
;;
;;; End of lightning-keymap-mode.el
