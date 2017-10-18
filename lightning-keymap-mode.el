;;; thunderbolt-mode.el - A lightning fast keymap provided as an Emacs minor mode.

(defcustom lightning-keymap-toggle-key "<F5>"
  "Sets a custom key to activate and deactivate `lightning-keymap-mode'.
Alternatively you can specify the key yourself using `global-set-key' 
by binding the `lightning-keymap-toggle' function."
  :group 'lightning-keymap
  :type 'string)

(defcustom lightning-keymap-basic nil
  "If set to non-nil all bindings will be activated. Setting it to `t' 
instead causes the `lightning-keymap-mode' to only bind the keys used
for navigation (j, k, l, ;) and for newlines (m)."
  :group 'lightning-keymap
  :type 'boolean)

;; customs for the evaluation functions in the three different layers
;; custom for activate -bonus: parts, which depend on additional packages
;; custom for jumping length

;; easier switchting between buffers (with alt key)
(require 'windmove)
(windmove-default-keybindings 'meta)

;; enable rectangle marking
(require 'rect-mark)

;; Interactive edit to manipulate multiple identical words at the time
(add-to-list 'load-path "~/git/configurations-and-scripts/emacs/iedit")
(require 'iedit)

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

;; Activating the customized keybindings with every major mode.
(define-minor-mode lightning-keymap-mode
  "Enforcing my customized keys on all major modes/"
  :init-value t
  :lighter " lightning-keymap")
(lightning-keymap-mode 1)
