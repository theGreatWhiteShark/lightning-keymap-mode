;;; lightning-keymap-mode-modes.el
;; Contains additional, mode-specific functions used within the
;; lightning-keymap-mode.

;; Author: Philipp Müller <thetruephil@googlemail.com>
;; Maintainer: Philipp Müller <thetruephil@googlemail.com>
;; URL: https://gitlab.com/theGreatWhiteShark/lightning-keymap-mode

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
;;   `lightning-keymap-ess-evaluation-layer-1'
;;
;;     Evaluate the current line or region in the corresponding *iESS*
;;     buffer.
;;
;;     After evaluation the focus remains in the current buffer and
;;     the point is moved to the next line. This feature will only be
;;     supported by ESS>=18.10.
;;     In order to invoke a *iESS* buffer use the \[R] command.
;;
;;   `lightning-keymap-ess-evaluation-layer-2'
;;
;;     Depending on whether a solid R document or a mixture of R and
;;     markdown (using the `polymode') is edited this function will do
;;     different things: 
;;     - R: It evaluates the current paragraphs and moves the point to
;;          the next one. The focus will remain in the current buffer.
;;     - RMarkdown: It evaluates the whole chunk the point (cursor) is
;;          located in. After evaluation the point remains at its
;;          position and the focus remains in the current buffer.
;;     Whether a R or RMarkdown document is edited the function will
;;     determine using `buffer-file-name'.
;;     All evaluations take place in a corresponding *iESS* buffer,
;;     which can be invoked using the \[R] command.
;;
;;   `lightning-keymap-ess-evaluation-layer-3'
;;
;;     Depending on whether a solid R document or a mixture of R and
;;     markdown (using the `polymode') is edited this function will do
;;     different things: 
;;     - R: It evaluates the whole buffer.The focus will remain in the
;;          current buffer and the point at its position.
;;     - RMarkdown: It exports the document into a .html file. Both
;;          the point and the focus remain unchanged.
;;     Whether a R or RMarkdown document is edited the function will
;;     determine using `buffer-file-name'.
;;     All evaluations take place in a corresponding *iESS* buffer,
;;     which can be invoked using the \[R] command.
;;
;;   `lightning-keymap-python-evaluation-layer-1'
;;
;;     Evaluate the current line or region in the corresponding
;;     *ipython* buffer.
;;     After evaluation the focus remains in the current buffer and
;;     the point is moved to the next line or at the end of the marked
;;     region.
;;
;;   `lightning-keymap-python-evaluation-layer-2'
;;
;;     Evaluate the current paragraph in the corresponding *ipython*
;;     buffer. 
;;     After evaluation the focus is switched to the *ipython*
;;     buffer.
;;
;;   `lightning-keymap-python-evaluation-layer-3'
;;
;;     Evaluate the whole document in the corresponding *ipython*
;;     buffer.
;;     After evaluation the focus is switched to the *ipython*
;;     buffer. 
;;
;;  For more information check out the projects GitLab page:
;;  https://gitlab.com/theGreatWhiteShark/lightning-keymap-mode

;;; Custom functions defined in this document
(defvar lightning-keymap-ess-evaluation-layer-1)
(defvar lightning-keymap-ess-evaluation-layer-2)
(defvar lightning-keymap-ess-evaluation-layer-3)
(defvar lightning-keymap-python-evaluation-layer-1)
(defvar lightning-keymap-python-evaluation-layer-2)
(defvar lightning-keymap-python-evaluation-layer-3)

;;; Function definitions
(defun lightning-keymap-ess-evaluation-layer-1 ()
  "Evaluate the current line or region in the corresponding *iESS*
buffer. 

After evaluation the focus remains in the current buffer and the point
is moved to the next line.

In order to invoke a *iESS* buffer use the \[R] command."
  (interactive)
  (ess-eval-region-or-line-visibly-and-step))

(defun lightning-keymap-ess-evaluation-layer-2 ()
  "Depending on whether a solid R document or a mixture of R and
markdown (using the `polymode') is edited this function will do 
different things:

R: It evaluates the current paragraphs and moves the point to the next 
   one. The focus will remain in the current buffer.

RMarkdown: It evaluates the whole chunk the point (cursor) is located
           in. After evaluation the point remains at its position and
           the focus remains in the current buffer.

Whether a R or RMarkdown document is edited the function will determine
using `buffer-file-name'.

All evaluations take place in a corresponding *iESS* buffer, which can
be invoked using the \[R] command."
  (interactive)
  ;; Check whether this is a .R or .Rmd document
  (if (string= (subseq buffer-file-name
		       (- (length buffer-file-name) 2)) ".R")
       ;; It's a .R file. Evaluate the next paragraph.
       (ess-eval-paragraph-and-step 1)
    ;; It's a .Rmd file. Export it to .html
    (polymode-eval-chunk (point))))

(defun lightning-keymap-ess-evaluation-layer-3 ()
  "Depending on whether a solid R document or a mixture of R and
markdown (using the `polymode') is edited this function will do 
different things:

R: It evaluates the whole buffer.The focus will remain in the current
   buffer and the point at its position.

RMarkdown: It exports the document into a .html file. Both the point
           and the focus remain unchanged.

Whether a R or RMarkdown document is edited the function will determine
using `buffer-file-name'.

All evaluations take place in a corresponding *iESS* buffer, which can
be invoked using the \[R] command."
  (interactive)
  ;; Check whether this is a .R or .Rmd document
  (if (string= (subseq buffer-file-name
		       (- (length buffer-file-name) 2))
	       ".R")
      ;; It's a .R file. Evaluate the whole document.
      (ess-eval-buffer 1)
    ;; It's a .Rmd file. Export it to .html.
    ;; Since this requires the `polymode' package and RMarkdown files
    ;; could be edited in the plain ESS mode as well, there will be a 
    ;; sanity check for whether the package is present or not.
    (progn
      (if (assq 'polymode-minor-mode minor-mode-alist)
	  (polymode-export "Rmarkdown" "html")
	(message "The 'polymode' package isn't installed yet!")))))

(defun lightning-keymap-python-evaluation-layer-1 ()
  "Evaluate the current line or region in the corresponding *ipython* 
buffer.

After evaluation the focus remains in the current buffer and the point
is moved to the next line or at the end of the marked region."
  (interactive)
  ;; Check whether a region was selected
  (if (and mark-ring mark-active) 
      (progn (py-execute-region (region-beginning) (region-end))
	     (goto-char (region-end)))
    ;; No region was set. Evaluate the current line instead.
    (progn (py-execute-line) (next-line)))
  nil)
	     
(defun lightning-keymap-python-evaluation-layer-2 ()
  "Evaluate the current paragraph in the corresponding *ipython* buffer.

After evaluation the focus is switched to the *ipython* buffer."
  (interactive)
  (py-execute-paragraph)
  (py-switch-to-shell))
	     
(defun lightning-keymap-python-evaluation-layer-3 ()
  "Evaluate the whole document in the corresponding *ipython* buffer.

After evaluation the focus is switched to the *ipython* buffer."
  (interactive)
  (py-execute-buffer)
  (py-switch-to-shell))

(provide 'lightning-keymap-mode-modes)
;;
;;; End of lightning-keymap-mode-modes.el
