

;; C-n is reserved for evaluating a region and thus has to assigned
;; for each mode independently
(add-hook 'ess-mode-hook
	  (lambda()
	    (local-set-key (kbd "M-j") 'left-word)
	    (local-set-key (kbd "C-n")
			   'ess-eval-region-or-line-and-step)
	    (local-set-key (kbd "<M-n>")
			   (lambda()
			     (interactive)
			     ;; Check whether this is a .R or .Rmd document
			     (if (string= (subseq buffer-file-name
						  (- (length buffer-file-name) 2))
					  ".R")
				 (progn
				   ;; It's a .R file. Evaluate the whole document.
				   (ess-eval-paragraph-and-step))
			       ;; It's a .Rmd file. Export it to .html
				   (ess-eval-chunk 1))))
	    ;; compile the whole file
	    (local-set-key (kbd "C-M-n")
			   (lambda()
			     (interactive)
			     ;; Check whether this is a .R or .Rmd document
			     (if (string= (subseq buffer-file-name
						  (- (length buffer-file-name) 2))
					  ".R")
				 ;; It's a .R file. Evaluate the whole document.
				 (progn
				   (mark-whole-buffer)
				   (ess-eval-region
				    (region-beginning)
				    (region-end)
				    nil))
			       ;; It's a .Rmd file. Export it to .html
			       (polymode-export "Rmarkdown" "html"))))))


;; In order to work in the markdown part of ESS as well
(add-hook 'markdown-mode-hook
	  (lambda()
	    (local-set-key (kbd "C-M-n")
			   (lambda()
			     (interactive)
			     ;; Check whether this is a .R or .Rmd document
			     (if (string= (subseq buffer-file-name
						  (- (length buffer-file-name) 2))
					  ".R")
				 ;; It's a .R file. Evaluate the whole document.
				 (progn
				   (mark-whole-buffer)
				   (ess-eval-region
				    (region-beginning)
				    (region-end)
				    nil))
			       ;; It's a .Rmd file. Export it to .html
			       (polymode-export "Rmarkdown" "html"))))))

