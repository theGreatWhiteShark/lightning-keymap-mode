v0.1.1
- Fixing the recognition of **ess-mode**. Previously, the presence of
  ESS was only checked by searching for the corresponding key in
  *package-alist*. This, however, will not work properly if the lisp
  code of the package was include manually and loaded in the .emacs
  file. Now, **functionp** will check for the presence of *ess-mode*,
  which should code with all possible installation scenarios.
- The evaluation in the **lightning-keymap-ess-evaluation-layer-1**
  was changed from *ess-eval-region-or-line-and-step* to
  *ess-eval-region-or-line-visibly-and-step*. This will restrict the
  compatibility to ESS>=18.10.
- The evaluation of chunks in
  **lightning-keymap-ess-evaluation-layer-2** for .Rmd files and
  *polymode* was fixed. Previously, the *ess-eval-chunk* function was
  used. But it is not intended to be used with Rmarkdown but with
  Rnoweb files instead. It was replaced by *polymode-eval-chunk*.
- Instead of marking the whole buffer and executing the region,
  **lightning-keymap-ess-evaluation-layer-3** now calls the
  *ess-eval-buffer* function for .R files.
