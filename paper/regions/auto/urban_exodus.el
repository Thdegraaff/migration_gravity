(TeX-add-style-hook
 "urban_exodus"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("cas-dc" "a4paper" "fleqn")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("natbib" "round")))
   (TeX-run-style-hooks
    "latex2e"
    "cas-dc"
    "cas-dc10"
    "natbib"
    "booktabs"
    "array"
    "microtype")
   (TeX-add-symbols
    "tsc"
    "WriteBookmarks"
    "floatpagepagefraction"
    "textpagefraction")
   (LaTeX-add-labels
    "Introduction"
    "fig:adam_mig"
    "eq:grav"
    "eq:gravfixed"
    "fig:gravity_network"
    "outcome"
    "linear"
    "mund"
    "muno"
    "model"
    "fig:hist_mig_corop"
    "fig:housing_mig")
   (LaTeX-add-bibliographies
    "references"))
 :latex)

