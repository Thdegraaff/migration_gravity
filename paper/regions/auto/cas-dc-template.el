(TeX-add-style-hook
 "cas-dc-template"
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
    "Introduction")
   (LaTeX-add-bibliographies
    "references"))
 :latex)

