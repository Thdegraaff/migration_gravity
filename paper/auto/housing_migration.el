(TeX-add-style-hook
 "housing_migration"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("SelfArx" "fleqn" "10pt")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("babel" "english") ("biblatex" "sortcites=false" "style=authoryear-comp" "bibencoding=utf8" "natbib=true" "firstinits=true" "maxcitenames=2" "maxbibnames=99" "uniquename=false" "backend=bibtex" "useprefix=true" "backref=false" "doi=false" "isbn=false" "url=false" "dashed=true")))
   (add-to-list 'LaTeX-verbatim-environments-local "lstlisting")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "lstinline")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperref")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "lstinline")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (TeX-run-style-hooks
    "latex2e"
    "SelfArx"
    "SelfArx10"
    "babel"
    "marvosym"
    "epigraph"
    "subfig"
    "listings"
    "biblatex"
    "hyperref")
   (TeX-add-symbols
    "keywordname")
   (LaTeX-add-labels
    "fig:hist_mig"
    "fig:housing_mig"
    "eq:grav"
    "eq:gravfixed"
    "outcome"
    "linear"
    "muno"
    "mund"
    "model"
    "tab:coef"
    "fig:forestplot")
   (LaTeX-add-bibliographies
    "references.bib"))
 :latex)

