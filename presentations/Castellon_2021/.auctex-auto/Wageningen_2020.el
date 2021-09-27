(TeX-add-style-hook
 "Wageningen_2020"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("biblatex" "sortcites=false" "style=authoryear-comp" "bibencoding=utf8" "natbib=true" "firstinits=true" "maxcitenames=2" "maxbibnames=99" "uniquename=false" "backend=bibtex" "useprefix=true" "backref=false" "doi=false" "isbn=false" "url=false" "dashed=true")))
   (add-to-list 'LaTeX-verbatim-environments-local "semiverbatim")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperref")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "href")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (TeX-run-style-hooks
    "latex2e"
    "beamer"
    "beamer10"
    "pgfplots"
    "appendixnumberbeamer"
    "booktabs"
    "amsmath"
    "array"
    "pifont"
    "biblatex")
   (LaTeX-add-labels
    "eq:gravfixed")
   (LaTeX-add-bibliographies
    "../../paper/regions/references.bib"))
 :latex)

