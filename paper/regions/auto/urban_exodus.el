(TeX-add-style-hook
 "urban_exodus"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("scrartcl" "11pt" "parskip" "abstracton" "notitlepage" "dvipsnames")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("appendix" "titletoc") ("footmisc" "hang" "flushmargin") ("caption" "font=footnotesize" "labelfont=bf") ("geometry" "a4paper") ("fontenc" "T1") ("csquotes" "style=british") ("babel" "dutch" "british") ("biblatex" "style=authoryear" "backend=biber" "natbib=true" "giveninits=true" "uniquename=init" "doi=false" "isbn=false" "url=false" "maxnames=2" "maxcitenames=2" "maxbibnames=10" "dashed=true" "useprefix=true") ("hyperref" "pdftex" "colorlinks=true" "linkcolor=BlueViolet" "citecolor=BlueViolet" "urlcolor=BlueViolet" "pdfstartview=FitH")))
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
    "scrartcl"
    "scrartcl11"
    "amssymb"
    "amsmath"
    "amsthm"
    "graphics"
    "graphicx"
    "longtable"
    "setspace"
    "makeidx"
    "marvosym"
    "microtype"
    "booktabs"
    "tabularx"
    "authblk"
    "lipsum"
    "siunitx"
    "lmodern"
    "makecell"
    "rotating"
    "pdflscape"
    "fullpage"
    "caption"
    "subcaption"
    "gensymb"
    "multirow"
    ""
    "tikz"
    "appendix"
    "footmisc"
    "geometry"
    "fontenc"
    "charter"
    "csquotes"
    "float"
    "babel"
    "dcolumn"
    "arydshln"
    "biblatex"
    "hyperref")
   (TeX-add-symbols
    "numberthis")
   (LaTeX-add-labels
    "Introduction"
    "fig:adam_mig"
    "eq:grav"
    "eq:gravfixed"
    "fig:gravity_network"
    "outcome"
    "model"
    "eq:prefmodel"
    "eq:randomregion"
    "fig:hist_mig_corop"
    "fig:housing_mig"
    "fig:housing_types"
    "eq:m1"
    "eq:m2"
    "eq:m3"
    "eq:m4"
    "table:results")
   (LaTeX-add-bibliographies
    "references")
   (LaTeX-add-array-newcolumntypes
    "d"))
 :latex)

