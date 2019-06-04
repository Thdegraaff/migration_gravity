library("shinystan")
library("brms")

library(bayesplot)
library(ggthemes)
color_scheme_set("orange")

load(file = "./output/m_nb.Rda")

forestplot <- stanplot(m2_neg)

pdf(file = "./fig/forestplot.pdf", width = 4, height = 3)
forestplot
dev.off()
