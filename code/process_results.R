library("shinystan")
library("brms")

load(file = "./output/m_nb.Rda")

forestplot <- stanplot(m2_neg)

pdf(file = "./fig/forestplot.pdf", width = 4, height = 3)
forestplot
dev.off()
