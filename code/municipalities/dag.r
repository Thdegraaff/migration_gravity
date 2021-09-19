library(dagitty)
library(rethinking) 
library(ggdag)

theme_set(theme_dag())

hhsize_dag <- dagify(migration ~ hhsize + housing,
                         housing ~ hhsize,
                         labels = c("migration" = "Migration", 
                                     "housing" = "Housing\n structure",
                                     "hhsize" = "Household\n size"),
                         exposure = "housing",
                         outcome = "migration")

ggdag(hhsize_dag, text = FALSE, use_labels = "label") + theme_dag_blank()

ggdag_paths(hhsize_dag, text = FALSE, use_labels = "label", shadow = TRUE)

ggdag_adjustment_set(hhsize_dag, text = FALSE, use_labels = "label", shadow = TRUE)
