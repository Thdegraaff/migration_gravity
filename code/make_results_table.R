library(tidyverse)
library(texreg)
library(brms)
library(knitr)
library(readxl)
library(rethinking)
library(stringr)

load("./output/m_base.rda")
load("./output/m_base_housing.rda")
load("./output/corop_country.rda")
load("./output/corop_dyad.rda")
load(file = "./data/derived/migration_COROP.Rda")

###################### Help functions

## Compute p-values
twosidep <- function(df){
    nvar <- ncol(df)
    p <- data.frame(matrix(NA, nrow=1, ncol=nvar))
    names(p) <- names(df)
    for(i in 1:nvar){
        data <- arrange_(df, sym(colnames(df)[i]))
        p1<-sum(df[,i]>0,na.rm=TRUE)/sum(!is.na(df[,i])) 
        p2<-sum(df[,i]<0,na.rm=TRUE)/sum(!is.na(df[,i])) 
        p[1,i]<-min(p1,p2)*2  
    }
    return(p)
}

# Extract statistics from posterior probability distributions
ex.results <- function(boot.data){
    results <- data.frame("term" = colnames(boot.data),
                          "estimate" = colMeans(boot.data, na.rm = TRUE),
                          "std.error" = apply(boot.data,2,sd,na.rm=TRUE),
                          "statistic" = colMeans(boot.data, na.rm = TRUE)/apply(boot.data,2,sd,na.rm=TRUE),
                          "p.value" = t(twosidep(boot.data)))
    return(results)
}

# Create main tex-reg objects
extract.tr <- function(results){
    names <- row.names(results)
    names[2] <- "(Intercept)"
    tr <- createTexreg(coef.names = names,
                       coef = results$estimate,
                       se = results$std.error,
                       pvalues = results$p.value,
                       gof.names = c( "PSIS_loo","R2"),
                       gof = c(0,0))
    return(tr)
}

# Append out-of-sample model statistics to tex-reg objects
append.tr <- function(tr.object, psis, r2){
    tr.object@gof[1] <- round(psis,2)
    tr.object@gof[2] <- round(r2,2)
    return(tr.object)
}

######################################## Full models

m_base_a <- extract.samples(m_base)
m_base_a <- as.data.frame(m_base_a)
results.m_1 <- ex.results(m_base_a)

m_base_housing_a <- extract.samples(m_base_housing)
m_base_housing_a <- as.data.frame(m_base_housing_a)
results.m_2 <- ex.results(m_base_housing_a)

m_country_a <- extract.samples(m_country)
m_country_a <- as.data.frame(m_country_a)
m_country_a <- dplyr::select(m_country_a, -starts_with("y."))
m_country_a <- dplyr::select(m_country_a, -starts_with("z1."))
m_country_a <- dplyr::select(m_country_a, -starts_with("gr."))
results.m_3 <- ex.results(m_country_a)

m_dyad_a <- extract.samples(m_dyad)
m_dyad_a <- as.data.frame(m_dyad_a)
m_dyad_a <- dplyr::select(m_dyad_a, -starts_with("y."))
m_dyad_a <- dplyr::select(m_dyad_a, -starts_with("z1."))
m_dyad_a <- dplyr::select(m_dyad_a, -starts_with("z2."))
m_dyad_a <- dplyr::select(m_dyad_a, -starts_with("gr."))
m_dyad_a <- dplyr::select(m_dyad_a, -starts_with("d."))
results.m_4 <- ex.results(m_dyad_a)

tr.m_1 <- extract.tr(results.m_1)
tr.m_2 <- extract.tr(results.m_2)
tr.m_3 <- extract.tr(results.m_3)
tr.m_4 <- extract.tr(results.m_4)

m1_loo <- PSIS(m_base)
mu_base <- link(m_base)
y_hat <- c(colMeans(mu_base$lambdaAB), colMeans(mu_base$lambdaBA) ) 
y <- c(df$mAB, df$mBA) 
y_mean <- mean(c(df$mAB, df$mBa))
ESS <- sum((y_hat - y_mean)^2)
TSS <- sum((y - y_mean)^2)
R2_1 <- ESS/TSS

m2_loo <- PSIS(m_base_housing)
mu_base_housing <- link(m_base_housing)
y_hat <- c(colMeans(mu_base_housing$lambdaAB), colMeans(mu_base_housing$lambdaBA) ) 
y <- c(df$mAB, df$mBA) 
y_mean <- mean(c(df$mAB, df$mBa))
ESS <- sum((y_hat - y_mean)^2)
TSS <- sum((y - y_mean)^2)
R2_2 <- ESS/TSS

m3_loo <- PSIS(m_country)
mu_country <- link(m_country)
y_hat <- c(colMeans(mu_country$lambdaAB), colMeans(mu_country$lambdaBA) ) 
y <- c(df$mAB, df$mBA) 
y_mean <- mean(c(df$mAB, df$mBa))
ESS <- sum((y_hat - y_mean)^2)
TSS <- sum((y - y_mean)^2)
R2_3 <- ESS/TSS

m4_loo <- PSIS(m_dyad)
mu_dyad <- link(m_dyad)
y_hat <- c(colMeans(mu_dyad$lambdaAB), colMeans(mu_dyad$lambdaBA) ) 
y <- c(df$mAB, df$mBA) 
y_mean <- mean(c(df$mAB, df$mBa))
ESS <- sum((y_hat - y_mean)^2)
TSS <- sum((y - y_mean)^2)
R2_4 <- ESS/TSS

tr.m_1 <- append.tr(tr.m_1, m1_loo$PSIS, R2_1)
tr.m_2 <- append.tr(tr.m_2, m2_loo$PSIS, R2_2)
tr.m_3 <- append.tr(tr.m_3, m3_loo$PSIS, R2_3)
tr.m_4 <- append.tr(tr.m_4, m4_loo$PSIS, R2_4)

tex_path <- here::here("output/benchmark_models.tex")

texreg(list(tr.m_1, tr.m_2, tr.m_3, tr.m_4),
       custom.model.names = c("m_1", "m_2", "m_3", "m_4"),
       digits = 3, single.row = FALSE, stars = c(0.01, 0.05, 0.10), 
       dcolumn = TRUE, booktabs = TRUE, longtable = TRUE, use.packages = FALSE,
       file =tex_path)

