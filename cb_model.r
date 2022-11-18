# author: "Jonathan Richir"
# date: "19 April 2021"


#Rscript

###############################
##  ##
###############################

#####Packages : 
#               
#               
library(magrittr)
#####Load arguments

args <- commandArgs(trailingOnly = TRUE)

#####Import data

if (length(args) < 1) {
    stop("This tool needs at least 1 argument")
}else {
    input_qecbnato0 <- args[1]

}
## SCRIPT for community analysis

#I shorten the name for clarity purpose
qecbNato0 <- readRDS(input_qecbnato0) # NB: in the file name 'NA' becomes 'Na'; didn't want to change it all in the script ...

# change Year & Numero.Quadrat variable format for modeling purpose first (Year not needed in community fct)

qecbNato0$Numero.Quadrat <- as.factor(qecbNato0$Numero.Quadrat)

qecbNato0$Year <- as.factor(qecbNato0$Year)

qecbNato0[, c("geomorphologie.gp")] <- as.factor(qecbNato0[, c("geomorphologie.gp")])

qecbNato0[, c("roche.gp")] <- as.factor(qecbNato0[, c("roche.gp")])

qecbNato0[, c("taille.bloc.gp")] <- as.factor(qecbNato0[, c("taille.bloc.gp")])

qecbNato0[, c("accessibilite.gp")] <- as.factor(qecbNato0[, c("accessibilite.gp")])

qecbNato0[, c("frequentation.gp")] <- as.factor(qecbNato0[, c("frequentation.gp")])

# create OrderNorm dependant variables

qecbNato0 <- tibble::add_column(qecbNato0, BM.BF_FS_dist.tr = bestNormalize::orderNorm(qecbNato0$BM.BF_FS_dist.)$x.t, .after = "BM.BF_FS_dist.")

qecbNato0 <- tibble::add_column(qecbNato0, BM_FS.FI_dist.tr = bestNormalize::orderNorm(qecbNato0$BM_FS.FI_dist.)$x.t, .after = "BM_FS.FI_dist.")

qecbNato0 <- tibble::add_column(qecbNato0, blocs.retournes.fr.tr = bestNormalize:::orderNorm(qecbNato0$blocs.retournes.fr.)$x.t, .after = "blocs.retournes.fr.")

qecbNato0 <- tibble::add_column(qecbNato0, richness.tr = bestNormalize:::center_scale(qecbNato0$richness)$x.t, .after = "richness")

qecbNato0 <- tibble::add_column(qecbNato0, Shannon.tr = bestNormalize::orderNorm(qecbNato0$Shannon)$x.t, .after = "Shannon")

# scale independant variables of interest for modeling

qecbNato0 <- tibble::add_column(qecbNato0, Sc.Period.nb = scale(qecbNato0$Period.nb), .after = "Period.nb")
qecbNato0 <- tibble::add_column(qecbNato0, Sc.blocs.retournes.fr. = scale(qecbNato0$blocs.retournes.fr.), .after = "blocs.retournes.fr.")
qecbNato0 <- tibble::add_column(qecbNato0, Sc.oceano.max.3m = scale(qecbNato0$oceano.max.3m), .after = "oceano.max.3m")
qecbNato0 <- tibble::add_column(qecbNato0, Sc.freq.max.3m = scale(qecbNato0$freq.max.3m), .after = "freq.max.3m")
qecbNato0 <- tibble::add_column(qecbNato0, Sc.max.ratio.norm15min.obs.pre3m = scale(qecbNato0$max.ratio.norm15min.obs.pre3m), .after = "max.ratio.norm15min.obs.pre3m")
qecbNato0 <- tibble::add_column(qecbNato0, Sc.geomorphologie.gp.nb = scale(qecbNato0$geomorphologie.gp.nb), .after = "geomorphologie.gp.nb")
qecbNato0 <- tibble::add_column(qecbNato0, Sc.accessibilite.gp.nb = scale(qecbNato0$accessibilite.gp.nb), .after = "accessibilite.gp.nb")
qecbNato0 <- tibble::add_column(qecbNato0, Sc.roche.gp.nb = scale(qecbNato0$roche.gp.nb), .after = "roche.gp.nb")
qecbNato0 <- tibble::add_column(qecbNato0, Sc.taille.bloc.gp.nb = scale(qecbNato0$taille.bloc.gp.nb), .after = "taille.bloc.gp.nb")
qecbNato0 <- tibble::add_column(qecbNato0, Sc.frequentation.gp.nb = scale(qecbNato0$frequentation.gp.nb), .after = "frequentation.gp.nb")
qecbNato0 <- as.data.frame(qecbNato0)


## vectors of explanatory variable names and their interaction in models

# models for qecb

model.short = c("Sc.blocs.retournes.fr.", 
                "Sc.Period.nb"     
                , "Sc.freq.max.3m" 
                , "Sc.max.ratio.norm15min.obs.pre3m" 
                , "Sc.oceano.max.3m"
                #, "Sc.roche.gp.nb" 
                #, "Sc.geomorphologie.gp.nb" 
                , "Sc.taille.bloc.gp.nb" 
                , "Sc.accessibilite.gp.nb" 
                , "Sc.blocs.retournes.fr.:Sc.taille.bloc.gp.nb"
                , "Sc.oceano.max.3m:Sc.taille.bloc.gp.nb" 
                , "Sc.freq.max.3m:Sc.accessibilite.gp.nb"
                , "Sc.max.ratio.norm15min.obs.pre3m:Sc.taille.bloc.gp.nb") # both quant. freq & cptmt

model.medium = c("Sc.blocs.retournes.fr.", 
                "Sc.Period.nb"     
                , "Sc.freq.max.3m" 
                , "Sc.oceano.max.3m"
                #, "Sc.roche.gp.nb" 
                #, "Sc.geomorphologie.gp.nb" 
                , "Sc.taille.bloc.gp.nb" 
                , "Sc.accessibilite.gp.nb" 
                , "Sc.blocs.retournes.fr.:Sc.taille.bloc.gp.nb"
                , "Sc.oceano.max.3m:Sc.taille.bloc.gp.nb" 
                , "Sc.freq.max.3m:Sc.accessibilite.gp.nb"
                , "Sc.freq.max.3m:Sc.taille.bloc.gp.nb") # only quant. freq ; and Sc.max.ratio.norm15min.obs.pre3m:Sc.taille.bloc.gp.nb therefore replace by Sc.freq.max.3m:Sc.taille.bloc.gp.nb

model.large = c("Sc.blocs.retournes.fr.", 
                "Sc.Period.nb" 
                , "Sc.frequentation.gp.nb"
                , "Sc.oceano.max.3m"
                #, "Sc.roche.gp.nb" 
                #, "Sc.geomorphologie.gp.nb" 
                , "Sc.taille.bloc.gp.nb" 
                , "Sc.accessibilite.gp.nb" 
                , "Sc.blocs.retournes.fr.:Sc.taille.bloc.gp.nb"
                , "Sc.oceano.max.3m:Sc.taille.bloc.gp.nb"
                , "Sc.frequentation.gp.nb:Sc.accessibilite.gp.nb"
                , "Sc.frequentation.gp.nb:Sc.taille.bloc.gp.nb") # qual. freq

# models for ivr

model.short.ivr = c(#"Sc.blocs.retournes.fr.", 
                "Sc.Period.nb"     
                , "Sc.freq.max.3m" 
                , "Sc.max.ratio.norm15min.obs.pre3m" 
                , "Sc.oceano.max.3m"
                #, "Sc.roche.gp.nb" 
                #, "Sc.geomorphologie.gp.nb" 
                , "Sc.taille.bloc.gp.nb" 
                , "Sc.accessibilite.gp.nb" 
                #, "Sc.blocs.retournes.fr.:Sc.taille.bloc.gp.nb"
                , "Sc.oceano.max.3m:Sc.taille.bloc.gp.nb" 
                , "Sc.freq.max.3m:Sc.accessibilite.gp.nb"
                , "Sc.max.ratio.norm15min.obs.pre3m:Sc.taille.bloc.gp.nb") # both quant. freq & cptmt

model.medium.ivr = c(#"Sc.blocs.retournes.fr.", 
                 "Sc.Period.nb"     
                 , "Sc.freq.max.3m" 
                 , "Sc.oceano.max.3m"
                 #, "Sc.roche.gp.nb" 
                 #, "Sc.geomorphologie.gp.nb" 
                 , "Sc.taille.bloc.gp.nb" 
                 , "Sc.accessibilite.gp.nb" 
                 #, "Sc.blocs.retournes.fr.:Sc.taille.bloc.gp.nb"
                 , "Sc.oceano.max.3m:Sc.taille.bloc.gp.nb" 
                 , "Sc.freq.max.3m:Sc.accessibilite.gp.nb"
                 , "Sc.freq.max.3m:Sc.taille.bloc.gp.nb") # only quant. freq ; and Sc.max.ratio.norm15min.obs.pre3m:Sc.taille.bloc.gp.nb therefore replace by Sc.freq.max.3m:Sc.taille.bloc.gp.nb

model.large.ivr = c(#"Sc.blocs.retournes.fr.", 
                "Sc.Period.nb" 
                , "Sc.frequentation.gp.nb"
                , "Sc.oceano.max.3m"
                #, "Sc.roche.gp.nb" 
                #, "Sc.geomorphologie.gp.nb" 
                , "Sc.taille.bloc.gp.nb" 
                , "Sc.accessibilite.gp.nb" 
                #, "Sc.blocs.retournes.fr.:Sc.taille.bloc.gp.nb"
                , "Sc.oceano.max.3m:Sc.taille.bloc.gp.nb"
                , "Sc.frequentation.gp.nb:Sc.accessibilite.gp.nb"
                , "Sc.frequentation.gp.nb:Sc.taille.bloc.gp.nb") 

# qual. freq


## vectors of variable names

# variables for qecb

vars.short <- c("Site", "Numero.Quadrat", "Sc.Period.nb", 
                #"Sc.geomorphologie.gp.nb", "Sc.roche.gp.nb", 
                "Sc.taille.bloc.gp.nb", 
                "Sc.blocs.retournes.fr.", 
                "Sc.oceano.max.3m", "Sc.accessibilite.gp.nb", "Sc.freq.max.3m", "Sc.max.ratio.norm15min.obs.pre3m", "Latitude", "Longitude")

vars.medium <- c("Site", "Numero.Quadrat", "Sc.Period.nb", 
                #"Sc.geomorphologie.gp.nb", "Sc.roche.gp.nb", 
                "Sc.taille.bloc.gp.nb", 
                "Sc.blocs.retournes.fr.", 
                "Sc.oceano.max.3m", "Sc.accessibilite.gp.nb", "Sc.freq.max.3m", "Latitude", "Longitude")

vars.large <- c("Site", "Numero.Quadrat", "Sc.Period.nb", 
                #"Sc.geomorphologie.gp.nb", "Sc.roche.gp.nb", 
                "Sc.taille.bloc.gp.nb", 
                "Sc.blocs.retournes.fr.", 
                "Sc.oceano.max.3m", "Sc.accessibilite.gp.nb", "Sc.frequentation.gp.nb", "Latitude", "Longitude")

# variables for ivr

vars.short.ivr <- c("Site", "Numero.Quadrat", "Sc.Period.nb", 
                #"Sc.geomorphologie.gp.nb", "Sc.roche.gp.nb", 
                "Sc.taille.bloc.gp.nb", 
                #"Sc.blocs.retournes.fr.", 
                "Sc.oceano.max.3m", "Sc.accessibilite.gp.nb", "Sc.freq.max.3m", "Sc.max.ratio.norm15min.obs.pre3m", "Latitude", "Longitude")

vars.medium.ivr <- c("Site", "Numero.Quadrat", "Sc.Period.nb", 
                 #"Sc.geomorphologie.gp.nb", "Sc.roche.gp.nb", 
                 "Sc.taille.bloc.gp.nb", 
                 "Sc.blocs.retournes.fr.", 
                 "Sc.oceano.max.3m", "Sc.accessibilite.gp.nb", "Sc.freq.max.3m", "Latitude", "Longitude")

vars.large.ivr <- c("Site", "Numero.Quadrat", "Sc.Period.nb", 
                #"Sc.geomorphologie.gp.nb", "Sc.roche.gp.nb", 
                "Sc.taille.bloc.gp.nb", 
                "Sc.blocs.retournes.fr.", 
                "Sc.oceano.max.3m", "Sc.accessibilite.gp.nb", "Sc.frequentation.gp.nb", "Latitude", "Longitude")


## reduced df. for subsequent analysis & modeling

df. <- qecbNato0[, c("Region", "Site", "Date", "Numero.Quadrat", "Type.Bloc", "Numéro.Bloc.échantillon", "Face", 
                     "BM.BF_FS_dist.tr", "BM_FS.FI_dist.tr",
                     "richness.tr", "Shannon.tr",
                     "blocs.retournes.fr.tr",
                     "Sc.Period.nb", 
                     #"Sc.geomorphologie.gp.nb", "Sc.roche.gp.nb",
                     "Sc.taille.bloc.gp.nb",
                     "Sc.blocs.retournes.fr.", 
                     "Sc.oceano.max.3m", "Sc.accessibilite.gp.nb", "Sc.frequentation.gp.nb", "Sc.freq.max.3m", "Sc.max.ratio.norm15min.obs.pre3m", "Latitude", "Longitude")]


## Correlation and colinearity analysis prior modeling using rcorr() and vif()


## correlation analysis

library("Hmisc")
library("PerformanceAnalytics")

# cor and flattenCorrMatrix => from cor matrix to df.
# cormat : matrix of the correlation coefficients
# pmat : matrix of the correlation p-values
# nmat : matrix of the correlation n-values

flattenCorrMatrix <- function(cormat, pmat, nmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut],
    n = nmat[ut]
  )
}

df.cor.full <- (df.[, c(#"BM_FS.FI_dist.tr", "BM.BF_FS_dist.tr", "richness.tr",  "Simpson.tr", "Shannon.tr", "blocs.retournes.fr.tr", 
  "Sc.blocs.retournes.fr.", "Sc.Period.nb", "Sc.taille.bloc.gp.nb", "Sc.accessibilite.gp.nb", "Sc.frequentation.gp.nb", "Sc.freq.max.3m", "Sc.max.ratio.norm15min.obs.pre3m", "Sc.oceano.max.3m")])

cor. <- rcorr(as.matrix(df.cor.full), type = "spearman")
cormat.spea. <- flattenCorrMatrix(round(cor.$r, digits = 3), round(cor.$P, digits = 4), cor.$n)
colnames(cormat.spea.)[3:5] <- c("r.spea.full", "p.spea.full", "n.spea.full")
cormat.full <- cormat.spea.#[!grepl(".tr", cormat.spea.$row),]
rm(cor., cormat.spea.)
chart.Correlation(df.cor.full, histogram = TRUE, pch = 19, method = 'spearman')

df.cor.large <- na.omit(df.[, vars.large[grepl("Sc.", vars.large)]])
cor. <- rcorr(as.matrix(df.cor.large), type = "spearman")
cormat.spea. <- flattenCorrMatrix(round(cor.$r, digits = 3), round(cor.$P, digits = 4), cor.$n)
colnames(cormat.spea.)[3:5] <- c("r.spea.large", "p.spea.large", "n.spea.large")
cormat.large <- cormat.spea.#[!grepl(".tr", cormat.full.spea.$row),]
rm(cor., cormat.spea.)
chart.Correlation(df.cor.large, histogram = TRUE, pch = 19, method = 'spearman')

df.cor.medium <- na.omit(df.[, vars.medium[grepl("Sc.", vars.medium)]])
cor. <- rcorr(as.matrix(df.cor.medium), type = "spearman")
cormat.spea. <- flattenCorrMatrix(round(cor.$r, digits = 3), round(cor.$P, digits = 4), cor.$n)
colnames(cormat.spea.)[3:5] <- c("r.spea.medium", "p.spea.medium", "n.spea.medium")
cormat.medium <- cormat.spea.#[!grepl(".tr", cormat.full.spea.$row),]
rm(cor., cormat.spea.)
chart.Correlation(df.cor.medium, histogram = TRUE, pch = 19, method = 'spearman')

df.cor.short <- na.omit(df.[, vars.short[grepl("Sc.", vars.short)]])
cor. <- rcorr(as.matrix(df.cor.short), type = "spearman")
cormat.spea. <- flattenCorrMatrix(round(cor.$r, digits = 3), round(cor.$P, digits = 4), cor.$n)
colnames(cormat.spea.)[3:5] <- c("r.spea.short", "p.spea.short", "n.spea.short")
cormat.short <- cormat.spea.#[!grepl(".tr", cormat.full.spea.$row),]
rm(cor., cormat.spea.)
#png("chart.png")
chart.Correlation(df.cor.short, histogram = TRUE, pch = 19, method = 'spearman')

cormat. <- dplyr::full_join(cormat.full, cormat.large, by = c("row", "column"))
cormat. <- dplyr::full_join(cormat., cormat.medium, by = c("row", "column"))
cormat. <- dplyr::full_join(cormat., cormat.short, by = c("row", "column"))

cormat. <- cormat.[
  with(cormat., order(abs(cormat.$r.spea.full), decreasing = T)),
]

saveRDS(cormat., "cor.matrix.RDS")

rm(df.cor.full, df.cor.large, df.cor.medium, df.cor.short)
rm(cormat.full, cormat.large, cormat.medium, cormat.short)


## colinearity analysis

col.fct.glmm <- function(mod.var., model., length.) {
  
  frmla. <- as.formula(paste(mod.var., paste(c(model., "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  glmm.gaus <- suppressWarnings(glmmTMB::glmmTMB(frmla., family = gaussian(link = "identity"), data = df.red)) 
  show(summary(glmm.gaus))
  
  vif_values <- as.data.frame(performance::multicollinearity(glmm.gaus)[, c("Term", "VIF")])
  vif_values.matrix <- t(as.matrix(vif_values[,"VIF"]))
  #par(mar = c(10, 4, 2, 2) + 0.1)
  #barplot(vif_values.matrix, main = "VIF Values", names.arg = vif_values$Term, horiz = F, col = "steelblue", las = 2) #create horizontal bar chart to display each VIF value
  #abline(h = 5, lwd = 3, lty = 2) #add horizontal line at 5 as after 5 there is severe correlation
  colnames(vif_values)[1] <- "var."
  colnames(vif_values)[2] <- paste0("vif", "-", mod.var., "-", length.)
  vif_values <<- vif_values
  
}

vif.df <- function(df.) {
  
  vif_t. <- as.data.frame(t(df.))
  colnames(vif_t.) <-  vif_t.[1,]
  vif_t. <- vif_t.[-1,]
  vif_t. <- tibble::add_column(vif_t., "dep.var._model" = NA, .before =  names(vif_t.[1]))
  vif_t.[, "dep.var._model"] <- rownames(vif_t.)
  rownames(vif_t.) <- NULL
  vif_t. <<- vif_t.
  
}

# vif function applied

{ 

# vif for modeled variable blocs.retournes.fr.tr

df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile")[, c("blocs.retournes.fr.tr", vars.large.ivr)])
col.fct.glmm(mod.var. = "blocs.retournes.fr.tr", model. = model.large.ivr, length. = "large")
vif_large_blocs.retournes.fr.tr <- vif_values
rm(vif_values)

df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile")[, c("blocs.retournes.fr.tr", vars.medium.ivr)])
col.fct.glmm(mod.var. = "blocs.retournes.fr.tr", model. = model.medium.ivr, length. = "medium")
vif_medium_blocs.retournes.fr.tr <- vif_values
rm(vif_values)

df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile")[, c("blocs.retournes.fr.tr", vars.short.ivr)])
col.fct.glmm(mod.var. = "blocs.retournes.fr.tr", model. = model.short.ivr, length. = "short")
vif_short_blocs.retournes.fr.tr <- vif_values
rm(vif_values)

vif_blocs.retournes.fr.tr <- dplyr::full_join(vif_large_blocs.retournes.fr.tr, vif_medium_blocs.retournes.fr.tr)
vif_blocs.retournes.fr.tr <- dplyr::full_join(vif_blocs.retournes.fr.tr, vif_short_blocs.retournes.fr.tr)
vif.df(df. = vif_blocs.retournes.fr.tr)
vif_blocs.retournes.fr.tr <- vif_t. ; rm(vif_t.)
rm(vif_large_blocs.retournes.fr.tr, vif_medium_blocs.retournes.fr.tr, vif_short_blocs.retournes.fr.tr)

# vif for modeled variable BM_FS.FI_dist.tr

df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile")[, c("BM_FS.FI_dist.tr", vars.large)])
col.fct.glmm(mod.var. = "BM_FS.FI_dist.tr", model. = model.large, length. = "large")
vif_large_BM_FS.FI_dist.tr <- vif_values
rm(vif_values)

df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile")[, c("BM_FS.FI_dist.tr", vars.medium)])
col.fct.glmm(mod.var. = "BM_FS.FI_dist.tr", model. = model.medium, length. = "medium")
vif_medium_BM_FS.FI_dist.tr <- vif_values
rm(vif_values)

df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile")[, c("BM_FS.FI_dist.tr", vars.short)])
col.fct.glmm(mod.var. = "BM_FS.FI_dist.tr", model. = model.short, length. = "short")
vif_short_BM_FS.FI_dist.tr <- vif_values
rm(vif_values)

vif_BM_FS.FI_dist.tr <- dplyr::full_join(vif_large_BM_FS.FI_dist.tr, vif_medium_BM_FS.FI_dist.tr)
vif_BM_FS.FI_dist.tr <- dplyr::full_join(vif_BM_FS.FI_dist.tr, vif_short_BM_FS.FI_dist.tr)
vif.df(df. = vif_BM_FS.FI_dist.tr)
vif_BM_FS.FI_dist.tr <- vif_t. ; rm(vif_t.)
rm(vif_large_BM_FS.FI_dist.tr, vif_medium_BM_FS.FI_dist.tr, vif_short_BM_FS.FI_dist.tr)

# vif for modeled variable BM.BF_FS_dist.tr 

df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile")[, c("BM.BF_FS_dist.tr", vars.large)])
col.fct.glmm(mod.var. = "BM.BF_FS_dist.tr", model. = model.large, length. = "large")
vif_large_BM.BF_FS_dist.tr <- vif_values
rm(vif_values)

df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile")[, c("BM.BF_FS_dist.tr", vars.medium)])
col.fct.glmm(mod.var. = "BM.BF_FS_dist.tr", model. = model.medium, length. = "medium")
vif_medium_BM.BF_FS_dist.tr <- vif_values
rm(vif_values)

df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile")[, c("BM.BF_FS_dist.tr", vars.short)])
col.fct.glmm(mod.var. = "BM.BF_FS_dist.tr", model. = model.short, length. = "short")
vif_short_BM.BF_FS_dist.tr <- vif_values
rm(vif_values)

vif_BM.BF_FS_dist.tr <- dplyr::full_join(vif_large_BM.BF_FS_dist.tr, vif_medium_BM.BF_FS_dist.tr)
vif_BM.BF_FS_dist.tr <- dplyr::full_join(vif_BM.BF_FS_dist.tr, vif_short_BM.BF_FS_dist.tr)
vif.df(df. = vif_BM.BF_FS_dist.tr)
vif_BM.BF_FS_dist.tr <- vif_t. ; rm(vif_t.)
rm(vif_large_BM.BF_FS_dist.tr, vif_medium_BM.BF_FS_dist.tr, vif_short_BM.BF_FS_dist.tr)

# vif for modeled variable richness.tr face supérieure bloc mobile

df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Face == "face supérieure")[, c("richness.tr", vars.large)])
col.fct.glmm(mod.var. = "richness.tr", model. = model.large, length. = "large")
vif_large_richness.tr <- vif_values
rm(vif_values)

df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Face == "face supérieure")[, c("richness.tr", vars.medium)])
col.fct.glmm(mod.var. = "richness.tr", model. = model.medium, length. = "medium")
vif_medium_richness.tr <- vif_values
rm(vif_values)

df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Face == "face supérieure")[, c("richness.tr", vars.short)])
col.fct.glmm(mod.var. = "richness.tr", model. = model.short, length. = "short")
vif_short_richness.tr <- vif_values
rm(vif_values)

vif_richness_BM_FS.tr <- dplyr::full_join(vif_large_richness.tr, vif_medium_richness.tr)
vif_richness_BM_FS.tr <- dplyr::full_join(vif_richness_BM_FS.tr, vif_short_richness.tr)
vif.df(df. = vif_richness_BM_FS.tr)
vif_richness_BM_FS.tr <- vif_t. ; rm(vif_t.)
rm(vif_large_richness.tr, vif_medium_richness.tr, vif_short_richness.tr)

# vif for modeled variable richness.tr face inférieure bloc mobile

df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Face == "face inférieure")[, c("richness.tr", vars.large)])
col.fct.glmm(mod.var. = "richness.tr", model. = model.large, length. = "large")
vif_large_richness.tr <- vif_values
rm(vif_values)

df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Face == "face inférieure")[, c("richness.tr", vars.medium)])
col.fct.glmm(mod.var. = "richness.tr", model. = model.medium, length. = "medium")
vif_medium_richness.tr <- vif_values
rm(vif_values)

df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Face == "face inférieure")[, c("richness.tr", vars.short)])
col.fct.glmm(mod.var. = "richness.tr", model. = model.short, length. = "short")
vif_short_richness.tr <- vif_values
rm(vif_values)

vif_richness_BM_FI.tr <- dplyr::full_join(vif_large_richness.tr, vif_medium_richness.tr)
vif_richness_BM_FI.tr <- dplyr::full_join(vif_richness_BM_FI.tr, vif_short_richness.tr)
vif.df(df. = vif_richness_BM_FI.tr)
vif_richness_BM_FI.tr <- vif_t. ; rm(vif_t.)
rm(vif_large_richness.tr, vif_medium_richness.tr, vif_short_richness.tr)

# vif for modeled variable richness.tr face supérieure bloc fixé

df.red <- na.omit(dplyr::filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Face == "face supérieure")[, c("richness.tr", vars.large)])
col.fct.glmm(mod.var. = "richness.tr", model. = model.large, length. = "large")
vif_large_richness.tr <- vif_values
rm(vif_values)

df.red <- na.omit(dplyr::filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Face == "face supérieure")[, c("richness.tr", vars.medium)])
col.fct.glmm(mod.var. = "richness.tr", model. = model.medium, length. = "medium")
vif_medium_richness.tr <- vif_values
rm(vif_values)

df.red <- na.omit(dplyr::filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Face == "face supérieure")[, c("richness.tr", vars.short)])
col.fct.glmm(mod.var. = "richness.tr", model. = model.short, length. = "short")
vif_short_richness.tr <- vif_values
rm(vif_values)

vif_richness_BF_FS.tr <- dplyr::full_join(vif_large_richness.tr, vif_medium_richness.tr)
vif_richness_BF_FS.tr <- dplyr::full_join(vif_richness_BF_FS.tr, vif_short_richness.tr)
vif.df(df. = vif_richness_BF_FS.tr)
vif_richness_BF_FS.tr <- vif_t. ; rm(vif_t.)
rm(vif_large_richness.tr, vif_medium_richness.tr, vif_short_richness.tr)

# vif for modeled variable Shannon.tr face supérieure bloc mobile

df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Face == "face supérieure")[, c("Shannon.tr", vars.large)])
col.fct.glmm(mod.var. = "Shannon.tr", model. = model.large, length. = "large")
vif_large_Shannon.tr <- vif_values
rm(vif_values)

df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Face == "face supérieure")[, c("Shannon.tr", vars.medium)])
col.fct.glmm(mod.var. = "Shannon.tr", model. = model.medium, length. = "medium")
vif_medium_Shannon.tr <- vif_values
rm(vif_values)

df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Face == "face supérieure")[, c("Shannon.tr", vars.short)])
col.fct.glmm(mod.var. = "Shannon.tr", model. = model.short, length. = "short")
vif_short_Shannon.tr <- vif_values
rm(vif_values)

vif_Shannon_BM_FS.tr <- dplyr::full_join(vif_large_Shannon.tr, vif_medium_Shannon.tr)
vif_Shannon_BM_FS.tr <- dplyr::full_join(vif_Shannon_BM_FS.tr, vif_short_Shannon.tr)
vif.df(df. = vif_Shannon_BM_FS.tr)
vif_Shannon_BM_FS.tr <- vif_t. ; rm(vif_t.)
rm(vif_large_Shannon.tr, vif_medium_Shannon.tr, vif_short_Shannon.tr)

# vif for modeled variable Shannon.tr face inférieure bloc mobile

df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Face == "face inférieure")[, c("Shannon.tr", vars.large)])
col.fct.glmm(mod.var. = "Shannon.tr", model. = model.large, length. = "large")
vif_large_Shannon.tr <- vif_values
rm(vif_values)

df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Face == "face inférieure")[, c("Shannon.tr", vars.medium)])
col.fct.glmm(mod.var. = "Shannon.tr", model. = model.medium, length. = "medium")
vif_medium_Shannon.tr <- vif_values
rm(vif_values)

df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Face == "face inférieure")[, c("Shannon.tr", vars.short)])
col.fct.glmm(mod.var. = "Shannon.tr", model. = model.short, length. = "short")
vif_short_Shannon.tr <- vif_values
rm(vif_values)

vif_Shannon_BM_FI.tr <- dplyr::full_join(vif_large_Shannon.tr, vif_medium_Shannon.tr)
vif_Shannon_BM_FI.tr <- dplyr::full_join(vif_Shannon_BM_FI.tr, vif_short_Shannon.tr)
vif.df(df. = vif_Shannon_BM_FI.tr)
vif_Shannon_BM_FI.tr <- vif_t. ; rm(vif_t.)
rm(vif_large_Shannon.tr, vif_medium_Shannon.tr, vif_short_Shannon.tr)

# vif for modeled variable Shannon.tr face supérieure bloc fixé

df.red <- na.omit(dplyr::filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Face == "face supérieure")[, c("Shannon.tr", vars.large)])
col.fct.glmm(mod.var. = "Shannon.tr", model. = model.large, length. = "large")
vif_large_Shannon.tr <- vif_values
rm(vif_values)

df.red <- na.omit(dplyr::filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Face == "face supérieure")[, c("Shannon.tr", vars.medium)])
col.fct.glmm(mod.var. = "Shannon.tr", model. = model.medium, length. = "medium")
vif_medium_Shannon.tr <- vif_values
rm(vif_values)

df.red <- na.omit(dplyr::filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Face == "face supérieure")[, c("Shannon.tr", vars.short)])
col.fct.glmm(mod.var. = "Shannon.tr", model. = model.short, length. = "short")
vif_short_Shannon.tr <- vif_values
rm(vif_values)

vif_Shannon_BF_FS.tr <- dplyr::full_join(vif_large_Shannon.tr, vif_medium_Shannon.tr)
vif_Shannon_BF_FS.tr <- dplyr::full_join(vif_Shannon_BF_FS.tr, vif_short_Shannon.tr)
vif.df(df. = vif_Shannon_BF_FS.tr)
vif_Shannon_BF_FS.tr <- vif_t.
rm(vif_t.)
rm(vif_large_Shannon.tr, vif_medium_Shannon.tr, vif_short_Shannon.tr)

}

# I only run the above vif function for dependant variables of the paper, the ones that will appear in the annex table.

vif. <- dplyr::bind_rows(vif_blocs.retournes.fr.tr, vif_BM.BF_FS_dist.tr)
vif. <- dplyr::bind_rows(vif., vif_BM_FS.FI_dist.tr)
vif. <- dplyr::bind_rows(vif., vif_richness_BF_FS.tr)
vif. <- dplyr::bind_rows(vif., vif_richness_BM_FI.tr)
vif. <- dplyr::bind_rows(vif., vif_richness_BM_FS.tr)
vif. <- dplyr::bind_rows(vif., vif_Shannon_BF_FS.tr)
vif. <- dplyr::bind_rows(vif., vif_Shannon_BM_FI.tr)
vif. <- dplyr::bind_rows(vif., vif_Shannon_BM_FS.tr)

rm(vif_blocs.retournes.fr.tr, vif_BM.BF_FS_dist.tr, vif_BM_FS.FI_dist.tr, vif_richness_BF_FS.tr, vif_richness_BM_FI.tr, vif_richness_BM_FS.tr, vif_Shannon_BF_FS.tr, vif_Shannon_BM_FI.tr, vif_Shannon_BM_FS.tr)
rm(df.red)

vif.[c('vif', 'mod.var.', 'model')] <- stringr::str_split_fixed(vif.$dep.var._model, '-', 3)
vif. <- vif.[, c(ncol(vif.)-1, ncol(vif.), 2:(ncol(vif.)-3))]

saveRDS(vif., "model.vif.RDS")


## glmm formula

matri.fct <- function(data., region., dataset., vars., transfo., frmla.) {
  

  
  tbl. <- data.frame(matrix(ncol = 23, nrow = 1))
  colnames(tbl.) <- c("region", "dataset", "transformation", "mod.var.", "function", "family", "n ef.fix", "formule", "AIC", "pseudo.R2", "R2.Cd", "R2.Mg", "n", "unif. p", "outl. p", "outl.boot. p", "disp. p", "disp.PeaChi. p", "0infl. p", "spat.auto.cor. p",
                      "int. Esti.", "int. SE", "int. p")
  
  #for (i in c(1:length(vars.))) { # was moved down in the function because of switch between var. names for some interaction terms
  
  #  if (length(vars.) != 0){
  
  #  tbl. %>% tibble::add_column(Empty_Col1 = NA, Empty_Col2 = NA, Empty_Col3 = NA, Empty_Col4 = NA, Empty_Col5 = NA) -> tbl.
  #  names(tbl.)[c((ncol(tbl.)-4):ncol(tbl.))] <- c(paste0(vars.[i]," Esti."), paste0(vars.[i]," SE"), paste0(vars.[i]," p"), paste0(vars.[i]," Chisq"), paste0(vars.[i]," multico."))
  
  #  }
  
  #  else {next}
  
  #}
  
  #data. <- na.omit(data.[, c(unique(c("dist.", unlist(str_split(vars., ":")))), "Site", "Numero.Quadrat", "Latitude", "Longitude")])
  
  tbl.[1, "region"] <- region.
  tbl.[1, "dataset"] <- dataset.
  tbl.[1, "transformation"] <- transfo.
  tbl.[1, "mod.var."] <- as.character(frmla.[2])
  tbl.[1, "function"] <- "glmmTMB::glmmTMB"
  tbl.[1, "family"] <- "gaussian(link = identity)"
  tbl.[1, "n ef.fix"] <- length(vars.)
  
  tbl. <<- tbl.
  
  #frmla <- frmla.
  tbl.[1, "formule"] <- as.character(paste0(frmla.[2], " ", frmla.[1], " ", frmla.[3]))

  frmla. <- as.formula(paste(colnames(data.)[1], paste(c(vars., "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  #tbl.[1, "formule"] <- paste(colnames(data.)[1], paste(c(vars., "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ ")

  glmm.gaus <- suppressWarnings(glmmTMB::glmmTMB(frmla., family = gaussian(link = "identity"), data = data.))
  
  #show(Anova(glmm.gaus))
  
  # model
  tbl.[1, "AIC"] <- AIC(glmm.gaus)
  # we can generate quasi-R2 values by calculating the ratio of variance in the residuals to total variance in the response
  totalss <- var(resid(glmm.gaus, type='pearson') + predict(glmm.gaus, type='link'))
  tbl.[1, "pseudo.R2"] <- 1 - var(residuals(glmm.gaus, type='pearson'))/(totalss)
  rm("totalss")
  tbl.[1, "R2.Cd"] <- ifelse(is.null(tryCatch(suppressWarnings(performance::model_performance(glmm.gaus)$R2_conditional))), NA, tryCatch(suppressWarnings(performance::model_performance(glmm.gaus)$R2_conditional)))  #proportion of variance explained by both the fixed and random factors
  tbl.[1, "R2.Mg"] <- ifelse(is.null(tryCatch(suppressWarnings(performance::model_performance(glmm.gaus)$R2_marginal))), NA, tryCatch(suppressWarnings(performance::model_performance(glmm.gaus)$R2_marginal))) #proportion of variance explained by the fixed factor(s) alone
  tbl.[1, "n"] <- (summary(glmm.gaus))$nobs
  
  simulationOutput <- suppressWarnings(DHARMa::simulateResiduals(fittedModel = glmm.gaus 
                                                         #,n = 10,
                                                         , plot = FALSE
                                                         #, refit = TRUE
  )) # obviously, better to use the refit = TRUE argument
  
  # Model fitting diagnostic
  
  plot(simulationOutput)
  #plotQQunif(simulationOutput) # left plot in plot.DHARMa()
  #plotResiduals(simulationOutput) # right plot in plot.DHARMa()
  tbl.[1, "unif. p"] <- DHARMa::testUniformity(simulationOutput, plot = T)["p.value"]
  tbl.[1, "outl. p"] <- DHARMa::testOutliers(simulationOutput, plot = T)["p.value"]
  # For Gaussian distribution: Since the Gaussian has a variance parameter, more dispersion will just be a larger variance parameter... so you don't have overdispersion with the Gaussian. (https://stats.stackexchange.com/questions/144531/overdispersion-in-glm-with-gaussian-distribution)
  tbl.[1, "disp. p"] <- NA
  #tbl.[1, "disp. p"] <- suppressWarnings(testDispersion(glmm.gaus, alternative = "two.sided", type = "DHARMa"))["p.value"] # default: a non-parametric test that compares the variance of the simulated residuals to the observed residuals (default).
  #tbl.[1, "disp. p"] <- sigma(glmm.gaus) # see sigma function for detail of sigma function. The most commonly used GLM families (binomial, poisson) have fixed dispersion parameters which are internally ignored.
  # "A common special case of overdispersion is zero-inflation, which is the situation when more zeros appear in the observation than expected under the fitted model." (https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html#zero-inflation-k-inflation-or-deficits). Since there is no overdispersion for Gaussian distribution, we might consider zero-inflation is not of concern neither I guess. 
  tbl.[1, "0infl. p"] <- NA
  #tbl.[1, "0infl. p"] <- suppressWarnings(testZeroInflation(glmm.gaus))["p.value"]
  glmm.gaus.multico. <- tryCatch(multicollinearity(glmm.gaus), error = function (e) ("model failure"))
  recalculateOutput <- DHARMa::recalculateResiduals(simulationOutput, group = data.$Site)
  tbl.[1, "spat.auto.cor. p"] <- DHARMa::testSpatialAutocorrelation(simulationOutput = recalculateOutput, x = unique(data.$Latitude), y = unique(data.$Longitude))$p.value
  
  # complementary tests to above 5
  tbl.[1, "disp.PeaChi. p"] <- NA
  # idem, you don't have overdispersion with the Gaussian distribution.
  #tbl.[1, "disp.PeaChi. p"] <- suppressWarnings(testDispersion(glmm.gaus, alternative = "greater", type = "PearsonChisq"))["p.value"] # PearsonChisq: popular in the literature, suggested in the glmm Wiki, and implemented in some other R packages such as performance::check_overdispersion; test biased for mixed models towards underdispersion; ecommandation: to test only with alternative = 'greater', i.e. test for overdispersion.
  tbl.[1, "outl.boot. p"] <- suppressWarnings(DHARMa::testOutliers(simulationOutput, type = "bootstrap"))["p.value"] # generate a simulation-based expectation for the outliers; for integer only
  
  # extract stats
  
  tbl.[1, "int. Esti."] <- tryCatch((summary(glmm.gaus))$coefficients$cond["(Intercept)", "Estimate"], error = function (e) (NA))
  tbl.[1, "int. SE"] <- tryCatch((summary(glmm.gaus))$coefficients$cond["(Intercept)", "Std. Error"], error = function (e) (NA))
  tbl.[1, "int. p"] <- tryCatch((summary(glmm.gaus))$coefficients$cond["(Intercept)", "Pr(>|z|)"], error = function (e) (NA))
  #tbl.[1, "int. Esti."] <- (summary(glmm.gaus))$coefficients$cond["(Intercept)", "Estimate"]
  #tbl.[1, "int. SE"] <- (summary(glmm.gaus))$coefficients$cond["(Intercept)", "Std. Error"]
  #tbl.[1, "int. p"] <- (summary(glmm.gaus))$coefficients$cond["(Intercept)", "Pr(>|z|)"]
  
  tbl. <<- tbl.
  
  vars.tbl. <- grep("Sc.", rownames(data.frame(summary(glmm.gaus)$coefficients$cond)), value = TRUE)
  
  for (i in c(1:length(vars.tbl.))) {
    
    if (length(vars.tbl.) != 0){
      
      tbl. %>% tibble::add_column(Empty_Col1 = NA, Empty_Col2 = NA, Empty_Col3 = NA, Empty_Col4 = NA, Empty_Col5 = NA) -> tbl.
      names(tbl.)[c((ncol(tbl.)-4):ncol(tbl.))] <- c(paste0(vars.tbl.[i]," Esti."), paste0(vars.tbl.[i]," SE"), paste0(vars.tbl.[i]," p"), paste0(vars.tbl.[i]," Chisq"), paste0(vars.tbl.[i]," multico."))
      
    }
    
    else {next}
    
  }
  
  for (i in c(1:length(vars.tbl.))) {
    #i <- 4
    
    if (length(vars.tbl.) != 0){
      
      name. <- vars.tbl.[i] 
      
      tbl.[1, paste0(name., " Esti.")]  <- tryCatch((summary(glmm.gaus))$coefficients$cond[name., "Estimate"], error = function (e) (NA))
      tbl.[1, paste0(name., " SE")]  <- tryCatch((summary(glmm.gaus))$coefficients$cond[name.,"Std. Error"], error = function (e) (NA))
      tbl.[1, paste0(name., " p")] <- tryCatch((car::Anova(glmm.gaus))$`Pr(>Chisq)`[rownames(car::Anova(glmm.gaus)) == name.], error = function (e) (NA)) #(summary(glmm.gaus))$coefficients$cond[name., "Pr(>|z|)"]
      tbl.[1, paste0(name., " Chisq")] <- tryCatch((car::Anova(glmm.gaus))[name., "Chisq"], error = function (e) (NA))
      tbl.[1, paste0(name., " multico.")] <- tryCatch(ifelse(is.null(glmm.gaus.multico.) == TRUE, NA, dplyr::filter(glmm.gaus.multico., Term == name.)[["VIF"]]), error = function (e) (NA))
      
      rm(name., i)
      
    }
    
    else {next}
    
  }
  
  
  rm(glmm.gaus, simulationOutput, glmm.gaus.multico.)
  
  tbl. <<- tbl.
  
  cat("\n")
  
}  


## test a dredge and model average procedure

library(MuMIn)
get.vars.<- function() {
  
  Full_Model <- suppressWarnings(glmmTMB::glmmTMB(frmla., family = gaussian(link = "identity"), data = df.red
                                         , na.action = "na.fail"
                                         ))
  DredgeOutput <<- MuMIn::dredge(Full_Model, 
                          #subset =! ("cond(Sc.oceano.max.3m)" && "cond(Sc.Period.nb)"), 
                          beta = TRUE, evaluate = TRUE, rank = "AICc")
  
  #M1 <<- MuMIn::model.avg(DredgeOutput, subset = delta < 2, fit = TRUE)
  tmp <- MuMIn::get.models(DredgeOutput, subset = delta < 2)
  if (length(tmp) == 1){
    tmp2 <- c(tmp, tmp)
    M1 <<- MuMIn::model.avg(tmp2)
  } else {M1 <<- MuMIn::model.avg(DredgeOutput, subset = delta < 2, fit = TRUE)}
  vars.list.M1 <<- unlist(strsplit(trimws(as.character(M1$formula)), "\\s"))[c(grep("Sc.", unlist(strsplit(trimws(as.character(M1$formula)), "\\s"))))]
  frmla.mod.M1 <<- as.formula(paste(M1$formula[2], paste(c(vars.list.M1, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  M1.sw <<- data.frame(var. = mgsub::mgsub(names(M1$sw), c("cond", "[][()]"), c("", "")), sw = M1$sw)

  M2 <<- MuMIn::model.avg(DredgeOutput, cumsum(weight) <= .95, fit = TRUE)
  vars.list.M2 <<- unlist(strsplit(trimws(as.character(M2$formula)), "\\s"))[c(grep("Sc.", unlist(strsplit(trimws(as.character(M2$formula)), "\\s"))))]
  frmla.mod.M2 <<- as.formula(paste(M2$formula[2], paste(c(vars.list.M2, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  M2.sw <<- data.frame(var. = mgsub::mgsub(names(M2$sw), c("cond", "[][()]"), c("", "")), sw = M2$sw)

}

sw_to_tbl. <- function(sw.df) {
  
  for(i in c(1:nrow(sw.df))) {
    var. <- noquote(paste0(sw.df[i, "var."], " sw"))
    tbl. <- tibble::add_column(tbl., sw = sw.df[i, "sw"], .after = paste0(sw.df[i, "var."], " multico."))
    names(tbl.)[names(tbl.) == 'sw'] <- paste0(sw.df[i, "var."], " sw")
  }
tbl. <<- tbl.

}

#for (i in 1:10) {
 # echant1 <- sample(unique(df.$Site), 1)
 # newdf. <- subset(df., df.$Site != echant1)
 # echant2 <- sample(unique(newdf.$Site), 1)
 # newdf. <- subset(newdf., newdf.$Site != echant2)
 # echant3 <- sample(unique(newdf.$Site), 1)
 # newdf. <- subset(newdf., newdf.$Site != echant3)
 # echant4 <- sample(unique(newdf.$Site), 1)
 # newdf. <- subset(newdf., newdf.$Site != echant4)
 # echant5 <- sample(unique(newdf.$Site), 1)
 # newdf. <- subset(newdf., newdf.$Site != echant5)
 # echant6 <- sample(unique(newdf.$Site), 1)
 # newdf. <- subset(newdf., newdf.$Site != echant6)


echant1 <- "EGMP_PerreAntiochat"
echant2 <- ""
echant3 <- ""
echant4 <- ""
newdf. <- subset(df., df.$Site != echant1)

### BM_FS.FI_dist.tr

## vars.short

frmla. <- as.formula(paste("BM_FS.FI_dist.tr", paste(c(model.short, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(dplyr::filter(newdf., Type.Bloc == "Bloc mobile")[, c("BM_FS.FI_dist.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM_FS.FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- tbl.
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM_FS.FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join <- tibble::add_column(tbl.join, vars. = NA, .before = "region")
tbl.join$vars. <- "vars.short"

tbl.join <- tibble::add_column(tbl.join, M.1vs2 = NA, .before = "region")
tbl.join$M.1vs2 <- c("M1","M2")

### BM.BF_FS_dist.tr

## vars.short


frmla. <- as.formula(paste("BM.BF_FS_dist.tr", paste(c(model.short, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(dplyr::filter(newdf., Type.Bloc == "Bloc mobile")[, c("BM.BF_FS_dist.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM.BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM.BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-1):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-1):length(tbl.join$M.1vs2))/2)

### richness.tr face supérieure bloc mobile

## vars.short

frmla. <- as.formula(paste("richness.tr", paste(c(model.short, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(dplyr::filter(newdf., Type.Bloc == "Bloc mobile", Face == "face supérieure")[, c("richness.tr", vars.short)])#
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "richness BM_FS", vars. = vars.list.M1, transfo. = "center_scale", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "richness BM_FS", vars. = vars.list.M2, transfo. = "center_scale", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-1):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-1):length(tbl.join$M.1vs2))/2)
### richness.tr face inférieure bloc mobile

## vars.short

frmla. <- as.formula(paste("richness.tr", paste(c(model.short, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(dplyr::filter(newdf., Type.Bloc == "Bloc mobile", Face == "face inférieure")[, c("richness.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "richness BM_FI", vars. = vars.list.M1, transfo. = "center_scale", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "richness BM_FI", vars. = vars.list.M2, transfo. = "center_scale", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-1):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-1):length(tbl.join$M.1vs2))/2)

### richness.tr face supérieure bloc fixé

## vars.short

frmla. <- as.formula(paste("richness.tr", paste(c(model.short, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(dplyr::filter(newdf., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Face == "face supérieure")[, c("richness.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "richness BF_FS", vars. = vars.list.M1, transfo. = "center_scale", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "richness BF_FS", vars. = vars.list.M2, transfo. = "center_scale", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-1):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-1):length(tbl.join$M.1vs2))/2)

### Shannon.tr face supérieure bloc mobile

## vars.short

frmla. <- as.formula(paste("Shannon.tr", paste(c(model.short, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(dplyr::filter(newdf., Type.Bloc == "Bloc mobile", Face == "face supérieure")[, c("Shannon.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Shannon BM_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Shannon BM_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-1):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-1):length(tbl.join$M.1vs2))/2)

### Shannon.tr face inférieure bloc mobile

## vars.short

frmla. <- as.formula(paste("Shannon.tr", paste(c(model.short, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(dplyr::filter(newdf., Type.Bloc == "Bloc mobile", Face == "face inférieure")[, c("Shannon.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Shannon BM_FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Shannon BM_FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-1):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-1):length(tbl.join$M.1vs2))/2)

### Shannon.tr face supérieure bloc fixé

## vars.short

frmla. <- as.formula(paste("Shannon.tr", paste(c(model.short, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(dplyr::filter(newdf., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Face == "face supérieure")[, c("Shannon.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Shannon BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Shannon BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
tbl.models.dredge.modavg <- tbl.join
#rm(tbl.join)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-1):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-1):length(tbl.join$M.1vs2))/2)



# Let's add ivr modeling next, if removed from the model.large.vs.short vectors


### blocs.retournes.fr.tr

## vars.short

#frmla. <- as.formula(paste("blocs.retournes.fr.tr", paste(c(model.short.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
#df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile")[, c("blocs.retournes.fr.tr", vars.short.ivr)])
#get.vars.()
#matri.fct(data. = df.red, region. = "Atlantique", dataset. = "blocs.retournes.fr.", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
#tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
#matri.fct(data. = df.red, region. = "Atlantique", dataset. = "blocs.retournes.fr.", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
#tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
#rm(df.red, frmla.mod.M1,Does frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

#tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short.ivr", tbl.join$vars.)
#tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-1):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-1):length(tbl.join$M.1vs2))/2)



model_average <- tbl.join

model_average <- tibble::add_column(model_average, approche = "model average", .before = "vars.")
model_average <- tibble::add_column(model_average, `ef. fix p<.05` = NA, .before = "formule")
model_average <- tibble::add_column(model_average, `n ef. fix p<.05` = NA, .before = "ef. fix p<.05")

for (i in c(1:nrow(model_average))) {
  df.i. <- model_average[i,]
  model_average[i, "ef. fix p<.05"] <- paste0(apply(df.i.[,grep('Sc..* p', names(df.i.), value = TRUE)] < .05, 1, function(x) names(which(x))), collapse = ", ")
  model_average[i, "n ef. fix p<.05"] <- length(apply(df.i.[,grep('Sc..* p', names(df.i.), value = TRUE)] < .05, 1, function(x) names(which(x))))
}

tbl.models.dredge.modavg <- model_average
rm(model_average, df.i.)


## df with model results of both approaches

#tbl.models <- dplyr::bind_rows(tbl.models.dredge.modavg, tbl.models.step.by.step)
tbl.models <- tbl.models.dredge.modavg

tbl.models[] <- lapply(tbl.models, gsub, pattern = "Sc.blocs.retournes.fr.", replacement = "IBFI", fixed = TRUE)
tbl.models[] <- lapply(tbl.models, gsub, pattern = "Sc.Period.nb", replacement = "period", fixed = TRUE)
tbl.models[] <- lapply(tbl.models, gsub, pattern = "Sc.freq.max.3m", replacement = "quant.freq.", fixed = TRUE)
tbl.models[] <- lapply(tbl.models, gsub, pattern = "Sc.max.ratio.norm15min.obs.pre3m", replacement = "fishing", fixed = TRUE)
tbl.models[] <- lapply(tbl.models, gsub, pattern = "Sc.oceano.max.3m", replacement = "hydro.", fixed = TRUE)
tbl.models[] <- lapply(tbl.models, gsub, pattern = "Sc.taille.bloc.gp.nb", replacement = "boulder", fixed = TRUE)
tbl.models[] <- lapply(tbl.models, gsub, pattern = "Sc.accessibilite.gp.nb", replacement = "acces.", fixed = TRUE)
tbl.models[] <- lapply(tbl.models, gsub, pattern = "Sc.frequentation.gp.nb", replacement = "qual.freq.", fixed = TRUE)

tbl.models$`ef. fix p<.05` <- gsub(tbl.models$`ef. fix p<.05`, pattern = " p", replacement = "", fixed = TRUE)
tbl.models$`ef. fix p<.05` <- gsub(tbl.models$`ef. fix p<.05`, pattern = "period", replacement = " period", fixed = TRUE)

var.names <- names(tbl.models)
var.names <- gsub(var.names, pattern = "Sc.blocs.retournes.fr.", replacement = "IBFI", fixed = TRUE)
var.names <- gsub(var.names, gsub, pattern = "Sc.Period.nb", replacement = "period", fixed = TRUE)
var.names <- gsub(var.names, pattern = "Sc.freq.max.3m", replacement = "quant.freq.", fixed = TRUE)
var.names <- gsub(var.names, pattern = "Sc.max.ratio.norm15min.obs.pre3m", replacement = "fishing", fixed = TRUE)
var.names <- gsub(var.names, pattern = "Sc.oceano.max.3m", replacement = "hydro.", fixed = TRUE)
var.names <- gsub(var.names, pattern = "Sc.taille.bloc.gp.nb", replacement = "boulder", fixed = TRUE)
var.names <- gsub(var.names, gsub, pattern = "Sc.accessibilite.gp.nb", replacement = "acces.", fixed = TRUE)
var.names <- gsub(var.names, gsub, pattern = "Sc.frequentation.gp.nb", replacement = "qual.freq.", fixed = TRUE)

colnames(tbl.models) <- var.names
names(tbl.models)

write.table(tbl.models, file = paste0(echant1, echant2, echant3, echant4, ".tabular"), sep = "\t", dec = ".", na = " ", row.names = FALSE, col.names = TRUE, quote = FALSE)
rm(echant1, echant2, echant3, echant4, newdf.)
#}
#write.table(tbl.models, file = "Models.tabular", sep = "\t", dec = ".", na = " ", row.names = FALSE, col.names = TRUE, quote = FALSE)

saveRDS(tbl.models, "tbl.models.RDS")
