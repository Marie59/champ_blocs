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
    input_ratiofull <- args[2]
    input_ratiobret <- args[3]
    input_ratiobasq <- args[4]

}

## suppl. functions

pairs2 <- function (var, scaleR=TRUE, Rmethod="pearson", reorder = FALSE,  gap=0, 
                    col= "grey50",
                    pch = 20, pt.cex = 1.4, scaleR.cex = 2.8, 
                    reg = TRUE, smooth = TRUE, line01 = FALSE, dummy = FALSE, ...){
  
  if(dummy) {
    var <- model.matrix(~.-1, var)
  } else 
    if(any(!sapply(var, is.numeric))){
      warning(paste0("The following variables have been converted to numeric : ",
                     do.call("paste", as.list(colnames(var)
                                              [!sapply(var, is.numeric)]))))
      var <- sapply(var, as.numeric)
    }
  
  
  if(reorder) {
    var2 <- sapply(as.data.frame(var), as.numeric)
    pr <- cor(var2, use="pairwise.complete.obs", method=Rmethod)
    require(vegan)
    cl <- hclust(as.dist(1-pr), "ward.D2")
    var <- var[,reorder(cl, rowMeans((1-pr), na.rm = TRUE))$order]
  } 
  
  pairs(var, gap=gap,
        upper.panel=function(x,y, ...) {
          d <- na.omit(data.frame(x = x, y = y, col = col, pch=pch))
          points(d$x,d$y,cex=pt.cex, col = as.character(d$col), pch = pch,...)
          if(reg) {abline(lm(y~x, data=d),lty=2,lwd=1,col="blue")}
          if(smooth) {lines(lowess(d$x,d$y),col="red", lwd=1)}
          if(line01) {abline(a = 0, b = 1)}
        },
        lower.panel=function(x,y,method=Rmethod){
          usr <- par("usr"); on.exit(par(usr))
          par(usr = c(0, 1, 0, 1))
          r <-cor(x,y,use="pairwise.complete.obs", method=Rmethod)
          txt <- round(r,2)
          if (scaleR) {text(0.5,0.5,txt,cex= scaleR.cex*(abs(r)+0.001)) } # +0.001 to avoid problems with null correlations
          else {text(0.5,0.5,txt,cex= 2.5) }
        },
        diag.panel=function(x){
          par(new=T)
          hist(x,main="",axes=FALSE,nclass=12, proba = TRUE)
          lines(density(x, na.rm = TRUE), lwd = 1, col = "red")
          
        })
}


# lets only work with the full final merged df. from now on; all previous df. loading and merging code are inactivated.

# this is the qecbNew dataset with all NAs replace by O according to Regions, joined to the ivr dataset.
# later should be joined the frequentation dataset as well.

#I shorten the name for clarity purpose
qecbNato0 <- readRDS("results/qecbNAto0.ivr.QEBM2.matri.div.freq.obs.oceano.RDS") # NB: in the file name 'NA' becomes 'Na'; didn't want to change it all in the script ...

# change some variable format for modeling purpose first


qecbNato0$Year <- as.factor(qecbNato0$Year)
qecbNato0$Numero.Quadrat <- as.factor(qecbNato0$Numero.Quadrat)


# create the 0vs1 qecb df. for binomial model
# below code comes from the Ecology R script

## 0 or >0 occurrence

qecbNato0.0vs1 <- qecbNato0
qecbNato0.0vs1[,c(  "X..algues.brunes"                       ,            
                    "Strate.algues.brunes"                   ,          
                    "X..algues.rouges"                       ,           
                    "Strate.algues.rouges"                   ,           
                    "X..algues.vertes"                       ,            
                    "Strate.algues.vertes"                   ,            
                    "X..Cladophora"                          ,           
                    "X..Lithophyllum"                        ,           
                    "X..Recouvrement.Sediment"               , # is NA, then replace by 0 as well because no sense to have a NA value for "% recouvrement sédiment" as well.          
                    #"Type.Sediment"                          ,           
                    "X..Roche.Nue"                           , # is NA, then replace by 0 as well because no sense to have a NA value for "% roche nue" as well.          
                    "Nb.Littorina.obtusata"                  ,           
                    "Nb.Gibbula.cineraria"                   ,           
                    "Nb.Gibbula.pennanti"                    ,           
                    "Nb.Gibbula.umbilicalis"                 ,           
                    "Nb.Phallusia.mamillata"                 ,           
                    "Nb.Tethya.aurantium"                    ,           
                    #"Nb.Spirobranchus.lamarckii.1B"          ,           
                    #"Nb.Spirobranchus.lamarckii.2B"          ,           
                    #"Nb.Spirobranchus.lamarckii.3B"          ,           
                    #"Nb.Spirobranchus.lamarckii.4B"          ,           
                    #"Nb.Spirobranchus.lamarckii.5B"          ,           
                    "Nb.Spirobranchus.lamarckii.total"       ,           
                    #"Nb.spirorbis.1A"                        ,           
                    #"Nb.spirorbis.2A"                        ,           
                    #"Nb.spirorbis.3A"                        ,           
                    #"Nb.spirorbis.4A"                        ,           
                    #"Nb.spirorbis.5A"                        ,           
                    "Nb.spirorbis.total"                     ,           
                    "Nb.Crassostrea.gigas"                   ,           
                    "Nb.Ostrea.edulis"                       ,           
                    "X..Mytilus.sp."                         ,           
                    "X..Hermelles"                           ,           
                    "X..Hydraires"                           ,           
                    "X..Eponges"                             ,           
                    "X..Ascidies.Coloniales"                 ,           
                    "X..Ascidies.Solitaires"                 ,           
                    "X..Bryozoaires.Dresses"                 ,           
                    "X..Balanes.Vivantes"                    ,           
                    #"Commentaires.Avant"                     ,            
                    "X..Surface.Accolement"                  , # is NA, then replace by 0 as well because no sense to have a NA value for "% surface accolement" as well.              
                    #"Type.sustrat.observé"                   ,           
                    #"Commentaires"                           ,            
                    "Nb.Cancer.pagurus..Tourteau."           ,           
                    "Nb.Necora.puber..Etrille."              ,           
                    "Nb.Carcinus.maenas..Crabe.vert."        ,           
                    "Nb.Nucella.lapilus..Pourpre."           ,           
                    "Nb.Eriphia.verrucosa..Crabe.verruqueux.",           
                    "Nb.Octopus.vulgaris..Poulpe."           ,           
                    "Nb.Galathea..Galathées."                ,          
                    "Nb.Paracentrotus.lividus..Oursin."      ,            
                    "Nb.Lophozozymus.incisus..ancien.Xantho.incisus."         ,
                    "Nb.Palaemon.sp..Crevette.bouquet.ou.crevette.rose."      ,
                    "Nb.Haliotis.tuberculata..Ormeau."                        ,
                    "Nb.Stramonita.haemastoma..Pourpre.bouche.de.sang."       ,
                    "Nb.Littorina.littorea..Bigorneau."      ,           
                    "Nb.Xantho.pilipes..Xanthe.poilu."       ,           
                    "Nb.Mimachlamys.varia..Pétoncle.noir."
)
] <- lapply(qecbNato0.0vs1[, c(  "X..algues.brunes"                       ,            
                                 "Strate.algues.brunes"                   ,          
                                 "X..algues.rouges"                       ,           
                                 "Strate.algues.rouges"                   ,           
                                 "X..algues.vertes"                       ,            
                                 "Strate.algues.vertes"                   ,            
                                 "X..Cladophora"                          ,           
                                 "X..Lithophyllum"                        ,           
                                 "X..Recouvrement.Sediment"               , # is NA, then replace by 0 as well because no sense to have a NA value for "% recouvrement sédiment" as well.          
                                 #"Type.Sediment"                          ,           
                                 "X..Roche.Nue"                           , # is NA, then replace by 0 as well because no sense to have a NA value for "% roche nue" as well.          
                                 "Nb.Littorina.obtusata"                  ,           
                                 "Nb.Gibbula.cineraria"                   ,           
                                 "Nb.Gibbula.pennanti"                    ,           
                                 "Nb.Gibbula.umbilicalis"                 ,           
                                 "Nb.Phallusia.mamillata"                 ,           
                                 "Nb.Tethya.aurantium"                    ,           
                                 #"Nb.Spirobranchus.lamarckii.1B"          ,           
                                 #"Nb.Spirobranchus.lamarckii.2B"          ,           
                                 #"Nb.Spirobranchus.lamarckii.3B"          ,           
                                 #"Nb.Spirobranchus.lamarckii.4B"          ,           
                                 #"Nb.Spirobranchus.lamarckii.5B"          ,           
                                 "Nb.Spirobranchus.lamarckii.total"       ,           
                                 #"Nb.spirorbis.1A"                        ,           
                                 #"Nb.spirorbis.2A"                        ,           
                                 #"Nb.spirorbis.3A"                        ,           
                                 #"Nb.spirorbis.4A"                        ,           
                                 #"Nb.spirorbis.5A"                        ,           
                                 "Nb.spirorbis.total"                     ,           
                                 "Nb.Crassostrea.gigas"                   ,           
                                 "Nb.Ostrea.edulis"                       ,           
                                 "X..Mytilus.sp."                         ,           
                                 "X..Hermelles"                           ,           
                                 "X..Hydraires"                           ,           
                                 "X..Eponges"                             ,           
                                 "X..Ascidies.Coloniales"                 ,           
                                 "X..Ascidies.Solitaires"                 ,           
                                 "X..Bryozoaires.Dresses"                 ,           
                                 "X..Balanes.Vivantes"                    ,           
                                 #"Commentaires.Avant"                     ,            
                                 "X..Surface.Accolement"                  , # is NA, then replace by 0 as well because no sense to have a NA value for "% surface accolement" as well.              
                                 #"Type.sustrat.observé"                   ,           
                                 #"Commentaires"                           ,            
                                 "Nb.Cancer.pagurus..Tourteau."           ,           
                                 "Nb.Necora.puber..Etrille."              ,           
                                 "Nb.Carcinus.maenas..Crabe.vert."        ,           
                                 "Nb.Nucella.lapilus..Pourpre."           ,           
                                 "Nb.Eriphia.verrucosa..Crabe.verruqueux.",           Flibra
                                 "Nb.Octopus.vulgaris..Poulpe."           ,           
                                 "Nb.Galathea..Galathées."                ,          
                                 "Nb.Paracentrotus.lividus..Oursin."      ,            
                                 "Nb.Lophozozymus.incisus..ancien.Xantho.incisus."         ,
                                 "Nb.Palaemon.sp..Crevette.bouquet.ou.crevette.rose."      ,
                                 "Nb.Haliotis.tuberculata..Ormeau."                        ,
                                 "Nb.Stramonita.haemastoma..Pourpre.bouche.de.sang."       ,
                                 "Nb.Littorina.littorea..Bigorneau."      ,           
                                 "Nb.Xantho.pilipes..Xanthe.poilu."       ,           
                                 "Nb.Mimachlamys.varia..Pétoncle.noir."
)
], function(x) replace(x, x > 0, 1))


# not needed anymore



df.ratio.full <- readRDS("results/Ecology/df.ratio.full.RDS")
df.ratio.Bretagne <- readRDS("results/Ecology/df.ratio.Bretagne.RDS")
df.ratio.EGMP.BASQ <- readRDS("results/Ecology/df.ratio.EGMP.BASQ.RDS")


qecbNato0.red. <- unique(qecbNato0[, c("Site.Year.Month.Day", "Numero.Quadrat", "Nb.Blocs.Non.Retournes", "Nb.Blocs.Retournes", "blocs.non.retournes.fr.", "blocs.retournes.fr.", "valeur.ivr.quadrat")])


# first, create vector (4) for qecb and fishing by region

Bret_EGMP.BASQ_qecb<- c(
  "X..algues.brunes"                       ,            
  "X..algues.rouges"                       ,           
  "X..algues.vertes"                       ,            
  "X..Cladophora"                          ,           
  "X..Lithophyllum"                        ,
  "Nb.Littorina.obtusata"                  ,
  "Nb.Gibbula.cineraria"                   ,           
  "Nb.Gibbula.pennanti"                    ,           
  "Nb.Gibbula.umbilicalis"                 ,           
  "Nb.Phallusia.mamillata"                 ,           
  "Nb.Tethya.aurantium"                    ,          
  "Nb.Spirobranchus.lamarckii.total"       ,  
  "Nb.spirorbis.total"                     ,           
  "X..Eponges"                             ,           
  "X..Ascidies.Coloniales"                 ,           
  "X..Ascidies.Solitaires"                 ,           
  "X..Bryozoaires.Dresses"                 ,           
  "X..Balanes.Vivantes")                   

EGMP.BASQ_qecb <- c("Nb.Crassostrea.gigas", "Nb.Ostrea.edulis", "X..Mytilus.sp.", "X..Hermelles" , "X..Hydraires")

Bret_EGMP.BASQ_fishing <- c("Nb.Cancer.pagurus..Tourteau.", 
                            "Nb.Necora.puber..Etrille.", 
                            "Nb.Carcinus.maenas..Crabe.vert.", 
                            "Nb.Nucella.lapilus..Pourpre.", 
                            "Nb.Galathea..Galathées.", 
                            "Nb.Lophozozymus.incisus..ancien.Xantho.incisus.", 
                            "Nb.Palaemon.sp..Crevette.bouquet.ou.crevette.rose.", 
                            "Nb.Haliotis.tuberculata..Ormeau.", 
                            "Nb.Stramonita.haemastoma..Pourpre.bouche.de.sang.", 
                            "Nb.Littorina.littorea..Bigorneau.", 
                            "Nb.Xantho.pilipes..Xanthe.poilu.", 
                            "Nb.Mimachlamys.varia..Pétoncle.noir.")

EGMP.BASQ_fishing <- c("Nb.Eriphia.verrucosa..Crabe.verruqueux.", "Nb.Octopus.vulgaris..Poulpe." , "Nb.Paracentrotus.lividus..Oursin.")   


###########################################################

#I shorten the name for clarity purpose
qecbNato0 <- readRDS("results/qecbNAto0.ivr.QEBM2.matri.div.freq.obs.oceano.RDS") # NB: in the file name 'NA' becomes 'Na'; didn't want to change it all in the script ...


unique(qecbNato0$Type.Bloc)
length(!is.na(qecbNato0$Type.Bloc))
unique(qecbNato0$Face)
length(!is.na(qecbNato0$Face))
unique(qecbNato0$Region)
length(!is.na(qecbNato0$Region))

# change Year & Numero.Quadrat variable format for modeling purpose first (Year not needed in community fct)
unique(qecbNato0[, c("Numero.Quadrat")])
qecbNato0$Numero.Quadrat <- as.factor(qecbNato0$Numero.Quadrat)
unique(qecbNato0[, c("Year")])
qecbNato0$Year <- as.factor(qecbNato0$Year)
unique(qecbNato0[, c("geomorphologie.gp")])
qecbNato0[, c("geomorphologie.gp")] <- as.factor(qecbNato0[, c("geomorphologie.gp")])
unique(qecbNato0[, c("roche.gp")])
qecbNato0[, c("roche.gp")] <- as.factor(qecbNato0[, c("roche.gp")])
unique(qecbNato0[, c("taille.bloc.gp")])
qecbNato0[, c("taille.bloc.gp")] <- as.factor(qecbNato0[, c("taille.bloc.gp")])
unique(qecbNato0[, c("accessibilite.gp")])
qecbNato0[, c("accessibilite.gp")] <- as.factor(qecbNato0[, c("accessibilite.gp")])
unique(qecbNato0[, c("frequentation.gp")])
qecbNato0[, c("frequentation.gp")] <- as.factor(qecbNato0[, c("frequentation.gp")])


# Previous script before I created the unique merged df.
###########################################################

###########################################################


plot(qecbNato0$QEBM.2, qecbNato0$BM.BF_FS_dist.)
plot(qecbNato0$QEBM.2, qecbNato0$BM_FS.FI_dist.)
hist(qecbNato0$QEBM.2, xlim = c(-360,360), breaks = 1000)
qecbNato0.ft <- filter(qecbNato0, QEBM.2 > -360)
qecbNato0.ft <- filter(qecbNato0.ft, QEBM.2 < 360)
plot(qecbNato0.ft$QEBM.2, qecbNato0.ft$BM.BF_FS_dist.)
plot(qecbNato0.ft$QEBM.2, qecbNato0.ft$BM_FS.FI_dist.)
rm(qecbNato0.ft)


# the below function is wrong, since when glmmTMB fails on raw data, it doesn't make the transformation to run the model on the transformed data. I fixed that issue by using the function TryCatch later in the script in the new formula.
###########################################################
matri.fct <- function(data., region., dataset., dist.) {

  tbl. <- data.frame(matrix(ncol = 59, nrow = 2))
  colnames(tbl.) <- c("region", "dataset", "transformation", "function", "family", 
                      "int. Esti.", "int. SE", "int. p", 
                      "ivr Esti.", "ivr SE", "ivr p","ivr Chisq", "ivr multico.",
                      "rich. Esti.", "rich. SE", "rich. p", "rich. Chisq", "rich. multico.",
                      "Simp. Esti.", "Simp. SE", "Simp. p", "Simp. Chisq", "Simp. multico.",
                      "freq. Esti.", "freq. SE", "freq. p", "freq. Chisq", "freq multico.",
                      "obs. Esti.", "obs. SE", "obs. p", "obs. Chisq", "obs. multico.",
                      "rere. Esti.", "rere. SE", "rere. p", "rere. Chisq", "rere. multico.",
                      "renonre. Esti.", "renonre. SE", "renonre. p", "renonre. Chisq", "renonre. multico.",
                      "denonre. Esti.", "denonre. SE", "denonre. p", "denonre. Chisq", "denonre. multico.",
                      "AIC", "pseudo.R2", "R2.Cd", "R2.Mg", "n", "unif. p", "outl. p", "outl.boot. p", "disp. p", "disp.PeaChi. p", "0infl. p")
  
  sort(round(data.[, "dist."], digits = 4))
  show(hist(data.[, "dist."]))
  
  glmm.gaus <- suppressWarnings(glmmTMB(dist. ~ blocs.retournes.fr. 
                                         #+ richness
                                         #+ Simpson
                                         #+ freq.max.3m
                                         + max.ratio.norm15min.obs.pre3m
                                         #+ median.Nb.Blocs.Retournes.remis.norm15min.pre3m
                                         #+ median.Nb.Blocs.Retournes.non.remis.norm15min.pre3m
                                         #+ median.Nb.Blocs.Deplaces.non.remis.norm15min.pre3m
                                         + (1|Numero.Quadrat)
                                         , family = gaussian(link = "identity"), data = data.))
  
  show(summary(glmm.gaus))
  show(Anova(glmm.gaus))
  
  simulationOutput <- suppressWarnings(simulateResiduals(fittedModel = glmm.gaus 
                                                         #,n = 10,
                                                         ,plot = F
                                                         #, refit = TRUE
  )) # obvisouly, better to use the refit = TRUE argument
  
  # Model fitting diagnostic
  
  plot(simulationOutput)
  #plotQQunif(simulationOutput) # left plot in plot.DHARMa()
  #plotResiduals(simulationOutput) # right plot in plot.DHARMa()
  
  glmm.gaus.unif.p <- testUniformity(simulationOutput, plot = T)["p.value"]
  glmm.gaus.outl.p <- testOutliers(simulationOutput, plot = T)["p.value"]
  glmm.gaus.disp.p <- suppressWarnings(testDispersion(glmm.gaus, alternative = "two.sided", type = "DHARMa"))["p.value"] # default: a non-parametric test that compares the variance of the simulated residuals to the observed residuals (default).
  #glmm.gaus.disp. <- sigma(glmm.gaus) # see sigma function for detail of sigma function. The most commonly used GLM families (binomial, poisson) have fixed dispersion parameters which are internally ignored.
  glmm.gaus.0infl.p <- suppressWarnings(testZeroInflation(glmm.gaus))["p.value"]
  glmm.gaus.multico. <- multicollinearity(glmm.gaus)
  
  # complementary tests to above 5
  glmm.gaus.disp.PeaChi.p <- suppressWarnings(testDispersion(glmm.gaus, alternative = "greater", type = "PearsonChisq"))["p.value"] # PearsonChisq: popular in the literature, suggested in the glmm Wiki, and implemented in some other R packages such as performance::check_overdispersion; test biased for mixed models towards underdispersion; ecommandation: to test only with alternative = 'greater', i.e. test for overdispersion.
  glmm.gaus.outl.boot.p <- suppressWarnings(testOutliers(simulationOutput, type = "bootstrap"))["p.value"] # generate a simulation-based expectation for the outliers; for integer only
  
  # extract stats
  
  # IVR
  glmm.gaus.intercept.Esti. <- (summary(glmm.gaus))$coefficients$cond["(Intercept)", "Estimate"]
  glmm.gaus.intercept.SE <- (summary(glmm.gaus))$coefficients$cond["(Intercept)", "Std. Error"]
  glmm.gaus.intercept.p <- (summary(glmm.gaus))$coefficients$cond["(Intercept)", "Pr(>|z|)"]
  glmm.gaus.ivr.Esti. <- (summary(glmm.gaus))$coefficients$cond["blocs.retournes.fr.", "Estimate"]
  glmm.gaus.ivr.SE <- (summary(glmm.gaus))$coefficients$cond["blocs.retournes.fr.","Std. Error"]
  glmm.gaus.ivr.p <- (Anova(glmm.gaus))$`Pr(>Chisq)`[rownames(Anova(glmm.gaus)) == "blocs.retournes.fr."] #(summary(glmm.gaus))$coefficients$cond["blocs.retournes.fr.", "Pr(>|z|)"]
  glmm.gaus.ivr.Chisq <- (Anova(glmm.gaus))["blocs.retournes.fr.", "Chisq"]
  glmm.gaus.ivr.multico. <- filter(glmm.gaus.multico., Term == "blocs.retournes.fr.")[["VIF"]]
  
  # richness
  #glmm.gaus.rich.Esti. <- (summary(glmm.gaus))$coefficients$cond["richness", "Estimate"]
  #glmm.gaus.rich.SE <- (summary(glmm.gaus))$coefficients$cond["richness","Std. Error"]
  #glmm.gaus.rich.p <- (Anova(glmm.gaus))$`Pr(>Chisq)`[rownames(Anova(glmm.gaus)) == "richness"]
  #glmm.gaus.rich.Chisq <- (Anova(glmm.gaus))["richness", "Chisq"]
  #glmm.gaus.rich.multico. <- filter(glmm.gaus.multico., Term == "richness")[["VIF"]]
  
  # Simpson
  #glmm.gaus.Simp.Esti. <- (summary(glmm.gaus))$coefficients$cond["Simpson", "Estimate"]
  #glmm.gaus.Simp.SE <- (summary(glmm.gaus))$coefficients$cond["Simpson","Std. Error"]
  #glmm.gaus.Simp.p <- (Anova(glmm.gaus))$`Pr(>Chisq)`[rownames(Anova(glmm.gaus)) == "Simpson"]
  #glmm.gaus.Simp.Chisq <- (Anova(glmm.gaus))["Simpson", "Chisq"]
  #glmm.gaus.Simp.multico. <- filter(glmm.gaus.multico., Term == "Simpson")[["VIF"]]
  
  # Freq.
  #glmm.gaus.freq.Esti. <- (summary(glmm.gaus))$coefficients$cond["freq.max.3m", "Estimate"]
  #glmm.gaus.freq.SE <- (summary(glmm.gaus))$coefficients$cond["freq.max.3m","Std. Error"]
  #glmm.gaus.freq.p <- (Anova(glmm.gaus))$`Pr(>Chisq)`[rownames(Anova(glmm.gaus)) == "freq.max.3m"] 
  #glmm.gaus.freq.Chisq <- (Anova(glmm.gaus))["freq.max.3m", "Chisq"]
  #glmm.gaus.freq.multico. <- filter(glmm.gaus.multico., Term == "freq.max.3m")[["VIF"]]
  
  # ratio.norm15min.obs.pre3m
  glmm.gaus.ratio.obs.Esti. <- (summary(glmm.gaus))$coefficients$cond["max.ratio.norm15min.obs.pre3m", "Estimate"]
  glmm.gaus.ratio.obs.SE <- (summary(glmm.gaus))$coefficients$cond["max.ratio.norm15min.obs.pre3m","Std. Error"]
  glmm.gaus.ratio.obs.p <- (Anova(glmm.gaus))$`Pr(>Chisq)`[rownames(Anova(glmm.gaus)) == "max.ratio.norm15min.obs.pre3m"] 
  glmm.gaus.ratio.obs.Chisq <- (Anova(glmm.gaus))["max.ratio.norm15min.obs.pre3m", "Chisq"]
  glmm.gaus.ratio.obs.Chisq <- (Anova(glmm.gaus))["max.ratio.norm15min.obs.pre3m", "Chisq"]
  glmm.gaus.ratio.obs.multico. <- filter(glmm.gaus.multico., Term == "max.ratio.norm15min.obs.pre3m")[["VIF"]]
  
  # Nb.Blocs.Retournes.remis.norm15min.pre3m
  #glmm.gaus.rere.Esti. <- (summary(glmm.gaus))$coefficients$cond["median.Nb.Blocs.Retournes.remis.norm15min.pre3m", "Estimate"]
  #glmm.gaus.rere.SE <- (summary(glmm.gaus))$coefficients$cond["median.Nb.Blocs.Retournes.remis.norm15min.pre3m","Std. Error"]
  #glmm.gaus.rere.p <- (Anova(glmm.gaus))$`Pr(>Chisq)`[rownames(Anova(glmm.gaus)) == "median.Nb.Blocs.Retournes.remis.norm15min.pre3m"] 
  #glmm.gaus.rere.Chisq <- (Anova(glmm.gaus))["median.Nb.Blocs.Retournes.remis.norm15min.pre3m", "Chisq"]
  #glmm.gaus.rere.multico. <- filter(glmm.gaus.multico., Term == "median.Nb.Blocs.Retournes.remis.norm15min.pre3m")[["VIF"]]
  
  # Nb.Blocs.Retournes.non.remis.norm15min.pre3m
  #glmm.gaus.renonre.Esti. <- (summary(glmm.gaus))$coefficients$cond["median.Nb.Blocs.Retournes.non.remis.norm15min.pre3m", "Estimate"]
  #glmm.gaus.renonre.SE <- (summary(glmm.gaus))$coefficients$cond["median.Nb.Blocs.Retournes.non.remis.norm15min.pre3m","Std. Error"]
  #glmm.gaus.renonre.p <- (Anova(glmm.gaus))$`Pr(>Chisq)`[rownames(Anova(glmm.gaus)) == "median.Nb.Blocs.Retournes.non.remis.norm15min.pre3m"] 
  #glmm.gaus.renonre.Chisq <- (Anova(glmm.gaus))["median.Nb.Blocs.Retournes.non.remis.norm15min.pre3m", "Chisq"]
  #glmm.gaus.renonre.multico. <- filter(glmm.gaus.multico., Term == "median.Nb.Blocs.Retournes.non.remis.norm15min.pre3m")[["VIF"]]
  
  # Nb.Blocs.Deplaces.non.remis.norm15min.pre3m
  #glmm.gaus.denonre.Esti. <- (summary(glmm.gaus))$coefficients$cond["median.Nb.Blocs.Deplaces.non.remis.norm15min.pre3m", "Estimate"]
  #glmm.gaus.denonre.SE <- (summary(glmm.gaus))$coefficients$cond["median.Nb.Blocs.Deplaces.non.remis.norm15min.pre3m","Std. Error"]
  #glmm.gaus.denonre.p <- (Anova(glmm.gaus))$`Pr(>Chisq)`[rownames(Anova(glmm.gaus)) == "median.Nb.Blocs.Deplaces.non.remis.norm15min.pre3m"] 
  #glmm.gaus.denonre.Chisq <- (Anova(glmm.gaus))["median.Nb.Blocs.Deplaces.non.remis.norm15min.pre3m", "Chisq"]
  #glmm.gaus.denonre.Chisq <- (Anova(glmm.gaus))["median.Nb.Blocs.Deplaces.non.remis.norm15min.pre3m", "Chisq"]
  #glmm.gaus.denonre.multico. <- filter(glmm.gaus.multico., Term == "median.Nb.Blocs.Deplaces.non.remis.norm15min.pre3m")[["VIF"]]
  
  # model
  glmm.gaus.AIC <- AIC(glmm.gaus)
  # we can generate quasi-R2 values by calculating the ratio of variance in the residuals to total variance in the response
  totalss <- var(resid(glmm.gaus, type='pearson') + predict(glmm.gaus, type='link'))
  glmm.gaus.pseudo.R2 <- 1 - var(residuals(glmm.gaus, type='pearson'))/(totalss)
  glmm.gaus.R2.Cd <- suppressWarnings(model_performance(glmm.gaus)$R2_conditional) #proportion of variance explained by both the fixed and random factors
  glmm.gaus.R2.Mg <- suppressWarnings(model_performance(glmm.gaus)$R2_marginal) #proportion of variance explained by the fixed factor(s) alone
  
  tbl.[1, "region"] <- region.
  tbl.[1, "dataset"] <- dataset.
  tbl.[1, "transformation"] <- NA
  tbl.[1, "function"] <- "glmmTMB"
  tbl.[1, "family"] <- "gaussian(link = identity)"
  tbl.[1, "int. Esti."] <- glmm.gaus.intercept.Esti.
  tbl.[1, "int. SE"] <- glmm.gaus.intercept.SE
  tbl.[1, "int. p"] <- glmm.gaus.intercept.p
  
  tbl.[1, "ivr Esti."] <- glmm.gaus.ivr.Esti.
  tbl.[1, "ivr SE"] <- glmm.gaus.ivr.SE
  tbl.[1, "ivr p"] <- glmm.gaus.ivr.p
  tbl.[1, "ivr Chisq"] <- glmm.gaus.ivr.Chisq
  tbl.[1, "ivr multico."] <- glmm.gaus.ivr.multico.

  #tbl.[1, "rich. Esti."] <- glmm.gaus.rich.Esti.
  #tbl.[1, "rich. SE"] <- glmm.gaus.rich.SE
  #tbl.[1, "rich. p"] <- glmm.gaus.rich.p
  #tbl.[1, "rich. Chisq"] <- glmm.gaus.rich.Chisq
  #tbl.[1, "rich. multico."] <- glmm.gaus.rich.multico.
  
  #tbl.[1, "Simp. Esti."] <- glmm.gaus.Simp.Esti.
  #tbl.[1, "Simp. SE"] <- glmm.gaus.Simp.SE
  #tbl.[1, "Simp. p"] <- glmm.gaus.Simp.p
  #tbl.[1, "Simp. Chisq"] <- glmm.gaus.Simp.Chisq
  #tbl.[1, "Simp. multico."] <- glmm.gaus.Simp.multico.
  
  #tbl.[1, "freq. Esti."] <- glmm.gaus.freq.Esti.
  #tbl.[1, "freq. SE"] <- glmm.gaus.freq.SE
  #tbl.[1, "freq. p"] <- glmm.gaus.freq.p
  #tbl.[1, "freq. Chisq"] <- glmm.gaus.freq.Chisq
  #tbl.[1, "freq. multico."] <- glmm.gaus.freq.multico.
  
  tbl.[1, "obs. Esti."] <- glmm.gaus.ratio.obs.Esti.
  tbl.[1, "obs. SE"] <- glmm.gaus.ratio.obs.SE
  tbl.[1, "obs. p"] <- glmm.gaus.ratio.obs.p
  tbl.[1, "obs. Chisq"] <- glmm.gaus.ratio.obs.Chisq
  tbl.[1, "obs. multico."] <- glmm.gaus.ratio.obs.multico.
  
  #tbl.[1, "rere. Esti."] <- glmm.gaus.rere.Esti.
  #tbl.[1, "rere. SE"] <- glmm.gaus.rere.SE
  #tbl.[1, "rere. p"] <- glmm.gaus.rere.p
  #tbl.[1, "rere. Chisq"] <- glmm.gaus.rere.Chisq
  #tbl.[1, "rere. multico."] <- glmm.gaus.rere.multico.
  
  #tbl.[1, "renonre. Esti."] <- glmm.gaus.renonre.Esti.
  #tbl.[1, "renonre. SE"] <- glmm.gaus.renonre.SE
  #tbl.[1, "renonre. p"] <- glmm.gaus.renonre.p
  #tbl.[1, "renonre. Chisq"] <- glmm.gaus.renonre.Chisq
  #tbl.[1, "renonre. multico."] <- glmm.gaus.renonre.multico.
  
  #tbl.[1, "denonre. Esti."] <- glmm.gaus.denonre.Esti.
  #tbl.[1, "denonre. SE"] <- glmm.gaus.denonre.SE
  #tbl.[1, "denonre. p"] <- glmm.gaus.denonre.p
  #tbl.[1, "denonre. Chisq"] <- glmm.gaus.denonre.Chisq
  #tbl.[1, "denonre. multico."] <- glmm.gaus.denonre.multico.
  
  tbl.[1, "AIC"] <- glmm.gaus.AIC
  tbl.[1, "pseudo.R2"] <- glmm.gaus.pseudo.R2
  tbl.[1, "R2.Cd"] <- glmm.gaus.R2.Cd
  tbl.[1, "R2.Mg"] <- glmm.gaus.R2.Mg
  
  #tbl.[1, "n"] <- sum(!is.na((data.[,df.[1, "variable"]])))
  tbl.[1, "n"] <- (summary(glmm.gaus))$nobs
  
  tbl.[1, "unif. p"] <- glmm.gaus.unif.p
  tbl.[1, "outl. p"] <- glmm.gaus.outl.p
  tbl.[1, "outl.boot. p"] <- glmm.gaus.outl.boot.p
  tbl.[1, "disp. p"] <- glmm.gaus.disp.p
  tbl.[1, "disp.PeaChi. p"] <- glmm.gaus.disp.PeaChi.p
  tbl.[1, "0infl. p"] <- glmm.gaus.0infl.p
  
  rm("glmm.gaus.intercept.Esti.", "glmm.gaus.intercept.SE", "glmm.gaus.intercept.p", "glmm.gaus.ivr.Esti.", "glmm.gaus.ivr.SE", "glmm.gaus.ivr.p", "glmm.gaus.ivr.Chisq", "glmm.gaus.ivr.multico.",
     "glmm.gaus.rich.Esti.", "glmm.gaus.rich.SE", "glmm.gaus.rich.p", "glmm.gaus.rich.Chisq", "glmm.gaus.rich.multico.", 
     "glmm.gaus.Simp.Esti.", "glmm.gaus.Simp.SE", "glmm.gaus.Simp.p", "glmm.gaus.Simp.Chisq", "glmm.gaus.Simp.multico.", 
     "glmm.gaus.freq.Esti.", "glmm.gaus.freq.SE", "glmm.gaus.freq.p", "glmm.gaus.freq.Chisq", "glmm.gaus.freq.multico.",
     "glmm.gaus.ratio.obs.Esti.", "glmm.gaus.ratio.obs.SE", "glmm.gaus.ratio.obs.p", "glmm.gaus.ratio.obs.Chisq", "glmm.gaus.ratio.obs.multico.",
     "glmm.gaus.rere.Esti.", "glmm.gaus.rere.SE", "glmm.gaus.rere.p", "glmm.gaus.rere.Chisq", "glmm.gaus.rere.multico.",
     "glmm.gaus.renonre.Esti.", "glmm.gaus.renonre.SE", "glmm.gaus.renonre.p", "glmm.gaus.renonre.Chisq", "glmm.gaus.renonre.multico.",
     "glmm.gaus.denonre.Esti.", "glmm.gaus.denonre.SE", "glmm.gaus.denonre.p", "glmm.gaus.denonre.Chisq", "glmm.gaus.denonre.multico.",
     "glmm.gaus.AIC", "glmm.gaus.pseudo.R2", "glmm.gaus.R2.Cd", "glmm.gaus.R2.Mg", "glmm.gaus.unif.p", "glmm.gaus.outl.p", "glmm.gaus.outl.boot.p", "glmm.gaus.disp.p", "glmm.gaus.disp.PeaChi.p", "glmm.gaus.0infl.p")
  
  rm("totalss")
  
  
  (bn <- bestNormalize(data.[, "dist."]))
  (bn.cst <- bestNormalize(data.[, "dist."] 
                           #+ 0.0001 #no Boxcox transfo so ok for 0
  ))
  tr. <- attributes(bn.cst$chosen_transform)$class[1]
  data.[, "dist.tr."] <- bn.cst$chosen_transform$x.t
  
  sort(round(data.[, "dist.tr."], digits = 4))
  show(hist(data.[, "dist.tr."]))
  
  glmm.gaus.tr <- suppressWarnings(glmmTMB(dist.tr. ~ blocs.retournes.fr. 
                                        #+ richness
                                        #+ Simpson
                                        #+ freq.max.3m
                                        + max.ratio.norm15min.obs.pre3m
                                        #+ median.Nb.Blocs.Retournes.remis.norm15min.pre3m
                                        #+ median.Nb.Blocs.Retournes.non.remis.norm15min.pre3m
                                        #+ median.Nb.Blocs.Deplaces.non.remis.norm15min.pre3m
                                        + (1|Numero.Quadrat)
                                        , family = gaussian(link = "identity"), data = data.))
  
  show(summary(glmm.gaus.tr))
  show(Anova(glmm.gaus.tr))
  
  simulationOutput <- suppressWarnings(simulateResiduals(fittedModel = glmm.gaus.tr 
                                                         #,n = 10,
                                                         ,plot = F
                                                         #, refit = TRUE
  )) # obvisouly, better to use the refit = TRUE argument
  
  # Model fitting diagnostic
  
  plot(simulationOutput)
  #plotQQunif(simulationOutput) # left plot in plot.DHARMa()
  #plotResiduals(simulationOutput) # right plot in plot.DHARMa()
  
  glmm.gaus.tr.unif.p <- testUniformity(simulationOutput, plot = T)["p.value"]
  glmm.gaus.tr.outl.p <- testOutliers(simulationOutput, plot = T)["p.value"]
  glmm.gaus.tr.disp.p <- suppressWarnings(testDispersion(glmm.gaus.tr, alternative = "two.sided", type = "DHARMa"))["p.value"] # default: a non-parametric test that compares the variance of the simulated residuals to the observed residuals (default).
  #glmm.gaus.tr.disp. <- sigma(glmm.gaus.tr) # see sigma function for detail of sigma function. The most commonly used GLM families (binomial, poisson) have fixed dispersion parameters which are internally ignored.
  glmm.gaus.tr.0infl.p <- suppressWarnings(testZeroInflation(glmm.gaus.tr))["p.value"]
  glmm.gaus.tr.multico. <- multicollinearity(glmm.gaus.tr)
  
  # complementary tests to above 5
  glmm.gaus.tr.disp.PeaChi.p <- suppressWarnings(testDispersion(glmm.gaus.tr, alternative = "greater", type = "PearsonChisq"))["p.value"] # PearsonChisq: popular in the literature, suggested in the glmm Wiki, and implemented in some other R packages such as performance::check_overdispersion; test biased for mixed models towards underdispersion; ecommandation: to test only with alternative = 'greater', i.e. test for overdispersion.
  glmm.gaus.tr.outl.boot.p <- suppressWarnings(testOutliers(simulationOutput, type = "bootstrap"))["p.value"] # generate a simulation-based expectation for the outliers; for integer only
  
  # extract stats
  
  # IVR
  glmm.gaus.tr.intercept.Esti. <- (summary(glmm.gaus.tr))$coefficients$cond["(Intercept)", "Estimate"]
  glmm.gaus.tr.intercept.SE <- (summary(glmm.gaus.tr))$coefficients$cond["(Intercept)", "Std. Error"]
  glmm.gaus.tr.intercept.p <- (summary(glmm.gaus.tr))$coefficients$cond["(Intercept)", "Pr(>|z|)"]
  glmm.gaus.tr.ivr.Esti. <- (summary(glmm.gaus.tr))$coefficients$cond["blocs.retournes.fr.", "Estimate"]
  glmm.gaus.tr.ivr.SE <- (summary(glmm.gaus.tr))$coefficients$cond["blocs.retournes.fr.","Std. Error"]
  glmm.gaus.tr.ivr.p <- (Anova(glmm.gaus.tr))$`Pr(>Chisq)`[rownames(Anova(glmm.gaus.tr)) == "blocs.retournes.fr."] #(summary(glmm.gaus.tr))$coefficients$cond["blocs.retournes.fr.", "Pr(>|z|)"]
  glmm.gaus.tr.ivr.Chisq <- (Anova(glmm.gaus.tr))["blocs.retournes.fr.", "Chisq"]
  glmm.gaus.tr.ivr.multico. <- filter(glmm.gaus.tr.multico., Term == "blocs.retournes.fr.")[["VIF"]]
  
  # richness
  #glmm.gaus.tr.rich.Esti. <- (summary(glmm.gaus.tr))$coefficients$cond["richness", "Estimate"]
  #glmm.gaus.tr.rich.SE <- (summary(glmm.gaus.tr))$coefficients$cond["richness","Std. Error"]
  #glmm.gaus.tr.rich.p <- (Anova(glmm.gaus.tr))$`Pr(>Chisq)`[rownames(Anova(glmm.gaus.tr)) == "richness"]
  #glmm.gaus.tr.rich.Chisq <- (Anova(glmm.gaus.tr))["richness", "Chisq"]
  #glmm.gaus.tr.rich.multico. <- filter(glmm.gaus.tr.multico., Term == "richness")[["VIF"]]
  
  # Simpson
  #glmm.gaus.tr.Simp.Esti. <- (summary(glmm.gaus.tr))$coefficients$cond["Simpson", "Estimate"]
  #glmm.gaus.tr.Simp.SE <- (summary(glmm.gaus.tr))$coefficients$cond["Simpson","Std. Error"]
  #glmm.gaus.tr.Simp.p <- (Anova(glmm.gaus.tr))$`Pr(>Chisq)`[rownames(Anova(glmm.gaus.tr)) == "Simpson"]
  #glmm.gaus.tr.Simp.Chisq <- (Anova(glmm.gaus.tr))["Simpson", "Chisq"]
  #glmm.gaus.tr.Simp.multico. <- filter(glmm.gaus.tr.multico., Term == "Simpson")[["VIF"]]
  
  # Freq.
  #glmm.gaus.tr.freq.Esti. <- (summary(glmm.gaus.tr))$coefficients$cond["freq.max.3m", "Estimate"]
  #glmm.gaus.tr.freq.SE <- (summary(glmm.gaus.tr))$coefficients$cond["freq.max.3m","Std. Error"]
  #glmm.gaus.tr.freq.p <- (Anova(glmm.gaus.tr))$`Pr(>Chisq)`[rownames(Anova(glmm.gaus.tr)) == "freq.max.3m"] 
  #glmm.gaus.tr.freq.Chisq <- (Anova(glmm.gaus.tr))["freq.max.3m", "Chisq"]
  #glmm.gaus.tr.freq.multico. <- filter(glmm.gaus.tr.multico., Term == "freq.max.3m")[["VIF"]]
  
  # ratio.norm15min.obs.pre3m
  glmm.gaus.tr.ratio.obs.Esti. <- (summary(glmm.gaus.tr))$coefficients$cond["max.ratio.norm15min.obs.pre3m", "Estimate"]
  glmm.gaus.tr.ratio.obs.SE <- (summary(glmm.gaus.tr))$coefficients$cond["max.ratio.norm15min.obs.pre3m","Std. Error"]
  glmm.gaus.tr.ratio.obs.p <- (Anova(glmm.gaus.tr))$`Pr(>Chisq)`[rownames(Anova(glmm.gaus.tr)) == "max.ratio.norm15min.obs.pre3m"] 
  glmm.gaus.tr.ratio.obs.Chisq <- (Anova(glmm.gaus.tr))["max.ratio.norm15min.obs.pre3m", "Chisq"]
  glmm.gaus.tr.ratio.obs.Chisq <- (Anova(glmm.gaus.tr))["max.ratio.norm15min.obs.pre3m", "Chisq"]
  glmm.gaus.tr.ratio.obs.multico. <- filter(glmm.gaus.tr.multico., Term == "max.ratio.norm15min.obs.pre3m")[["VIF"]]
  
  # Nb.Blocs.Retournes.remis.norm15min.pre3m
  #glmm.gaus.tr.rere.Esti. <- (summary(glmm.gaus.tr))$coefficients$cond["median.Nb.Blocs.Retournes.remis.norm15min.pre3m", "Estimate"]
  #glmm.gaus.tr.rere.SE <- (summary(glmm.gaus.tr))$coefficients$cond["median.Nb.Blocs.Retournes.remis.norm15min.pre3m","Std. Error"]
  #glmm.gaus.tr.rere.p <- (Anova(glmm.gaus.tr))$`Pr(>Chisq)`[rownames(Anova(glmm.gaus.tr)) == "median.Nb.Blocs.Retournes.remis.norm15min.pre3m"] 
  #glmm.gaus.tr.rere.Chisq <- (Anova(glmm.gaus.tr))["median.Nb.Blocs.Retournes.remis.norm15min.pre3m", "Chisq"]
  #glmm.gaus.tr.rere.multico. <- filter(glmm.gaus.tr.multico., Term == "median.Nb.Blocs.Retournes.remis.norm15min.pre3m")[["VIF"]]
  
  # Nb.Blocs.Retournes.non.remis.norm15min.pre3m
  #glmm.gaus.tr.renonre.Esti. <- (summary(glmm.gaus.tr))$coefficients$cond["median.Nb.Blocs.Retournes.non.remis.norm15min.pre3m", "Estimate"]
  #glmm.gaus.tr.renonre.SE <- (summary(glmm.gaus.tr))$coefficients$cond["median.Nb.Blocs.Retournes.non.remis.norm15min.pre3m","Std. Error"]
  #glmm.gaus.tr.renonre.p <- (Anova(glmm.gaus.tr))$`Pr(>Chisq)`[rownames(Anova(glmm.gaus.tr)) == "median.Nb.Blocs.Retournes.non.remis.norm15min.pre3m"] 
  #glmm.gaus.tr.renonre.Chisq <- (Anova(glmm.gaus.tr))["median.Nb.Blocs.Retournes.non.remis.norm15min.pre3m", "Chisq"]
  #glmm.gaus.tr.renonre.multico. <- filter(glmm.gaus.tr.multico., Term == "median.Nb.Blocs.Retournes.non.remis.norm15min.pre3m")[["VIF"]]
  
  # Nb.Blocs.Deplaces.non.remis.norm15min.pre3m
  #glmm.gaus.tr.denonre.Esti. <- (summary(glmm.gaus.tr))$coefficients$cond["median.Nb.Blocs.Deplaces.non.remis.norm15min.pre3m", "Estimate"]
  #glmm.gaus.tr.denonre.SE <- (summary(glmm.gaus.tr))$coefficients$cond["median.Nb.Blocs.Deplaces.non.remis.norm15min.pre3m","Std. Error"]
  #glmm.gaus.tr.denonre.p <- (Anova(glmm.gaus.tr))$`Pr(>Chisq)`[rownames(Anova(glmm.gaus.tr)) == "median.Nb.Blocs.Deplaces.non.remis.norm15min.pre3m"] 
  #glmm.gaus.tr.denonre.Chisq <- (Anova(glmm.gaus.tr))["median.Nb.Blocs.Deplaces.non.remis.norm15min.pre3m", "Chisq"]
  #glmm.gaus.tr.denonre.Chisq <- (Anova(glmm.gaus.tr))["median.Nb.Blocs.Deplaces.non.remis.norm15min.pre3m", "Chisq"]
  #glmm.gaus.tr.denonre.multico. <- filter(glmm.gaus.tr.multico., Term == "median.Nb.Blocs.Deplaces.non.remis.norm15min.pre3m")[["VIF"]]
  
  # model
  glmm.gaus.tr.AIC <- AIC(glmm.gaus.tr)
  # we can generate quasi-R2 values by calculating the ratio of variance in the residuals to total variance in the response
  totalss <- var(resid(glmm.gaus.tr, type='pearson') + predict(glmm.gaus.tr, type='link'))
  glmm.gaus.tr.pseudo.R2 <- 1 - var(residuals(glmm.gaus.tr, type='pearson'))/(totalss)
  glmm.gaus.tr.R2.Cd <- suppressWarnings(model_performance(glmm.gaus.tr)$R2_conditional) #proportion of variance explained by both the fixed and random factors
  glmm.gaus.tr.R2.Mg <- suppressWarnings(model_performance(glmm.gaus.tr)$R2_marginal) #proportion of variance explained by the fixed factor(s) alone
  
  tbl.[2, "region"] <- region.
  tbl.[2, "dataset"] <- dataset.
  tbl.[2, "transformation"] <- tr.
  tbl.[2, "function"] <- "glmmTMB"
  tbl.[2, "family"] <- "gaussian(link = identity)"
  tbl.[2, "int. Esti."] <- glmm.gaus.tr.intercept.Esti.
  tbl.[2, "int. SE"] <- glmm.gaus.tr.intercept.SE
  tbl.[2, "int. p"] <- glmm.gaus.tr.intercept.p
  
  tbl.[2, "ivr Esti."] <- glmm.gaus.tr.ivr.Esti.
  tbl.[2, "ivr SE"] <- glmm.gaus.tr.ivr.SE
  tbl.[2, "ivr p"] <- glmm.gaus.tr.ivr.p
  tbl.[2, "ivr Chisq"] <- glmm.gaus.tr.ivr.Chisq
  tbl.[2, "ivr multico."] <- glmm.gaus.tr.ivr.multico.
  
  #tbl.[2, "rich. Esti."] <- glmm.gaus.tr.rich.Esti.
  #tbl.[2, "rich. SE"] <- glmm.gaus.tr.rich.SE
  #tbl.[2, "rich. p"] <- glmm.gaus.tr.rich.p
  #tbl.[2, "rich. Chisq"] <- glmm.gaus.tr.rich.Chisq
  #tbl.[2, "rich. multico."] <- glmm.gaus.tr.rich.multico.
  
  #tbl.[2, "Simp. Esti."] <- glmm.gaus.tr.Simp.Esti.
  #tbl.[2, "Simp. SE"] <- glmm.gaus.tr.Simp.SE
  #tbl.[2, "Simp. p"] <- glmm.gaus.tr.Simp.p
  #tbl.[2, "Simp. Chisq"] <- glmm.gaus.tr.Simp.Chisq
  #tbl.[2, "Simp. multico."] <- glmm.gaus.tr.Simp.multico.
  
  #tbl.[2, "freq. Esti."] <- glmm.gaus.tr.freq.Esti.
  #tbl.[2, "freq. SE"] <- glmm.gaus.tr.freq.SE
  #tbl.[2, "freq. p"] <- glmm.gaus.tr.freq.p
  #tbl.[2, "freq. Chisq"] <- glmm.gaus.tr.freq.Chisq
  #tbl.[2, "freq. multico."] <- glmm.gaus.tr.freq.multico.
  
  tbl.[2, "obs. Esti."] <- glmm.gaus.tr.ratio.obs.Esti.
  tbl.[2, "obs. SE"] <- glmm.gaus.tr.ratio.obs.SE
  tbl.[2, "obs. p"] <- glmm.gaus.tr.ratio.obs.p
  tbl.[2, "obs. Chisq"] <- glmm.gaus.tr.ratio.obs.Chisq
  tbl.[2, "obs. multico."] <- glmm.gaus.tr.ratio.obs.multico.
  
  #tbl.[2, "rere. Esti."] <- glmm.gaus.tr.rere.Esti.
  #tbl.[2, "rere. SE"] <- glmm.gaus.tr.rere.SE
  #tbl.[2, "rere. p"] <- glmm.gaus.tr.rere.p
  #tbl.[2, "rere. Chisq"] <- glmm.gaus.tr.rere.Chisq
  #tbl.[2, "rere. multico."] <- glmm.gaus.tr.rere.multico.
  
  #tbl.[2, "renonre. Esti."] <- glmm.gaus.tr.renonre.Esti.
  #tbl.[2, "renonre. SE"] <- glmm.gaus.tr.renonre.SE
  #tbl.[2, "renonre. p"] <- glmm.gaus.tr.renonre.p
  #tbl.[2, "renonre. Chisq"] <- glmm.gaus.tr.renonre.Chisq
  #tbl.[2, "renonre. multico."] <- glmm.gaus.tr.renonre.multico.
  
  #tbl.[2, "denonre. Esti."] <- glmm.gaus.tr.denonre.Esti.
  #tbl.[2, "denonre. SE"] <- glmm.gaus.tr.denonre.SE
  #tbl.[2, "denonre. p"] <- glmm.gaus.tr.denonre.p
  #tbl.[2, "denonre. Chisq"] <- glmm.gaus.tr.denonre.Chisq
  #tbl.[2, "denonre. multico."] <- glmm.gaus.tr.denonre.multico.
  
  tbl.[2, "AIC"] <- glmm.gaus.tr.AIC
  tbl.[2, "pseudo.R2"] <- glmm.gaus.tr.pseudo.R2
  tbl.[2, "R2.Cd"] <- glmm.gaus.tr.R2.Cd
  tbl.[2, "R2.Mg"] <- glmm.gaus.tr.R2.Mg
  
  #tbl.[2, "n"] <- sum(!is.na((data.[,df.[2, "variable"]])))
  tbl.[2, "n"] <- (summary(glmm.gaus.tr))$nobs
  
  tbl.[2, "unif. p"] <- glmm.gaus.tr.unif.p
  tbl.[2, "outl. p"] <- glmm.gaus.tr.outl.p
  tbl.[2, "outl.boot. p"] <- glmm.gaus.tr.outl.boot.p
  tbl.[2, "disp. p"] <- glmm.gaus.tr.disp.p
  tbl.[2, "disp.PeaChi. p"] <- glmm.gaus.tr.disp.PeaChi.p
  tbl.[2, "0infl. p"] <- glmm.gaus.tr.0infl.p
  
  rm("glmm.gaus.tr.intercept.Esti.", "glmm.gaus.tr.intercept.SE", "glmm.gaus.tr.intercept.p", "glmm.gaus.tr.ivr.Esti.", "glmm.gaus.tr.ivr.SE", "glmm.gaus.tr.ivr.p", "glmm.gaus.tr.ivr.Chisq", "glmm.gaus.tr.ivr.multico.",
     "glmm.gaus.tr.rich.Esti.", "glmm.gaus.tr.rich.SE", "glmm.gaus.tr.rich.p", "glmm.gaus.tr.rich.Chisq", "glmm.gaus.tr.rich.multico.", 
     "glmm.gaus.tr.Simp.Esti.", "glmm.gaus.tr.Simp.SE", "glmm.gaus.tr.Simp.p", "glmm.gaus.tr.Simp.Chisq", "glmm.gaus.tr.Simp.multico.", 
     "glmm.gaus.tr.freq.Esti.", "glmm.gaus.tr.freq.SE", "glmm.gaus.tr.freq.p", "glmm.gaus.tr.freq.Chisq", "glmm.gaus.tr.freq.multico.",
     "glmm.gaus.tr.ratio.obs.Esti.", "glmm.gaus.tr.ratio.obs.SE", "glmm.gaus.tr.ratio.obs.p", "glmm.gaus.tr.ratio.obs.Chisq", "glmm.gaus.tr.ratio.obs.multico.",
     "glmm.gaus.tr.rere.Esti.", "glmm.gaus.tr.rere.SE", "glmm.gaus.tr.rere.p", "glmm.gaus.tr.rere.Chisq", "glmm.gaus.tr.rere.multico.",
     "glmm.gaus.tr.renonre.Esti.", "glmm.gaus.tr.renonre.SE", "glmm.gaus.tr.renonre.p", "glmm.gaus.tr.renonre.Chisq", "glmm.gaus.tr.renonre.multico.",
     "glmm.gaus.tr.denonre.Esti.", "glmm.gaus.tr.denonre.SE", "glmm.gaus.tr.denonre.p", "glmm.gaus.tr.denonre.Chisq", "glmm.gaus.tr.denonre.multico.",
     "glmm.gaus.tr.AIC", "glmm.gaus.tr.pseudo.R2", "glmm.gaus.tr.R2.Cd", "glmm.gaus.tr.R2.Mg", "glmm.gaus.tr.unif.p", "glmm.gaus.tr.outl.p", "glmm.gaus.tr.outl.boot.p", "glmm.gaus.tr.disp.p", "glmm.gaus.tr.disp.PeaChi.p", "glmm.gaus.tr.0infl.p")
  
  rm("totalss")
  
  rm("data.", "bn", "bn.cst")
  
  tbl. <<- tbl.
  
}


matri.full.BM.BF_FS <- filter(qecbNato0, Face == "face supérieure")
matri.full.BM.BF_FS <- rename(matri.full.BM.BF_FS, dist. = BM.BF_FS_dist.)
matri.fct.(data. = matri.full.BM.BF_FS, region. = "Atlantique", dataset. = "BM.BF_FS")
tbl.matri.full.BM.BF_FS <- tbl. ; rm(tbl.)

matri.full.BM_FS.FI <- filter(qecbNato0, Type.Bloc == "Bloc mobile")
matri.full.BM_FS.FI <- rename(matri.full.BM_FS.FI, dist. = BM_FS.FI_dist.)
matri.fct.(data. = matri.full.BM_FS.FI, region. = "Atlantique", dataset. = "BM_FS.FI")
tbl.matri.full.BM_FS.FI <- tbl. ; rm(tbl.) 

matri.Bretagne.BM.BF_FS <- filter(qecbNato0, Face == "face supérieure")
matri.Bretagne.BM.BF_FS <- rename(matri.Bretagne.BM.BF_FS, dist. = BM.BF_FS_dist.)
matri.fct.(data. = matri.Bretagne.BM.BF_FS, region. = "Bretagne", dataset. = "BM.BF_FS")
tbl.matri.Bretagne.BM.BF_FS <- tbl. ; rm(tbl.)

matri.Bretagne.BM_FS.FI <- filter(qecbNato0, Type.Bloc == "Bloc mobile")
matri.Bretagne.BM_FS.FI <- rename(matri.Bretagne.BM_FS.FI, dist. = BM_FS.FI_dist.)
matri.fct.(data. = matri.Bretagne.BM_FS.FI, region. = "Bretagne", dataset. = "BM_FS.FI")
tbl.matri.Bretagne.BM_FS.FI <- tbl. ; rm(tbl.) 

matri.EGMP.BASQ.BM.BF_FS <- filter(qecbNato0, Face == "face supérieure")
matri.EGMP.BASQ.BM.BF_FS <- rename(matri.EGMP.BASQ.BM.BF_FS, dist. = BM.BF_FS_dist.)
matri.fct.(data. = matri.EGMP.BASQ.BM.BF_FS, region. = "EGMP.BASQ", dataset. = "BM.BF_FS")
tbl.matri.EGMP.BASQ.BM.BF_FS <- tbl. ; rm(tbl.)

matri.EGMP.BASQ.BM_FS.FI <- filter(qecbNato0, Type.Bloc == "Bloc mobile")
matri.EGMP.BASQ.BM_FS.FI <- rename(matri.EGMP.BASQ.BM_FS.FI, dist. = BM_FS.FI_dist.)
matri.fct.(data. = matri.EGMP.BASQ.BM_FS.FI, region. = "EGMP.BASQ", dataset. = "BM_FS.FI")
tbl.matri.EGMP.BASQ.BM_FS.FI <- tbl. ; rm(tbl.) 


tbl.matri. <- bind_rows(tbl.matri.full.BM.BF_FS, tbl.matri.full.BM_FS.FI)
tbl.matri. <- bind_rows(tbl.matri., tbl.matri.Bretagne.BM.BF_FS)
tbl.matri. <- bind_rows(tbl.matri., tbl.matri.Bretagne.BM_FS.FI)
tbl.matri. <- bind_rows(tbl.matri., tbl.matri.EGMP.BASQ.BM.BF_FS)
tbl.matri. <- bind_rows(tbl.matri., tbl.matri.EGMP.BASQ.BM_FS.FI)
###########################################################

# create OrderNorm dist. variables
hist(qecbNato0$QEBM.2, xlim = c(-360,360), breaks = 1000)
hist(qecbNato0$BM.BF_FS_dist.)
qecbNato0 <- add_column(qecbNato0, BM.BF_FS_dist.orderNorm = bestNormalize::orderNorm(qecbNato0$BM.BF_FS_dist.)$x.t, .after = "BM.BF_FS_dist.")
hist(qecbNato0$BM.BF_FS_dist.orderNorm)
hist(qecbNato0$BM_FS.FI_dist.)
qecbNato0 <- add_column(qecbNato0, BM_FS.FI_dist.orderNorm = bestNormalize::orderNorm(qecbNato0$BM_FS.FI_dist.)$x.t, .after = "BM_FS.FI_dist.")
hist(qecbNato0$BM_FS.FI_dist.orderNorm)

hist(qecbNato0$richness)
bestNormalize(qecbNato0$richness)
qecbNato0 <- add_column(qecbNato0, richness.center_scale = bestNormalize:::center_scale(qecbNato0$richness)$x.t, .after = "richness")
hist(qecbNato0$richness.center_scale)
hist(qecbNato0$Simpson)
bestNormalize(qecbNato0$Simpson)
qecbNato0 <- add_column(qecbNato0, Simpson.orderNorm = bestNormalize::orderNorm(qecbNato0$Simpson)$x.t, .after = "Simpson")
hist(qecbNato0$Simpson.orderNorm)
hist(qecbNato0$Shannon)
bestNormalize(qecbNato0$Shannon)
qecbNato0 <- add_column(qecbNato0, Shannon.orderNorm = bestNormalize::orderNorm(qecbNato0$Shannon)$x.t, .after = "Shannon")
hist(qecbNato0$Shannon.orderNorm)

# scale variables of interest for modeling
qecbNato0 <- add_column(qecbNato0, Sc.Period.nb = scale(qecbNato0$Period.nb), .after = "Period.nb")
qecbNato0 <- add_column(qecbNato0, Sc.blocs.retournes.fr. = scale(qecbNato0$blocs.retournes.fr.), .after = "blocs.retournes.fr.")
qecbNato0 <- add_column(qecbNato0, Sc.oceano.max.3m = scale(qecbNato0$oceano.max.3m), .after = "oceano.max.3m")
qecbNato0 <- add_column(qecbNato0, Sc.freq.max.3m = scale(qecbNato0$freq.max.3m), .after = "freq.max.3m")
qecbNato0 <- add_column(qecbNato0, Sc.max.ratio.norm15min.obs.pre3m = scale(qecbNato0$max.ratio.norm15min.obs.pre3m), .after = "max.ratio.norm15min.obs.pre3m")
qecbNato0 <- add_column(qecbNato0, Sc.geomorphologie.gp.nb = scale(qecbNato0$geomorphologie.gp.nb), .after = "geomorphologie.gp.nb")
qecbNato0 <- add_column(qecbNato0, Sc.accessibilite.gp.nb = scale(qecbNato0$accessibilite.gp.nb), .after = "accessibilite.gp.nb")
qecbNato0 <- add_column(qecbNato0, Sc.roche.gp.nb = scale(qecbNato0$roche.gp.nb), .after = "roche.gp.nb")
qecbNato0 <- add_column(qecbNato0, Sc.taille.bloc.gp.nb = scale(qecbNato0$taille.bloc.gp.nb), .after = "taille.bloc.gp.nb")
qecbNato0 <- add_column(qecbNato0, Sc.frequentation.gp.nb = scale(qecbNato0$frequentation.gp.nb), .after = "frequentation.gp.nb")

# correlation between variables

# some Na,NaN,Inf data might remain in the df ; replace by NA
qecbNato0$QEBM.2 <- ifelse(qecbNato0$QEBM.2 %in% c(NA, NaN, Inf), NA, qecbNato0$QEBM.2) 

pairs2(qecbNato0[, c("BM.BF_FS_dist.", "BM_FS.FI_dist.", "QEBM.2", "blocs.retournes.fr.", "Period.nb", "roche.gp.nb", "geomorphologie.gp.nb", "taille.bloc.gp.nb", "accessibilite.gp.nb", "freq.median.3m", "max.ratio.norm15min.obs.pre3m", "oceano.max.3m")], scaleR = FALSE, Rmethod = 'spearman', reorder = FALSE)

pairs2(na.omit(qecbNato0[, c("richness",  "Simpson", "Shannon", "blocs.retournes.fr.", "Period.nb", "roche.gp.nb", "geomorphologie.gp.nb", "taille.bloc.gp.nb", "accessibilite.gp.nb", "freq.median.3m", "max.ratio.norm15min.obs.pre3m", "oceano.max.3m")], scaleR = FALSE, Rmethod = 'spearman', reorder = FALSE))


## new formula

# NB: I left join according to qecb ecological data, therefore many parameter values were duplicated, e.g. distance, which makes no sense to compare them with e.g. diversity values ...
# I have to check first the df I have makes sense prior analysis ! 
# this is an issue we have to talk about, I don't consider diversity index for now.

matri.fct <- function(data., region., dataset., dist., vars., transfo.) {
  
  # test 1
  #data. <- filter(qecbNato0, Type.Bloc == "Bloc mobile")
  #data. <- rename(data., dist. = BM_FS.FI_dist.orderNorm)
  #transfo. <- "orderNorm"
  #region. <- "Atlantique"
  #dataset. <- "BM_FS.FI"
  #vars. <- c("blocs.retournes.fr.", "oceano.max.3m")

  
  tbl. <- data.frame(matrix(ncol = 22, nrow = 1))
  colnames(tbl.) <- c("region", "dataset", "transformation", "function", "family", "n ef.fix", "formule", "AIC", "pseudo.R2", "R2.Cd", "R2.Mg", "n", "unif. p", "outl. p", "outl.boot. p", "disp. p", "disp.PeaChi. p", "0infl. p", "spat.auto.cor.",
                      "int. Esti.", "int. SE", "int. p")
    
  for (i in c(1:length(vars.))) {
  
        tbl. %>% add_column(Empty_Col1 = NA, Empty_Col2 = NA, Empty_Col3 = NA, Empty_Col4 = NA, Empty_Col5 = NA) -> tbl.
      names(tbl.)[c((ncol(tbl.)-4):ncol(tbl.))] <- c(paste0(vars.[i]," Esti."), paste0(vars.[i]," SE"), paste0(vars.[i]," p"), paste0(vars.[i]," Chisq"), paste0(vars.[i]," multico."))
    
    }
  
  data. <- na.omit(data.[, c(unique(c("dist.", unlist(str_split(vars., ":")))), "Site", "Numero.Quadrat", "Latitude", "Longitude")])
  
  tbl.[1, "region"] <- region.
  tbl.[1, "dataset"] <- dataset.
  tbl.[1, "transformation"] <- transfo.
  tbl.[1, "function"] <- "glmmTMB"
  tbl.[1, "family"] <- "gaussian(link = identity)"
  tbl.[1, "n ef.fix"] <- length(vars.)
  
  tbl. <<- tbl.
  
  frmla <- as.formula(paste(colnames(data.)[1], paste(c(vars., "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  tbl.[1, "formule"] <- paste(colnames(data.)[1], paste(c(vars., "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ ")
  
  glmm.gaus <- suppressWarnings(glmmTMB(frmla, family = gaussian(link = "identity"), data = data.))
  
  show(summary(glmm.gaus))
  show(Anova(glmm.gaus))
  
  # model
  tbl.[1, "AIC"] <- AIC(glmm.gaus)
  # we can generate quasi-R2 values by calculating the ratio of variance in the residuals to total variance in the response
  totalss <- var(resid(glmm.gaus, type='pearson') + predict(glmm.gaus, type='link'))
  tbl.[1, "pseudo.R2"] <- 1 - var(residuals(glmm.gaus, type='pearson'))/(totalss)
  rm("totalss")
  tbl.[1, "R2.Cd"] <- suppressWarnings(model_performance(glmm.gaus)$R2_conditional) #proportion of variance explained by both the fixed and random factors
  tbl.[1, "R2.Mg"] <- suppressWarnings(model_performance(glmm.gaus)$R2_marginal) #proportion of variance explained by the fixed factor(s) alone
  tbl.[1, "n"] <- (summary(glmm.gaus))$nobs
  
  simulationOutput <- suppressWarnings(simulateResiduals(fittedModel = glmm.gaus 
                                                         #,n = 10,
                                                         ,plot = F
                                                         #, refit = TRUE
  )) # obviously, better to use the refit = TRUE argument
  
  # Model fitting diagnostic
  
  plot(simulationOutput)
  #plotQQunif(simulationOutput) # left plot in plot.DHARMa()
  #plotResiduals(simulationOutput) # right plot in plot.DHARMa()
  tbl.[1, "unif. p"] <- testUniformity(simulationOutput, plot = T)["p.value"]
  tbl.[1, "outl. p"] <- testOutliers(simulationOutput, plot = T)["p.value"]
  # For Gaussian distribution: Since the Gaussian has a variance parameter, more dispersion will just be a larger variance parameter... so you don't have overdispersion with the Gaussian. (https://stats.stackexchange.com/questions/144531/overdispersion-in-glm-with-gaussian-distribution)
  tbl.[1, "disp. p"] <- NA
  #tbl.[1, "disp. p"] <- suppressWarnings(testDispersion(glmm.gaus, alternative = "two.sided", type = "DHARMa"))["p.value"] # default: a non-parametric test that compares the variance of the simulated residuals to the observed residuals (default).
  #tbl.[1, "disp. p"] <- sigma(glmm.gaus) # see sigma function for detail of sigma function. The most commonly used GLM families (binomial, poisson) have fixed dispersion parameters which are internally ignored.
  # "A common special case of overdispersion is zero-inflation, which is the situation when more zeros appear in the observation than expected under the fitted model." (https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html#zero-inflation-k-inflation-or-deficits). Since there is no overdispersion for Gaussian distribution, we might consider zero-inflation is not of concern neither I guess. 
  tbl.[1, "0infl. p"] <- NA
  #tbl.[1, "0infl. p"] <- suppressWarnings(testZeroInflation(glmm.gaus))["p.value"]
  glmm.gaus.multico. <- multicollinearity(glmm.gaus)
  recalculateOutput <- recalculateResiduals(simulationOutput, group = data.$Site)
  tbl.[1, "spat.auto.cor."] <- testSpatialAutocorrelation(simulationOutput = recalculateOutput, x = unique(data.$Latitude), y = unique(data.$Longitude))$p.value
  
  # complementary tests to above 5
  tbl.[1, "disp.PeaChi. p"] <- NA
  # idem, you don't have overdispersion with the Gaussian distribution.
  #tbl.[1, "disp.PeaChi. p"] <- suppressWarnings(testDispersion(glmm.gaus, alternative = "greater", type = "PearsonChisq"))["p.value"] # PearsonChisq: popular in the literature, suggested in the glmm Wiki, and implemented in some other R packages such as performance::check_overdispersion; test biased for mixed models towards underdispersion; ecommandation: to test only with alternative = 'greater', i.e. test for overdispersion.
  tbl.[1, "outl.boot. p"] <- suppressWarnings(testOutliers(simulationOutput, type = "bootstrap"))["p.value"] # generate a simulation-based expectation for the outliers; for integer only
  
  # extract stats
  
  # IVR
  tbl.[1, "int. Esti."] <- (summary(glmm.gaus))$coefficients$cond["(Intercept)", "Estimate"]
  tbl.[1, "int. SE"] <- (summary(glmm.gaus))$coefficients$cond["(Intercept)", "Std. Error"]
  tbl.[1, "int. p"] <- (summary(glmm.gaus))$coefficients$cond["(Intercept)", "Pr(>|z|)"]
  
  for (i in c(2:(ncol(data.)-4))) { 
    #i <- 3
    name. <- names(data.[i]) 
    
    tbl.[1, paste0(name., " Esti.")]  <- (summary(glmm.gaus))$coefficients$cond[name., "Estimate"]
    tbl.[1, paste0(name., " SE")]  <- (summary(glmm.gaus))$coefficients$cond[name.,"Std. Error"]
    tbl.[1, paste0(name., " p")] <- (Anova(glmm.gaus))$`Pr(>Chisq)`[rownames(Anova(glmm.gaus)) == name.] #(summary(glmm.gaus))$coefficients$cond[name., "Pr(>|z|)"]
    tbl.[1, paste0(name., " Chisq")] <- (Anova(glmm.gaus))[name., "Chisq"]
    tbl.[1, paste0(name., " multico.")] <- ifelse(is.null(glmm.gaus.multico.) == TRUE, NA, filter(glmm.gaus.multico., Term == name.)[["VIF"]])
    
    rm(name., i)
    
  }
  
  rm(frmla, glmm.gaus, simulationOutput, glmm.gaus.multico.)
  
  tbl. <<- tbl.
  
  cat("\n")
  
}  
  
unique(qecbNato0$Face)
unique(qecbNato0$Type.Bloc)
unique(qecbNato0$Region)

# Don't run below code anymore, cfr two new functions to avoid copy paste when running below previous function
###########################################################

# previous function

vars.fct <- function(vars.list) {

df.red <- filter(qecbNato0, Face == "face supérieure")
df.red <- rename(df.red, dist. = "BM.BF_FS_dist.")
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM.BF_FS", vars. = vars.list, transfo. = NA)
tbl.raw <- tbl. ; rm(tbl.)
bn <- bestNormalize(df.red[, "dist."])
df.red[, "dist."] <- bn$chosen_transform$x.t
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM.BF_FS", vars. = vars.list, transfo. = attributes(bn$chosen_transform)$class[1])
tbl.tr. <- tbl. ; rm(tbl.)
tbl.bind_FS_Bretagne <<- bind_rows(tbl.raw, tbl.tr.)
rm(df.red)

df.red <- filter(qecbNato0, Face == "face supérieure")
df.red <- filter(df.red, Region == "Bretagne")
df.red <- rename(df.red, dist. = "BM.BF_FS_dist.")
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM.BF_FS", vars. = vars.list, transfo. = NA)
tbl.raw <- tbl. ; rm(tbl.)
bn <- bestNormalize(df.red[, "dist."])
df.red[, "dist."] <- bn$chosen_transform$x.t
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM.BF_FS", vars. = vars.list, transfo. = attributes(bn$chosen_transform)$class[1])
tbl.tr. <- tbl. ; rm(tbl.)
tbl.bind_FS_Bretagne <<- bind_rows(tbl.raw, tbl.tr.)
rm(df.red)

df.red <- filter(qecbNato0, Face == "face supérieure")
df.red <- filter(df.red, Region == "EGMP.BASQ")
df.red <- rename(df.red, dist. = "BM.BF_FS_dist.")
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM.BF_FS", vars. = vars.list, transfo. = NA)
tbl.raw <- tbl. ; rm(tbl.)
bn <- bestNormalize(df.red[, "dist."])
df.red[, "dist."] <- bn$chosen_transform$x.t
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM.BF_FS", vars. = vars.list, transfo. = attributes(bn$chosen_transform)$class[1])
tbl.tr. <- tbl. ; rm(tbl.)
tbl.bind_FS_EGMP.BASQ <<- bind_rows(tbl.raw, tbl.tr.)
rm(df.red)

df.red <- filter(qecbNato0, Type.Bloc == "Bloc mobile")
df.red <- rename(df.red, dist. = "BM_FS.FI_dist.")
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM_FS.FI", vars. = vars.list, transfo. = NA)
tbl.raw <- tbl. ; rm(tbl.)
bn <- bestNormalize(df.red[, "dist."])
df.red[, "dist."] <- bn$chosen_transform$x.t
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM_FS.FI", vars. = vars.list, transfo. = attributes(bn$chosen_transform)$class[1])
tbl.tr. <- tbl. ; rm(tbl.)
tbl.bind_BM_Atlantique <<- bind_rows(tbl.raw, tbl.tr.)
rm(df.red)

df.red <- filter(qecbNato0, Type.Bloc == "Bloc mobile")
df.red <- filter(df.red, Region == "Bretagne")
df.red <- rename(df.red, dist. = "BM_FS.FI_dist.")
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM_FS.FI", vars. = vars.list, transfo. = NA)
tbl.raw <- tbl. ; rm(tbl.)
bn <- bestNormalize(df.red[, "dist."])
df.red[, "dist."] <- bn$chosen_transform$x.t
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM_FS.FI", vars. = vars.list, transfo. = attributes(bn$chosen_transform)$class[1])
tbl.tr. <- tbl. ; rm(tbl.)
tbl.bind_BM_Bretagne <<- bind_rows(tbl.raw, tbl.tr.)
rm(df.red)

df.red <- filter(qecbNato0, Type.Bloc == "Bloc mobile")
df.red <- filter(df.red, Region == "EGMP.BASQ")
df.red <- rename(df.red, dist. = "BM_FS.FI_dist.")
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM_FS.FI", vars. = vars.list, transfo. = NA)
tbl.raw <- tbl. ; rm(tbl.)
bn <- bestNormalize(df.red[, "dist."])
df.red[, "dist."] <- bn$chosen_transform$x.t
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM_FS.FI", vars. = vars.list, transfo. = attributes(bn$chosen_transform)$class[1])
tbl.tr. <- tbl. ; rm(tbl.)
tbl.bind_BM_EGMP.BASQ <<- bind_rows(tbl.raw, tbl.tr.)
rm(df.red)

tbl.bind <- bind_rows(tbl.bind_FS_Atlantique, tbl.bind_FS_Bretagne)
tbl.bind <- bind_rows(tbl.bind, tbl.bind_FS_EGMP.BASQ)
tbl.bind <- bind_rows(tbl.bind, tbl.bind_BM_Atlantique)
tbl.bind <- bind_rows(tbl.bind, tbl.bind_BM_Bretagne)
tbl.bind <- bind_rows(tbl.bind, tbl.bind_BM_EGMP.BASQ)

tbl.bind <<- tbl.bind

rm("tbl.bind_FS_Atlantique", "tbl.bind_FS_Bretagne", "tbl.bind_FS_EGMP.BASQ", "tbl.bind_BM_Atlantique", "tbl.bind_BM_Bretagne", "tbl.bind_BM_EGMP.BASQ")

}

# test and comparison with above function
#vars.fct(vars.list = c("blocs.retournes.fr.", "max.ratio.norm15min.obs.pre3m"))
#tbl_ivr.rich.obs. <- tbl.bind ; rm(tbl.bind)
# work fine, now we can test all the possibilities for blocs mobiles blocs fixes face supérieure

# Don't run below code anymore, cfr two new functions to avoid copy paste
vars.fct(vars.list = c("blocs.retournes.fr."))
tbl_ivr <- tbl.bind ; rm(tbl.bind)
#vars.fct(vars.list = c("richness"))
#tbl_rich. <- tbl.bind ; rm(tbl.bind)
vars.fct(vars.list = c("freq.max.3m"))
tbl_freq. <- tbl.bind ; rm(tbl.bind)
vars.fct(vars.list = c("max.ratio.norm15min.obs.pre3m"))
tbl_obs. <- tbl.bind ; rm(tbl.bind)
#tbl.1var <- bind_rows(tbl_ivr, tbl_rich.)
tbl.1var <- bind_rows(tbl_ivr, tbl_freq.)#replace tbl_ivr by tbl.1var if I run model with richness
tbl.1var <- bind_rows(tbl.1var, tbl_obs.)

#vars.fct(vars.list = c("blocs.retournes.fr.", "richness"))
#tbl_ivr.rich. <- tbl.bind ; rm(tbl.bind)
vars.fct(vars.list = c("blocs.retournes.fr.", "freq.max.3m"))
tbl_ivr.freq. <- tbl.bind ; rm(tbl.bind)
vars.fct(vars.list = c("blocs.retournes.fr.", "max.ratio.norm15min.obs.pre3m"))
tbl_ivr.obs. <- tbl.bind ; rm(tbl.bind)
#vars.fct(vars.list = c("richness", "freq.max.3m"))
#tbl_rich.freq. <- tbl.bind ; rm(tbl.bind)
#vars.fct(vars.list = c("richness", "max.ratio.norm15min.obs.pre3m"))
#tbl_rich.obs. <- tbl.bind ; rm(tbl.bind)
# model fails
#vars.fct(vars.list = c("freq.max.3m", "max.ratio.norm15min.obs.pre3m"))
#tbl_freq.obs. <- tbl.bind ; rm(tbl.bind)
#tbl.2vars <- bind_rows(tbl_ivr.rich., tbl_ivr.freq.)
tbl.2vars <- bind_rows(tbl_ivr.freq., tbl_ivr.obs.)#replace tbl_ivr.freq. by tbl.2vars if I run model with richness
#tbl.2vars <- bind_rows(tbl.2vars, tbl_rich.freq.)
#tbl.2vars <- bind_rows(tbl.2vars, tbl_rich.obs.)
#tbl.2vars <- bind_rows(tbl.2vars, tbl_freq.obs.)

#vars.fct(vars.list = c("blocs.retournes.fr.", "richness", "freq.max.3m"))
#tbl_ivr.rich.freq. <- tbl.bind ; rm(tbl.bind)
#vars.fct(vars.list = c("blocs.retournes.fr.", "richness", "max.ratio.norm15min.obs.pre3m"))
#tbl_ivr.rich.obs. <- tbl.bind ; rm(tbl.bind)
# model fails
#vars.fct(vars.list = c("blocs.retournes.fr.", "freq.max.3m", "max.ratio.norm15min.obs.pre3m"))
#tbl_ivr.freq.obs. <- tbl.bind ; rm(tbl.bind)
# model fails
#vars.fct(vars.list = c("richness", "freq.max.3m", "max.ratio.norm15min.obs.pre3m"))
#tbl_rich.freq.obs. <- tbl.bind ; rm(tbl.bind)
#tbl.3vars <- bind_rows(tbl_ivr.rich.freq., tbl_ivr.rich.obs.)
#tbl.3vars <- bind_rows(tbl.3vars, tbl_rich.freq.obs.)

# model fails
#vars.fct(vars.list = c("blocs.retournes.fr.", "richness", "freq.max.3m", "max.ratio.norm15min.obs.pre3m"))
#tbl_ivr.rich.freq.obs. <- tbl.bind ; rm(tbl.bind)
#tbl.4vars. <- tbl_ivr.rich.freq.obs.

tbl <- bind_rows(tbl.1var, tbl.2vars)
#tbl <- bind_rows(tbl, tbl.3vars)
#tbl <- bind_rows(tbl, tbl.4vars)

###########################################################

# new function

# Run again code from here below 

tbl.syn <- data.frame(matrix(ncol = 22, nrow = 1))
colnames(tbl.syn) <- c("region", "dataset", "transformation", "function", "family", "n ef.fix", "formule", "AIC", "pseudo.R2", "R2.Cd", "R2.Mg", "n", "unif. p", "outl. p", "outl.boot. p", "disp. p", "disp.PeaChi. p", "0infl. p", "spat.auto.cor.",
                       "int. Esti.", "int. SE", "int. p")

vars. <- c("blocs.retournes.fr.", "freq.max.3m", "max.ratio.norm15min.obs.pre3m", "oceano.max.3m")

for (i in c(1:length(vars.))) {
  
  tbl.syn %>% add_column(Empty_Col1 = NA, Empty_Col2 = NA, Empty_Col3 = NA, Empty_Col4 = NA, Empty_Col5 = NA) -> tbl.syn
  names(tbl.syn)[c((ncol(tbl.syn)-4):ncol(tbl.syn))] <- c(paste0(vars.[i]," Esti."), paste0(vars.[i]," SE"), paste0(vars.[i]," p"), paste0(vars.[i]," Chisq"), paste0(vars.[i]," multico."))
  
}

rm(vars.)

matri.fct.run <- function(dist.in, Region.in, region.out, dataset.out, vars.list) { #.in are arguments to run the model, .out are text outputs in the table; NB: filter region will not filter anything if "Atlantique" cfr Atlantique is not a region, therefore we model the full dataset.
  
  df.red <- filter(qecbNato0.red, Region %in% Region.in)#e.g. region.in = c("Bretagne", "EGMP.BASQ")
  df.red <- rename(df.red, dist. = dist.in) #e.g. dist. = "BM.BF_FS_dist."
  
  tryCatch(matri.fct(data. = df.red, region. = region.out, dataset. = dataset.out, vars. = vars.list, transfo. = NA),
           error = function(e) {
             cat(paste("raw.dist ~ ", vars.list, "\n"))
             cat("ERROR :",conditionMessage(e), "\n")
             cat("Model Fails", "\n")
             cat("\n")
             # Choose a return value in case of error
             tbl.[, c(7:length(tbl.))] <- NA
           })#e.g. region.out = "Atlantique", dataset.out = "BM.BF_FS", vars. = vars.list
  #tbl.raw <- tbl. ; rm(tbl.)
  tbl.syn <- bind_rows(tbl.syn, tbl.) ; rm(tbl.)
  

  bn <- bestNormalize(df.red[, "dist."])
  df.red[, "dist."] <- bn$chosen_transform$x.t
  bn.oN <- bestNormalize::orderNorm(df.red[, "dist."])
  df.red[, "dist."] <- bn.oN$x.t
  tryCatch(matri.fct(data. = df.red, region. = region.out, dataset. = dataset.out, vars. = vars.list, 
   #                  transfo. = attributes(bn$chosen_transform)$class[1]
                     transfo. = "orderNorm"),
             error = function(e) {
             cat(paste("transf.dist ~ ", vars.list, "\n"))
             cat("ERROR :",conditionMessage(e), "\n")
             cat("Model Fails", "\n")
             cat("\n")
  #           # Choose a return value in case of error
             tbl.[, c(7:length(tbl.))] <- NA
           })
  #tbl.tr. <- tbl. ; rm(tbl.)
  tbl.syn <- bind_rows(tbl.syn, tbl.) ; rm(tbl.)
  #tbl.bind_FS_Atlantique <<- bind_rows(tbl.raw, tbl.tr.)
  
  tbl.syn <<- tbl.syn
  rm(df.red)
  
}

matri.fct.run.vars.BM.BF_FS_dist. <- function(run.vars) {

 matri.fct.run(dist.in = "BM.BF_FS_dist.", Region.in = c("Bretagne", "EGMP.BASQ"), region.out = "Atlantique", dataset.out = "BM.BF_FS", vars.list = run.vars)# keep the c() for Region.in cfr filter %in%

 matri.fct.run(dist.in = "BM.BF_FS_dist.", Region.in = c("Bretagne"), region.out = "Bretagne", dataset.out = "BM.BF_FS", vars.list = run.vars)# keep the c() for Region.in cfr filter %in%

 matri.fct.run(dist.in = "BM.BF_FS_dist.", Region.in = c("EGMP.BASQ"), region.out = "EGMP.BASQ", dataset.out = "BM.BF_FS", vars.list = run.vars)# keep the c() for Region.in cfr filter %in%

}

# the first filter has to be out of the function because had to be applied to face supérieure or bloc mobile

qecbNato0.red <- filter(qecbNato0, Face == "face supérieure")

matri.fct.run.vars.BM.BF_FS_dist.(run.vars = c("blocs.retournes.fr."))
matri.fct.run.vars.BM.BF_FS_dist.(run.vars = c("freq.max.3m"))
matri.fct.run.vars.BM.BF_FS_dist.(run.vars = c("max.ratio.norm15min.obs.pre3m"))
matri.fct.run.vars.BM.BF_FS_dist.(run.vars = c("oceano.max.3m"))

matri.fct.run.vars.BM.BF_FS_dist.(run.vars = c("blocs.retournes.fr.", "freq.max.3m"))
matri.fct.run.vars.BM.BF_FS_dist.(run.vars = c("blocs.retournes.fr.", "max.ratio.norm15min.obs.pre3m"))
matri.fct.run.vars.BM.BF_FS_dist.(run.vars = c("blocs.retournes.fr.", "oceano.max.3m"))
matri.fct.run.vars.BM.BF_FS_dist.(run.vars = c("freq.max.3m", "max.ratio.norm15min.obs.pre3m"))
matri.fct.run.vars.BM.BF_FS_dist.(run.vars = c("freq.max.3m", "oceano.max.3m"))
matri.fct.run.vars.BM.BF_FS_dist.(run.vars = c("max.ratio.norm15min.obs.pre3m", "oceano.max.3m"))

matri.fct.run.vars.BM.BF_FS_dist.(run.vars = c("blocs.retournes.fr.", "freq.max.3m", "max.ratio.norm15min.obs.pre3m"))
matri.fct.run.vars.BM.BF_FS_dist.(run.vars = c("blocs.retournes.fr.", "freq.max.3m", "oceano.max.3m"))
matri.fct.run.vars.BM.BF_FS_dist.(run.vars = c("blocs.retournes.fr.", "max.ratio.norm15min.obs.pre3m", "oceano.max.3m"))
matri.fct.run.vars.BM.BF_FS_dist.(run.vars = c("freq.max.3m", "max.ratio.norm15min.obs.pre3m", "oceano.max.3m"))

matri.fct.run.vars.BM.BF_FS_dist.(run.vars = c("blocs.retournes.fr.", "freq.max.3m", "max.ratio.norm15min.obs.pre3m", "oceano.max.3m"))

matri.fct.run.vars.BM_FS.FI_dist. <- function(run.vars) {
  
  matri.fct.run(dist.in = "BM_FS.FI_dist.", Region.in = c("Bretagne", "EGMP.BASQ"), region.out = "Atlantique", dataset.out = "BM_FS.FI", vars.list = run.vars)# keep the c() for Region.in cfr filter %in%
  
  matri.fct.run(dist.in = "BM_FS.FI_dist.", Region.in = c("Bretagne"), region.out = "Bretagne", dataset.out = "BM_FS.FI", vars.list = run.vars)# keep the c() for Region.in cfr filter %in%
  
  matri.fct.run(dist.in = "BM_FS.FI_dist.", Region.in = c("EGMP.BASQ"), region.out = "EGMP.BASQ", dataset.out = "BM_FS.FI", vars.list = run.vars)# keep the c() for Region.in cfr filter %in%
  
}

# the first filter has to be out of the function because had to be applied to face supérieure or bloc mobile

qecbNato0.red <- filter(qecbNato0, Type.Bloc == "Bloc mobile")

matri.fct.run.vars.BM_FS.FI_dist.(run.vars = c("blocs.retournes.fr."))
matri.fct.run.vars.BM_FS.FI_dist.(run.vars = c("freq.max.3m"))
matri.fct.run.vars.BM_FS.FI_dist.(run.vars = c("max.ratio.norm15min.obs.pre3m"))
matri.fct.run.vars.BM_FS.FI_dist.(run.vars = c("oceano.max.3m"))

matri.fct.run.vars.BM_FS.FI_dist.(run.vars = c("blocs.retournes.fr.", "freq.max.3m"))
matri.fct.run.vars.BM_FS.FI_dist.(run.vars = c("blocs.retournes.fr.", "max.ratio.norm15min.obs.pre3m"))
matri.fct.run.vars.BM_FS.FI_dist.(run.vars = c("blocs.retournes.fr.", "oceano.max.3m"))
matri.fct.run.vars.BM_FS.FI_dist.(run.vars = c("freq.max.3m", "max.ratio.norm15min.obs.pre3m"))
matri.fct.run.vars.BM_FS.FI_dist.(run.vars = c("freq.max.3m", "oceano.max.3m"))
matri.fct.run.vars.BM_FS.FI_dist.(run.vars = c("max.ratio.norm15min.obs.pre3m", "oceano.max.3m"))

matri.fct.run.vars.BM_FS.FI_dist.(run.vars = c("blocs.retournes.fr.", "freq.max.3m", "max.ratio.norm15min.obs.pre3m"))
matri.fct.run.vars.BM_FS.FI_dist.(run.vars = c("blocs.retournes.fr.", "freq.max.3m", "oceano.max.3m"))
matri.fct.run.vars.BM_FS.FI_dist.(run.vars = c("blocs.retournes.fr.", "max.ratio.norm15min.obs.pre3m", "oceano.max.3m"))
matri.fct.run.vars.BM_FS.FI_dist.(run.vars = c("freq.max.3m", "max.ratio.norm15min.obs.pre3m", "oceano.max.3m"))

matri.fct.run.vars.BM_FS.FI_dist.(run.vars = c("blocs.retournes.fr.", "freq.max.3m", "max.ratio.norm15min.obs.pre3m", "oceano.max.3m"))


# Remove tbl.syn empty first line
tbl.syn <- tbl.syn[2:nrow(tbl.syn),]
write.csv(tbl.syn, "results/tbl.syn.csv", row.names = T)
#tbl.syn.fin <- tbl.syn


options(scipen = 999) # remove scientific 10^ format

tbl.syn$AIC <- round(tbl.syn$AIC, digits = 1)
tbl.syn$pseudo.R2 <- round(tbl.syn$pseudo.R2, digits = 4)
tbl.syn$R2.Cd <- round(tbl.syn$R2.Cd, digits = 4)
tbl.syn$R2.Mg <- round(tbl.syn$R2.Mg, digits = 4)
tbl.syn[, c(grep(" multico.", names(tbl.syn), value = TRUE))] <- lapply(tbl.syn[, c(grep(" multico.", names(tbl.syn), value = TRUE))], round, digits = 4)
tbl.syn[, c(grep(" Esti.", names(tbl.syn), value = TRUE))] <- lapply(tbl.syn[, c(grep(" Esti.", names(tbl.syn), value = TRUE))], round, digits = 4)
tbl.syn[, c(grep(" SE", names(tbl.syn), value = TRUE))] <- lapply(tbl.syn[, c(grep(" SE", names(tbl.syn), value = TRUE))], round, digits = 4)
tbl.syn[, c(grep(" p", names(tbl.syn), value = TRUE))] <- lapply(tbl.syn[, c(grep(" p", names(tbl.syn), value = TRUE))], round, digits = 4)
#tbl.syn$`outl.boot. p` <- sprintf("%.4f", tbl.syn$`outl.boot. p`)
tbl.syn[, c(grep("Chisq", names(tbl.syn), value = TRUE))] <- lapply(tbl.syn[, c(grep("Chisq", names(tbl.syn), value = TRUE))], round, digits = 4)

options(scipen = 0, digits = 7) # default

tbl.syn.sprintf <- tbl.syn

options(scipen = 999) # remove scientific 10^ format

tbl.syn.sprintf$AIC <- sprintf("%.1f", tbl.syn.sprintf$AIC)
tbl.syn.sprintf$pseudo.R2 <- sprintf("%.4f", tbl.syn.sprintf$pseudo.R2)
tbl.syn.sprintf$R2.Cd <- sprintf("%.4f", tbl.syn.sprintf$R2.Cd)
tbl.syn.sprintf$R2.Mg <- sprintf("%.4f", tbl.syn.sprintf$R2.Mg)
tbl.syn.sprintf[, c(grep(" multico.", names(tbl.syn.sprintf), value = TRUE))] <- lapply(tbl.syn.sprintf[, c(grep(" multico.", names(tbl.syn.sprintf), value = TRUE))], sprintf, fmt = "%.4f")
tbl.syn.sprintf[, c(grep(" Esti.", names(tbl.syn.sprintf), value = TRUE))] <- lapply(tbl.syn.sprintf[, c(grep(" Esti.", names(tbl.syn.sprintf), value = TRUE))], sprintf, fmt = "%.4f")
tbl.syn.sprintf[, c(grep(" SE", names(tbl.syn.sprintf), value = TRUE))] <- lapply(tbl.syn.sprintf[, c(grep(" SE", names(tbl.syn.sprintf), value = TRUE))], sprintf, fmt = "%.4f")
tbl.syn.sprintf[, c(grep(" p", names(tbl.syn.sprintf), value = TRUE))] <- lapply(tbl.syn.sprintf[, c(grep(" p", names(tbl.syn.sprintf), value = TRUE))], sprintf, fmt = "%.4f")
#tbl.syn.sprintf$`outl.boot. p` <- sprintf("%.4f", tbl.syn.sprintf$`outl.boot. p`)
tbl.syn.sprintf[, c(grep("Chisq", names(tbl.syn.sprintf), value = TRUE))] <- lapply(tbl.syn.sprintf[, c(grep("Chisq", names(tbl.syn.sprintf), value = TRUE))], sprintf, fmt = "%.4f")

options(scipen = 0, digits = 7) # default


tbl.syn.sprintf.Atlantique.tr.BM.BF_FS <- filter(tbl.syn.sprintf, region == "Atlantique", !is.na(tbl.syn$transformation), dataset == "BM.BF_FS")
tbl.syn.sprintf.Atlantique.tr.BM_FS.FI <- filter(tbl.syn.sprintf, region == "Atlantique", !is.na(tbl.syn$transformation), dataset == "BM_FS.FI")

tbl.syn.sprintf.Bretagne.tr.BM.BF_FS <- filter(tbl.syn.sprintf, region == "Bretagne", !is.na(tbl.syn$transformation), dataset == "BM.BF_FS")
tbl.syn.sprintf.Bretagne.tr.BM_FS.FI <- filter(tbl.syn.sprintf, region == "Bretagne", !is.na(tbl.syn$transformation), dataset == "BM_FS.FI")

rm(tbl.syn.sprintf.Atlantique.tr.BM.BF_FS, tbl.syn.sprintf.Atlantique.tr.BM_FS.FI, tbl.syn.sprintf.Bretagne.tr.BM.BF_FS, tbl.syn.sprintf.Bretagne.tr.BM_FS.FI)

rm(list = ls()[!ls() %in% c("qecbNato0", "tbl.syn", "tbl.syn.sprintf", "matri.fct", "var.fct")])

saveRDS(tbl.syn, "results/Ecology/tbl.dist.RDS")
saveRDS(tbl.syn.sprintf, "results/Ecology/tbl.dist.sprintf.RDS")
write.csv(tbl.syn.sprintf, "results/Ecology/tbl.dist.sprintf.csv", row.names = T)

tbl.syn.sprintf <- readRDS("results/Ecology/tbl.dist.sprintf.RDS")

tbl.syn.sprintf.tr <- filter(tbl.syn.sprintf, !is.na(tbl.syn.sprintf$transformation))
tbl.syn.sprintf.tr <- arrange(tbl.syn.sprintf.tr, dataset, region, AIC)
write.csv(tbl.syn.sprintf.tr, "results/Ecology/tbl.syn.sprintf.tr.csv", row.names = T)
tbl.syn.sprintf.tr.Atlantique <- filter(tbl.syn.sprintf.tr, region == "Atlantique")

###########################################################

## NEW SCRIPT for community analysis

#I shorten the name for clarity purpose
qecbNato0 <- readRDS("results/qecbNAto0.ivr.QEBM2.matri.div.freq.obs.oceano.RDS") # NB: in the file name 'NA' becomes 'Na'; didn't want to change it all in the script ...

unique(qecbNato0$Type.Bloc)
length(!is.na(qecbNato0$Type.Bloc))
unique(qecbNato0$Face)
length(!is.na(qecbNato0$Face))
unique(qecbNato0$Region)
length(!is.na(qecbNato0$Region))

# change Year & Numero.Quadrat variable format for modeling purpose first (Year not needed in community fct)
unique(qecbNato0[, c("Numero.Quadrat")])
qecbNato0$Numero.Quadrat <- as.factor(qecbNato0$Numero.Quadrat)
unique(qecbNato0[, c("Year")])
qecbNato0$Year <- as.factor(qecbNato0$Year)
unique(qecbNato0[, c("geomorphologie.gp")])
qecbNato0[, c("geomorphologie.gp")] <- as.factor(qecbNato0[, c("geomorphologie.gp")])
unique(qecbNato0[, c("roche.gp")])
qecbNato0[, c("roche.gp")] <- as.factor(qecbNato0[, c("roche.gp")])
unique(qecbNato0[, c("taille.bloc.gp")])
qecbNato0[, c("taille.bloc.gp")] <- as.factor(qecbNato0[, c("taille.bloc.gp")])
unique(qecbNato0[, c("accessibilite.gp")])
qecbNato0[, c("accessibilite.gp")] <- as.factor(qecbNato0[, c("accessibilite.gp")])
unique(qecbNato0[, c("frequentation.gp")])
qecbNato0[, c("frequentation.gp")] <- as.factor(qecbNato0[, c("frequentation.gp")])

# create OrderNorm dist. variables

qecbNato0[, "QEBM.2"][is.infinite(qecbNato0[, "QEBM.2"])] = NA
qecbNato0[, "QEBM.2"][is.nan(qecbNato0[, "QEBM.2"])] = NA
qecbNato0$QEBM.2.tr <- qecbNato0$QEBM.2
qecbNato0$QEBM.2.tr <- ifelse(qecbNato0$QEBM.2.tr < -360, NA, qecbNato0$QEBM.2.tr)
qecbNato0$QEBM.2.tr <- ifelse(qecbNato0$QEBM.2.tr > 360, NA, qecbNato0$QEBM.2.tr)
hist(qecbNato0$QEBM.2.tr, xlim = c(-360,360), breaks = 1000)
bestNormalize::bestNormalize(qecbNato0$QEBM.2.tr)
qecbNato0$QEBM.2.tr <- bestNormalize::orderNorm(qecbNato0$QEBM.2.tr)$x.t
hist(qecbNato0$QEBM.2.tr)

hist(qecbNato0$BM.BF_FS_dist.)
bestNormalize::bestNormalize(qecbNato0$BM.BF_FS_dist.)
qecbNato0 <- add_column(qecbNato0, BM.BF_FS_dist.tr = bestNormalize::orderNorm(qecbNato0$BM.BF_FS_dist.)$x.t, .after = "BM.BF_FS_dist.")
hist(qecbNato0$BM.BF_FS_dist.tr)
hist(qecbNato0$BM_FS.FI_dist.)
bestNormalize::bestNormalize(qecbNato0$BM_FS.FI_dist.)
qecbNato0 <- add_column(qecbNato0, BM_FS.FI_dist.tr = bestNormalize::orderNorm(qecbNato0$BM_FS.FI_dist.)$x.t, .after = "BM_FS.FI_dist.")
hist(qecbNato0$BM_FS.FI_dist.tr)

hist(qecbNato0$BM.subnorm2.BF_dist.)
bestNormalize::bestNormalize(qecbNato0$BM.subnorm2.BF_dist.)
qecbNato0 <- add_column(qecbNato0, BM.subnorm2.BF_dist.tr = bestNormalize::orderNorm(qecbNato0$BM.subnorm2.BF_dist.)$x.t, .after = "BM.subnorm2.BF_dist.")
hist(qecbNato0$BM.subnorm2.BF_dist.tr)
hist(qecbNato0$BM.sum.BF_dist.)
bestNormalize::bestNormalize(qecbNato0$BM.sum.BF_dist.)
qecbNato0 <- add_column(qecbNato0, BM.sum.BF_dist.tr = bestNormalize::orderNorm(qecbNato0$BM.sum.BF_dist.)$x.t, .after = "BM.sum.BF_dist.")
hist(qecbNato0$BM.sum.BF_dist.tr)
hist(qecbNato0$BM.prod.BF_dist.)
bestNormalize::bestNormalize(qecbNato0$BM.prod.BF_dist.)
qecbNato0 <- add_column(qecbNato0, BM.prod.BF_dist.tr = bestNormalize::orderNorm(qecbNato0$BM.prod.BF_dist.)$x.t, .after = "BM.prod.BF_dist.")
hist(qecbNato0$BM.prod.BF_dist.tr)

hist(qecbNato0$BM.BF_FS_matri.full.median.BM.BF_FS_dist.)
bestNormalize(qecbNato0$BM.BF_FS_matri.full.median.BM.BF_FS_dist.)
qecbNato0 <- add_column(qecbNato0, BM.BF_FS_matri.full.median.BM.BF_FS_dist.tr = bestNormalize::orderNorm(qecbNato0$BM.BF_FS_matri.full.median.BM.BF_FS_dist.)$x.t, .after = "BM.BF_FS_matri.full.median.BM.BF_FS_dist.")
hist(qecbNato0$BM.BF_FS_matri.full.median.BM.BF_FS_dist.tr)
hist(qecbNato0$BM.BF_FS_matri.full.Third.Quartile.BM.BF_FS_dist.)
bestNormalize(qecbNato0$BM.BF_FS_matri.full.Third.Quartile.BM.BF_FS_dist.)
qecbNato0 <- add_column(qecbNato0, BM.BF_FS_matri.full.Third.Quartile.BM.BF_FS_dist.tr = bestNormalize::orderNorm(qecbNato0$BM.BF_FS_matri.full.Third.Quartile.BM.BF_FS_dist.)$x.t, .after = "BM.BF_FS_matri.full.Third.Quartile.BM.BF_FS_dist.")
hist(qecbNato0$BM.BF_FS_matri.full.Third.Quartile.BM.BF_FS_dist.tr)

hist(qecbNato0$BM_FS.FI_matri.full.median.BM_FS.FI_dist.)
bestNormalize(qecbNato0$BM_FS.FI_matri.full.median.BM_FS.FI_dist.)
qecbNato0 <- add_column(qecbNato0, BM_FS.FI_matri.full.median.BM_FS.FI_dist.tr = bestNormalize::orderNorm(qecbNato0$BM_FS.FI_matri.full.median.BM_FS.FI_dist.)$x.t, .after = "BM_FS.FI_matri.full.median.BM_FS.FI_dist.")
hist(qecbNato0$BM_FS.FI_matri.full.median.BM_FS.FI_dist.tr)
hist(qecbNato0$BM_FS.FI_matri.full.Third.Quartile.BM_FS.FI_dist.)
bestNormalize(qecbNato0$BM_FS.FI_matri.full.Third.Quartile.BM_FS.FI_dist.)
qecbNato0 <- add_column(qecbNato0, BM_FS.FI_matri.full.Third.Quartile.BM_FS.FI_dist.tr = bestNormalize::orderNorm(qecbNato0$BM_FS.FI_matri.full.Third.Quartile.BM_FS.FI_dist.)$x.t, .after = "BM_FS.FI_matri.full.Third.Quartile.BM_FS.FI_dist.")
hist(qecbNato0$BM_FS.FI_matri.full.Third.Quartile.BM_FS.FI_dist.tr)

hist(qecbNato0$BM.subnorm2.BF_median_dist.)
bestNormalize::bestNormalize(qecbNato0$BM.subnorm2.BF_median_dist.)
qecbNato0 <- add_column(qecbNato0, BM.subnorm2.BF_median_dist.tr = bestNormalize::orderNorm(qecbNato0$BM.subnorm2.BF_median_dist.)$x.t, .after = "BM.subnorm2.BF_median_dist.")
hist(qecbNato0$BM.subnorm2.BF_median_dist.tr)
hist(qecbNato0$BM.sum.BF_median_dist.)
bestNormalize::bestNormalize(qecbNato0$BM.sum.BF_median_dist.)
qecbNato0 <- add_column(qecbNato0, BM.sum.BF_median_dist.tr = bestNormalize::orderNorm(qecbNato0$BM.sum.BF_median_dist.)$x.t, .after = "BM.sum.BF_median_dist.")
hist(qecbNato0$BM.sum.BF_median_dist.tr)
hist(qecbNato0$BM.prod.BF_median_dist.)
bestNormalize::bestNormalize(qecbNato0$BM.prod.BF_median_dist.)
qecbNato0 <- add_column(qecbNato0, BM.prod.BF_median_dist.tr = bestNormalize::orderNorm(qecbNato0$BM.prod.BF_median_dist.)$x.t, .after = "BM.prod.BF_median_dist.")
hist(qecbNato0$BM.prod.BF_median_dist.tr)

hist(qecbNato0$BM.subnorm2.BF_Third.Quartile_dist.)
bestNormalize::bestNormalize(qecbNato0$BM.subnorm2.BF_Third.Quartile_dist.)
qecbNato0 <- add_column(qecbNato0, BM.subnorm2.BF_Third.Quartile_dist.tr = bestNormalize::orderNorm(qecbNato0$BM.subnorm2.BF_Third.Quartile_dist.)$x.t, .after = "BM.subnorm2.BF_Third.Quartile_dist.")
hist(qecbNato0$BM.subnorm2.BF_Third.Quartile_dist.tr)
hist(qecbNato0$BM.sum.BF_Third.Quartile_dist.)
bestNormalize::bestNormalize(qecbNato0$BM.sum.BF_Third.Quartile_dist.)
qecbNato0 <- add_column(qecbNato0, BM.sum.BF_Third.Quartile_dist.tr = bestNormalize::orderNorm(qecbNato0$BM.sum.BF_Third.Quartile_dist.)$x.t, .after = "BM.sum.BF_Third.Quartile_dist.")
hist(qecbNato0$BM.sum.BF_Third.Quartile_dist.tr)
hist(qecbNato0$BM.prod.BF_Third.Quartile_dist.)
bestNormalize::bestNormalize(qecbNato0$BM.prod.BF_Third.Quartile_dist.)
qecbNato0 <- add_column(qecbNato0, BM.prod.BF_Third.Quartile_dist.tr = bestNormalize::orderNorm(qecbNato0$BM.prod.BF_Third.Quartile_dist.)$x.t, .after = "BM.prod.BF_Third.Quartile_dist.")
hist(qecbNato0$BM.prod.BF_Third.Quartile_dist.tr)

hist(qecbNato0$blocs.retournes.fr.)
bestNormalize(qecbNato0$blocs.retournes.fr.)
qecbNato0 <- add_column(qecbNato0, blocs.retournes.fr.tr = bestNormalize:::orderNorm(qecbNato0$blocs.retournes.fr.)$x.t, .after = "blocs.retournes.fr.")
hist(qecbNato0$blocs.retournes.fr.tr)

hist(qecbNato0$richness)
bestNormalize(qecbNato0$richness)
qecbNato0 <- add_column(qecbNato0, richness.tr = bestNormalize:::center_scale(qecbNato0$richness)$x.t, .after = "richness")
hist(qecbNato0$richness.tr)
hist(qecbNato0$Simpson)
bestNormalize(qecbNato0$Simpson)
qecbNato0 <- add_column(qecbNato0, Simpson.tr = bestNormalize::orderNorm(qecbNato0$Simpson)$x.t, .after = "Simpson")
hist(qecbNato0$Simpson.tr)
hist(qecbNato0$Shannon)
bestNormalize(qecbNato0$Shannon)
qecbNato0 <- add_column(qecbNato0, Shannon.tr = bestNormalize::orderNorm(qecbNato0$Shannon)$x.t, .after = "Shannon")
hist(qecbNato0$Shannon.tr)

# eventually change variable factor level order

unique(qecbNato0[, c("Period", "Period.nb")])
unique(qecbNato0[, c("accessibilite.gp", "accessibilite.gp.nb")] )
unique(qecbNato0[, c("frequentation.gp", "frequentation.gp.nb")])
unique(qecbNato0[, c("taille.bloc.gp", "taille.bloc.gp.nb")])
unique(qecbNato0[, c("geomorphologie.gp", "geomorphologie.gp.nb")])
unique(qecbNato0[, c("roche.gp", "roche.gp.nb")])

# scale variables of interest for modeling
qecbNato0 <- add_column(qecbNato0, Sc.Period.nb = scale(qecbNato0$Period.nb), .after = "Period.nb")
qecbNato0 <- add_column(qecbNato0, Sc.blocs.retournes.fr. = scale(qecbNato0$blocs.retournes.fr.), .after = "blocs.retournes.fr.")
qecbNato0 <- add_column(qecbNato0, Sc.oceano.max.3m = scale(qecbNato0$oceano.max.3m), .after = "oceano.max.3m")
qecbNato0 <- add_column(qecbNato0, Sc.freq.max.3m = scale(qecbNato0$freq.max.3m), .after = "freq.max.3m")
qecbNato0 <- add_column(qecbNato0, Sc.max.ratio.norm15min.obs.pre3m = scale(qecbNato0$max.ratio.norm15min.obs.pre3m), .after = "max.ratio.norm15min.obs.pre3m")
qecbNato0 <- add_column(qecbNato0, Sc.geomorphologie.gp.nb = scale(qecbNato0$geomorphologie.gp.nb), .after = "geomorphologie.gp.nb")
qecbNato0 <- add_column(qecbNato0, Sc.accessibilite.gp.nb = scale(qecbNato0$accessibilite.gp.nb), .after = "accessibilite.gp.nb")
qecbNato0 <- add_column(qecbNato0, Sc.roche.gp.nb = scale(qecbNato0$roche.gp.nb), .after = "roche.gp.nb")
qecbNato0 <- add_column(qecbNato0, Sc.taille.bloc.gp.nb = scale(qecbNato0$taille.bloc.gp.nb), .after = "taille.bloc.gp.nb")
qecbNato0 <- add_column(qecbNato0, Sc.frequentation.gp.nb = scale(qecbNato0$frequentation.gp.nb), .after = "frequentation.gp.nb")
qecbNato0 <- as.data.frame(qecbNato0)

# correlation between variables; idem for data or scaled data

pairs2(na.omit(qecbNato0[, c("BM.BF_FS_dist.", "BM_FS.FI_dist.", "QEBM.2", "blocs.retournes.fr.", "Period.nb", 
                             #"roche.gp.nb", "geomorphologie.gp.nb", 
                             "taille.bloc.gp.nb", "accessibilite.gp.nb", "freq.max.3m", "max.ratio.norm15min.obs.pre3m", "oceano.max.3m")], scaleR = FALSE, Rmethod = 'spearman', reorder = FALSE))

pairs2(na.omit(qecbNato0[, c("BM.BF_FS_dist.", "BM_FS.FI_dist.", "QEBM.2", "Sc.blocs.retournes.fr.", 
                             "Sc.Period.nb", 
                             #"Sc.roche.gp.nb", "Sc.geomorphologie.gp.nb", 
                             "Sc.taille.bloc.gp.nb", "Sc.accessibilite.gp.nb", "Sc.freq.max.3m", "Sc.max.ratio.norm15min.obs.pre3m", "Sc.oceano.max.3m")], scaleR = FALSE, Rmethod = 'spearman', reorder = FALSE))

pairs2(na.omit(qecbNato0[, c("richness",  "Simpson", "Shannon", "blocs.retournes.fr.", 
                             "Period.nb", 
                             #"roche.gp.nb", "geomorphologie.gp.nb", 
                             "taille.bloc.gp.nb", "accessibilite.gp.nb", "freq.max.3m", "max.ratio.norm15min.obs.pre3m", "oceano.max.3m")], scaleR = FALSE, Rmethod = 'spearman', reorder = FALSE))

pairs2(na.omit(qecbNato0[, c("richness",  "Simpson", "Shannon", "Sc.blocs.retournes.fr.", "Sc.Period.nb", 
                             #"Sc.roche.gp.nb", "Sc.geomorphologie.gp.nb", 
                             "Sc.taille.bloc.gp.nb", "Sc.accessibilite.gp.nb", "Sc.freq.max.3m", "Sc.max.ratio.norm15min.obs.pre3m", "Sc.oceano.max.3m")], scaleR = FALSE, Rmethod = 'spearman', reorder = FALSE))




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
model.short.cbi <- unlist(lapply(1:length(model.short),
                                 combinat::combn, 
                                 x = model.short,
                                 simplify = FALSE), 
                          recursive = FALSE)
model.short.cbi       

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
model.medium.cbi <- unlist(lapply(1:length(model.medium),
                                 combinat::combn, 
                                 x = model.medium,
                                 simplify = FALSE), 
                          recursive = FALSE)
model.medium.cbi       

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
model.large.cbi <- unlist(lapply(1:length(model.large),
                                 combinat::combn, 
                                 x = model.large,
                                 simplify = FALSE), 
                          recursive = FALSE)
model.large.cbi   

# We cannot test all the possibilities of model vars. combinations ... It was possible with up to 3 or 4 combinations, but not 10 or more ...
# We will therefore use a step by step glmm model construction procedure later on in the script

rm(model.large.cbi, model.medium.cbi, model.short.cbi)

# model for ivr

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
                , "Sc.frequentation.gp.nb:Sc.taille.bloc.gp.nb") # qual. freq


## formula

matri.fct <- function(data., region., dataset., vars., transfo., frmla.) {
  
  # test 1
  #data. <- df.red
  #transfo. <- NA
  #region. <- "EGMP.BASQ"
  #dataset. <- "QEBM.2"
  #vars. <- vars.list
  #frmla. <- frmla.mod.
  
  #test 2
  #data. <- df.red
  #transfo. <- "ordeNorm"
  #region. <- "Atlantique"
  #dataset. <- "BM_FS.FI_dist.tr"
  #vars. <- vars.list.M1
  #frmla. <- frmla.mod.M1
 
  
  tbl. <- data.frame(matrix(ncol = 23, nrow = 1))
  colnames(tbl.) <- c("region", "dataset", "transformation", "mod.var.", "function", "family", "n ef.fix", "formule", "AIC", "pseudo.R2", "R2.Cd", "R2.Mg", "n", "unif. p", "outl. p", "outl.boot. p", "disp. p", "disp.PeaChi. p", "0infl. p", "spat.auto.cor. p",
                      "int. Esti.", "int. SE", "int. p")
  
  #for (i in c(1:length(vars.))) { # was moved down in the function because of switch between var. names for some interaction terms
    
  #  if (length(vars.) != 0){
    
  #  tbl. %>% add_column(Empty_Col1 = NA, Empty_Col2 = NA, Empty_Col3 = NA, Empty_Col4 = NA, Empty_Col5 = NA) -> tbl.
  #  names(tbl.)[c((ncol(tbl.)-4):ncol(tbl.))] <- c(paste0(vars.[i]," Esti."), paste0(vars.[i]," SE"), paste0(vars.[i]," p"), paste0(vars.[i]," Chisq"), paste0(vars.[i]," multico."))
    
  #  }
    
  #  else {next}
    
  #}
  
  #data. <- na.omit(data.[, c(unique(c("dist.", unlist(str_split(vars., ":")))), "Site", "Numero.Quadrat", "Latitude", "Longitude")])
  
  tbl.[1, "region"] <- region.
  tbl.[1, "dataset"] <- dataset.
  tbl.[1, "transformation"] <- transfo.
  tbl.[1, "mod.var."] <- as.character(frmla.[2])
  tbl.[1, "function"] <- "glmmTMB"
  tbl.[1, "family"] <- "gaussian(link = identity)"
  tbl.[1, "n ef.fix"] <- length(vars.)
  
  tbl. <<- tbl.
  
  #frmla <- frmla.
  tbl.[1, "formule"] <- as.character(paste0(frmla.[2], " ", frmla.[1], " ", frmla.[3]))
  
  #frmla <- as.formula(paste(colnames(data.)[1], paste(c(vars., "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  #tbl.[1, "formule"] <- paste(colnames(data.)[1], paste(c(vars., "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ ")
  
  glmm.gaus <- suppressWarnings(glmmTMB(frmla., family = gaussian(link = "identity"), data = data.))
  
  show(summary(glmm.gaus))
  #show(Anova(glmm.gaus))
  
  # model
  tbl.[1, "AIC"] <- AIC(glmm.gaus)
  # we can generate quasi-R2 values by calculating the ratio of variance in the residuals to total variance in the response
  totalss <- var(resid(glmm.gaus, type='pearson') + predict(glmm.gaus, type='link'))
  tbl.[1, "pseudo.R2"] <- 1 - var(residuals(glmm.gaus, type='pearson'))/(totalss)
  rm("totalss")
  tbl.[1, "R2.Cd"] <- ifelse(is.null(tryCatch(suppressWarnings(model_performance(glmm.gaus)$R2_conditional))), NA, tryCatch(suppressWarnings(model_performance(glmm.gaus)$R2_conditional)))  #proportion of variance explained by both the fixed and random factors
  tbl.[1, "R2.Mg"] <- ifelse(is.null(tryCatch(suppressWarnings(model_performance(glmm.gaus)$R2_marginal))), NA, tryCatch(suppressWarnings(model_performance(glmm.gaus)$R2_marginal))) #proportion of variance explained by the fixed factor(s) alone
  tbl.[1, "n"] <- (summary(glmm.gaus))$nobs
  
  simulationOutput <- suppressWarnings(simulateResiduals(fittedModel = glmm.gaus 
                                                         #,n = 10,
                                                         ,plot = F
                                                         #, refit = TRUE
  )) # obviously, better to use the refit = TRUE argument
  
  # Model fitting diagnostic
  
  plot(simulationOutput)
  #plotQQunif(simulationOutput) # left plot in plot.DHARMa()
  #plotResiduals(simulationOutput) # right plot in plot.DHARMa()
  tbl.[1, "unif. p"] <- testUniformity(simulationOutput, plot = T)["p.value"]
  tbl.[1, "outl. p"] <- testOutliers(simulationOutput, plot = T)["p.value"]
  # For Gaussian distribution: Since the Gaussian has a variance parameter, more dispersion will just be a larger variance parameter... so you don't have overdispersion with the Gaussian. (https://stats.stackexchange.com/questions/144531/overdispersion-in-glm-with-gaussian-distribution)
  tbl.[1, "disp. p"] <- NA
  #tbl.[1, "disp. p"] <- suppressWarnings(testDispersion(glmm.gaus, alternative = "two.sided", type = "DHARMa"))["p.value"] # default: a non-parametric test that compares the variance of the simulated residuals to the observed residuals (default).
  #tbl.[1, "disp. p"] <- sigma(glmm.gaus) # see sigma function for detail of sigma function. The most commonly used GLM families (binomial, poisson) have fixed dispersion parameters which are internally ignored.
  # "A common special case of overdispersion is zero-inflation, which is the situation when more zeros appear in the observation than expected under the fitted model." (https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html#zero-inflation-k-inflation-or-deficits). Since there is no overdispersion for Gaussian distribution, we might consider zero-inflation is not of concern neither I guess. 
  tbl.[1, "0infl. p"] <- NA
  #tbl.[1, "0infl. p"] <- suppressWarnings(testZeroInflation(glmm.gaus))["p.value"]
  glmm.gaus.multico. <- tryCatch(multicollinearity(glmm.gaus), error = function (e) ("model failure"))
  recalculateOutput <- recalculateResiduals(simulationOutput, group = data.$Site)
  tbl.[1, "spat.auto.cor. p"] <- testSpatialAutocorrelation(simulationOutput = recalculateOutput, x = unique(data.$Latitude), y = unique(data.$Longitude))$p.value
  
  # complementary tests to above 5
  tbl.[1, "disp.PeaChi. p"] <- NA
  # idem, you don't have overdispersion with the Gaussian distribution.
  #tbl.[1, "disp.PeaChi. p"] <- suppressWarnings(testDispersion(glmm.gaus, alternative = "greater", type = "PearsonChisq"))["p.value"] # PearsonChisq: popular in the literature, suggested in the glmm Wiki, and implemented in some other R packages such as performance::check_overdispersion; test biased for mixed models towards underdispersion; ecommandation: to test only with alternative = 'greater', i.e. test for overdispersion.
  tbl.[1, "outl.boot. p"] <- suppressWarnings(testOutliers(simulationOutput, type = "bootstrap"))["p.value"] # generate a simulation-based expectation for the outliers; for integer only
  
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
      
      tbl. %>% add_column(Empty_Col1 = NA, Empty_Col2 = NA, Empty_Col3 = NA, Empty_Col4 = NA, Empty_Col5 = NA) -> tbl.
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
    tbl.[1, paste0(name., " p")] <- tryCatch((Anova(glmm.gaus))$`Pr(>Chisq)`[rownames(Anova(glmm.gaus)) == name.], error = function (e) (NA)) #(summary(glmm.gaus))$coefficients$cond[name., "Pr(>|z|)"]
    tbl.[1, paste0(name., " Chisq")] <- tryCatch((Anova(glmm.gaus))[name., "Chisq"], error = function (e) (NA))
    tbl.[1, paste0(name., " multico.")] <- tryCatch(ifelse(is.null(glmm.gaus.multico.) == TRUE, NA, filter(glmm.gaus.multico., Term == name.)[["VIF"]]), error = function (e) (NA))
    
    rm(name., i)
    
    }
    
   else {next}
    
  }
  
  
  rm(glmm.gaus, simulationOutput, glmm.gaus.multico.)
  
  tbl. <<- tbl.
  
  cat("\n")
  
}  


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

# vars. for ivr

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


#library(buildmer)
library(githubinstall)
githubinstall("buildmer")


df. <- qecbNato0[, c("Region", "Site", "Date", "Numero.Quadrat", "Type.Bloc", "Numéro.Bloc.échantillon", "Face", 
                     "QEBM.2.tr",  
                     "BM.BF_FS_dist.tr", "BM_FS.FI_dist.tr",
                     "BM.BF_FS_matri.full.median.BM.BF_FS_dist.tr", "BM.BF_FS_matri.full.Third.Quartile.BM.BF_FS_dist.tr" , "BM_FS.FI_matri.full.median.BM_FS.FI_dist.tr" , "BM_FS.FI_matri.full.Third.Quartile.BM_FS.FI_dist.tr",
                     "BM.subnorm2.BF_dist.tr", "BM.sum.BF_dist.tr", "BM.prod.BF_dist.tr", "BM.subnorm2.BF_median_dist.tr", "BM.sum.BF_median_dist.tr", "BM.prod.BF_median_dist.tr", "BM.subnorm2.BF_Third.Quartile_dist.tr", "BM.sum.BF_Third.Quartile_dist.tr", "BM.prod.BF_Third.Quartile_dist.tr",  
                     "richness.tr", "Simpson.tr", "Shannon.tr",
                     "blocs.retournes.fr.tr",
                     "Sc.Period.nb", 
                     #"Sc.geomorphologie.gp.nb", "Sc.roche.gp.nb",
                     "Sc.taille.bloc.gp.nb",
                     "Sc.blocs.retournes.fr.", 
                     "Sc.oceano.max.3m", "Sc.accessibilite.gp.nb", "Sc.frequentation.gp.nb", "Sc.freq.max.3m", "Sc.max.ratio.norm15min.obs.pre3m", "Latitude", "Longitude")]

#saveRDS(qecbNato0, "qecbNato0.RDS")


## test a step by step procedure

# long version only for below BM_FS.FI_dist.tr.Atl.short e.g
frmla. <- as.formula(paste("BM_FS.FI_dist.tr", paste(c(model.short, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
df.red <- filter(df., Type.Bloc == "Bloc mobile")
BM_FS.FI_dist.tr.Atl.short <- na.omit(df.red[, c("BM_FS.FI_dist.tr", vars.short)])
nrow(BM_FS.FI_dist.tr.Atl.short)
buildglmm. <- buildglmmTMB(frmla., family = gaussian(link = "identity"), data = BM_FS.FI_dist.tr.Atl.short)
summary(buildglmm.)
simulationOutput <- suppressWarnings(simulateResiduals(fittedModel = buildglmm., plot = T))
recalculateOutput <- recalculateResiduals(simulationOutput, group = BM_FS.FI_dist.tr.Atl.short$Site)
testSpatialAutocorrelation(simulationOutput = recalculateOutput, x = unique(BM_FS.FI_dist.tr.Atl.short$Latitude), y = unique(BM_FS.FI_dist.tr.Atl.short$Longitude))
frmla.mod. <- as.formula(as.character(buildglmm.@model$call[2]))
all.vars(as.formula(as.character(buildglmm.@model$call[2])))
unlist(strsplit(trimws(as.character(buildglmm.@model$call[2])), "\\s"))
vars.list <- unlist(strsplit(trimws(as.character(buildglmm.@model$call[2])), "\\s"))[c(grep("Sc.", unlist(strsplit(trimws(as.character(buildglmm.@model$call[2])), "\\s"))))]
matri.fct(data. = BM_FS.FI_dist.tr.Atl.short, region. = "Atlantique", dataset. = "BM_FS.FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- tbl. ; rm(tbl., buildglmm., df.red)

#get.vars.<- function() {
#buildglmm. <<- buildglmmTMB(frmla., family = gaussian(link = "identity"), data = df.red)
#show(summary(buildglmm.))
#frmla.mod. <<- as.formula(as.character(buildglmm.@model$call[2]))
#vars.list <<- unlist(strsplit(trimws(as.character(buildglmm.@model$call[2])), "\\s"))[c(grep("Sc.", unlist(strsplit(trimws(as.character(buildglmm.@model$call[2])), "\\s"))))]
#}

get.vars.<- function() {
  buildglmm. <<- buildglmmTMB(frmla., family = gaussian(link = "identity"), data = df.red)
  show(summary(buildglmm.))
  vars.list <<- unlist(strsplit(trimws(as.character(buildglmm.@model$call[2])), "\\s"))[c(grep("Sc.", unlist(strsplit(trimws(as.character(buildglmm.@model$call[2])), "\\s"))))]
  frmla.mod. <<- as.formula(paste(unlist(strsplit(trimws(as.character(buildglmm.@model$call[2])), "\\s"))[1], paste(c(vars.list, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
}


{ 
### BM_FS.FI_dist.tr

## vars.short

frmla. <- as.formula(paste("BM_FS.FI_dist.tr", paste(c(model.short, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM_FS.FI_dist.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM_FS.FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- tbl.
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM_FS.FI_dist.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM_FS.FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM_FS.FI_dist.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM_FS.FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join <- add_column(tbl.join, vars. = NA, .before = "region")
tbl.join$vars. <- "vars.short"

## vars.medium

frmla. <- as.formula(paste("BM_FS.FI_dist.tr", paste(c(model.medium, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM_FS.FI_dist.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM_FS.FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM_FS.FI_dist.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM_FS.FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM_FS.FI_dist.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM_FS.FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium", tbl.join$vars.)

## vars.large

frmla. <- as.formula(paste("BM_FS.FI_dist.tr", paste(c(model.large, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM_FS.FI_dist.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM_FS.FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM_FS.FI_dist.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM_FS.FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM_FS.FI_dist.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM_FS.FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large", tbl.join$vars.)


### BM.BF_FS_dist.tr

## vars.short

frmla. <- as.formula(paste("BM.BF_FS_dist.tr", paste(c(model.short, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.BF_FS_dist.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM.BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.BF_FS_dist.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM.BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.BF_FS_dist.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM.BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short", tbl.join$vars.)

## vars.medium

frmla. <- as.formula(paste("BM.BF_FS_dist.tr", paste(c(model.medium, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.BF_FS_dist.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM.BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.BF_FS_dist.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM.BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.BF_FS_dist.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM.BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium", tbl.join$vars.)

## vars.large

frmla. <- as.formula(paste("BM.BF_FS_dist.tr", paste(c(model.large, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.BF_FS_dist.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM.BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.BF_FS_dist.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM.BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.BF_FS_dist.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM.BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large", tbl.join$vars.)


### BM.subnorm2.BF_dist.tr

## vars.short

frmla. <- as.formula(paste("BM.subnorm2.BF_dist.tr", paste(c(model.short, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.subnorm2.BF_dist.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM.subnorm2.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.subnorm2.BF_dist.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM.subnorm2.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.subnorm2.BF_dist.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM.subnorm2.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short", tbl.join$vars.)

## vars.medium

frmla. <- as.formula(paste("BM.subnorm2.BF_dist.tr", paste(c(model.medium, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.subnorm2.BF_dist.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM.subnorm2.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.subnorm2.BF_dist.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM.subnorm2.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.subnorm2.BF_dist.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM.subnorm2.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium", tbl.join$vars.)

## vars.large

frmla. <- as.formula(paste("BM.subnorm2.BF_dist.tr", paste(c(model.large, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.subnorm2.BF_dist.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM.subnorm2.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.subnorm2.BF_dist.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM.subnorm2.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.subnorm2.BF_dist.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM.subnorm2.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large", tbl.join$vars.)


### BM.sum.BF_dist.tr

## vars.short

frmla. <- as.formula(paste("BM.sum.BF_dist.tr", paste(c(model.short, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.sum.BF_dist.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM.sum.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.sum.BF_dist.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM.sum.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.sum.BF_dist.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM.sum.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short", tbl.join$vars.)

## vars.medium

frmla. <- as.formula(paste("BM.sum.BF_dist.tr", paste(c(model.medium, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.sum.BF_dist.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM.sum.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.sum.BF_dist.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM.sum.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.sum.BF_dist.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM.sum.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium", tbl.join$vars.)

## vars.large

frmla. <- as.formula(paste("BM.sum.BF_dist.tr", paste(c(model.large, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.sum.BF_dist.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM.sum.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.sum.BF_dist.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM.sum.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.sum.BF_dist.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM.sum.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large", tbl.join$vars.)


### BM.prod.BF_dist.tr

## vars.short

frmla. <- as.formula(paste("BM.prod.BF_dist.tr", paste(c(model.short, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.prod.BF_dist.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM.prod.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.prod.BF_dist.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM.prod.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.prod.BF_dist.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM.prod.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short", tbl.join$vars.)

## vars.medium

frmla. <- as.formula(paste("BM.prod.BF_dist.tr", paste(c(model.medium, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.prod.BF_dist.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM.prod.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.prod.BF_dist.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM.prod.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.prod.BF_dist.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM.prod.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium", tbl.join$vars.)

## vars.large

frmla. <- as.formula(paste("BM.prod.BF_dist.tr", paste(c(model.large, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.prod.BF_dist.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM.prod.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.prod.BF_dist.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM.prod.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.prod.BF_dist.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM.prod.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large", tbl.join$vars.)


### Third.Quartile.BM_FS.FI_dist.tr

## vars.short

frmla. <- as.formula(paste("BM_FS.FI_matri.full.Third.Quartile.BM_FS.FI_dist.tr", paste(c(model.short, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM_FS.FI_matri.full.Third.Quartile.BM_FS.FI_dist.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Third.Quartile.BM_FS.FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM_FS.FI_matri.full.Third.Quartile.BM_FS.FI_dist.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Third.Quartile.BM_FS.FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM_FS.FI_matri.full.Third.Quartile.BM_FS.FI_dist.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Third.Quartile.BM_FS.FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short", tbl.join$vars.)

## vars.medium

frmla. <- as.formula(paste("BM_FS.FI_matri.full.Third.Quartile.BM_FS.FI_dist.tr", paste(c(model.medium, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM_FS.FI_matri.full.Third.Quartile.BM_FS.FI_dist.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Third.Quartile.BM_FS.FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM_FS.FI_matri.full.Third.Quartile.BM_FS.FI_dist.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Third.Quartile.BM_FS.FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM_FS.FI_matri.full.Third.Quartile.BM_FS.FI_dist.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Third.Quartile.BM_FS.FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium", tbl.join$vars.)

## vars.large

frmla. <- as.formula(paste("BM_FS.FI_matri.full.Third.Quartile.BM_FS.FI_dist.tr", paste(c(model.large, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM_FS.FI_matri.full.Third.Quartile.BM_FS.FI_dist.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Third.Quartile.BM_FS.FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM_FS.FI_matri.full.Third.Quartile.BM_FS.FI_dist.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Third.Quartile.BM_FS.FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM_FS.FI_matri.full.Third.Quartile.BM_FS.FI_dist.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Third.Quartile.BM_FS.FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large", tbl.join$vars.)


### "Third.Quartile.BM.BF_FS_dist.tr"

## vars.short

frmla. <- as.formula(paste("BM.BF_FS_matri.full.Third.Quartile.BM.BF_FS_dist.tr", paste(c(model.short, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.BF_FS_matri.full.Third.Quartile.BM.BF_FS_dist.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Third.Quartile.BM.BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.BF_FS_matri.full.Third.Quartile.BM.BF_FS_dist.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Third.Quartile.BM.BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.BF_FS_matri.full.Third.Quartile.BM.BF_FS_dist.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Third.Quartile.BM.BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short", tbl.join$vars.)

## vars.medium

frmla. <- as.formula(paste("BM.BF_FS_matri.full.Third.Quartile.BM.BF_FS_dist.tr", paste(c(model.medium, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.BF_FS_matri.full.Third.Quartile.BM.BF_FS_dist.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Third.Quartile.BM.BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.BF_FS_matri.full.Third.Quartile.BM.BF_FS_dist.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Third.Quartile.BM.BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.BF_FS_matri.full.Third.Quartile.BM.BF_FS_dist.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Third.Quartile.BM.BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium", tbl.join$vars.)

## vars.large

frmla. <- as.formula(paste("BM.BF_FS_matri.full.Third.Quartile.BM.BF_FS_dist.tr", paste(c(model.large, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.BF_FS_matri.full.Third.Quartile.BM.BF_FS_dist.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Third.Quartile.BM.BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.BF_FS_matri.full.Third.Quartile.BM.BF_FS_dist.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Third.Quartile.BM.BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.BF_FS_matri.full.Third.Quartile.BM.BF_FS_dist.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Third.Quartile.BM.BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large", tbl.join$vars.)


### BM.subnorm2.BF_Third.Quartile_dist.tr

## vars.short

frmla. <- as.formula(paste("BM.subnorm2.BF_Third.Quartile_dist.tr", paste(c(model.short, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.subnorm2.BF_Third.Quartile_dist.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Third.Quartile.BM.subnorm2.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.subnorm2.BF_Third.Quartile_dist.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Third.Quartile.BM.subnorm2.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.subnorm2.BF_Third.Quartile_dist.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Third.Quartile.BM.subnorm2.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short", tbl.join$vars.)

## vars.medium

frmla. <- as.formula(paste("BM.subnorm2.BF_Third.Quartile_dist.tr", paste(c(model.medium, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.subnorm2.BF_Third.Quartile_dist.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Third.Quartile.BM.subnorm2.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.subnorm2.BF_Third.Quartile_dist.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Third.Quartile.BM.subnorm2.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.subnorm2.BF_Third.Quartile_dist.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Third.Quartile.BM.subnorm2.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium", tbl.join$vars.)

## vars.large

frmla. <- as.formula(paste("BM.subnorm2.BF_Third.Quartile_dist.tr", paste(c(model.large, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.subnorm2.BF_Third.Quartile_dist.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Third.Quartile.BM.subnorm2.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.subnorm2.BF_Third.Quartile_dist.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Third.Quartile.BM.subnorm2.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.subnorm2.BF_Third.Quartile_dist.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Third.Quartile.BM.subnorm2.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large", tbl.join$vars.)


### BM.sum.BF_Third.Quartile_dist.tr

## vars.short

frmla. <- as.formula(paste("BM.sum.BF_Third.Quartile_dist.tr", paste(c(model.short, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.sum.BF_Third.Quartile_dist.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Third.Quartile.BM.sum.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.sum.BF_Third.Quartile_dist.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Third.Quartile.BM.sum.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.sum.BF_Third.Quartile_dist.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Third.Quartile.BM.sum.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short", tbl.join$vars.)

## vars.medium

frmla. <- as.formula(paste("BM.sum.BF_Third.Quartile_dist.tr", paste(c(model.medium, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.sum.BF_Third.Quartile_dist.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Third.Quartile.BM.sum.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.sum.BF_Third.Quartile_dist.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Third.Quartile.BM.sum.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.sum.BF_Third.Quartile_dist.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Third.Quartile.BM.sum.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium", tbl.join$vars.)

## vars.large

frmla. <- as.formula(paste("BM.sum.BF_Third.Quartile_dist.tr", paste(c(model.large, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.sum.BF_Third.Quartile_dist.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Third.Quartile.BM.sum.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.sum.BF_Third.Quartile_dist.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Third.Quartile.BM.sum.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.sum.BF_Third.Quartile_dist.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Third.Quartile.BM.sum.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large", tbl.join$vars.)


### BM.prod.BF_Third.Quartile_dist.tr

## vars.short

frmla. <- as.formula(paste("BM.prod.BF_Third.Quartile_dist.tr", paste(c(model.short, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.prod.BF_Third.Quartile_dist.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Third.Quartile.BM.prod.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.prod.BF_Third.Quartile_dist.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Third.Quartile.BM.prod.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.prod.BF_Third.Quartile_dist.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Third.Quartile.BM.prod.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short", tbl.join$vars.)

## vars.medium

frmla. <- as.formula(paste("BM.prod.BF_Third.Quartile_dist.tr", paste(c(model.medium, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.prod.BF_Third.Quartile_dist.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Third.Quartile.BM.prod.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.prod.BF_Third.Quartile_dist.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Third.Quartile.BM.prod.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.prod.BF_Third.Quartile_dist.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Third.Quartile.BM.prod.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium", tbl.join$vars.)

## vars.large

frmla. <- as.formula(paste("BM.prod.BF_Third.Quartile_dist.tr", paste(c(model.large, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.prod.BF_Third.Quartile_dist.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Third.Quartile.BM.prod.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.prod.BF_Third.Quartile_dist.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Third.Quartile.BM.prod.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.prod.BF_Third.Quartile_dist.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Third.Quartile.BM.prod.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large", tbl.join$vars.)


### median.BM_FS.FI_dist.tr

## vars.short

frmla. <- as.formula(paste("BM_FS.FI_matri.full.median.BM_FS.FI_dist.tr", paste(c(model.short, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM_FS.FI_matri.full.median.BM_FS.FI_dist.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "median.BM_FS.FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM_FS.FI_matri.full.median.BM_FS.FI_dist.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "median.BM_FS.FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM_FS.FI_matri.full.median.BM_FS.FI_dist.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "median.BM_FS.FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short", tbl.join$vars.)

## vars.medium

frmla. <- as.formula(paste("BM_FS.FI_matri.full.median.BM_FS.FI_dist.tr", paste(c(model.medium, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM_FS.FI_matri.full.median.BM_FS.FI_dist.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "median.BM_FS.FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM_FS.FI_matri.full.median.BM_FS.FI_dist.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "median.BM_FS.FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM_FS.FI_matri.full.median.BM_FS.FI_dist.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "median.BM_FS.FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium", tbl.join$vars.)

## vars.large

frmla. <- as.formula(paste("BM_FS.FI_matri.full.median.BM_FS.FI_dist.tr", paste(c(model.large, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM_FS.FI_matri.full.median.BM_FS.FI_dist.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "median.BM_FS.FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM_FS.FI_matri.full.median.BM_FS.FI_dist.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "median.BM_FS.FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM_FS.FI_matri.full.median.BM_FS.FI_dist.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "median.BM_FS.FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large", tbl.join$vars.)


### "median.BM.BF_FS_dist.tr"

## vars.short

frmla. <- as.formula(paste("BM.BF_FS_matri.full.median.BM.BF_FS_dist.tr", paste(c(model.short, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.BF_FS_matri.full.median.BM.BF_FS_dist.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "median.BM.BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.BF_FS_matri.full.median.BM.BF_FS_dist.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "median.BM.BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.BF_FS_matri.full.median.BM.BF_FS_dist.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "median.BM.BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short", tbl.join$vars.)

## vars.medium

frmla. <- as.formula(paste("BM.BF_FS_matri.full.median.BM.BF_FS_dist.tr", paste(c(model.medium, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.BF_FS_matri.full.median.BM.BF_FS_dist.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "median.BM.BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.BF_FS_matri.full.median.BM.BF_FS_dist.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "median.BM.BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.BF_FS_matri.full.median.BM.BF_FS_dist.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "median.BM.BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium", tbl.join$vars.)

## vars.large

frmla. <- as.formula(paste("BM.BF_FS_matri.full.median.BM.BF_FS_dist.tr", paste(c(model.large, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.BF_FS_matri.full.median.BM.BF_FS_dist.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "median.BM.BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.BF_FS_matri.full.median.BM.BF_FS_dist.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "median.BM.BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.BF_FS_matri.full.median.BM.BF_FS_dist.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "median.BM.BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large", tbl.join$vars.)


### BM.subnorm2.BF_median_dist.tr

## vars.short

frmla. <- as.formula(paste("BM.subnorm2.BF_median_dist.tr", paste(c(model.short, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.subnorm2.BF_median_dist.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "median.BM.subnorm2.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.subnorm2.BF_median_dist.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "median.BM.subnorm2.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.subnorm2.BF_median_dist.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "median.BM.subnorm2.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short", tbl.join$vars.)

## vars.medium

frmla. <- as.formula(paste("BM.subnorm2.BF_median_dist.tr", paste(c(model.medium, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.subnorm2.BF_median_dist.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "median.BM.subnorm2.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.subnorm2.BF_median_dist.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "median.BM.subnorm2.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.subnorm2.BF_median_dist.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "median.BM.subnorm2.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium", tbl.join$vars.)

## vars.large

frmla. <- as.formula(paste("BM.subnorm2.BF_median_dist.tr", paste(c(model.large, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.subnorm2.BF_median_dist.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "median.BM.subnorm2.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.subnorm2.BF_median_dist.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "median.BM.subnorm2.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.subnorm2.BF_median_dist.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "median.BM.subnorm2.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large", tbl.join$vars.)


### BM.sum.BF_median_dist.tr

## vars.short

frmla. <- as.formula(paste("BM.sum.BF_median_dist.tr", paste(c(model.short, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.sum.BF_median_dist.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "median.BM.sum.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.sum.BF_median_dist.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "median.BM.sum.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.sum.BF_median_dist.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "median.BM.sum.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short", tbl.join$vars.)

## vars.medium

frmla. <- as.formula(paste("BM.sum.BF_median_dist.tr", paste(c(model.medium, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.sum.BF_median_dist.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "median.BM.sum.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.sum.BF_median_dist.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "median.BM.sum.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.sum.BF_median_dist.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "median.BM.sum.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium", tbl.join$vars.)

## vars.large

frmla. <- as.formula(paste("BM.sum.BF_median_dist.tr", paste(c(model.large, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.sum.BF_median_dist.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "median.BM.sum.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.sum.BF_median_dist.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "median.BM.sum.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.sum.BF_median_dist.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "median.BM.sum.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large", tbl.join$vars.)


### BM.prod.BF_median_dist.tr

## vars.short

frmla. <- as.formula(paste("BM.prod.BF_median_dist.tr", paste(c(model.short, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.prod.BF_median_dist.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "median.BM.prod.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.prod.BF_median_dist.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "median.BM.prod.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.prod.BF_median_dist.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "median.BM.prod.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short", tbl.join$vars.)

## vars.medium

frmla. <- as.formula(paste("BM.prod.BF_median_dist.tr", paste(c(model.medium, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.prod.BF_median_dist.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "median.BM.prod.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.prod.BF_median_dist.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "median.BM.prod.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.prod.BF_median_dist.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "median.BM.prod.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium", tbl.join$vars.)

## vars.large

frmla. <- as.formula(paste("BM.prod.BF_median_dist.tr", paste(c(model.large, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.prod.BF_median_dist.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "median.BM.prod.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.prod.BF_median_dist.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "median.BM.prod.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.prod.BF_median_dist.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "median.BM.prod.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large", tbl.join$vars.)


### richness.tr face supérieure bloc mobile

## vars.short

frmla. <- as.formula(paste("richness.tr", paste(c(model.short, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Face == "face supérieure")[, c("richness.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "richness BM_FS", vars. = vars.list, transfo. = "center_scale", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne", Face == "face supérieure")[, c("richness.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "richness BM_FS", vars. = vars.list, transfo. = "center_scale", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ", Face == "face supérieure")[, c("richness.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "richness BM_FS", vars. = vars.list, transfo. = "center_scale", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short", tbl.join$vars.)

## vars.medium

frmla. <- as.formula(paste("richness.tr", paste(c(model.medium, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Face == "face supérieure")[, c("richness.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "richness BM_FS", vars. = vars.list, transfo. = "center_scale", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne", Face == "face supérieure")[, c("richness.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "richness BM_FS", vars. = vars.list, transfo. = "center_scale", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ", Face == "face supérieure")[, c("richness.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "richness BM_FS", vars. = vars.list, transfo. = "center_scale", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium", tbl.join$vars.)

## vars.large

frmla. <- as.formula(paste("richness.tr", paste(c(model.large, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Face == "face supérieure")[, c("richness.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "richness BM_FS", vars. = vars.list, transfo. = "center_scale", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne", Face == "face supérieure")[, c("richness.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "richness BM_FS", vars. = vars.list, transfo. = "center_scale", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ", Face == "face supérieure")[, c("richness.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "richness BM_FS", vars. = vars.list, transfo. = "center_scale", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large", tbl.join$vars.)


### richness.tr face inférieure bloc mobile

## vars.short

frmla. <- as.formula(paste("richness.tr", paste(c(model.short, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Face == "face inférieure")[, c("richness.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "richness BM_FI", vars. = vars.list, transfo. = "center_scale", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne", Face == "face inférieure")[, c("richness.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "richness BM_FI", vars. = vars.list, transfo. = "center_scale", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ", Face == "face inférieure")[, c("richness.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "richness BM_FI", vars. = vars.list, transfo. = "center_scale", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short", tbl.join$vars.)

## vars.medium

frmla. <- as.formula(paste("richness.tr", paste(c(model.medium, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Face == "face inférieure")[, c("richness.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "richness BM_FI", vars. = vars.list, transfo. = "center_scale", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne", Face == "face inférieure")[, c("richness.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "richness BM_FI", vars. = vars.list, transfo. = "center_scale", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ", Face == "face inférieure")[, c("richness.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "richness BM_FI", vars. = vars.list, transfo. = "center_scale", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium", tbl.join$vars.)

## vars.large

frmla. <- as.formula(paste("richness.tr", paste(c(model.large, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Face == "face inférieure")[, c("richness.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "richness BM_FI", vars. = vars.list, transfo. = "center_scale", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne", Face == "face inférieure")[, c("richness.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "richness BM_FI", vars. = vars.list, transfo. = "center_scale", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ", Face == "face inférieure")[, c("richness.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "richness BM_FI", vars. = vars.list, transfo. = "center_scale", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large", tbl.join$vars.)


### richness.tr face supérieure bloc fixé

## vars.short

frmla. <- as.formula(paste("richness.tr", paste(c(model.short, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Face == "face supérieure")[, c("richness.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "richness BF_FS", vars. = vars.list, transfo. = "center_scale", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Region == "Bretagne", Face == "face supérieure")[, c("richness.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "richness BF_FS", vars. = vars.list, transfo. = "center_scale", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Region == "EGMP.BASQ", Face == "face supérieure")[, c("richness.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "richness BF_FS", vars. = vars.list, transfo. = "center_scale", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short", tbl.join$vars.)

## vars.medium

frmla. <- as.formula(paste("richness.tr", paste(c(model.medium, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Face == "face supérieure")[, c("richness.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "richness BF_FS", vars. = vars.list, transfo. = "center_scale", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Region == "Bretagne", Face == "face supérieure")[, c("richness.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "richness BF_FS", vars. = vars.list, transfo. = "center_scale", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Region == "EGMP.BASQ", Face == "face supérieure")[, c("richness.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "richness BF_FS", vars. = vars.list, transfo. = "center_scale", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium", tbl.join$vars.)

## vars.large

frmla. <- as.formula(paste("richness.tr", paste(c(model.large, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Face == "face supérieure")[, c("richness.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "richness BF_FS", vars. = vars.list, transfo. = "center_scale", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Region == "Bretagne", Face == "face supérieure")[, c("richness.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "richness BF_FS", vars. = vars.list, transfo. = "center_scale", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Region == "EGMP.BASQ", Face == "face supérieure")[, c("richness.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "richness BF_FS", vars. = vars.list, transfo. = "center_scale", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large", tbl.join$vars.)


### Shannon.tr face supérieure bloc mobile

## vars.short

frmla. <- as.formula(paste("Shannon.tr", paste(c(model.short, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Face == "face supérieure")[, c("Shannon.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Shannon BM_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne", Face == "face supérieure")[, c("Shannon.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Shannon BM_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ", Face == "face supérieure")[, c("Shannon.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Shannon BM_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short", tbl.join$vars.)

## vars.medium

frmla. <- as.formula(paste("Shannon.tr", paste(c(model.medium, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Face == "face supérieure")[, c("Shannon.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Shannon BM_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne", Face == "face supérieure")[, c("Shannon.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Shannon BM_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ", Face == "face supérieure")[, c("Shannon.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Shannon BM_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium", tbl.join$vars.)

## vars.large

frmla. <- as.formula(paste("Shannon.tr", paste(c(model.large, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Face == "face supérieure")[, c("Shannon.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Shannon BM_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne", Face == "face supérieure")[, c("Shannon.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Shannon BM_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ", Face == "face supérieure")[, c("Shannon.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Shannon BM_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large", tbl.join$vars.)


### Shannon.tr face inférieure bloc mobile

## vars.short

frmla. <- as.formula(paste("Shannon.tr", paste(c(model.short, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Face == "face inférieure")[, c("Shannon.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Shannon BM_FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne", Face == "face inférieure")[, c("Shannon.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Shannon BM_FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ", Face == "face inférieure")[, c("Shannon.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Shannon BM_FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short", tbl.join$vars.)

## vars.medium

frmla. <- as.formula(paste("Shannon.tr", paste(c(model.medium, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Face == "face inférieure")[, c("Shannon.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Shannon BM_FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne", Face == "face inférieure")[, c("Shannon.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Shannon BM_FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ", Face == "face inférieure")[, c("Shannon.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Shannon BM_FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium", tbl.join$vars.)

## vars.large

frmla. <- as.formula(paste("Shannon.tr", paste(c(model.large, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Face == "face inférieure")[, c("Shannon.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Shannon BM_FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne", Face == "face inférieure")[, c("Shannon.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Shannon BM_FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ", Face == "face inférieure")[, c("Shannon.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Shannon BM_FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large", tbl.join$vars.)


### Shannon.tr face supérieure bloc fixé

## vars.short

frmla. <- as.formula(paste("Shannon.tr", paste(c(model.short, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Face == "face supérieure")[, c("Shannon.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Shannon BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Region == "Bretagne", Face == "face supérieure")[, c("Shannon.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Shannon BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Region == "EGMP.BASQ", Face == "face supérieure")[, c("Shannon.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Shannon BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short", tbl.join$vars.)

## vars.medium

frmla. <- as.formula(paste("Shannon.tr", paste(c(model.medium, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Face == "face supérieure")[, c("Shannon.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Shannon BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Region == "Bretagne", Face == "face supérieure")[, c("Shannon.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Shannon BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Region == "EGMP.BASQ", Face == "face supérieure")[, c("Shannon.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Shannon BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium", tbl.join$vars.)

## vars.large

frmla. <- as.formula(paste("Shannon.tr", paste(c(model.large, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Face == "face supérieure")[, c("Shannon.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Shannon BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Region == "Bretagne", Face == "face supérieure")[, c("Shannon.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Shannon BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Region == "EGMP.BASQ", Face == "face supérieure")[, c("Shannon.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Shannon BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large", tbl.join$vars.)


### Simpson.tr face supérieure bloc mobile

## vars.short

frmla. <- as.formula(paste("Simpson.tr", paste(c(model.short, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Face == "face supérieure")[, c("Simpson.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Simpson BM_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne", Face == "face supérieure")[, c("Simpson.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Simpson BM_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ", Face == "face supérieure")[, c("Simpson.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Simpson BM_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short", tbl.join$vars.)

## vars.medium

frmla. <- as.formula(paste("Simpson.tr", paste(c(model.medium, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Face == "face supérieure")[, c("Simpson.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Simpson BM_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne", Face == "face supérieure")[, c("Simpson.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Simpson BM_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ", Face == "face supérieure")[, c("Simpson.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Simpson BM_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium", tbl.join$vars.)

## vars.large

frmla. <- as.formula(paste("Simpson.tr", paste(c(model.large, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Face == "face supérieure")[, c("Simpson.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Simpson BM_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne", Face == "face supérieure")[, c("Simpson.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Simpson BM_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ", Face == "face supérieure")[, c("Simpson.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Simpson BM_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large", tbl.join$vars.)


### Simpson.tr face inférieure bloc mobile

## vars.short

frmla. <- as.formula(paste("Simpson.tr", paste(c(model.short, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Face == "face inférieure")[, c("Simpson.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Simpson BM_FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne", Face == "face inférieure")[, c("Simpson.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Simpson BM_FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ", Face == "face inférieure")[, c("Simpson.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Simpson BM_FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short", tbl.join$vars.)

## vars.medium

frmla. <- as.formula(paste("Simpson.tr", paste(c(model.medium, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Face == "face inférieure")[, c("Simpson.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Simpson BM_FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne", Face == "face inférieure")[, c("Simpson.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Simpson BM_FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ", Face == "face inférieure")[, c("Simpson.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Simpson BM_FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium", tbl.join$vars.)

## vars.large

frmla. <- as.formula(paste("Simpson.tr", paste(c(model.large, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Face == "face inférieure")[, c("Simpson.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Simpson BM_FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne", Face == "face inférieure")[, c("Simpson.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Simpson BM_FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ", Face == "face inférieure")[, c("Simpson.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Simpson BM_FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large", tbl.join$vars.)


### Simpson.tr face supérieure bloc fixé

## vars.short

frmla. <- as.formula(paste("Simpson.tr", paste(c(model.short, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Face == "face supérieure")[, c("Simpson.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Simpson BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Region == "Bretagne", Face == "face supérieure")[, c("Simpson.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Simpson BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Region == "EGMP.BASQ", Face == "face supérieure")[, c("Simpson.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Simpson BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short", tbl.join$vars.)

## vars.medium

frmla. <- as.formula(paste("Simpson.tr", paste(c(model.medium, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Face == "face supérieure")[, c("Simpson.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Simpson BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Region == "Bretagne", Face == "face supérieure")[, c("Simpson.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Simpson BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Region == "EGMP.BASQ", Face == "face supérieure")[, c("Simpson.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Simpson BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium", tbl.join$vars.)

## vars.large

frmla. <- as.formula(paste("Simpson.tr", paste(c(model.large, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Face == "face supérieure")[, c("Simpson.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Simpson BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Region == "Bretagne", Face == "face supérieure")[, c("Simpson.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Simpson BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Region == "EGMP.BASQ", Face == "face supérieure")[, c("Simpson.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Simpson BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large", tbl.join$vars.)


### QEBM.2.tr bloc mobile

## vars.short

frmla. <- as.formula(paste("QEBM.2.tr", paste(c(model.short, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("QEBM.2.tr", vars.short)])
#filter(df.red, QEBM.2 >= -360) -> df.red 
#filter(df.red, QEBM.2 <= 360) -> df.red 
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "QEBM.2", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("QEBM.2.tr", vars.short)])
#filter(df.red, QEBM.2 >= -360) -> df.red 
#filter(df.red, QEBM.2 <= 360) -> df.red 
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "QEBM.2", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("QEBM.2.tr", vars.short)])
#filter(df.red, QEBM.2 >= -360) -> df.red 
#filter(df.red, QEBM.2 <= 360) -> df.red 
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "QEBM.2", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short", tbl.join$vars.)

## vars.medium

frmla. <- as.formula(paste("QEBM.2.tr", paste(c(model.medium, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("QEBM.2.tr", vars.medium)])
#filter(df.red, QEBM.2 >= -360) -> df.red 
#filter(df.red, QEBM.2 <= 360) -> df.red 
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "QEBM.2", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("QEBM.2.tr", vars.medium)])
#filter(df.red, QEBM.2 >= -360) -> df.red 
#filter(df.red, QEBM.2 <= 360) -> df.red 
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "QEBM.2", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("QEBM.2.tr", vars.medium)])
#filter(df.red, QEBM.2 >= -360) -> df.red 
#filter(df.red, QEBM.2 <= 360) -> df.red 
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "QEBM.2", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium", tbl.join$vars.)

## vars.large

frmla. <- as.formula(paste("QEBM.2.tr", paste(c(model.large, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("QEBM.2.tr", vars.large)])
#filter(df.red, QEBM.2 >= -360) -> df.red 
#filter(df.red, QEBM.2 <= 360) -> df.red 
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "QEBM.2", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("QEBM.2.tr", vars.large)])
#filter(df.red, QEBM.2 >= -360) -> df.red 
#filter(df.red, QEBM.2 <= 360) -> df.red 
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "QEBM.2", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("QEBM.2.tr", vars.large)])
#filter(df.red, QEBM.2 >= -360) -> df.red 
#filter(df.red, QEBM.2 <= 360) -> df.red 
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "QEBM.2", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large", tbl.join$vars.)
}


# Let's add ivr modeling next, if removed from the model.large.vs.medium.vs.short vectors

{
### blocs.retournes.fr.tr

## vars.short

frmla. <- as.formula(paste("blocs.retournes.fr.tr", paste(c(model.short.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("blocs.retournes.fr.tr", vars.short.ivr)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "blocs.retournes.fr.", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("blocs.retournes.fr.tr", vars.short.ivr)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "blocs.retournes.fr.", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("blocs.retournes.fr.tr", vars.short.ivr)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "blocs.retournes.fr.", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short.ivr", tbl.join$vars.)

## vars.medium

frmla. <- as.formula(paste("blocs.retournes.fr.tr", paste(c(model.medium.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("blocs.retournes.fr.tr", vars.medium.ivr)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "blocs.retournes.fr.", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("blocs.retournes.fr.tr", vars.medium.ivr)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "blocs.retournes.fr.", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("blocs.retournes.fr.tr", vars.medium.ivr)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "blocs.retournes.fr.", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium.ivr", tbl.join$vars.)

## vars.large

frmla. <- as.formula(paste("blocs.retournes.fr.tr", paste(c(model.large.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("blocs.retournes.fr.tr", vars.large.ivr)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "blocs.retournes.fr.", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("blocs.retournes.fr.tr", vars.large.ivr)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "blocs.retournes.fr.", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("blocs.retournes.fr.tr", vars.large.ivr)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "blocs.retournes.fr.", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large.ivr", tbl.join$vars.)
}


# and now remove ivr from model to test the effect of the other variables ivr excluded

{ 
  ### BM_FS.FI_dist.tr
  
  ## vars.short.ivr
  
  frmla. <- as.formula(paste("BM_FS.FI_dist.tr", paste(c(model.short.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM_FS.FI_dist.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM_FS.FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM_FS.FI_dist.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM_FS.FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM_FS.FI_dist.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM_FS.FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short.ivr", tbl.join$vars.)
  
  ## vars.medium.ivr
  
  frmla. <- as.formula(paste("BM_FS.FI_dist.tr", paste(c(model.medium.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM_FS.FI_dist.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM_FS.FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM_FS.FI_dist.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM_FS.FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM_FS.FI_dist.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM_FS.FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium.ivr", tbl.join$vars.)
  
  ## vars.large.ivr
  
  frmla. <- as.formula(paste("BM_FS.FI_dist.tr", paste(c(model.large.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM_FS.FI_dist.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM_FS.FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM_FS.FI_dist.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM_FS.FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM_FS.FI_dist.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM_FS.FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large.ivr", tbl.join$vars.)
  
  
  ### BM.BF_FS_dist.tr
  
  ## vars.short.ivr
  
  frmla. <- as.formula(paste("BM.BF_FS_dist.tr", paste(c(model.short.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.BF_FS_dist.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM.BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.BF_FS_dist.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM.BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.BF_FS_dist.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM.BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short.ivr", tbl.join$vars.)
  
  ## vars.medium.ivr
  
  frmla. <- as.formula(paste("BM.BF_FS_dist.tr", paste(c(model.medium.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.BF_FS_dist.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM.BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.BF_FS_dist.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM.BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.BF_FS_dist.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM.BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium.ivr", tbl.join$vars.)
  
  ## vars.large.ivr
  
  frmla. <- as.formula(paste("BM.BF_FS_dist.tr", paste(c(model.large.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.BF_FS_dist.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM.BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.BF_FS_dist.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM.BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.BF_FS_dist.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM.BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large.ivr", tbl.join$vars.)
  
  
  ### BM.subnorm2.BF_dist.tr
  
  ## vars.short.ivr
  
  frmla. <- as.formula(paste("BM.subnorm2.BF_dist.tr", paste(c(model.short.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.subnorm2.BF_dist.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM.subnorm2.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.subnorm2.BF_dist.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM.subnorm2.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.subnorm2.BF_dist.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM.subnorm2.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short.ivr", tbl.join$vars.)
  
  ## vars.medium.ivr
  
  frmla. <- as.formula(paste("BM.subnorm2.BF_dist.tr", paste(c(model.medium.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.subnorm2.BF_dist.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM.subnorm2.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.subnorm2.BF_dist.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM.subnorm2.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.subnorm2.BF_dist.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM.subnorm2.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium", tbl.join$vars.)
  
  ## vars.large.ivr
  
  frmla. <- as.formula(paste("BM.subnorm2.BF_dist.tr", paste(c(model.large.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.subnorm2.BF_dist.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM.subnorm2.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.subnorm2.BF_dist.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM.subnorm2.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.subnorm2.BF_dist.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM.subnorm2.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large.ivr", tbl.join$vars.)
  
  
  ### BM.sum.BF_dist.tr
  
  ## vars.short.ivr
  
  frmla. <- as.formula(paste("BM.sum.BF_dist.tr", paste(c(model.short.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.sum.BF_dist.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM.sum.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.sum.BF_dist.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM.sum.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.sum.BF_dist.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM.sum.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short.ivr", tbl.join$vars.)
  
  ## vars.medium.ivr
  
  frmla. <- as.formula(paste("BM.sum.BF_dist.tr", paste(c(model.medium.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.sum.BF_dist.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM.sum.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.sum.BF_dist.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM.sum.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.sum.BF_dist.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM.sum.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium.ivr", tbl.join$vars.)
  
  ## vars.large.ivr
  
  frmla. <- as.formula(paste("BM.sum.BF_dist.tr", paste(c(model.large.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.sum.BF_dist.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM.sum.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.sum.BF_dist.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM.sum.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.sum.BF_dist.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM.sum.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large.ivr", tbl.join$vars.)
  
  
  ### BM.prod.BF_dist.tr
  
  ## vars.short.ivr
  
  frmla. <- as.formula(paste("BM.prod.BF_dist.tr", paste(c(model.short.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.prod.BF_dist.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM.prod.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.prod.BF_dist.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM.prod.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.prod.BF_dist.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM.prod.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short.ivr", tbl.join$vars.)
  
  ## vars.medium.ivr
  
  frmla. <- as.formula(paste("BM.prod.BF_dist.tr", paste(c(model.medium.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.prod.BF_dist.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM.prod.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.prod.BF_dist.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM.prod.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.prod.BF_dist.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM.prod.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium.ivr", tbl.join$vars.)
  
  ## vars.large.ivr
  
  frmla. <- as.formula(paste("BM.prod.BF_dist.tr", paste(c(model.large.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.prod.BF_dist.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM.prod.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.prod.BF_dist.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM.prod.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.prod.BF_dist.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM.prod.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large.ivr", tbl.join$vars.)
  
  
  ### Third.Quartile.BM_FS.FI_dist.tr
  
  ## vars.short.ivr
  
  frmla. <- as.formula(paste("BM_FS.FI_matri.full.Third.Quartile.BM_FS.FI_dist.tr", paste(c(model.short.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM_FS.FI_matri.full.Third.Quartile.BM_FS.FI_dist.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Third.Quartile.BM_FS.FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM_FS.FI_matri.full.Third.Quartile.BM_FS.FI_dist.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Third.Quartile.BM_FS.FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM_FS.FI_matri.full.Third.Quartile.BM_FS.FI_dist.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Third.Quartile.BM_FS.FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short.ivr", tbl.join$vars.)
  
  ## vars.medium.ivr
  
  frmla. <- as.formula(paste("BM_FS.FI_matri.full.Third.Quartile.BM_FS.FI_dist.tr", paste(c(model.medium.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM_FS.FI_matri.full.Third.Quartile.BM_FS.FI_dist.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Third.Quartile.BM_FS.FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM_FS.FI_matri.full.Third.Quartile.BM_FS.FI_dist.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Third.Quartile.BM_FS.FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM_FS.FI_matri.full.Third.Quartile.BM_FS.FI_dist.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Third.Quartile.BM_FS.FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium.ivr", tbl.join$vars.)
  
  ## vars.large.ivr
  
  frmla. <- as.formula(paste("BM_FS.FI_matri.full.Third.Quartile.BM_FS.FI_dist.tr", paste(c(model.large.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM_FS.FI_matri.full.Third.Quartile.BM_FS.FI_dist.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Third.Quartile.BM_FS.FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM_FS.FI_matri.full.Third.Quartile.BM_FS.FI_dist.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Third.Quartile.BM_FS.FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM_FS.FI_matri.full.Third.Quartile.BM_FS.FI_dist.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Third.Quartile.BM_FS.FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large.ivr", tbl.join$vars.)
  
  
  ### "Third.Quartile.BM.BF_FS_dist.tr"
  
  ## vars.short.ivr
  
  frmla. <- as.formula(paste("BM.BF_FS_matri.full.Third.Quartile.BM.BF_FS_dist.tr", paste(c(model.short.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.BF_FS_matri.full.Third.Quartile.BM.BF_FS_dist.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Third.Quartile.BM.BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.BF_FS_matri.full.Third.Quartile.BM.BF_FS_dist.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Third.Quartile.BM.BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.BF_FS_matri.full.Third.Quartile.BM.BF_FS_dist.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Third.Quartile.BM.BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short.ivr", tbl.join$vars.)
  
  ## vars.medium.ivr
  
  frmla. <- as.formula(paste("BM.BF_FS_matri.full.Third.Quartile.BM.BF_FS_dist.tr", paste(c(model.medium.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.BF_FS_matri.full.Third.Quartile.BM.BF_FS_dist.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Third.Quartile.BM.BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.BF_FS_matri.full.Third.Quartile.BM.BF_FS_dist.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Third.Quartile.BM.BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.BF_FS_matri.full.Third.Quartile.BM.BF_FS_dist.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Third.Quartile.BM.BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium.ivr", tbl.join$vars.)
  
  ## vars.large.ivr
  
  frmla. <- as.formula(paste("BM.BF_FS_matri.full.Third.Quartile.BM.BF_FS_dist.tr", paste(c(model.large.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.BF_FS_matri.full.Third.Quartile.BM.BF_FS_dist.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Third.Quartile.BM.BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.BF_FS_matri.full.Third.Quartile.BM.BF_FS_dist.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Third.Quartile.BM.BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.BF_FS_matri.full.Third.Quartile.BM.BF_FS_dist.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Third.Quartile.BM.BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large.ivr", tbl.join$vars.)
  
  
  ### BM.subnorm2.BF_Third.Quartile_dist.tr
  
  ## vars.short.ivr
  
  frmla. <- as.formula(paste("BM.subnorm2.BF_Third.Quartile_dist.tr", paste(c(model.short.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.subnorm2.BF_Third.Quartile_dist.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Third.Quartile.BM.subnorm2.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.subnorm2.BF_Third.Quartile_dist.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Third.Quartile.BM.subnorm2.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.subnorm2.BF_Third.Quartile_dist.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Third.Quartile.BM.subnorm2.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short.ivr", tbl.join$vars.)
  
  ## vars.medium.ivr
  
  frmla. <- as.formula(paste("BM.subnorm2.BF_Third.Quartile_dist.tr", paste(c(model.medium.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.subnorm2.BF_Third.Quartile_dist.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Third.Quartile.BM.subnorm2.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.subnorm2.BF_Third.Quartile_dist.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Third.Quartile.BM.subnorm2.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.subnorm2.BF_Third.Quartile_dist.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Third.Quartile.BM.subnorm2.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium.ivr", tbl.join$vars.)
  
  ## vars.large.ivr
  
  frmla. <- as.formula(paste("BM.subnorm2.BF_Third.Quartile_dist.tr", paste(c(model.large.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.subnorm2.BF_Third.Quartile_dist.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Third.Quartile.BM.subnorm2.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.subnorm2.BF_Third.Quartile_dist.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Third.Quartile.BM.subnorm2.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.subnorm2.BF_Third.Quartile_dist.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Third.Quartile.BM.subnorm2.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large.ivr", tbl.join$vars.)
  
  
  ### BM.sum.BF_Third.Quartile_dist.tr
  
  ## vars.short.ivr
  
  frmla. <- as.formula(paste("BM.sum.BF_Third.Quartile_dist.tr", paste(c(model.short.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.sum.BF_Third.Quartile_dist.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Third.Quartile.BM.sum.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.sum.BF_Third.Quartile_dist.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Third.Quartile.BM.sum.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.sum.BF_Third.Quartile_dist.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Third.Quartile.BM.sum.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short.ivr", tbl.join$vars.)
  
  ## vars.medium.ivr
  
  frmla. <- as.formula(paste("BM.sum.BF_Third.Quartile_dist.tr", paste(c(model.medium.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.sum.BF_Third.Quartile_dist.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Third.Quartile.BM.sum.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.sum.BF_Third.Quartile_dist.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Third.Quartile.BM.sum.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.sum.BF_Third.Quartile_dist.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Third.Quartile.BM.sum.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium.ivr", tbl.join$vars.)
  
  ## vars.large.ivr
  
  frmla. <- as.formula(paste("BM.sum.BF_Third.Quartile_dist.tr", paste(c(model.large.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.sum.BF_Third.Quartile_dist.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Third.Quartile.BM.sum.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.sum.BF_Third.Quartile_dist.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Third.Quartile.BM.sum.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.sum.BF_Third.Quartile_dist.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Third.Quartile.BM.sum.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large.ivr", tbl.join$vars.)
  
  
  ### BM.prod.BF_Third.Quartile_dist.tr
  
  ## vars.short.ivr
  
  frmla. <- as.formula(paste("BM.prod.BF_Third.Quartile_dist.tr", paste(c(model.short.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.prod.BF_Third.Quartile_dist.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Third.Quartile.BM.prod.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.prod.BF_Third.Quartile_dist.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Third.Quartile.BM.prod.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.prod.BF_Third.Quartile_dist.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Third.Quartile.BM.prod.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short.ivr", tbl.join$vars.)
  
  ## vars.medium.ivr
  
  frmla. <- as.formula(paste("BM.prod.BF_Third.Quartile_dist.tr", paste(c(model.medium.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.prod.BF_Third.Quartile_dist.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Third.Quartile.BM.prod.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.prod.BF_Third.Quartile_dist.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Third.Quartile.BM.prod.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.prod.BF_Third.Quartile_dist.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Third.Quartile.BM.prod.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium.ivr", tbl.join$vars.)
  
  ## vars.large.ivr
  
  frmla. <- as.formula(paste("BM.prod.BF_Third.Quartile_dist.tr", paste(c(model.large.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.prod.BF_Third.Quartile_dist.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Third.Quartile.BM.prod.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.prod.BF_Third.Quartile_dist.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Third.Quartile.BM.prod.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.prod.BF_Third.Quartile_dist.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Third.Quartile.BM.prod.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large.ivr", tbl.join$vars.)
  
  
  ### median.BM_FS.FI_dist.tr
  
  ## vars.short.ivr
  
  frmla. <- as.formula(paste("BM_FS.FI_matri.full.median.BM_FS.FI_dist.tr", paste(c(model.short.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM_FS.FI_matri.full.median.BM_FS.FI_dist.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "median.BM_FS.FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM_FS.FI_matri.full.median.BM_FS.FI_dist.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "median.BM_FS.FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM_FS.FI_matri.full.median.BM_FS.FI_dist.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "median.BM_FS.FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short.ivr", tbl.join$vars.)
  
  ## vars.medium.ivr
  
  frmla. <- as.formula(paste("BM_FS.FI_matri.full.median.BM_FS.FI_dist.tr", paste(c(model.medium.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM_FS.FI_matri.full.median.BM_FS.FI_dist.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "median.BM_FS.FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM_FS.FI_matri.full.median.BM_FS.FI_dist.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "median.BM_FS.FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM_FS.FI_matri.full.median.BM_FS.FI_dist.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "median.BM_FS.FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium.ivr", tbl.join$vars.)
  
  ## vars.large.ivr
  
  frmla. <- as.formula(paste("BM_FS.FI_matri.full.median.BM_FS.FI_dist.tr", paste(c(model.large.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM_FS.FI_matri.full.median.BM_FS.FI_dist.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "median.BM_FS.FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM_FS.FI_matri.full.median.BM_FS.FI_dist.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "median.BM_FS.FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM_FS.FI_matri.full.median.BM_FS.FI_dist.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "median.BM_FS.FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large.ivr", tbl.join$vars.)
  
  
  ### "median.BM.BF_FS_dist.tr"
  
  ## vars.short.ivr
  
  frmla. <- as.formula(paste("BM.BF_FS_matri.full.median.BM.BF_FS_dist.tr", paste(c(model.short.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.BF_FS_matri.full.median.BM.BF_FS_dist.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "median.BM.BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.BF_FS_matri.full.median.BM.BF_FS_dist.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "median.BM.BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.BF_FS_matri.full.median.BM.BF_FS_dist.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "median.BM.BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short.ivr", tbl.join$vars.)
  
  ## vars.medium.ivr
  
  frmla. <- as.formula(paste("BM.BF_FS_matri.full.median.BM.BF_FS_dist.tr", paste(c(model.medium.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.BF_FS_matri.full.median.BM.BF_FS_dist.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "median.BM.BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.BF_FS_matri.full.median.BM.BF_FS_dist.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "median.BM.BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.BF_FS_matri.full.median.BM.BF_FS_dist.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "median.BM.BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium.ivr", tbl.join$vars.)
  
  ## vars.large.ivr
  
  frmla. <- as.formula(paste("BM.BF_FS_matri.full.median.BM.BF_FS_dist.tr", paste(c(model.large.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.BF_FS_matri.full.median.BM.BF_FS_dist.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "median.BM.BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.BF_FS_matri.full.median.BM.BF_FS_dist.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "median.BM.BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.BF_FS_matri.full.median.BM.BF_FS_dist.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "median.BM.BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large.ivr", tbl.join$vars.)
  
  
  ### BM.subnorm2.BF_median_dist.tr
  
  ## vars.short.ivr
  
  frmla. <- as.formula(paste("BM.subnorm2.BF_median_dist.tr", paste(c(model.short.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.subnorm2.BF_median_dist.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "median.BM.subnorm2.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.subnorm2.BF_median_dist.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "median.BM.subnorm2.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.subnorm2.BF_median_dist.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "median.BM.subnorm2.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short.ivr", tbl.join$vars.)
  
  ## vars.medium.ivr
  
  frmla. <- as.formula(paste("BM.subnorm2.BF_median_dist.tr", paste(c(model.medium.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.subnorm2.BF_median_dist.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "median.BM.subnorm2.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.subnorm2.BF_median_dist.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "median.BM.subnorm2.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.subnorm2.BF_median_dist.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "median.BM.subnorm2.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium.ivr", tbl.join$vars.)
  
  ## vars.large.ivr
  
  frmla. <- as.formula(paste("BM.subnorm2.BF_median_dist.tr", paste(c(model.large.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.subnorm2.BF_median_dist.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "median.BM.subnorm2.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.subnorm2.BF_median_dist.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "median.BM.subnorm2.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.subnorm2.BF_median_dist.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "median.BM.subnorm2.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large.ivr", tbl.join$vars.)
  
  
  ### BM.sum.BF_median_dist.tr
  
  ## vars.short.ivr
  
  frmla. <- as.formula(paste("BM.sum.BF_median_dist.tr", paste(c(model.short.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.sum.BF_median_dist.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "median.BM.sum.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.sum.BF_median_dist.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "median.BM.sum.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.sum.BF_median_dist.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "median.BM.sum.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short.ivr", tbl.join$vars.)
  
  ## vars.medium.ivr
  
  frmla. <- as.formula(paste("BM.sum.BF_median_dist.tr", paste(c(model.medium.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.sum.BF_median_dist.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "median.BM.sum.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.sum.BF_median_dist.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "median.BM.sum.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.sum.BF_median_dist.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "median.BM.sum.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium.ivr", tbl.join$vars.)
  
  ## vars.large.ivr
  
  frmla. <- as.formula(paste("BM.sum.BF_median_dist.tr", paste(c(model.large.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.sum.BF_median_dist.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "median.BM.sum.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.sum.BF_median_dist.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "median.BM.sum.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.sum.BF_median_dist.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "median.BM.sum.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large.ivr", tbl.join$vars.)
  
  
  ### BM.prod.BF_median_dist.tr
  
  ## vars.short.ivr
  
  frmla. <- as.formula(paste("BM.prod.BF_median_dist.tr", paste(c(model.short.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.prod.BF_median_dist.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "median.BM.prod.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.prod.BF_median_dist.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "median.BM.prod.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.prod.BF_median_dist.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "median.BM.prod.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short.ivr", tbl.join$vars.)
  
  ## vars.medium.ivr
  
  frmla. <- as.formula(paste("BM.prod.BF_median_dist.tr", paste(c(model.medium.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.prod.BF_median_dist.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "median.BM.prod.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.prod.BF_median_dist.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "median.BM.prod.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.prod.BF_median_dist.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "median.BM.prod.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium.ivr", tbl.join$vars.)
  
  ## vars.large.ivr
  
  frmla. <- as.formula(paste("BM.prod.BF_median_dist.tr", paste(c(model.large.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.prod.BF_median_dist.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "median.BM.prod.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.prod.BF_median_dist.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "median.BM.prod.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.prod.BF_median_dist.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "median.BM.prod.BF", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large.ivr", tbl.join$vars.)
  
  
  ### richness.tr face supérieure bloc mobile
  
  ## vars.short.ivr
  
  frmla. <- as.formula(paste("richness.tr", paste(c(model.short.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Face == "face supérieure")[, c("richness.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "richness BM_FS", vars. = vars.list, transfo. = "center_scale", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne", Face == "face supérieure")[, c("richness.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "richness BM_FS", vars. = vars.list, transfo. = "center_scale", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ", Face == "face supérieure")[, c("richness.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "richness BM_FS", vars. = vars.list, transfo. = "center_scale", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short.ivr", tbl.join$vars.)
  
  ## vars.medium.ivr
  
  frmla. <- as.formula(paste("richness.tr", paste(c(model.medium.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Face == "face supérieure")[, c("richness.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "richness BM_FS", vars. = vars.list, transfo. = "center_scale", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne", Face == "face supérieure")[, c("richness.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "richness BM_FS", vars. = vars.list, transfo. = "center_scale", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ", Face == "face supérieure")[, c("richness.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "richness BM_FS", vars. = vars.list, transfo. = "center_scale", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium.ivr", tbl.join$vars.)
  
  ## vars.large.ivr
  
  frmla. <- as.formula(paste("richness.tr", paste(c(model.large.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Face == "face supérieure")[, c("richness.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "richness BM_FS", vars. = vars.list, transfo. = "center_scale", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne", Face == "face supérieure")[, c("richness.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "richness BM_FS", vars. = vars.list, transfo. = "center_scale", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ", Face == "face supérieure")[, c("richness.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "richness BM_FS", vars. = vars.list, transfo. = "center_scale", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large.ivr", tbl.join$vars.)
  
  
  ### richness.tr face inférieure bloc mobile
  
  ## vars.short.ivr
  
  frmla. <- as.formula(paste("richness.tr", paste(c(model.short.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Face == "face inférieure")[, c("richness.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "richness BM_FI", vars. = vars.list, transfo. = "center_scale", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne", Face == "face inférieure")[, c("richness.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "richness BM_FI", vars. = vars.list, transfo. = "center_scale", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ", Face == "face inférieure")[, c("richness.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "richness BM_FI", vars. = vars.list, transfo. = "center_scale", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short.ivr", tbl.join$vars.)
  
  ## vars.medium.ivr
  
  frmla. <- as.formula(paste("richness.tr", paste(c(model.medium.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Face == "face inférieure")[, c("richness.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "richness BM_FI", vars. = vars.list, transfo. = "center_scale", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne", Face == "face inférieure")[, c("richness.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "richness BM_FI", vars. = vars.list, transfo. = "center_scale", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ", Face == "face inférieure")[, c("richness.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "richness BM_FI", vars. = vars.list, transfo. = "center_scale", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium.ivr", tbl.join$vars.)
  
  ## vars.large.ivr
  
  frmla. <- as.formula(paste("richness.tr", paste(c(model.large.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Face == "face inférieure")[, c("richness.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "richness BM_FI", vars. = vars.list, transfo. = "center_scale", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne", Face == "face inférieure")[, c("richness.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "richness BM_FI", vars. = vars.list, transfo. = "center_scale", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ", Face == "face inférieure")[, c("richness.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "richness BM_FI", vars. = vars.list, transfo. = "center_scale", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large.ivr", tbl.join$vars.)
  
  
  ### richness.tr face supérieure bloc fixé
  
  ## vars.short.ivr
  
  frmla. <- as.formula(paste("richness.tr", paste(c(model.short.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Face == "face supérieure")[, c("richness.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "richness BF_FS", vars. = vars.list, transfo. = "center_scale", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Region == "Bretagne", Face == "face supérieure")[, c("richness.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "richness BF_FS", vars. = vars.list, transfo. = "center_scale", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Region == "EGMP.BASQ", Face == "face supérieure")[, c("richness.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "richness BF_FS", vars. = vars.list, transfo. = "center_scale", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short.ivr", tbl.join$vars.)
  
  ## vars.medium.ivr
  
  frmla. <- as.formula(paste("richness.tr", paste(c(model.medium.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Face == "face supérieure")[, c("richness.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "richness BF_FS", vars. = vars.list, transfo. = "center_scale", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Region == "Bretagne", Face == "face supérieure")[, c("richness.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "richness BF_FS", vars. = vars.list, transfo. = "center_scale", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Region == "EGMP.BASQ", Face == "face supérieure")[, c("richness.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "richness BF_FS", vars. = vars.list, transfo. = "center_scale", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium.ivr", tbl.join$vars.)
  
  ## vars.large.ivr
  
  frmla. <- as.formula(paste("richness.tr", paste(c(model.large.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Face == "face supérieure")[, c("richness.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "richness BF_FS", vars. = vars.list, transfo. = "center_scale", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Region == "Bretagne", Face == "face supérieure")[, c("richness.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "richness BF_FS", vars. = vars.list, transfo. = "center_scale", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Region == "EGMP.BASQ", Face == "face supérieure")[, c("richness.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "richness BF_FS", vars. = vars.list, transfo. = "center_scale", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large.ivr", tbl.join$vars.)
  
  
  ### Shannon.tr face supérieure bloc mobile
  
  ## vars.short.ivr
  
  frmla. <- as.formula(paste("Shannon.tr", paste(c(model.short.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Face == "face supérieure")[, c("Shannon.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Shannon BM_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne", Face == "face supérieure")[, c("Shannon.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Shannon BM_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ", Face == "face supérieure")[, c("Shannon.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Shannon BM_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short.ivr", tbl.join$vars.)
  
  ## vars.medium.ivr
  
  frmla. <- as.formula(paste("Shannon.tr", paste(c(model.medium.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Face == "face supérieure")[, c("Shannon.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Shannon BM_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne", Face == "face supérieure")[, c("Shannon.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Shannon BM_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ", Face == "face supérieure")[, c("Shannon.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Shannon BM_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium.ivr", tbl.join$vars.)
  
  ## vars.large.ivr
  
  frmla. <- as.formula(paste("Shannon.tr", paste(c(model.large.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Face == "face supérieure")[, c("Shannon.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Shannon BM_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne", Face == "face supérieure")[, c("Shannon.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Shannon BM_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ", Face == "face supérieure")[, c("Shannon.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Shannon BM_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large.ivr", tbl.join$vars.)
  
  
  ### Shannon.tr face inférieure bloc mobile
  
  ## vars.short.ivr
  
  frmla. <- as.formula(paste("Shannon.tr", paste(c(model.short.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Face == "face inférieure")[, c("Shannon.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Shannon BM_FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne", Face == "face inférieure")[, c("Shannon.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Shannon BM_FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ", Face == "face inférieure")[, c("Shannon.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Shannon BM_FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short.ivr", tbl.join$vars.)
  
  ## vars.medium.ivr
  
  frmla. <- as.formula(paste("Shannon.tr", paste(c(model.medium.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Face == "face inférieure")[, c("Shannon.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Shannon BM_FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne", Face == "face inférieure")[, c("Shannon.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Shannon BM_FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ", Face == "face inférieure")[, c("Shannon.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Shannon BM_FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium.ivr", tbl.join$vars.)
  
  ## vars.large.ivr
  
  frmla. <- as.formula(paste("Shannon.tr", paste(c(model.large.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Face == "face inférieure")[, c("Shannon.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Shannon BM_FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne", Face == "face inférieure")[, c("Shannon.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Shannon BM_FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ", Face == "face inférieure")[, c("Shannon.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Shannon BM_FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large.ivr", tbl.join$vars.)
  
  
  ### Shannon.tr face supérieure bloc fixé
  
  ## vars.short.ivr
  
  frmla. <- as.formula(paste("Shannon.tr", paste(c(model.short.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Face == "face supérieure")[, c("Shannon.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Shannon BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Region == "Bretagne", Face == "face supérieure")[, c("Shannon.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Shannon BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Region == "EGMP.BASQ", Face == "face supérieure")[, c("Shannon.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Shannon BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short.ivr", tbl.join$vars.)
  
  ## vars.medium.ivr
  
  frmla. <- as.formula(paste("Shannon.tr", paste(c(model.medium.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Face == "face supérieure")[, c("Shannon.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Shannon BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Region == "Bretagne", Face == "face supérieure")[, c("Shannon.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Shannon BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Region == "EGMP.BASQ", Face == "face supérieure")[, c("Shannon.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Shannon BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium.ivr", tbl.join$vars.)
  
  ## vars.large.ivr
  
  frmla. <- as.formula(paste("Shannon.tr", paste(c(model.large.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Face == "face supérieure")[, c("Shannon.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Shannon BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Region == "Bretagne", Face == "face supérieure")[, c("Shannon.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Shannon BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Region == "EGMP.BASQ", Face == "face supérieure")[, c("Shannon.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Shannon BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large.ivr", tbl.join$vars.)
  
  
  ### Simpson.tr face supérieure bloc mobile
  
  ## vars.short.ivr
  
  frmla. <- as.formula(paste("Simpson.tr", paste(c(model.short.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Face == "face supérieure")[, c("Simpson.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Simpson BM_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne", Face == "face supérieure")[, c("Simpson.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Simpson BM_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ", Face == "face supérieure")[, c("Simpson.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Simpson BM_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short.ivr", tbl.join$vars.)
  
  ## vars.medium.ivr
  
  frmla. <- as.formula(paste("Simpson.tr", paste(c(model.medium.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Face == "face supérieure")[, c("Simpson.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Simpson BM_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne", Face == "face supérieure")[, c("Simpson.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Simpson BM_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ", Face == "face supérieure")[, c("Simpson.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Simpson BM_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium.ivr", tbl.join$vars.)
  
  ## vars.large.ivr
  
  frmla. <- as.formula(paste("Simpson.tr", paste(c(model.large.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Face == "face supérieure")[, c("Simpson.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Simpson BM_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne", Face == "face supérieure")[, c("Simpson.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Simpson BM_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ", Face == "face supérieure")[, c("Simpson.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Simpson BM_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large.ivr", tbl.join$vars.)
  
  
  ### Simpson.tr face inférieure bloc mobile
  
  ## vars.short.ivr
  
  frmla. <- as.formula(paste("Simpson.tr", paste(c(model.short.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Face == "face inférieure")[, c("Simpson.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Simpson BM_FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne", Face == "face inférieure")[, c("Simpson.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Simpson BM_FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ", Face == "face inférieure")[, c("Simpson.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Simpson BM_FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short.ivr", tbl.join$vars.)
  
  ## vars.medium.ivr
  
  frmla. <- as.formula(paste("Simpson.tr", paste(c(model.medium.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Face == "face inférieure")[, c("Simpson.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Simpson BM_FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne", Face == "face inférieure")[, c("Simpson.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Simpson BM_FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ", Face == "face inférieure")[, c("Simpson.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Simpson BM_FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium.ivr", tbl.join$vars.)
  
  ## vars.large.ivr
  
  frmla. <- as.formula(paste("Simpson.tr", paste(c(model.large.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Face == "face inférieure")[, c("Simpson.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Simpson BM_FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne", Face == "face inférieure")[, c("Simpson.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Simpson BM_FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ", Face == "face inférieure")[, c("Simpson.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Simpson BM_FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large.ivr", tbl.join$vars.)
  
  
  ### Simpson.tr face supérieure bloc fixé
  
  ## vars.short.ivr
  
  frmla. <- as.formula(paste("Simpson.tr", paste(c(model.short.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Face == "face supérieure")[, c("Simpson.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Simpson BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Region == "Bretagne", Face == "face supérieure")[, c("Simpson.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Simpson BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Region == "EGMP.BASQ", Face == "face supérieure")[, c("Simpson.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Simpson BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short.ivr", tbl.join$vars.)
  
  ## vars.medium.ivr
  
  frmla. <- as.formula(paste("Simpson.tr", paste(c(model.medium.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Face == "face supérieure")[, c("Simpson.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Simpson BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Region == "Bretagne", Face == "face supérieure")[, c("Simpson.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Simpson BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Region == "EGMP.BASQ", Face == "face supérieure")[, c("Simpson.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Simpson BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium.ivr", tbl.join$vars.)
  
  ## vars.large.ivr
  
  frmla. <- as.formula(paste("Simpson.tr", paste(c(model.large.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Face == "face supérieure")[, c("Simpson.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Simpson BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Region == "Bretagne", Face == "face supérieure")[, c("Simpson.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Simpson BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Region == "EGMP.BASQ", Face == "face supérieure")[, c("Simpson.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Simpson BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large.ivr", tbl.join$vars.)
  
  
  ### QEBM.2.tr bloc mobile
  
  ## vars.short.ivr
  
  frmla. <- as.formula(paste("QEBM.2.tr", paste(c(model.short.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("QEBM.2.tr", vars.short.ivr)])
  #filter(df.red, QEBM.2 >= -360) -> df.red 
  #filter(df.red, QEBM.2 <= 360) -> df.red 
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "QEBM.2", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("QEBM.2.tr", vars.short.ivr)])
  #filter(df.red, QEBM.2 >= -360) -> df.red 
  #filter(df.red, QEBM.2 <= 360) -> df.red 
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "QEBM.2", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("QEBM.2.tr", vars.short.ivr)])
  #filter(df.red, QEBM.2 >= -360) -> df.red 
  #filter(df.red, QEBM.2 <= 360) -> df.red 
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "QEBM.2", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short.ivr", tbl.join$vars.)
  
  ## vars.medium.ivr
  
  frmla. <- as.formula(paste("QEBM.2.tr", paste(c(model.medium.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("QEBM.2.tr", vars.medium.ivr)])
  #filter(df.red, QEBM.2 >= -360) -> df.red 
  #filter(df.red, QEBM.2 <= 360) -> df.red 
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "QEBM.2", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("QEBM.2.tr", vars.medium.ivr)])
  #filter(df.red, QEBM.2 >= -360) -> df.red 
  #filter(df.red, QEBM.2 <= 360) -> df.red 
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "QEBM.2", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("QEBM.2.tr", vars.medium.ivr)])
  #filter(df.red, QEBM.2 >= -360) -> df.red 
  #filter(df.red, QEBM.2 <= 360) -> df.red 
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "QEBM.2", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium.ivr", tbl.join$vars.)
  
  ## vars.large.ivr
  
  frmla. <- as.formula(paste("QEBM.2.tr", paste(c(model.large.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("QEBM.2.tr", vars.large.ivr)])
  #filter(df.red, QEBM.2 >= -360) -> df.red 
  #filter(df.red, QEBM.2 <= 360) -> df.red 
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "QEBM.2", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("QEBM.2.tr", vars.large.ivr)])
  #filter(df.red, QEBM.2 >= -360) -> df.red 
  #filter(df.red, QEBM.2 <= 360) -> df.red 
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "QEBM.2", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("QEBM.2.tr", vars.large.ivr)])
  #filter(df.red, QEBM.2 >= -360) -> df.red 
  #filter(df.red, QEBM.2 <= 360) -> df.red 
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "QEBM.2", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large.ivr", tbl.join$vars.)
}


tbl.models.step.by.step <- tbl.join


## test a dredge and model average procedure


frmla. <- as.formula(paste("BM_FS.FI_dist.tr", paste(c(model.short, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM_FS.FI_dist.tr", vars.short)])

Full_Model <- suppressWarnings(glmmTMB(frmla., family = gaussian(link = "identity"), data = df.red
                                       , na.action = "na.fail"
                                       ))
summary(Full_Model)
getAllTerms(Full_Model)
DredgeOutput <- dredge(Full_Model, subset =! ("cond(Sc.oceano.max.3m)" && "cond(Sc.Period.nb)"), beta = TRUE, evaluate = TRUE, rank = "AICc")

# if only one model
tmp <- get.models(DredgeOutput, subset = delta < 2)
if (length(tmp) == 1){
  tmp2 <- c(tmp, tmp)
  M1 <- model.avg(tmp2)
} else {M1 <- model.avg(DredgeOutput, subset = delta < 2, fit = TRUE)}
# if 2 models or more
M1 <- model.avg(DredgeOutput, subset = delta < 2, fit = TRUE)
M1$formula
M1$sw
summary(M1)

M2 <- model.avg(DredgeOutput, cumsum(weight) <= .95, fit = TRUE)
M2$formula
M2$sw
summary(M2)

#get.vars.<- function() {
#  Full_Model <- suppressWarnings(glmmTMB(frmla., family = gaussian(link = "identity"), data = df.red))
#  DredgeOutput <<- dredge(Full_Model, subset =! ("cond(Sc.oceano.max.3m)" && "cond(Sc.Period.nb)"), beta = TRUE, evaluate = TRUE, rank = "AICc")
#  M1 <<- model.avg(DredgeOutput, subset = delta < 7, fit = TRUE)
#  frmla.mod.M1 <<- M1$formula
#  vars.list.M1 <<- unlist(strsplit(trimws(as.character(M1$formula)), "\\s"))[c(grep("Sc.", unlist(strsplit(trimws(as.character(M1$formula)), "\\s"))))]
#  M2 <<- model.avg(DredgeOutput, cumsum(weight) <= .95, fit = TRUE)
#  frmla.mod.M2 <<- M2$formula
#  vars.list.M2 <<- unlist(strsplit(trimws(as.character(M2$formula)), "\\s"))[c(grep("Sc.", unlist(strsplit(trimws(as.character(M2$formula)), "\\s"))))]
#}

get.vars.<- function() {
  Full_Model <- suppressWarnings(glmmTMB(frmla., family = gaussian(link = "identity"), data = df.red
                                         , na.action = "na.fail"
                                         ))
  DredgeOutput <<- dredge(Full_Model, subset =! ("cond(Sc.oceano.max.3m)" && "cond(Sc.Period.nb)"), beta = TRUE, evaluate = TRUE, rank = "AICc")
  
  #M1 <<- model.avg(DredgeOutput, subset = delta < 2, fit = TRUE)
  tmp <- get.models(DredgeOutput, subset = delta < 2)
  if (length(tmp) == 1){
    tmp2 <- c(tmp, tmp)
    M1 <<- model.avg(tmp2)
  } else {M1 <<- model.avg(DredgeOutput, subset = delta < 2, fit = TRUE)}
  vars.list.M1 <<- unlist(strsplit(trimws(as.character(M1$formula)), "\\s"))[c(grep("Sc.", unlist(strsplit(trimws(as.character(M1$formula)), "\\s"))))]
  frmla.mod.M1 <<- as.formula(paste(M1$formula[2], paste(c(vars.list.M1, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  M1.sw <<- data.frame(var. = mgsub(names(M1$sw), c("cond", "[][()]"), c("", "")), sw = M1$sw)
  
  M2 <<- model.avg(DredgeOutput, cumsum(weight) <= .95, fit = TRUE)
  vars.list.M2 <<- unlist(strsplit(trimws(as.character(M2$formula)), "\\s"))[c(grep("Sc.", unlist(strsplit(trimws(as.character(M2$formula)), "\\s"))))]
  frmla.mod.M2 <<- as.formula(paste(M2$formula[2], paste(c(vars.list.M2, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  M2.sw <<- data.frame(var. = mgsub(names(M2$sw), c("cond", "[][()]"), c("", "")), sw = M2$sw)
}

sw_to_tbl. <- function(sw.df) {
  for(i in c(1:nrow(sw.df))) {
    var. <- noquote(paste0(sw.df[i, "var."], " sw"))
    tbl. <- add_column(tbl., sw = sw.df[i, "sw"], .after = paste0(sw.df[i, "var."], " multico."))
    names(tbl.)[names(tbl.) == 'sw'] <- paste0(sw.df[i, "var."], " sw")
  }
tbl. <<- tbl.
}


{
### BM_FS.FI_dist.tr

## vars.short

frmla. <- as.formula(paste("BM_FS.FI_dist.tr", paste(c(model.short, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM_FS.FI_dist.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM_FS.FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- tbl.
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM_FS.FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM_FS.FI_dist.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM_FS.FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM_FS.FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM_FS.FI_dist.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM_FS.FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM_FS.FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join <- add_column(tbl.join, vars. = NA, .before = "region")
tbl.join$vars. <- "vars.short"
tbl.join <- add_column(tbl.join, M.1vs2 = NA, .before = "region")
tbl.join$M.1vs2 <- c("M1","M2","M1","M2","M1","M2")

## vars.medium

frmla. <- as.formula(paste("BM_FS.FI_dist.tr", paste(c(model.medium, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM_FS.FI_dist.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM_FS.FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM_FS.FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM_FS.FI_dist.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM_FS.FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM_FS.FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM_FS.FI_dist.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM_FS.FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM_FS.FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)

## vars.large

frmla. <- as.formula(paste("BM_FS.FI_dist.tr", paste(c(model.large, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM_FS.FI_dist.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM_FS.FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM_FS.FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM_FS.FI_dist.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM_FS.FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM_FS.FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM_FS.FI_dist.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM_FS.FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM_FS.FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)


### BM.BF_FS_dist.tr

## vars.short


frmla. <- as.formula(paste("BM.BF_FS_dist.tr", paste(c(model.short, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.BF_FS_dist.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM.BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM.BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.BF_FS_dist.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM.BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM.BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.BF_FS_dist.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM.BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM.BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)

## vars.medium

frmla. <- as.formula(paste("BM.BF_FS_dist.tr", paste(c(model.medium, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.BF_FS_dist.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM.BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM.BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.BF_FS_dist.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM.BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM.BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.BF_FS_dist.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM.BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM.BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)

## vars.large

frmla. <- as.formula(paste("BM.BF_FS_dist.tr", paste(c(model.large, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.BF_FS_dist.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM.BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM.BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.BF_FS_dist.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM.BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM.BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.BF_FS_dist.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM.BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM.BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)


### BM.subnorm2.BF_dist.tr

## vars.short

frmla. <- as.formula(paste("BM.subnorm2.BF_dist.tr", paste(c("1", model.short, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.subnorm2.BF_dist.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM.subnorm2.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM.subnorm2.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.subnorm2.BF_dist.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM.subnorm2.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM.subnorm2.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.subnorm2.BF_dist.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM.subnorm2.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM.subnorm2.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)

## vars.medium

frmla. <- as.formula(paste("BM.subnorm2.BF_dist.tr", paste(c(model.medium, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.subnorm2.BF_dist.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM.subnorm2.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM.subnorm2.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.subnorm2.BF_dist.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM.subnorm2.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM.subnorm2.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.subnorm2.BF_dist.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM.subnorm2.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM.subnorm2.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)

## vars.large

frmla. <- as.formula(paste("BM.subnorm2.BF_dist.tr", paste(c(model.large, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.subnorm2.BF_dist.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM.subnorm2.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM.subnorm2.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.subnorm2.BF_dist.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM.subnorm2.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM.subnorm2.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.subnorm2.BF_dist.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM.subnorm2.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM.subnorm2.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)


### BM.sum.BF_dist.tr

## vars.short

frmla. <- as.formula(paste("BM.sum.BF_dist.tr", paste(c("1", model.short, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.sum.BF_dist.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM.sum.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM.sum.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.sum.BF_dist.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM.sum.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM.sum.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.sum.BF_dist.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM.sum.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM.sum.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)

## vars.medium

frmla. <- as.formula(paste("BM.sum.BF_dist.tr", paste(c(model.medium, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.sum.BF_dist.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM.sum.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM.sum.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.sum.BF_dist.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM.sum.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM.sum.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.sum.BF_dist.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM.sum.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM.sum.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)

## vars.large

frmla. <- as.formula(paste("BM.sum.BF_dist.tr", paste(c(model.large, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.sum.BF_dist.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM.sum.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM.sum.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.sum.BF_dist.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM.sum.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM.sum.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.sum.BF_dist.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM.sum.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM.sum.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)


### BM.prod.BF_dist.tr

## vars.short

frmla. <- as.formula(paste("BM.prod.BF_dist.tr", paste(c("1", model.short, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.prod.BF_dist.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM.prod.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM.prod.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.prod.BF_dist.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM.prod.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM.prod.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.prod.BF_dist.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM.prod.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM.prod.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)

## vars.medium

frmla. <- as.formula(paste("BM.prod.BF_dist.tr", paste(c(model.medium, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.prod.BF_dist.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM.prod.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM.prod.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.prod.BF_dist.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM.prod.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM.prod.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.prod.BF_dist.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM.prod.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM.prod.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)

## vars.large

frmla. <- as.formula(paste("BM.prod.BF_dist.tr", paste(c(model.large, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.prod.BF_dist.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM.prod.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM.prod.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.prod.BF_dist.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM.prod.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM.prod.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.prod.BF_dist.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM.prod.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM.prod.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)


### Third.Quartile.BM_FS.FI_dist.tr

## vars.short

frmla. <- as.formula(paste("BM_FS.FI_matri.full.Third.Quartile.BM_FS.FI_dist.tr", paste(c("1", model.short, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM_FS.FI_matri.full.Third.Quartile.BM_FS.FI_dist.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Third.Quartile.BM_FS.FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Third.Quartile.BM_FS.FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM_FS.FI_matri.full.Third.Quartile.BM_FS.FI_dist.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Third.Quartile.BM_FS.FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Third.Quartile.BM_FS.FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM_FS.FI_matri.full.Third.Quartile.BM_FS.FI_dist.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Third.Quartile.BM_FS.FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Third.Quartile.BM_FS.FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)

## vars.medium

frmla. <- as.formula(paste("BM_FS.FI_matri.full.Third.Quartile.BM_FS.FI_dist.tr", paste(c(model.medium, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM_FS.FI_matri.full.Third.Quartile.BM_FS.FI_dist.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Third.Quartile.BM_FS.FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Third.Quartile.BM_FS.FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM_FS.FI_matri.full.Third.Quartile.BM_FS.FI_dist.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Third.Quartile.BM_FS.FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Third.Quartile.BM_FS.FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM_FS.FI_matri.full.Third.Quartile.BM_FS.FI_dist.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Third.Quartile.BM_FS.FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Third.Quartile.BM_FS.FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)

## vars.large

frmla. <- as.formula(paste("BM_FS.FI_matri.full.Third.Quartile.BM_FS.FI_dist.tr", paste(c(model.large, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM_FS.FI_matri.full.Third.Quartile.BM_FS.FI_dist.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Third.Quartile.BM_FS.FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Third.Quartile.BM_FS.FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM_FS.FI_matri.full.Third.Quartile.BM_FS.FI_dist.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Third.Quartile.BM_FS.FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Third.Quartile.BM_FS.FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM_FS.FI_matri.full.Third.Quartile.BM_FS.FI_dist.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Third.Quartile.BM_FS.FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Third.Quartile.BM_FS.FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)


### Third.Quartile.BM.BF_FS_dist.tr

## vars.short

frmla. <- as.formula(paste("BM.BF_FS_matri.full.Third.Quartile.BM.BF_FS_dist.tr", paste(c(model.short, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.BF_FS_matri.full.Third.Quartile.BM.BF_FS_dist.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Third.Quartile.BM.BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Third.Quartile.BM.BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.BF_FS_matri.full.Third.Quartile.BM.BF_FS_dist.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Third.Quartile.BM.BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Third.Quartile.BM.BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.BF_FS_matri.full.Third.Quartile.BM.BF_FS_dist.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Third.Quartile.BM.BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Third.Quartile.BM.BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)

## vars.medium

frmla. <- as.formula(paste("BM.BF_FS_matri.full.Third.Quartile.BM.BF_FS_dist.tr", paste(c(model.medium, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.BF_FS_matri.full.Third.Quartile.BM.BF_FS_dist.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Third.Quartile.BM.BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Third.Quartile.BM.BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.BF_FS_matri.full.Third.Quartile.BM.BF_FS_dist.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Third.Quartile.BM.BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Third.Quartile.BM.BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.BF_FS_matri.full.Third.Quartile.BM.BF_FS_dist.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Third.Quartile.BM.BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Third.Quartile.BM.BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)

## vars.large

frmla. <- as.formula(paste("BM.BF_FS_matri.full.Third.Quartile.BM.BF_FS_dist.tr", paste(c(model.large, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.BF_FS_matri.full.Third.Quartile.BM.BF_FS_dist.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Third.Quartile.BM.BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Third.Quartile.BM.BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.BF_FS_matri.full.Third.Quartile.BM.BF_FS_dist.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Third.Quartile.BM.BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Third.Quartile.BM.BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.BF_FS_matri.full.Third.Quartile.BM.BF_FS_dist.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Third.Quartile.BM.BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Third.Quartile.BM.BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)


### BM.subnorm2.BF_Third.Quartile_dist.tr

## vars.short

frmla. <- as.formula(paste("BM.subnorm2.BF_Third.Quartile_dist.tr", paste(c("1", model.short, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.subnorm2.BF_Third.Quartile_dist.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Third.Quartile.BM.subnorm2.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Third.Quartile.BM.subnorm2.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.subnorm2.BF_Third.Quartile_dist.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Third.Quartile.BM.subnorm2.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Third.Quartile.BM.subnorm2.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.subnorm2.BF_Third.Quartile_dist.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Third.Quartile.BM.subnorm2.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Third.Quartile.BM.subnorm2.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)

## vars.medium

frmla. <- as.formula(paste("BM.subnorm2.BF_Third.Quartile_dist.tr", paste(c(model.medium, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.subnorm2.BF_Third.Quartile_dist.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Third.Quartile.BM.subnorm2.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Third.Quartile.BM.subnorm2.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.subnorm2.BF_Third.Quartile_dist.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Third.Quartile.BM.subnorm2.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Third.Quartile.BM.subnorm2.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.subnorm2.BF_Third.Quartile_dist.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Third.Quartile.BM.subnorm2.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Third.Quartile.BM.subnorm2.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)

## vars.large

frmla. <- as.formula(paste("BM.subnorm2.BF_Third.Quartile_dist.tr", paste(c(model.large, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.subnorm2.BF_Third.Quartile_dist.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Third.Quartile.BM.subnorm2.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Third.Quartile.BM.subnorm2.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.subnorm2.BF_Third.Quartile_dist.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Third.Quartile.BM.subnorm2.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Third.Quartile.BM.subnorm2.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.subnorm2.BF_Third.Quartile_dist.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Third.Quartile.BM.subnorm2.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Third.Quartile.BM.subnorm2.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)


### BM.sum.BF_Third.Quartile_dist.tr

## vars.short

frmla. <- as.formula(paste("BM.sum.BF_Third.Quartile_dist.tr", paste(c("1", model.short, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.sum.BF_Third.Quartile_dist.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Third.Quartile.BM.sum.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Third.Quartile.BM.sum.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.sum.BF_Third.Quartile_dist.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Third.Quartile.BM.sum.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Third.Quartile.BM.sum.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.sum.BF_Third.Quartile_dist.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Third.Quartile.BM.sum.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Third.Quartile.BM.sum.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)

## vars.medium

frmla. <- as.formula(paste("BM.sum.BF_Third.Quartile_dist.tr", paste(c(model.medium, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.sum.BF_Third.Quartile_dist.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Third.Quartile.BM.sum.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Third.Quartile.BM.sum.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.sum.BF_Third.Quartile_dist.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Third.Quartile.BM.sum.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Third.Quartile.BM.sum.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.sum.BF_Third.Quartile_dist.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Third.Quartile.BM.sum.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Third.Quartile.BM.sum.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)

## vars.large

frmla. <- as.formula(paste("BM.sum.BF_Third.Quartile_dist.tr", paste(c(model.large, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.sum.BF_Third.Quartile_dist.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Third.Quartile.BM.sum.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Third.Quartile.BM.sum.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.sum.BF_Third.Quartile_dist.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Third.Quartile.BM.sum.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Third.Quartile.BM.sum.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.sum.BF_Third.Quartile_dist.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Third.Quartile.BM.sum.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Third.Quartile.BM.sum.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)


### BM.prod.BF_Third.Quartile_dist.tr

## vars.short

frmla. <- as.formula(paste("BM.prod.BF_Third.Quartile_dist.tr", paste(c("1", model.short, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.prod.BF_Third.Quartile_dist.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Third.Quartile.BM.prod.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Third.Quartile.BM.prod.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.prod.BF_Third.Quartile_dist.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Third.Quartile.BM.prod.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Third.Quartile.BM.prod.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.prod.BF_Third.Quartile_dist.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Third.Quartile.BM.prod.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Third.Quartile.BM.prod.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)

## vars.medium

frmla. <- as.formula(paste("BM.prod.BF_Third.Quartile_dist.tr", paste(c(model.medium, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.prod.BF_Third.Quartile_dist.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Third.Quartile.BM.prod.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Third.Quartile.BM.prod.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.prod.BF_Third.Quartile_dist.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Third.Quartile.BM.prod.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Third.Quartile.BM.prod.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.prod.BF_Third.Quartile_dist.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Third.Quartile.BM.prod.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Third.Quartile.BM.prod.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)

## vars.large

frmla. <- as.formula(paste("BM.prod.BF_Third.Quartile_dist.tr", paste(c(model.large, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.prod.BF_Third.Quartile_dist.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Third.Quartile.BM.prod.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Third.Quartile.BM.prod.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.prod.BF_Third.Quartile_dist.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Third.Quartile.BM.prod.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Third.Quartile.BM.prod.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.prod.BF_Third.Quartile_dist.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Third.Quartile.BM.prod.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Third.Quartile.BM.prod.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)


### median.BM_FS.FI_dist.tr

## vars.short

frmla. <- as.formula(paste("BM_FS.FI_matri.full.median.BM_FS.FI_dist.tr", paste(c("1", model.short, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM_FS.FI_matri.full.median.BM_FS.FI_dist.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "median.BM_FS.FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "median.BM_FS.FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM_FS.FI_matri.full.median.BM_FS.FI_dist.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "median.BM_FS.FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "median.BM_FS.FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM_FS.FI_matri.full.median.BM_FS.FI_dist.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "median.BM_FS.FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "median.BM_FS.FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)

## vars.medium

frmla. <- as.formula(paste("BM_FS.FI_matri.full.median.BM_FS.FI_dist.tr", paste(c(model.medium, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM_FS.FI_matri.full.median.BM_FS.FI_dist.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "median.BM_FS.FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "median.BM_FS.FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM_FS.FI_matri.full.median.BM_FS.FI_dist.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "median.BM_FS.FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "median.BM_FS.FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM_FS.FI_matri.full.median.BM_FS.FI_dist.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "median.BM_FS.FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "median.BM_FS.FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)

## vars.large

frmla. <- as.formula(paste("BM_FS.FI_matri.full.median.BM_FS.FI_dist.tr", paste(c(model.large, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM_FS.FI_matri.full.median.BM_FS.FI_dist.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "median.BM_FS.FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "median.BM_FS.FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM_FS.FI_matri.full.median.BM_FS.FI_dist.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "median.BM_FS.FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "median.BM_FS.FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM_FS.FI_matri.full.median.BM_FS.FI_dist.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "median.BM_FS.FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "median.BM_FS.FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)


### median.BM.BF_FS_dist.tr

## vars.short

frmla. <- as.formula(paste("BM.BF_FS_matri.full.median.BM.BF_FS_dist.tr", paste(c(model.short, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.BF_FS_matri.full.median.BM.BF_FS_dist.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "median.BM.BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "median.BM.BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.BF_FS_matri.full.median.BM.BF_FS_dist.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "median.BM.BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "median.BM.BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.BF_FS_matri.full.median.BM.BF_FS_dist.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "median.BM.BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "median.BM.BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)

## vars.medium

frmla. <- as.formula(paste("BM.BF_FS_matri.full.median.BM.BF_FS_dist.tr", paste(c(model.medium, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.BF_FS_matri.full.median.BM.BF_FS_dist.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "median.BM.BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "median.BM.BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.BF_FS_matri.full.median.BM.BF_FS_dist.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "median.BM.BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "median.BM.BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.BF_FS_matri.full.median.BM.BF_FS_dist.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "median.BM.BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "median.BM.BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)

## vars.large

frmla. <- as.formula(paste("BM.BF_FS_matri.full.median.BM.BF_FS_dist.tr", paste(c(model.large, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.BF_FS_matri.full.median.BM.BF_FS_dist.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "median.BM.BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "median.BM.BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.BF_FS_matri.full.median.BM.BF_FS_dist.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "median.BM.BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "median.BM.BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.BF_FS_matri.full.median.BM.BF_FS_dist.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "median.BM.BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "median.BM.BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)


### BM.subnorm2.BF_median_dist.tr

## vars.short

frmla. <- as.formula(paste("BM.subnorm2.BF_median_dist.tr", paste(c("1", model.short, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.subnorm2.BF_median_dist.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "median.BM.subnorm2.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "median.BM.subnorm2.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.subnorm2.BF_median_dist.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "median.BM.subnorm2.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "median.BM.subnorm2.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.subnorm2.BF_median_dist.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "median.BM.subnorm2.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "median.BM.subnorm2.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)

## vars.medium

frmla. <- as.formula(paste("BM.subnorm2.BF_median_dist.tr", paste(c(model.medium, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.subnorm2.BF_median_dist.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "median.BM.subnorm2.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "median.BM.subnorm2.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.subnorm2.BF_median_dist.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "median.BM.subnorm2.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "median.BM.subnorm2.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.subnorm2.BF_median_dist.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "median.BM.subnorm2.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "median.BM.subnorm2.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)

## vars.large

frmla. <- as.formula(paste("BM.subnorm2.BF_median_dist.tr", paste(c(model.large, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.subnorm2.BF_median_dist.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "median.BM.subnorm2.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "median.BM.subnorm2.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.subnorm2.BF_median_dist.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "median.BM.subnorm2.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "median.BM.subnorm2.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.subnorm2.BF_median_dist.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "median.BM.subnorm2.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "median.BM.subnorm2.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)


### BM.sum.BF_median_dist.tr

## vars.short

frmla. <- as.formula(paste("BM.sum.BF_median_dist.tr", paste(c("1", model.short, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.sum.BF_median_dist.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "median.BM.sum.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "median.BM.sum.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.sum.BF_median_dist.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "median.BM.sum.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "median.BM.sum.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.sum.BF_median_dist.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "median.BM.sum.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "median.BM.sum.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)

## vars.medium

frmla. <- as.formula(paste("BM.sum.BF_median_dist.tr", paste(c(model.medium, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.sum.BF_median_dist.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "median.BM.sum.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "median.BM.sum.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.sum.BF_median_dist.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "median.BM.sum.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "median.BM.sum.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.sum.BF_median_dist.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "median.BM.sum.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "median.BM.sum.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)

## vars.large

frmla. <- as.formula(paste("BM.sum.BF_median_dist.tr", paste(c(model.large, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.sum.BF_median_dist.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "median.BM.sum.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "median.BM.sum.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.sum.BF_median_dist.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "median.BM.sum.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "median.BM.sum.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.sum.BF_median_dist.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "median.BM.sum.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "median.BM.sum.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)


### BM.prod.BF_median_dist.tr

## vars.short

frmla. <- as.formula(paste("BM.prod.BF_median_dist.tr", paste(c("1", model.short, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.prod.BF_median_dist.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "median.BM.prod.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "median.BM.prod.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.prod.BF_median_dist.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "median.BM.prod.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "median.BM.prod.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.prod.BF_median_dist.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "median.BM.prod.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "median.BM.prod.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)

## vars.medium

frmla. <- as.formula(paste("BM.prod.BF_median_dist.tr", paste(c(model.medium, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.prod.BF_median_dist.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "median.BM.prod.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "median.BM.prod.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.prod.BF_median_dist.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "median.BM.prod.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "median.BM.prod.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.prod.BF_median_dist.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "median.BM.prod.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "median.BM.prod.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)

## vars.large

frmla. <- as.formula(paste("BM.prod.BF_median_dist.tr", paste(c(model.large, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.prod.BF_median_dist.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "median.BM.prod.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "median.BM.prod.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.prod.BF_median_dist.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "median.BM.prod.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "median.BM.prod.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.prod.BF_median_dist.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "median.BM.prod.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "median.BM.prod.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)


### richness.tr face supérieure bloc mobile

## vars.short

frmla. <- as.formula(paste("richness.tr", paste(c(model.short, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Face == "face supérieure")[, c("richness.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "richness BM_FS", vars. = vars.list.M1, transfo. = "center_scale", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "richness BM_FS", vars. = vars.list.M2, transfo. = "center_scale", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne", Face == "face supérieure")[, c("richness.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "richness BM_FS", vars. = vars.list.M1, transfo. = "center_scale", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "richness BM_FS", vars. = vars.list.M2, transfo. = "center_scale", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ", Face == "face supérieure")[, c("richness.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "richness BM_FS", vars. = vars.list.M1, transfo. = "center_scale", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "richness BM_FS", vars. = vars.list.M2, transfo. = "center_scale", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)

## vars.medium

frmla. <- as.formula(paste("richness.tr", paste(c(model.medium, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Face == "face supérieure")[, c("richness.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "richness BM_FS", vars. = vars.list.M1, transfo. = "center_scale", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "richness BM_FS", vars. = vars.list.M2, transfo. = "center_scale", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne", Face == "face supérieure")[, c("richness.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "richness BM_FS", vars. = vars.list.M1, transfo. = "center_scale", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "richness BM_FS", vars. = vars.list.M2, transfo. = "center_scale", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ", Face == "face supérieure")[, c("richness.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "richness BM_FS", vars. = vars.list.M1, transfo. = "center_scale", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "richness BM_FS", vars. = vars.list.M2, transfo. = "center_scale", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)

## vars.large

frmla. <- as.formula(paste("richness.tr", paste(c(model.large, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Face == "face supérieure")[, c("richness.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "richness BM_FS", vars. = vars.list.M1, transfo. = "center_scale", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "richness BM_FS", vars. = vars.list.M2, transfo. = "center_scale", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne", Face == "face supérieure")[, c("richness.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "richness BM_FS", vars. = vars.list.M1, transfo. = "center_scale", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "richness BM_FS", vars. = vars.list.M2, transfo. = "center_scale", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ", Face == "face supérieure")[, c("richness.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "richness BM_FS", vars. = vars.list.M1, transfo. = "center_scale", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "richness BM_FS", vars. = vars.list.M2, transfo. = "center_scale", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)


### richness.tr face inférieure bloc mobile

## vars.short

frmla. <- as.formula(paste("richness.tr", paste(c(model.short, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Face == "face inférieure")[, c("richness.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "richness BM_FI", vars. = vars.list.M1, transfo. = "center_scale", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "richness BM_FI", vars. = vars.list.M2, transfo. = "center_scale", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne", Face == "face inférieure")[, c("richness.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "richness BM_FI", vars. = vars.list.M1, transfo. = "center_scale", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "richness BM_FI", vars. = vars.list.M2, transfo. = "center_scale", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ", Face == "face inférieure")[, c("richness.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "richness BM_FI", vars. = vars.list.M1, transfo. = "center_scale", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "richness BM_FI", vars. = vars.list.M2, transfo. = "center_scale", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)

## vars.medium

frmla. <- as.formula(paste("richness.tr", paste(c(model.medium, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Face == "face inférieure")[, c("richness.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "richness BM_FI", vars. = vars.list.M1, transfo. = "center_scale", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "richness BM_FI", vars. = vars.list.M2, transfo. = "center_scale", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne", Face == "face inférieure")[, c("richness.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "richness BM_FI", vars. = vars.list.M1, transfo. = "center_scale", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "richness BM_FI", vars. = vars.list.M2, transfo. = "center_scale", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ", Face == "face inférieure")[, c("richness.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "richness BM_FI", vars. = vars.list.M1, transfo. = "center_scale", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "richness BM_FI", vars. = vars.list.M2, transfo. = "center_scale", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)

## vars.large

frmla. <- as.formula(paste("richness.tr", paste(c(model.large, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Face == "face inférieure")[, c("richness.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "richness BM_FI", vars. = vars.list.M1, transfo. = "center_scale", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "richness BM_FI", vars. = vars.list.M2, transfo. = "center_scale", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne", Face == "face inférieure")[, c("richness.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "richness BM_FI", vars. = vars.list.M1, transfo. = "center_scale", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "richness BM_FI", vars. = vars.list.M2, transfo. = "center_scale", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ", Face == "face inférieure")[, c("richness.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "richness BM_FI", vars. = vars.list.M1, transfo. = "center_scale", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "richness BM_FI", vars. = vars.list.M2, transfo. = "center_scale", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)


### richness.tr face supérieure bloc fixé

## vars.short

frmla. <- as.formula(paste("richness.tr", paste(c(model.short, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Face == "face supérieure")[, c("richness.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "richness BF_FS", vars. = vars.list.M1, transfo. = "center_scale", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "richness BF_FS", vars. = vars.list.M2, transfo. = "center_scale", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Region == "Bretagne", Face == "face supérieure")[, c("richness.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "richness BF_FS", vars. = vars.list.M1, transfo. = "center_scale", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "richness BF_FS", vars. = vars.list.M2, transfo. = "center_scale", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Region == "EGMP.BASQ", Face == "face supérieure")[, c("richness.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "richness BF_FS", vars. = vars.list.M1, transfo. = "center_scale", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "richness BF_FS", vars. = vars.list.M2, transfo. = "center_scale", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)

## vars.medium

frmla. <- as.formula(paste("richness.tr", paste(c(model.medium, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Face == "face supérieure")[, c("richness.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "richness BF_FS", vars. = vars.list.M1, transfo. = "center_scale", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "richness BF_FS", vars. = vars.list.M2, transfo. = "center_scale", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Region == "Bretagne", Face == "face supérieure")[, c("richness.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "richness BF_FS", vars. = vars.list.M1, transfo. = "center_scale", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "richness BF_FS", vars. = vars.list.M2, transfo. = "center_scale", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Region == "EGMP.BASQ", Face == "face supérieure")[, c("richness.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "richness BF_FS", vars. = vars.list.M1, transfo. = "center_scale", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "richness BF_FS", vars. = vars.list.M2, transfo. = "center_scale", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)

## vars.large

frmla. <- as.formula(paste("richness.tr", paste(c(model.large, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Face == "face supérieure")[, c("richness.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "richness BF_FS", vars. = vars.list.M1, transfo. = "center_scale", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "richness BF_FS", vars. = vars.list.M2, transfo. = "center_scale", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Region == "Bretagne", Face == "face supérieure")[, c("richness.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "richness BF_FS", vars. = vars.list.M1, transfo. = "center_scale", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "richness BF_FS", vars. = vars.list.M2, transfo. = "center_scale", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Region == "EGMP.BASQ", Face == "face supérieure")[, c("richness.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "richness BF_FS", vars. = vars.list.M1, transfo. = "center_scale", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "richness BF_FS", vars. = vars.list.M2, transfo. = "center_scale", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)


### Shannon.tr face supérieure bloc mobile

## vars.short

frmla. <- as.formula(paste("Shannon.tr", paste(c(model.short, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Face == "face supérieure")[, c("Shannon.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Shannon BM_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Shannon BM_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne", Face == "face supérieure")[, c("Shannon.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Shannon BM_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Shannon BM_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ", Face == "face supérieure")[, c("Shannon.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Shannon BM_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Shannon BM_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)

## vars.medium

frmla. <- as.formula(paste("Shannon.tr", paste(c(model.medium, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Face == "face supérieure")[, c("Shannon.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Shannon BM_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Shannon BM_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne", Face == "face supérieure")[, c("Shannon.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Shannon BM_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Shannon BM_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ", Face == "face supérieure")[, c("Shannon.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Shannon BM_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Shannon BM_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)

## vars.large

frmla. <- as.formula(paste("Shannon.tr", paste(c(model.large, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Face == "face supérieure")[, c("Shannon.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Shannon BM_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Shannon BM_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne", Face == "face supérieure")[, c("Shannon.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Shannon BM_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Shannon BM_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ", Face == "face supérieure")[, c("Shannon.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Shannon BM_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Shannon BM_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)


### Shannon.tr face inférieure bloc mobile

## vars.short

frmla. <- as.formula(paste("Shannon.tr", paste(c(model.short, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Face == "face inférieure")[, c("Shannon.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Shannon BM_FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Shannon BM_FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne", Face == "face inférieure")[, c("Shannon.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Shannon BM_FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Shannon BM_FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ", Face == "face inférieure")[, c("Shannon.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Shannon BM_FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Shannon BM_FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)

## vars.medium

frmla. <- as.formula(paste("Shannon.tr", paste(c(model.medium, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Face == "face inférieure")[, c("Shannon.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Shannon BM_FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Shannon BM_FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne", Face == "face inférieure")[, c("Shannon.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Shannon BM_FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Shannon BM_FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ", Face == "face inférieure")[, c("Shannon.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Shannon BM_FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Shannon BM_FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)

## vars.large

frmla. <- as.formula(paste("Shannon.tr", paste(c(model.large, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Face == "face inférieure")[, c("Shannon.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Shannon BM_FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Shannon BM_FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne", Face == "face inférieure")[, c("Shannon.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Shannon BM_FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Shannon BM_FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ", Face == "face inférieure")[, c("Shannon.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Shannon BM_FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Shannon BM_FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)


### Shannon.tr face supérieure bloc fixé

## vars.short

frmla. <- as.formula(paste("Shannon.tr", paste(c(model.short, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Face == "face supérieure")[, c("Shannon.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Shannon BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Shannon BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Region == "Bretagne", Face == "face supérieure")[, c("Shannon.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Shannon BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Shannon BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Region == "EGMP.BASQ", Face == "face supérieure")[, c("Shannon.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Shannon BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Shannon BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)

## vars.medium

frmla. <- as.formula(paste("Shannon.tr", paste(c(model.medium, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Face == "face supérieure")[, c("Shannon.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Shannon BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Shannon BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Region == "Bretagne", Face == "face supérieure")[, c("Shannon.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Shannon BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Shannon BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Region == "EGMP.BASQ", Face == "face supérieure")[, c("Shannon.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Shannon BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Shannon BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)

## vars.large

frmla. <- as.formula(paste("Shannon.tr", paste(c(model.large, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Face == "face supérieure")[, c("Shannon.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Shannon BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Shannon BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Region == "Bretagne", Face == "face supérieure")[, c("Shannon.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Shannon BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Shannon BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Region == "EGMP.BASQ", Face == "face supérieure")[, c("Shannon.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Shannon BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Shannon BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)


### Simpson.tr face supérieure bloc mobile

## vars.short

frmla. <- as.formula(paste("Simpson.tr", paste(c(model.short, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Face == "face supérieure")[, c("Simpson.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Simpson BM_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Simpson BM_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne", Face == "face supérieure")[, c("Simpson.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Simpson BM_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Simpson BM_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ", Face == "face supérieure")[, c("Simpson.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Simpson BM_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Simpson BM_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)

## vars.medium

frmla. <- as.formula(paste("Simpson.tr", paste(c(model.medium, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Face == "face supérieure")[, c("Simpson.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Simpson BM_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Simpson BM_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne", Face == "face supérieure")[, c("Simpson.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Simpson BM_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Simpson BM_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ", Face == "face supérieure")[, c("Simpson.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Simpson BM_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Simpson BM_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)

## vars.large

frmla. <- as.formula(paste("Simpson.tr", paste(c(model.large, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Face == "face supérieure")[, c("Simpson.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Simpson BM_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Simpson BM_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne", Face == "face supérieure")[, c("Simpson.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Simpson BM_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Simpson BM_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ", Face == "face supérieure")[, c("Simpson.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Simpson BM_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Simpson BM_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)


### Simpson.tr face inférieure bloc mobile

## vars.short

frmla. <- as.formula(paste("Simpson.tr", paste(c(model.short, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Face == "face inférieure")[, c("Simpson.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Simpson BM_FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Simpson BM_FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne", Face == "face inférieure")[, c("Simpson.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Simpson BM_FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Simpson BM_FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ", Face == "face inférieure")[, c("Simpson.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Simpson BM_FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Simpson BM_FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)

## vars.medium

frmla. <- as.formula(paste("Simpson.tr", paste(c(model.medium, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Face == "face inférieure")[, c("Simpson.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Simpson BM_FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Simpson BM_FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne", Face == "face inférieure")[, c("Simpson.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Simpson BM_FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Simpson BM_FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ", Face == "face inférieure")[, c("Simpson.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Simpson BM_FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Simpson BM_FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)

## vars.large

frmla. <- as.formula(paste("Simpson.tr", paste(c(model.large, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Face == "face inférieure")[, c("Simpson.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Simpson BM_FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Simpson BM_FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne", Face == "face inférieure")[, c("Simpson.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Simpson BM_FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Simpson BM_FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ", Face == "face inférieure")[, c("Simpson.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Simpson BM_FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Simpson BM_FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)


### Simpson.tr face supérieure bloc fixé

## vars.short

frmla. <- as.formula(paste("Simpson.tr", paste(c(model.short, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Face == "face supérieure")[, c("Simpson.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Simpson BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Simpson BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Region == "Bretagne", Face == "face supérieure")[, c("Simpson.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Simpson BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Simpson BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Region == "EGMP.BASQ", Face == "face supérieure")[, c("Simpson.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Simpson BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Simpson BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)

## vars.medium

frmla. <- as.formula(paste("Simpson.tr", paste(c(model.medium, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Face == "face supérieure")[, c("Simpson.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Simpson BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Simpson BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Region == "Bretagne", Face == "face supérieure")[, c("Simpson.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Simpson BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Simpson BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Region == "EGMP.BASQ", Face == "face supérieure")[, c("Simpson.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Simpson BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Simpson BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)

## vars.large

frmla. <- as.formula(paste("Simpson.tr", paste(c(model.large, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Face == "face supérieure")[, c("Simpson.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Simpson BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Simpson BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Region == "Bretagne", Face == "face supérieure")[, c("Simpson.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Simpson BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Simpson BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Region == "EGMP.BASQ", Face == "face supérieure")[, c("Simpson.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Simpson BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Simpson BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)


### QEBM.2.tr bloc mobile

## vars.short

frmla. <- as.formula(paste("QEBM.2.tr", paste(c(model.short, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("QEBM.2.tr", vars.short)])
#filter(df.red, QEBM.2 >= -360) -> df.red 
#filter(df.red, QEBM.2 <= 360) -> df.red 
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "QEBM.2", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "QEBM.2", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("QEBM.2.tr", vars.short)])
#filter(df.red, QEBM.2 >= -360) -> df.red 
#filter(df.red, QEBM.2 <= 360) -> df.red 
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "QEBM.2", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "QEBM.2", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("QEBM.2.tr", vars.short)])
#filter(df.red, QEBM.2 >= -360) -> df.red 
#filter(df.red, QEBM.2 <= 360) -> df.red 
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "QEBM.2", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "QEBM.2", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)

## vars.medium

frmla. <- as.formula(paste("QEBM.2.tr", paste(c(model.medium, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("QEBM.2.tr", vars.medium)])
#filter(df.red, QEBM.2 >= -360) -> df.red 
#filter(df.red, QEBM.2 <= 360) -> df.red 
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "QEBM.2", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "QEBM.2", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("QEBM.2.tr", vars.medium)])
#filter(df.red, QEBM.2 >= -360) -> df.red 
#filter(df.red, QEBM.2 <= 360) -> df.red 
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "QEBM.2", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "QEBM.2", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("QEBM.2.tr", vars.medium)])
#filter(df.red, QEBM.2 >= -360) -> df.red 
#filter(df.red, QEBM.2 <= 360) -> df.red 
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "QEBM.2", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "QEBM.2", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)

## vars.large

frmla. <- as.formula(paste("QEBM.2.tr", paste(c(model.large, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("QEBM.2.tr", vars.large)])
#filter(df.red, QEBM.2 >= -360) -> df.red 
#filter(df.red, QEBM.2 <= 360) -> df.red 
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "QEBM.2", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "QEBM.2", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("QEBM.2.tr", vars.large)])
#filter(df.red, QEBM.2 >= -360) -> df.red 
#filter(df.red, QEBM.2 <= 360) -> df.red 
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "QEBM.2", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "QEBM.2", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("QEBM.2.tr", vars.large)])
#filter(df.red, QEBM.2 >= -360) -> df.red 
#filter(df.red, QEBM.2 <= 360) -> df.red 
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "QEBM.2", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "QEBM.2", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)
}


# Let's add ivr modelling next, if removed from the model.large.vs.short vectors

{
### blocs.retournes.fr.tr

## vars.short

frmla. <- as.formula(paste("blocs.retournes.fr.tr", paste(c(model.short.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("blocs.retournes.fr.tr", vars.short.ivr)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "blocs.retournes.fr.", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "blocs.retournes.fr.", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("blocs.retournes.fr.tr", vars.short.ivr)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "blocs.retournes.fr.", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "blocs.retournes.fr.", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("blocs.retournes.fr.tr", vars.short.ivr)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "blocs.retournes.fr.", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "blocs.retournes.fr.", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short.ivr", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)

## vars.medium

frmla. <- as.formula(paste("blocs.retournes.fr.tr", paste(c(model.medium.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("blocs.retournes.fr.tr", vars.medium.ivr)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "blocs.retournes.fr.", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "blocs.retournes.fr.", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("blocs.retournes.fr.tr", vars.medium.ivr)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "blocs.retournes.fr.", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "blocs.retournes.fr.", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("blocs.retournes.fr.tr", vars.medium.ivr)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "blocs.retournes.fr.", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "blocs.retournes.fr.", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium.ivr", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)

## vars.large

frmla. <- as.formula(paste("blocs.retournes.fr.tr", paste(c(model.large.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("blocs.retournes.fr.tr", vars.large.ivr)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "blocs.retournes.fr.", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "blocs.retournes.fr.", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("blocs.retournes.fr.tr", vars.large.ivr)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "blocs.retournes.fr.", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "blocs.retournes.fr.", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("blocs.retournes.fr.tr", vars.large.ivr)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "blocs.retournes.fr.", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "blocs.retournes.fr.", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large.ivr", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)
}


# and now remove ivr from model to test the effect of the other variables ivr excluded

{
  ### BM_FS.FI_dist.tr
  
  ## vars.short.ivr
  
  frmla. <- as.formula(paste("BM_FS.FI_dist.tr", paste(c(model.short.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM_FS.FI_dist.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM_FS.FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM_FS.FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM_FS.FI_dist.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM_FS.FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM_FS.FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM_FS.FI_dist.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM_FS.FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM_FS.FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short.ivr", tbl.join$vars.)
  tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)
  
  ## vars.medium.ivr
  
  frmla. <- as.formula(paste("BM_FS.FI_dist.tr", paste(c(model.medium.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM_FS.FI_dist.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM_FS.FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM_FS.FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM_FS.FI_dist.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM_FS.FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM_FS.FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM_FS.FI_dist.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM_FS.FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM_FS.FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium.ivr", tbl.join$vars.)
  tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)
  
  ## vars.large.ivr
  
  frmla. <- as.formula(paste("BM_FS.FI_dist.tr", paste(c(model.large.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM_FS.FI_dist.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM_FS.FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM_FS.FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM_FS.FI_dist.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM_FS.FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM_FS.FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM_FS.FI_dist.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM_FS.FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM_FS.FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large.ivr", tbl.join$vars.)
  tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)
  
  
  ### BM.BF_FS_dist.tr
  
  ## vars.short.ivr
  
  frmla. <- as.formula(paste("BM.BF_FS_dist.tr", paste(c(model.short.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.BF_FS_dist.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM.BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM.BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.BF_FS_dist.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM.BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM.BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.BF_FS_dist.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM.BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM.BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short.ivr", tbl.join$vars.)
  tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)
  
  ## vars.medium.ivr
  
  frmla. <- as.formula(paste("BM.BF_FS_dist.tr", paste(c(model.medium.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.BF_FS_dist.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM.BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM.BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.BF_FS_dist.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM.BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM.BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.BF_FS_dist.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM.BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM.BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium.ivr", tbl.join$vars.)
  tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)
  
  ## vars.large.ivr
  
  frmla. <- as.formula(paste("BM.BF_FS_dist.tr", paste(c(model.large.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.BF_FS_dist.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM.BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM.BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.BF_FS_dist.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM.BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM.BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.BF_FS_dist.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM.BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM.BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large.ivr", tbl.join$vars.)
  tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)
  
  
  ### BM.subnorm2.BF_dist.tr
  
  ## vars.short.ivr
  
  frmla. <- as.formula(paste("BM.subnorm2.BF_dist.tr", paste(c("1", model.short.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.subnorm2.BF_dist.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM.subnorm2.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM.subnorm2.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.subnorm2.BF_dist.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM.subnorm2.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM.subnorm2.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.subnorm2.BF_dist.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM.subnorm2.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM.subnorm2.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short.ivr", tbl.join$vars.)
  tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)
  
  ## vars.medium.ivr
  
  frmla. <- as.formula(paste("BM.subnorm2.BF_dist.tr", paste(c(model.medium.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.subnorm2.BF_dist.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM.subnorm2.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM.subnorm2.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.subnorm2.BF_dist.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM.subnorm2.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM.subnorm2.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.subnorm2.BF_dist.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM.subnorm2.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM.subnorm2.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium.ivr", tbl.join$vars.)
  tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)
  
  ## vars.large.ivr
  
  frmla. <- as.formula(paste("BM.subnorm2.BF_dist.tr", paste(c(model.large.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.subnorm2.BF_dist.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM.subnorm2.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM.subnorm2.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.subnorm2.BF_dist.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM.subnorm2.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM.subnorm2.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.subnorm2.BF_dist.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM.subnorm2.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM.subnorm2.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large.ivr", tbl.join$vars.)
  tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)
  
  
  ### BM.sum.BF_dist.tr
  
  ## vars.short.ivr
  
  frmla. <- as.formula(paste("BM.sum.BF_dist.tr", paste(c("1", model.short.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.sum.BF_dist.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM.sum.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM.sum.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.sum.BF_dist.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM.sum.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM.sum.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.sum.BF_dist.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM.sum.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM.sum.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short.ivr", tbl.join$vars.)
  tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)
  
  ## vars.medium.ivr
  
  frmla. <- as.formula(paste("BM.sum.BF_dist.tr", paste(c(model.medium.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.sum.BF_dist.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM.sum.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM.sum.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.sum.BF_dist.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM.sum.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM.sum.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.sum.BF_dist.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM.sum.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM.sum.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium.ivr", tbl.join$vars.)
  tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)
  
  ## vars.large.ivr
  
  frmla. <- as.formula(paste("BM.sum.BF_dist.tr", paste(c(model.large.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.sum.BF_dist.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM.sum.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM.sum.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.sum.BF_dist.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM.sum.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM.sum.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.sum.BF_dist.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM.sum.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM.sum.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large.ivr", tbl.join$vars.)
  tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)
  
  
  ### BM.prod.BF_dist.tr
  
  ## vars.short.ivr
  
  frmla. <- as.formula(paste("BM.prod.BF_dist.tr", paste(c("1", model.short.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.prod.BF_dist.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM.prod.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM.prod.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.prod.BF_dist.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM.prod.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM.prod.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.prod.BF_dist.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM.prod.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM.prod.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short.ivr", tbl.join$vars.)
  tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)
  
  ## vars.medium.ivr
  
  frmla. <- as.formula(paste("BM.prod.BF_dist.tr", paste(c(model.medium.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.prod.BF_dist.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM.prod.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM.prod.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.prod.BF_dist.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM.prod.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM.prod.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.prod.BF_dist.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM.prod.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM.prod.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium.ivr", tbl.join$vars.)
  tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)
  
  ## vars.large.ivr
  
  frmla. <- as.formula(paste("BM.prod.BF_dist.tr", paste(c(model.large.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.prod.BF_dist.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM.prod.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM.prod.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.prod.BF_dist.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM.prod.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM.prod.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.prod.BF_dist.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM.prod.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM.prod.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large.ivr", tbl.join$vars.)
  tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)
  
  
  ### Third.Quartile.BM_FS.FI_dist.tr
  
  ## vars.short.ivr
  
  frmla. <- as.formula(paste("BM_FS.FI_matri.full.Third.Quartile.BM_FS.FI_dist.tr", paste(c("1", model.short.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM_FS.FI_matri.full.Third.Quartile.BM_FS.FI_dist.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Third.Quartile.BM_FS.FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Third.Quartile.BM_FS.FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM_FS.FI_matri.full.Third.Quartile.BM_FS.FI_dist.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Third.Quartile.BM_FS.FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Third.Quartile.BM_FS.FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM_FS.FI_matri.full.Third.Quartile.BM_FS.FI_dist.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Third.Quartile.BM_FS.FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Third.Quartile.BM_FS.FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short.ivr", tbl.join$vars.)
  tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)
  
  ## vars.medium.ivr
  
  frmla. <- as.formula(paste("BM_FS.FI_matri.full.Third.Quartile.BM_FS.FI_dist.tr", paste(c(model.medium.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM_FS.FI_matri.full.Third.Quartile.BM_FS.FI_dist.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Third.Quartile.BM_FS.FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Third.Quartile.BM_FS.FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM_FS.FI_matri.full.Third.Quartile.BM_FS.FI_dist.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Third.Quartile.BM_FS.FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Third.Quartile.BM_FS.FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM_FS.FI_matri.full.Third.Quartile.BM_FS.FI_dist.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Third.Quartile.BM_FS.FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Third.Quartile.BM_FS.FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium.ivr", tbl.join$vars.)
  tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)
  
  ## vars.large.ivr
  
  frmla. <- as.formula(paste("BM_FS.FI_matri.full.Third.Quartile.BM_FS.FI_dist.tr", paste(c(model.large.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM_FS.FI_matri.full.Third.Quartile.BM_FS.FI_dist.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Third.Quartile.BM_FS.FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Third.Quartile.BM_FS.FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM_FS.FI_matri.full.Third.Quartile.BM_FS.FI_dist.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Third.Quartile.BM_FS.FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Third.Quartile.BM_FS.FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM_FS.FI_matri.full.Third.Quartile.BM_FS.FI_dist.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Third.Quartile.BM_FS.FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Third.Quartile.BM_FS.FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large.ivr", tbl.join$vars.)
  tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)
  
  
  ### Third.Quartile.BM.BF_FS_dist.tr
  
  ## vars.short.ivr
  
  frmla. <- as.formula(paste("BM.BF_FS_matri.full.Third.Quartile.BM.BF_FS_dist.tr", paste(c(model.short.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.BF_FS_matri.full.Third.Quartile.BM.BF_FS_dist.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Third.Quartile.BM.BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Third.Quartile.BM.BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.BF_FS_matri.full.Third.Quartile.BM.BF_FS_dist.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Third.Quartile.BM.BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Third.Quartile.BM.BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.BF_FS_matri.full.Third.Quartile.BM.BF_FS_dist.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Third.Quartile.BM.BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Third.Quartile.BM.BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short.ivr", tbl.join$vars.)
  tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)
  
  ## vars.medium.ivr
  
  frmla. <- as.formula(paste("BM.BF_FS_matri.full.Third.Quartile.BM.BF_FS_dist.tr", paste(c(model.medium.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.BF_FS_matri.full.Third.Quartile.BM.BF_FS_dist.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Third.Quartile.BM.BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Third.Quartile.BM.BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.BF_FS_matri.full.Third.Quartile.BM.BF_FS_dist.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Third.Quartile.BM.BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Third.Quartile.BM.BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.BF_FS_matri.full.Third.Quartile.BM.BF_FS_dist.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Third.Quartile.BM.BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Third.Quartile.BM.BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium.ivr", tbl.join$vars.)
  tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)
  
  ## vars.large.ivr
  
  frmla. <- as.formula(paste("BM.BF_FS_matri.full.Third.Quartile.BM.BF_FS_dist.tr", paste(c(model.large.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.BF_FS_matri.full.Third.Quartile.BM.BF_FS_dist.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Third.Quartile.BM.BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Third.Quartile.BM.BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.BF_FS_matri.full.Third.Quartile.BM.BF_FS_dist.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Third.Quartile.BM.BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Third.Quartile.BM.BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.BF_FS_matri.full.Third.Quartile.BM.BF_FS_dist.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Third.Quartile.BM.BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Third.Quartile.BM.BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large.ivr", tbl.join$vars.)
  tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)
  
  
  ### BM.subnorm2.BF_Third.Quartile_dist.tr
  
  ## vars.short.ivr
  
  frmla. <- as.formula(paste("BM.subnorm2.BF_Third.Quartile_dist.tr", paste(c("1", model.short.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.subnorm2.BF_Third.Quartile_dist.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Third.Quartile.BM.subnorm2.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Third.Quartile.BM.subnorm2.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.subnorm2.BF_Third.Quartile_dist.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Third.Quartile.BM.subnorm2.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Third.Quartile.BM.subnorm2.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.subnorm2.BF_Third.Quartile_dist.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Third.Quartile.BM.subnorm2.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Third.Quartile.BM.subnorm2.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short.ivr", tbl.join$vars.)
  tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)
  
  ## vars.medium.ivr
  
  frmla. <- as.formula(paste("BM.subnorm2.BF_Third.Quartile_dist.tr", paste(c(model.medium.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.subnorm2.BF_Third.Quartile_dist.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Third.Quartile.BM.subnorm2.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Third.Quartile.BM.subnorm2.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.subnorm2.BF_Third.Quartile_dist.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Third.Quartile.BM.subnorm2.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Third.Quartile.BM.subnorm2.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.subnorm2.BF_Third.Quartile_dist.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Third.Quartile.BM.subnorm2.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Third.Quartile.BM.subnorm2.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium.ivr", tbl.join$vars.)
  tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)
  
  ## vars.large.ivr
  
  frmla. <- as.formula(paste("BM.subnorm2.BF_Third.Quartile_dist.tr", paste(c(model.large.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.subnorm2.BF_Third.Quartile_dist.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Third.Quartile.BM.subnorm2.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Third.Quartile.BM.subnorm2.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.subnorm2.BF_Third.Quartile_dist.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Third.Quartile.BM.subnorm2.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Third.Quartile.BM.subnorm2.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.subnorm2.BF_Third.Quartile_dist.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Third.Quartile.BM.subnorm2.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Third.Quartile.BM.subnorm2.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large.ivr", tbl.join$vars.)
  tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)
  
  
  ### BM.sum.BF_Third.Quartile_dist.tr
  
  ## vars.short.ivr
  
  frmla. <- as.formula(paste("BM.sum.BF_Third.Quartile_dist.tr", paste(c("1", model.short.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.sum.BF_Third.Quartile_dist.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Third.Quartile.BM.sum.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Third.Quartile.BM.sum.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.sum.BF_Third.Quartile_dist.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Third.Quartile.BM.sum.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Third.Quartile.BM.sum.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.sum.BF_Third.Quartile_dist.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Third.Quartile.BM.sum.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Third.Quartile.BM.sum.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short.ivr", tbl.join$vars.)
  tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)
  
  ## vars.medium.ivr
  
  frmla. <- as.formula(paste("BM.sum.BF_Third.Quartile_dist.tr", paste(c(model.medium.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.sum.BF_Third.Quartile_dist.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Third.Quartile.BM.sum.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Third.Quartile.BM.sum.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.sum.BF_Third.Quartile_dist.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Third.Quartile.BM.sum.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Third.Quartile.BM.sum.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.sum.BF_Third.Quartile_dist.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Third.Quartile.BM.sum.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Third.Quartile.BM.sum.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium.ivr", tbl.join$vars.)
  tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)
  
  ## vars.large.ivr
  
  frmla. <- as.formula(paste("BM.sum.BF_Third.Quartile_dist.tr", paste(c(model.large.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.sum.BF_Third.Quartile_dist.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Third.Quartile.BM.sum.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Third.Quartile.BM.sum.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.sum.BF_Third.Quartile_dist.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Third.Quartile.BM.sum.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Third.Quartile.BM.sum.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.sum.BF_Third.Quartile_dist.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Third.Quartile.BM.sum.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Third.Quartile.BM.sum.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large.ivr", tbl.join$vars.)
  tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)
  
  
  ### BM.prod.BF_Third.Quartile_dist.tr
  
  ## vars.short.ivr
  
  frmla. <- as.formula(paste("BM.prod.BF_Third.Quartile_dist.tr", paste(c("1", model.short.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.prod.BF_Third.Quartile_dist.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Third.Quartile.BM.prod.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Third.Quartile.BM.prod.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.prod.BF_Third.Quartile_dist.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Third.Quartile.BM.prod.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Third.Quartile.BM.prod.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.prod.BF_Third.Quartile_dist.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Third.Quartile.BM.prod.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Third.Quartile.BM.prod.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short.ivr", tbl.join$vars.)
  tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)
  
  ## vars.medium.ivr
  
  frmla. <- as.formula(paste("BM.prod.BF_Third.Quartile_dist.tr", paste(c(model.medium.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.prod.BF_Third.Quartile_dist.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Third.Quartile.BM.prod.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Third.Quartile.BM.prod.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.prod.BF_Third.Quartile_dist.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Third.Quartile.BM.prod.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Third.Quartile.BM.prod.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.prod.BF_Third.Quartile_dist.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Third.Quartile.BM.prod.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Third.Quartile.BM.prod.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium.ivr", tbl.join$vars.)
  tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)
  
  ## vars.large.ivr
  
  frmla. <- as.formula(paste("BM.prod.BF_Third.Quartile_dist.tr", paste(c(model.large.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.prod.BF_Third.Quartile_dist.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Third.Quartile.BM.prod.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Third.Quartile.BM.prod.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.prod.BF_Third.Quartile_dist.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Third.Quartile.BM.prod.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Third.Quartile.BM.prod.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.prod.BF_Third.Quartile_dist.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Third.Quartile.BM.prod.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Third.Quartile.BM.prod.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large.ivr", tbl.join$vars.)
  tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)
  
  
  ### median.BM_FS.FI_dist.tr
  
  ## vars.short.ivr
  
  frmla. <- as.formula(paste("BM_FS.FI_matri.full.median.BM_FS.FI_dist.tr", paste(c("1", model.short.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM_FS.FI_matri.full.median.BM_FS.FI_dist.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "median.BM_FS.FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "median.BM_FS.FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM_FS.FI_matri.full.median.BM_FS.FI_dist.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "median.BM_FS.FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "median.BM_FS.FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM_FS.FI_matri.full.median.BM_FS.FI_dist.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "median.BM_FS.FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "median.BM_FS.FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short.ivr", tbl.join$vars.)
  tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)
  
  ## vars.medium.ivr
  
  frmla. <- as.formula(paste("BM_FS.FI_matri.full.median.BM_FS.FI_dist.tr", paste(c(model.medium.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM_FS.FI_matri.full.median.BM_FS.FI_dist.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "median.BM_FS.FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "median.BM_FS.FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM_FS.FI_matri.full.median.BM_FS.FI_dist.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "median.BM_FS.FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "median.BM_FS.FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM_FS.FI_matri.full.median.BM_FS.FI_dist.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "median.BM_FS.FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "median.BM_FS.FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium.ivr", tbl.join$vars.)
  tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)
  
  ## vars.large.ivr
  
  frmla. <- as.formula(paste("BM_FS.FI_matri.full.median.BM_FS.FI_dist.tr", paste(c(model.large.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM_FS.FI_matri.full.median.BM_FS.FI_dist.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "median.BM_FS.FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "median.BM_FS.FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM_FS.FI_matri.full.median.BM_FS.FI_dist.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "median.BM_FS.FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "median.BM_FS.FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM_FS.FI_matri.full.median.BM_FS.FI_dist.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "median.BM_FS.FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "median.BM_FS.FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large.ivr", tbl.join$vars.)
  tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)
  
  
  ### median.BM.BF_FS_dist.tr
  
  ## vars.short.ivr
  
  frmla. <- as.formula(paste("BM.BF_FS_matri.full.median.BM.BF_FS_dist.tr", paste(c(model.short.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.BF_FS_matri.full.median.BM.BF_FS_dist.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "median.BM.BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "median.BM.BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.BF_FS_matri.full.median.BM.BF_FS_dist.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "median.BM.BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "median.BM.BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.BF_FS_matri.full.median.BM.BF_FS_dist.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "median.BM.BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "median.BM.BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short.ivr", tbl.join$vars.)
  tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)
  
  ## vars.medium.ivr
  
  frmla. <- as.formula(paste("BM.BF_FS_matri.full.median.BM.BF_FS_dist.tr", paste(c(model.medium.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.BF_FS_matri.full.median.BM.BF_FS_dist.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "median.BM.BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "median.BM.BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.BF_FS_matri.full.median.BM.BF_FS_dist.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "median.BM.BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "median.BM.BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.BF_FS_matri.full.median.BM.BF_FS_dist.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "median.BM.BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "median.BM.BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium.ivr", tbl.join$vars.)
  tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)
  
  ## vars.large.ivr
  
  frmla. <- as.formula(paste("BM.BF_FS_matri.full.median.BM.BF_FS_dist.tr", paste(c(model.large.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.BF_FS_matri.full.median.BM.BF_FS_dist.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "median.BM.BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "median.BM.BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.BF_FS_matri.full.median.BM.BF_FS_dist.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "median.BM.BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "median.BM.BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.BF_FS_matri.full.median.BM.BF_FS_dist.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "median.BM.BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "median.BM.BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large.ivr", tbl.join$vars.)
  tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)
  
  
  ### BM.subnorm2.BF_median_dist.tr
  
  ## vars.short.ivr
  
  frmla. <- as.formula(paste("BM.subnorm2.BF_median_dist.tr", paste(c("1", model.short.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.subnorm2.BF_median_dist.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "median.BM.subnorm2.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "median.BM.subnorm2.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.subnorm2.BF_median_dist.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "median.BM.subnorm2.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "median.BM.subnorm2.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.subnorm2.BF_median_dist.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "median.BM.subnorm2.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "median.BM.subnorm2.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short.ivr", tbl.join$vars.)
  tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)
  
  ## vars.medium.ivr
  
  frmla. <- as.formula(paste("BM.subnorm2.BF_median_dist.tr", paste(c(model.medium.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.subnorm2.BF_median_dist.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "median.BM.subnorm2.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "median.BM.subnorm2.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.subnorm2.BF_median_dist.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "median.BM.subnorm2.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "median.BM.subnorm2.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.subnorm2.BF_median_dist.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "median.BM.subnorm2.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "median.BM.subnorm2.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium.ivr", tbl.join$vars.)
  tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)
  
  ## vars.large.ivr
  
  frmla. <- as.formula(paste("BM.subnorm2.BF_median_dist.tr", paste(c(model.large.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.subnorm2.BF_median_dist.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "median.BM.subnorm2.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "median.BM.subnorm2.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.subnorm2.BF_median_dist.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "median.BM.subnorm2.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "median.BM.subnorm2.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.subnorm2.BF_median_dist.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "median.BM.subnorm2.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "median.BM.subnorm2.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large.ivr", tbl.join$vars.)
  tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)
  
  
  ### BM.sum.BF_median_dist.tr
  
  ## vars.short.ivr
  
  frmla. <- as.formula(paste("BM.sum.BF_median_dist.tr", paste(c("1", model.short.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.sum.BF_median_dist.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "median.BM.sum.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "median.BM.sum.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.sum.BF_median_dist.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "median.BM.sum.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "median.BM.sum.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.sum.BF_median_dist.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "median.BM.sum.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "median.BM.sum.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short.ivr", tbl.join$vars.)
  tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)
  
  ## vars.medium.ivr
  
  frmla. <- as.formula(paste("BM.sum.BF_median_dist.tr", paste(c(model.medium.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.sum.BF_median_dist.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "median.BM.sum.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "median.BM.sum.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.sum.BF_median_dist.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "median.BM.sum.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "median.BM.sum.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.sum.BF_median_dist.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "median.BM.sum.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "median.BM.sum.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium.ivr", tbl.join$vars.)
  tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)
  
  ## vars.large.ivr
  
  frmla. <- as.formula(paste("BM.sum.BF_median_dist.tr", paste(c(model.large.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.sum.BF_median_dist.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "median.BM.sum.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "median.BM.sum.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.sum.BF_median_dist.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "median.BM.sum.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "median.BM.sum.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.sum.BF_median_dist.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "median.BM.sum.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "median.BM.sum.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large.ivr", tbl.join$vars.)
  tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)
  
  
  ### BM.prod.BF_median_dist.tr
  
  ## vars.short.ivr
  
  frmla. <- as.formula(paste("BM.prod.BF_median_dist.tr", paste(c("1", model.short.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.prod.BF_median_dist.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "median.BM.prod.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "median.BM.prod.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.prod.BF_median_dist.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "median.BM.prod.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "median.BM.prod.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.prod.BF_median_dist.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "median.BM.prod.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "median.BM.prod.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short.ivr", tbl.join$vars.)
  tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)
  
  ## vars.medium.ivr
  
  frmla. <- as.formula(paste("BM.prod.BF_median_dist.tr", paste(c(model.medium.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.prod.BF_median_dist.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "median.BM.prod.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "median.BM.prod.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.prod.BF_median_dist.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "median.BM.prod.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "median.BM.prod.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.prod.BF_median_dist.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "median.BM.prod.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "median.BM.prod.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium.ivr", tbl.join$vars.)
  tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)
  
  ## vars.large.ivr
  
  frmla. <- as.formula(paste("BM.prod.BF_median_dist.tr", paste(c(model.large.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("BM.prod.BF_median_dist.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "median.BM.prod.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "median.BM.prod.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.prod.BF_median_dist.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "median.BM.prod.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "median.BM.prod.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.prod.BF_median_dist.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "median.BM.prod.BF", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "median.BM.prod.BF", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large.ivr", tbl.join$vars.)
  tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)
  
  
  ### richness.tr face supérieure bloc mobile
  
  ## vars.short.ivr
  
  frmla. <- as.formula(paste("richness.tr", paste(c(model.short.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Face == "face supérieure")[, c("richness.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "richness BM_FS", vars. = vars.list.M1, transfo. = "center_scale", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "richness BM_FS", vars. = vars.list.M2, transfo. = "center_scale", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne", Face == "face supérieure")[, c("richness.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "richness BM_FS", vars. = vars.list.M1, transfo. = "center_scale", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "richness BM_FS", vars. = vars.list.M2, transfo. = "center_scale", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ", Face == "face supérieure")[, c("richness.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "richness BM_FS", vars. = vars.list.M1, transfo. = "center_scale", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "richness BM_FS", vars. = vars.list.M2, transfo. = "center_scale", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short.ivr", tbl.join$vars.)
  tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)
  
  ## vars.medium.ivr
  
  frmla. <- as.formula(paste("richness.tr", paste(c(model.medium.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Face == "face supérieure")[, c("richness.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "richness BM_FS", vars. = vars.list.M1, transfo. = "center_scale", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "richness BM_FS", vars. = vars.list.M2, transfo. = "center_scale", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne", Face == "face supérieure")[, c("richness.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "richness BM_FS", vars. = vars.list.M1, transfo. = "center_scale", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "richness BM_FS", vars. = vars.list.M2, transfo. = "center_scale", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ", Face == "face supérieure")[, c("richness.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "richness BM_FS", vars. = vars.list.M1, transfo. = "center_scale", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "richness BM_FS", vars. = vars.list.M2, transfo. = "center_scale", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium.ivr", tbl.join$vars.)
  tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)
  
  ## vars.large.ivr
  
  frmla. <- as.formula(paste("richness.tr", paste(c(model.large.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Face == "face supérieure")[, c("richness.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "richness BM_FS", vars. = vars.list.M1, transfo. = "center_scale", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "richness BM_FS", vars. = vars.list.M2, transfo. = "center_scale", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne", Face == "face supérieure")[, c("richness.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "richness BM_FS", vars. = vars.list.M1, transfo. = "center_scale", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "richness BM_FS", vars. = vars.list.M2, transfo. = "center_scale", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ", Face == "face supérieure")[, c("richness.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "richness BM_FS", vars. = vars.list.M1, transfo. = "center_scale", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "richness BM_FS", vars. = vars.list.M2, transfo. = "center_scale", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large.ivr", tbl.join$vars.)
  tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)
  
  
  ### richness.tr face inférieure bloc mobile
  
  ## vars.short.ivr
  
  frmla. <- as.formula(paste("richness.tr", paste(c(model.short.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Face == "face inférieure")[, c("richness.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "richness BM_FI", vars. = vars.list.M1, transfo. = "center_scale", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "richness BM_FI", vars. = vars.list.M2, transfo. = "center_scale", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne", Face == "face inférieure")[, c("richness.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "richness BM_FI", vars. = vars.list.M1, transfo. = "center_scale", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "richness BM_FI", vars. = vars.list.M2, transfo. = "center_scale", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ", Face == "face inférieure")[, c("richness.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "richness BM_FI", vars. = vars.list.M1, transfo. = "center_scale", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "richness BM_FI", vars. = vars.list.M2, transfo. = "center_scale", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short.ivr", tbl.join$vars.)
  tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)
  
  ## vars.medium.ivr
  
  frmla. <- as.formula(paste("richness.tr", paste(c(model.medium.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Face == "face inférieure")[, c("richness.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "richness BM_FI", vars. = vars.list.M1, transfo. = "center_scale", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "richness BM_FI", vars. = vars.list.M2, transfo. = "center_scale", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne", Face == "face inférieure")[, c("richness.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "richness BM_FI", vars. = vars.list.M1, transfo. = "center_scale", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "richness BM_FI", vars. = vars.list.M2, transfo. = "center_scale", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ", Face == "face inférieure")[, c("richness.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "richness BM_FI", vars. = vars.list.M1, transfo. = "center_scale", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "richness BM_FI", vars. = vars.list.M2, transfo. = "center_scale", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium.ivr", tbl.join$vars.)
  tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)
  
  ## vars.large.ivr
  
  frmla. <- as.formula(paste("richness.tr", paste(c(model.large.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Face == "face inférieure")[, c("richness.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "richness BM_FI", vars. = vars.list.M1, transfo. = "center_scale", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "richness BM_FI", vars. = vars.list.M2, transfo. = "center_scale", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne", Face == "face inférieure")[, c("richness.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "richness BM_FI", vars. = vars.list.M1, transfo. = "center_scale", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "richness BM_FI", vars. = vars.list.M2, transfo. = "center_scale", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ", Face == "face inférieure")[, c("richness.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "richness BM_FI", vars. = vars.list.M1, transfo. = "center_scale", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "richness BM_FI", vars. = vars.list.M2, transfo. = "center_scale", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large.ivr", tbl.join$vars.)
  tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)
  
  
  ### richness.tr face supérieure bloc fixé
  
  ## vars.short.ivr
  
  frmla. <- as.formula(paste("richness.tr", paste(c(model.short.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Face == "face supérieure")[, c("richness.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "richness BF_FS", vars. = vars.list.M1, transfo. = "center_scale", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "richness BF_FS", vars. = vars.list.M2, transfo. = "center_scale", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Region == "Bretagne", Face == "face supérieure")[, c("richness.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "richness BF_FS", vars. = vars.list.M1, transfo. = "center_scale", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "richness BF_FS", vars. = vars.list.M2, transfo. = "center_scale", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Region == "EGMP.BASQ", Face == "face supérieure")[, c("richness.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "richness BF_FS", vars. = vars.list.M1, transfo. = "center_scale", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "richness BF_FS", vars. = vars.list.M2, transfo. = "center_scale", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short.ivr", tbl.join$vars.)
  tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)
  
  ## vars.medium.ivr
  
  frmla. <- as.formula(paste("richness.tr", paste(c(model.medium.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Face == "face supérieure")[, c("richness.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "richness BF_FS", vars. = vars.list.M1, transfo. = "center_scale", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "richness BF_FS", vars. = vars.list.M2, transfo. = "center_scale", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Region == "Bretagne", Face == "face supérieure")[, c("richness.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "richness BF_FS", vars. = vars.list.M1, transfo. = "center_scale", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "richness BF_FS", vars. = vars.list.M2, transfo. = "center_scale", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Region == "EGMP.BASQ", Face == "face supérieure")[, c("richness.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "richness BF_FS", vars. = vars.list.M1, transfo. = "center_scale", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "richness BF_FS", vars. = vars.list.M2, transfo. = "center_scale", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium.ivr", tbl.join$vars.)
  tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)
  
  ## vars.large.ivr
  
  frmla. <- as.formula(paste("richness.tr", paste(c(model.large.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Face == "face supérieure")[, c("richness.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "richness BF_FS", vars. = vars.list.M1, transfo. = "center_scale", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "richness BF_FS", vars. = vars.list.M2, transfo. = "center_scale", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Region == "Bretagne", Face == "face supérieure")[, c("richness.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "richness BF_FS", vars. = vars.list.M1, transfo. = "center_scale", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "richness BF_FS", vars. = vars.list.M2, transfo. = "center_scale", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Region == "EGMP.BASQ", Face == "face supérieure")[, c("richness.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "richness BF_FS", vars. = vars.list.M1, transfo. = "center_scale", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "richness BF_FS", vars. = vars.list.M2, transfo. = "center_scale", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large.ivr", tbl.join$vars.)
  tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)
  
  
  ### Shannon.tr face supérieure bloc mobile
  
  ## vars.short.ivr
  
  frmla. <- as.formula(paste("Shannon.tr", paste(c(model.short.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Face == "face supérieure")[, c("Shannon.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Shannon BM_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Shannon BM_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne", Face == "face supérieure")[, c("Shannon.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Shannon BM_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Shannon BM_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ", Face == "face supérieure")[, c("Shannon.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Shannon BM_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Shannon BM_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short.ivr", tbl.join$vars.)
  tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)
  
  ## vars.medium.ivr
  
  frmla. <- as.formula(paste("Shannon.tr", paste(c(model.medium.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Face == "face supérieure")[, c("Shannon.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Shannon BM_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Shannon BM_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne", Face == "face supérieure")[, c("Shannon.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Shannon BM_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Shannon BM_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ", Face == "face supérieure")[, c("Shannon.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Shannon BM_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Shannon BM_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium.ivr", tbl.join$vars.)
  tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)
  
  ## vars.large.ivr
  
  frmla. <- as.formula(paste("Shannon.tr", paste(c(model.large.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Face == "face supérieure")[, c("Shannon.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Shannon BM_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Shannon BM_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne", Face == "face supérieure")[, c("Shannon.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Shannon BM_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Shannon BM_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ", Face == "face supérieure")[, c("Shannon.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Shannon BM_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Shannon BM_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large.ivr", tbl.join$vars.)
  tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)
  
  
  ### Shannon.tr face inférieure bloc mobile
  
  ## vars.short.ivr
  
  frmla. <- as.formula(paste("Shannon.tr", paste(c(model.short.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Face == "face inférieure")[, c("Shannon.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Shannon BM_FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Shannon BM_FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne", Face == "face inférieure")[, c("Shannon.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Shannon BM_FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Shannon BM_FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ", Face == "face inférieure")[, c("Shannon.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Shannon BM_FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Shannon BM_FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short.ivr", tbl.join$vars.)
  tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)
  
  ## vars.medium.ivr
  
  frmla. <- as.formula(paste("Shannon.tr", paste(c(model.medium.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Face == "face inférieure")[, c("Shannon.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Shannon BM_FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Shannon BM_FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne", Face == "face inférieure")[, c("Shannon.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Shannon BM_FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Shannon BM_FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ", Face == "face inférieure")[, c("Shannon.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Shannon BM_FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Shannon BM_FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium.ivr", tbl.join$vars.)
  tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)
  
  ## vars.large.ivr
  
  frmla. <- as.formula(paste("Shannon.tr", paste(c(model.large.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Face == "face inférieure")[, c("Shannon.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Shannon BM_FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Shannon BM_FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne", Face == "face inférieure")[, c("Shannon.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Shannon BM_FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Shannon BM_FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ", Face == "face inférieure")[, c("Shannon.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Shannon BM_FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Shannon BM_FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large.ivr", tbl.join$vars.)
  tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)
  
  
  ### Shannon.tr face supérieure bloc fixé
  
  ## vars.short.ivr
  
  frmla. <- as.formula(paste("Shannon.tr", paste(c(model.short.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Face == "face supérieure")[, c("Shannon.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Shannon BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Shannon BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Region == "Bretagne", Face == "face supérieure")[, c("Shannon.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Shannon BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Shannon BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Region == "EGMP.BASQ", Face == "face supérieure")[, c("Shannon.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Shannon BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Shannon BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short.ivr", tbl.join$vars.)
  tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)
  
  ## vars.medium.ivr
  
  frmla. <- as.formula(paste("Shannon.tr", paste(c(model.medium.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Face == "face supérieure")[, c("Shannon.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Shannon BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Shannon BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Region == "Bretagne", Face == "face supérieure")[, c("Shannon.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Shannon BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Shannon BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Region == "EGMP.BASQ", Face == "face supérieure")[, c("Shannon.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Shannon BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Shannon BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium.ivr", tbl.join$vars.)
  tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)
  
  ## vars.large.ivr
  
  frmla. <- as.formula(paste("Shannon.tr", paste(c(model.large.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Face == "face supérieure")[, c("Shannon.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Shannon BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Shannon BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Region == "Bretagne", Face == "face supérieure")[, c("Shannon.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Shannon BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Shannon BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Region == "EGMP.BASQ", Face == "face supérieure")[, c("Shannon.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Shannon BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Shannon BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large.ivr", tbl.join$vars.)
  tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)
  
  
  ### Simpson.tr face supérieure bloc mobile
  
  ## vars.short.ivr
  
  frmla. <- as.formula(paste("Simpson.tr", paste(c(model.short.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Face == "face supérieure")[, c("Simpson.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Simpson BM_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Simpson BM_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne", Face == "face supérieure")[, c("Simpson.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Simpson BM_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Simpson BM_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ", Face == "face supérieure")[, c("Simpson.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Simpson BM_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Simpson BM_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short.ivr", tbl.join$vars.)
  tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)
  
  ## vars.medium.ivr
  
  frmla. <- as.formula(paste("Simpson.tr", paste(c(model.medium.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Face == "face supérieure")[, c("Simpson.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Simpson BM_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Simpson BM_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne", Face == "face supérieure")[, c("Simpson.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Simpson BM_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Simpson BM_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ", Face == "face supérieure")[, c("Simpson.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Simpson BM_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Simpson BM_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium.ivr", tbl.join$vars.)
  tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)
  
  ## vars.large.ivr
  
  frmla. <- as.formula(paste("Simpson.tr", paste(c(model.large.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Face == "face supérieure")[, c("Simpson.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Simpson BM_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Simpson BM_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne", Face == "face supérieure")[, c("Simpson.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Simpson BM_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Simpson BM_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ", Face == "face supérieure")[, c("Simpson.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Simpson BM_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Simpson BM_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large.ivr", tbl.join$vars.)
  tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)
  
  
  ### Simpson.tr face inférieure bloc mobile
  
  ## vars.short.ivr
  
  frmla. <- as.formula(paste("Simpson.tr", paste(c(model.short.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Face == "face inférieure")[, c("Simpson.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Simpson BM_FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Simpson BM_FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne", Face == "face inférieure")[, c("Simpson.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Simpson BM_FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Simpson BM_FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ", Face == "face inférieure")[, c("Simpson.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Simpson BM_FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Simpson BM_FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short.ivr", tbl.join$vars.)
  tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)
  
  ## vars.medium.ivr
  
  frmla. <- as.formula(paste("Simpson.tr", paste(c(model.medium.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Face == "face inférieure")[, c("Simpson.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Simpson BM_FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Simpson BM_FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne", Face == "face inférieure")[, c("Simpson.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Simpson BM_FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Simpson BM_FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ", Face == "face inférieure")[, c("Simpson.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Simpson BM_FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Simpson BM_FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium.ivr", tbl.join$vars.)
  tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)
  
  ## vars.large.ivr
  
  frmla. <- as.formula(paste("Simpson.tr", paste(c(model.large.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Face == "face inférieure")[, c("Simpson.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Simpson BM_FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Simpson BM_FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne", Face == "face inférieure")[, c("Simpson.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Simpson BM_FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Simpson BM_FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ", Face == "face inférieure")[, c("Simpson.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Simpson BM_FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Simpson BM_FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large.ivr", tbl.join$vars.)
  tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)
  
  
  ### Simpson.tr face supérieure bloc fixé
  
  ## vars.short.ivr
  
  frmla. <- as.formula(paste("Simpson.tr", paste(c(model.short.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Face == "face supérieure")[, c("Simpson.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Simpson BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Simpson BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Region == "Bretagne", Face == "face supérieure")[, c("Simpson.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Simpson BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Simpson BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Region == "EGMP.BASQ", Face == "face supérieure")[, c("Simpson.tr", vars.short.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Simpson BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Simpson BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short.ivr", tbl.join$vars.)
  tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)
  
  ## vars.medium.ivr
  
  frmla. <- as.formula(paste("Simpson.tr", paste(c(model.medium.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Face == "face supérieure")[, c("Simpson.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Simpson BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Simpson BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Region == "Bretagne", Face == "face supérieure")[, c("Simpson.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Simpson BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Simpson BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Region == "EGMP.BASQ", Face == "face supérieure")[, c("Simpson.tr", vars.medium.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Simpson BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Simpson BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium.ivr", tbl.join$vars.)
  tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)
  
  ## vars.large.ivr
  
  frmla. <- as.formula(paste("Simpson.tr", paste(c(model.large.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Face == "face supérieure")[, c("Simpson.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Simpson BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Simpson BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Region == "Bretagne", Face == "face supérieure")[, c("Simpson.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Simpson BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Simpson BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Region == "EGMP.BASQ", Face == "face supérieure")[, c("Simpson.tr", vars.large.ivr)])
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Simpson BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Simpson BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large.ivr", tbl.join$vars.)
  tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)
  
  
  ### QEBM.2.tr bloc mobile
  
  ## vars.short.ivr
  
  frmla. <- as.formula(paste("QEBM.2.tr", paste(c(model.short.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("QEBM.2.tr", vars.short.ivr)])
  #filter(df.red, QEBM.2 >= -360) -> df.red 
  #filter(df.red, QEBM.2 <= 360) -> df.red 
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "QEBM.2", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "QEBM.2", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("QEBM.2.tr", vars.short.ivr)])
  #filter(df.red, QEBM.2 >= -360) -> df.red 
  #filter(df.red, QEBM.2 <= 360) -> df.red 
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "QEBM.2", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "QEBM.2", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("QEBM.2.tr", vars.short.ivr)])
  #filter(df.red, QEBM.2 >= -360) -> df.red 
  #filter(df.red, QEBM.2 <= 360) -> df.red 
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "QEBM.2", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "QEBM.2", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short.ivr", tbl.join$vars.)
  tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)
  
  ## vars.medium.ivr
  
  frmla. <- as.formula(paste("QEBM.2.tr", paste(c(model.medium.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("QEBM.2.tr", vars.medium.ivr)])
  #filter(df.red, QEBM.2 >= -360) -> df.red 
  #filter(df.red, QEBM.2 <= 360) -> df.red 
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "QEBM.2", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "QEBM.2", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("QEBM.2.tr", vars.medium.ivr)])
  #filter(df.red, QEBM.2 >= -360) -> df.red 
  #filter(df.red, QEBM.2 <= 360) -> df.red 
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "QEBM.2", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "QEBM.2", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("QEBM.2.tr", vars.medium.ivr)])
  #filter(df.red, QEBM.2 >= -360) -> df.red 
  #filter(df.red, QEBM.2 <= 360) -> df.red 
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "QEBM.2", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "QEBM.2", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium.ivr", tbl.join$vars.)
  tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)
  
  ## vars.large.ivr
  
  frmla. <- as.formula(paste("QEBM.2.tr", paste(c(model.large.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))
  
  # Atlantique
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile")[, c("QEBM.2.tr", vars.large.ivr)])
  #filter(df.red, QEBM.2 >= -360) -> df.red 
  #filter(df.red, QEBM.2 <= 360) -> df.red 
  get.vars.()
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "QEBM.2", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Atlantique", dataset. = "QEBM.2", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # Bretagne
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("QEBM.2.tr", vars.large.ivr)])
  #filter(df.red, QEBM.2 >= -360) -> df.red 
  #filter(df.red, QEBM.2 <= 360) -> df.red 
  get.vars.()
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "QEBM.2", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "Bretagne", dataset. = "QEBM.2", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  # EGMP.BASQ
  df.red <- na.omit(filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("QEBM.2.tr", vars.large.ivr)])
  #filter(df.red, QEBM.2 >= -360) -> df.red 
  #filter(df.red, QEBM.2 <= 360) -> df.red 
  get.vars.()
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "QEBM.2", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "QEBM.2", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
  tbl.join <- bind_rows(tbl.join, tbl.)
  rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)
  
  tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large.ivr", tbl.join$vars.)
  tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)
}


tbl.models.dredge.modavg <- tbl.join


## Comparison

nrow(unique(tbl.models.step.by.step[, c("vars.", "region", "dataset")]))
nrow(unique(tbl.models.dredge.modavg[, c("vars.", "region", "dataset")]))
setdiff(
  unique(tbl.models.dredge.modavg[, c("vars.", "region", "dataset")]),
  unique(tbl.models.step.by.step[, c("vars.", "region", "dataset")])
)


diffdf(unique(tbl.models.dredge.modavg[, c("vars.", "region", "dataset")]), unique(tbl.models.step.by.step[, c("vars.", "region", "dataset")]))

all.equal(unique(tbl.models.dredge.modavg[, c("vars.", "region", "dataset")]), unique(tbl.models.step.by.step[, c("vars.", "region", "dataset")]))

compa.df <- full_join(unique(tbl.models.dredge.modavg[, c("vars.", "region", "dataset")]), unique(tbl.models.step.by.step[, c("vars.", "region", "dataset")]))
compa.df[rowSums(is.na(compa.df)) > 0,]


step_by_step <- tbl.models.step.by.step

step_by_step <- add_column(step_by_step, approche = "step by step", .before = "vars.")
step_by_step <- add_column(step_by_step, `ef. fix p<.05` = NA, .before = "formule")
step_by_step <- add_column(step_by_step, `n ef. fix p<.05` = NA, .before = "ef. fix p<.05")

for (i in c(1:nrow(step_by_step))) {
  df.i. <- step_by_step[i,]
  step_by_step[i, "ef. fix p<.05"] <- paste0(apply(df.i.[,grep('Sc..* p', names(df.i.), value = TRUE)] < .05, 1, function(x) names(which(x))), collapse = ", ")
  step_by_step[i, "n ef. fix p<.05"] <- length(apply(df.i.[,grep('Sc..* p', names(df.i.), value = TRUE)] < .05, 1, function(x) names(which(x))))
}

tbl.models.step.by.step <- step_by_step


model_average <- tbl.models.dredge.modavg

model_average <- add_column(model_average, approche = "model average", .before = "vars.")
model_average <- add_column(model_average, `ef. fix p<.05` = NA, .before = "formule")
model_average <- add_column(model_average, `n ef. fix p<.05` = NA, .before = "ef. fix p<.05")

for (i in c(1:nrow(model_average))) {
  df.i <- model_average[i,]
  model_average[i, "ef. fix p<.05"] <- paste0(apply(df.i[,grep('Sc..* p', names(df.i), value = TRUE)] < .05, 1, function(x) names(which(x))), collapse = ", ")
  model_average[i, "n ef. fix p<.05"] <- length(apply(df.i[,grep('Sc..* p', names(df.i), value = TRUE)] < .05, 1, function(x) names(which(x))))
}

tbl.models.dredge.modavg <- model_average


tbl.models <- bind_rows(tbl.models.dredge.modavg, tbl.models.step.by.step)
saveRDS(tbl.models, "results/Ecology/tbl.models.RDS") #results/Ecology/
write.csv(tbl.models, "results/Ecology/tbl.models.csv") #results/Ecology/

unique(c(vars.short, vars.short.ivr, vars.medium, vars.medium.ivr, vars.large, vars.large.ivr))
unique(c(model.short, model.short.ivr, model.medium, model.medium.ivr, model.large, model.large.ivr))
tbl.models

tbl.models[] <- lapply(tbl.models, gsub, pattern = "Sc.blocs.retournes.fr.", replacement = "IBFI", fixed = TRUE)
tbl.models[] <- lapply(tbl.models, gsub, pattern = "Sc.Period.nb", replacement = "period", fixed = TRUE)
tbl.models[] <- lapply(tbl.models, gsub, pattern = "Sc.freq.max.3m", replacement = "quant.freq.", fixed = TRUE)
tbl.models[] <- lapply(tbl.models, gsub, pattern = "Sc.max.ratio.norm15min.obs.pre3m", replacement = "fishing", fixed = TRUE)
tbl.models[] <- lapply(tbl.models, gsub, pattern = "Sc.oceano.max.3m", replacement = "hydro.", fixed = TRUE)
tbl.models[] <- lapply(tbl.models, gsub, pattern = "Sc.taille.bloc.gp.nb", replacement = "boulder", fixed = TRUE)
tbl.models[] <- lapply(tbl.models, gsub, pattern = "Sc.accessibilite.gp.nb", replacement = "acces.", fixed = TRUE)
tbl.models[] <- lapply(tbl.models, gsub, pattern = "Sc.frequentation.gp.nb", replacement = "qual.freq.", fixed = TRUE)

tbl.models$`ef. fix p<.05` <- gsub(tbl.models$`ef. fix p<.05`, pattern = " p", replacement = "", fixed = TRUE)
tbl.models$`ef. fix p<.05` <- gsub(tbl.models$`ef. fix p<.05`, pattern = "eriod", replacement = " period", fixed = TRUE)

var.names <- names(tbl.models)

var.names <- gsub(var.names, pattern = "Sc.blocs.retournes.fr.", replacement = "IBFI", fixed = TRUE)
var.names <- gsub(var.names, gsub, pattern = "Sc.Period.nb", replacement = "period", fixed = TRUE)
var.names <- gsub(var.names, pattern = "Sc.freq.max.3m", replacement = "quant.freq.", fixed = TRUE)
var.names <- gsub(var.names, pattern = "Sc.max.ratio.norm15min.obs.pre3m", replacement = "fishing", fixed = TRUE)
var.names <- gsub(var.names, pattern = "Sc.oceano.max.3m", replacement = "hydro.", fixed = TRUE)
var.names <- gsub(var.names, pattern = "Sc.taille.bloc.gp.nb", replacement = "boulder", fixed = TRUE)
var.names <- gsub(var.names, gsub, pattern = "Sc.accessibilite.gp.nb", replacement = "acces.", fixed = TRUE)
var.names <- gsub(var.names, gsub, pattern = "Sc.frequentation.gp.nb", replacement = "qual.freq.", fixed = TRUE)
var.names

colnames(tbl.models) <- var.names
names(tbl.models)

saveRDS(tbl.models, "results/Ecology/tbl.models.RDS") #results/Ecology/
write.csv(tbl.models, "results/Ecology/tbl.models.csv") #results/Ecology/



plot(df.$BM.BF_FS_dist.tr, df.$BM.BF_FS_matri.full.Third.Quartile.BM.BF_FS_dist.tr)
plot(df.$BM.BF_FS_dist.tr, df.$BM.BF_FS_matri.full.median.BM.BF_FS_dist.tr)

plot(df.$BM.BF_FS_dist.tr, df.$BM.BF_FS_matri.full.Third.Quartile.BM.BF_FS_dist.tr)
plot(df.$BM.BF_FS_dist.tr, df.$BM.BF_FS_matri.full.median.BM.BF_FS_dist.tr)

save.image("results/models.last.RData")


#################################################
# END OF SCRIPT
#################################################

# function on dissimilarity distance matrix calculated without variables that were removed from analysis: freq < 5% and ivr p-value > 0.05
# NB: this script will probably need to be updated since I change the function above.

matri.full.rem.BM.BF_FS <- readRDS("results/Ecology/matri.full.rem.BM.BF_FS.RDS")
matri.full.rem.BM.BF_FS <- rename(matri.full.rem.BM.BF_FS, Quadrat.bis = Quadrat.left)
# we have to join ivr data from qecb.ivr df. because in the raw ivr df. sampling dates do not always correspond, eg ARMO_Bilfot 2014.10.07 vs 2014.10.10; standardized/homogenized for date in qecb.ivr
matri.full.rem.BM.BF_FS.ivr <- left_join(matri.full.rem.BM.BF_FS, qecb.ivr.red., by = c("Site.Year.Month.Day", "Quadrat.bis"))

matri.full.rem.BM_FS.FI <- readRDS("results/Ecology/matri.full.rem.BM_FS.FI.RDS")
matri.full.rem.BM_FS.FI <- rename(matri.full.rem.BM_FS.FI, Quadrat.bis = Quadrat.left)
matri.full.rem.BM_FS.FI.ivr <- left_join(matri.full.rem.BM_FS.FI, qecb.ivr.red., by = c("Site.Year.Month.Day", "Quadrat.bis"))

matri.Bretagne.rem.BM.BF_FS <- readRDS("results/Ecology/matri.Bretagne.rem.BM.BF_FS.RDS")
matri.Bretagne.rem.BM.BF_FS <- rename(matri.Bretagne.rem.BM.BF_FS, Quadrat.bis = Quadrat.left)
matri.Bretagne.rem.BM.BF_FS.ivr <- left_join(matri.Bretagne.rem.BM.BF_FS, qecb.ivr.red., by = c("Site.Year.Month.Day", "Quadrat.bis"))

matri.Bretagne.rem.BM_FS.FI <- readRDS("results/Ecology/matri.Bretagne.rem.BM_FS.FI.RDS")
matri.Bretagne.rem.BM_FS.FI <- rename(matri.Bretagne.rem.BM_FS.FI, Quadrat.bis = Quadrat.left)
matri.Bretagne.rem.BM_FS.FI.ivr <- left_join(matri.Bretagne.rem.BM_FS.FI, qecb.ivr.red., by = c("Site.Year.Month.Day", "Quadrat.bis"))

matri.EGMP.BASQ.rem.BM.BF_FS <- readRDS("results/Ecology/matri.EGMP.BASQ.rem.BM.BF_FS.RDS")
matri.EGMP.BASQ.rem.BM.BF_FS <- rename(matri.EGMP.BASQ.rem.BM.BF_FS, Quadrat.bis = Quadrat.left)
matri.EGMP.BASQ.rem.BM.BF_FS.ivr <- left_join(matri.EGMP.BASQ.rem.BM.BF_FS, qecb.ivr.red., by = c("Site.Year.Month.Day", "Quadrat.bis"))

matri.EGMP.BASQ.rem.BM_FS.FI <- readRDS("results/Ecology/matri.EGMP.BASQ.rem.BM_FS.FI.RDS")
matri.EGMP.BASQ.rem.BM_FS.FI <- rename(matri.EGMP.BASQ.rem.BM_FS.FI, Quadrat.bis = Quadrat.left)
matri.EGMP.BASQ.rem.BM_FS.FI.ivr <- left_join(matri.EGMP.BASQ.rem.BM_FS.FI, qecb.ivr.red., by = c("Site.Year.Month.Day", "Quadrat.bis"))


#matri.full.rem.BM.BF_FS.ivr
#matri.full.rem.BM_FS.FI.ivr
#matri.Bretagne.rem.BM.BF_FS.ivr
#matri.Bretagne.rem.BM_FS.FI.ivr
#matri.EGMP.BASQ.rem.BM.BF_FS.ivr
#matri.EGMP.BASQ.rem.BM_FS.FI.ivr

matri.fct.(data. = matri.full.rem.BM.BF_FS.ivr, region. = "Atlantique", dataset. = "BM.BF_FS")
tbl.matri.full.rem.BM.BF_FS.ivr <- tbl. ; rm(tbl.)

matri.fct.(data. = matri.full.rem.BM_FS.FI.ivr, region. = "Atlantique", dataset. = "BM_FS.FI")
tbl.matri.full.rem.BM_FS.FI.ivr <- tbl. ; rm(tbl.) 

matri.fct.(data. = matri.Bretagne.rem.BM.BF_FS.ivr, region. = "Bretagne", dataset. = "BM.BF_FS")
tbl.matri.Bretagne.rem.BM.BF_FS.ivr <- tbl. ; rm(tbl.) 

matri.fct.(data. = matri.Bretagne.rem.BM_FS.FI.ivr, region. = "Bretagne", dataset. = "BM_FS.FI")
tbl.matri.Bretagne.rem.BM_FS.FI.ivr <- tbl. ; rm(tbl.) 

matri.fct.(data. = matri.EGMP.BASQ.rem.BM.BF_FS.ivr, region. = "EGMP.BASQ", dataset. = "BM.BF_FS")
tbl.matri.EGMP.BASQ.rem.BM.BF_FS.ivr <- tbl. ; rm(tbl.) 

matri.fct.(data. = matri.EGMP.BASQ.rem.BM_FS.FI.ivr, region. = "EGMP.BASQ", dataset. = "BM_FS.FI")
tbl.matri.EGMP.BASQ.rem.BM_FS.FI.ivr <- tbl. ; rm(tbl.) 


tbl.matri.rem. <- bind_rows(tbl.matri.full.rem.BM.BF_FS.ivr, tbl.matri.full.rem.BM_FS.FI.ivr)
tbl.matri.rem. <- bind_rows(tbl.matri.rem., tbl.matri.Bretagne.rem.BM.BF_FS.ivr)
tbl.matri.rem. <- bind_rows(tbl.matri.rem., tbl.matri.Bretagne.rem.BM_FS.FI.ivr)
tbl.matri.rem. <- bind_rows(tbl.matri.rem., tbl.matri.EGMP.BASQ.rem.BM.BF_FS.ivr)
tbl.matri.rem. <- bind_rows(tbl.matri.rem., tbl.matri.EGMP.BASQ.rem.BM_FS.FI.ivr)

options(scipen = 999) # remove scientific 10^ format
tbl.matri.rem.$`int. Esti.` <- round(tbl.matri.rem.$`int. Esti.`, digits = 4)
tbl.matri.rem.$`int. SE` <- round(tbl.matri.rem.$`int. SE`, digits = 4)
tbl.matri.rem.$`int. p` <- round(tbl.matri.rem.$`int. p`, digits = 4)
tbl.matri.rem.$`ivr Esti.` <- round(tbl.matri.rem.$`ivr Esti.`, digits = 4)
tbl.matri.rem.$`ivr SE` <- round(tbl.matri.rem.$`ivr SE`, digits = 4)
tbl.matri.rem.$`ivr p` <- round(tbl.matri.rem.$`ivr p`, digits = 4)
tbl.matri.rem.$Chisq <- round(tbl.matri.rem.$Chisq, digits = 4)
tbl.matri.rem.$AIC <- round(tbl.matri.rem.$AIC, digits = 1)
tbl.matri.rem.$pseudo.R2 <- round(tbl.matri.rem.$pseudo.R2, digits = 2)
tbl.matri.rem.$R2.Cd <- round(tbl.matri.rem.$R2.Cd, digits = 2)
tbl.matri.rem.$R2.Mg <- round(tbl.matri.rem.$R2.Mg, digits = 2)
tbl.matri.rem.$AIC <- round(tbl.matri.rem.$AIC, digits = 1)
tbl.matri.rem.$Chisq <- round(tbl.matri.rem.$Chisq, digits = 4)
tbl.matri.rem.

#tbl.matri.rem.$`ivr p` <- format(tbl.matri.rem.$`ivr p`, nsmall = 4)
#tbl.matri.rem.$`ivr p` <- ifelse(tbl.matri.rem.$`ivr p` == "0.0000", "< 0.0001", tbl.matri.rem.$`ivr p`)

options(scipen = 0, digits = 7) # default

saveRDS(tbl.matri.rem., "results/Ecology/tbl.matri.rem.RDS")
