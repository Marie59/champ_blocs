# author: "Jonathan Richir"
# date: "05 July 2021"

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


#qecb.val. <- readRDS("results/QECB/qecb.val.RDS")
qecb <- readRDS("qecbNew.RDS")

names(qecb)

#filter(qecb, Site == "GDMO_Locmariaquer" & Date.fiche == "2018-09-10") -> issue.Locma

`%notin%` <- Negate(`%in%`)
#qecb <- qecb[ , -which(names(qecb) %in% c("DateYmd"))]

#qecb <- add_column(qecb, qecb$Site.bis, .after = "Site")
#qecb <- qecb[ , -which(names(qecb) %in% c("Site.bis"))]
#qecb <- rename(qecb, Site.bis = `qecb$Site.bis`)

qecb <- add_column(qecb, qecb$Site.Year.Month.Day, .after = "Site.bis")
qecb <- qecb[ , -which(names(qecb) %in% c("Site.Year.Month.Day"))]
qecb <- rename(qecb, Site.Year.Month.Day = `qecb$Site.Year.Month.Day`)

# create a new var. area Bretagne vs Pertuis Charentais & Pays basque

unique(qecb$Site.Year.Month.Day)
unique(qecb$Region)
unique(qecb$Site)
unique(qecb[, c("Region", "Site")])
#library(tibble)
#Region <- rep(NA, nrow(qecb))
#qecb <- add_column(qecb, Region, .after = "Site.bis")
#qecb$Region <- ifelse(qecb$Site %in% c("EGMP_GroinCou", "EGMP_PasEmsembert",    "EGMP_BreeBains", "EGMP_PerreAntiochat", "EGMP_Chassiron", "BASQ_FlotsBleusZP", "BASQ_FlotsBleusZF"), "EGMP.BASQ", "Bretagne")
#rm(Region)

Period <- rep(NA, nrow(qecb))
qecb <- add_column(qecb, Period, .after = "Day")
qecb$Period <- ifelse(as.numeric(qecb$Month) < 7, "p1", "p2")
qecb$Period <- as.factor(qecb$Period)
rm(Period)

qecb <- arrange(qecb, Region, Site.Year.Month.Day, Type.Bloc, Numéro.Bloc.échantillon, Face)


# Do not run below code anymore for correcting for missing Spirobranchus and spirorbis data; it is now corrected in the qecbNew df, in the CB_qecb R script.
#####################################################################

# Solve issue with Nb.Spirobranchus.lamarckii.total and Nb.spirorbis.total missing data 

Spirobranchus <- qecb[, c("Nb.Spirobranchus.lamarckii.1B", "Nb.Spirobranchus.lamarckii.2B", "Nb.Spirobranchus.lamarckii.3B", "Nb.Spirobranchus.lamarckii.4B", "Nb.Spirobranchus.lamarckii.5B", "Nb.Spirobranchus.lamarckii.total")]
Spirobranchus$Nb.Spirobranchus.lamarckii.total.bis <- rowSums(Spirobranchus[, c(1:5)], na.rm = T)
unique(Spirobranchus$Nb.Spirobranchus.lamarckii.total/Spirobranchus$Nb.Spirobranchus.lamarckii.total.bis)
table(Spirobranchus$Nb.Spirobranchus.lamarckii.total/Spirobranchus$Nb.Spirobranchus.lamarckii.total.bis)
Spirobranchus$Nb.Spirobranchus.lamarckii.total.bis <- rowSums(Spirobranchus[, c(1:5)], na.rm = T)*20
table(Spirobranchus$Nb.Spirobranchus.lamarckii.total/Spirobranchus$Nb.Spirobranchus.lamarckii.total.bis)
df. <- filter(Spirobranchus, Nb.Spirobranchus.lamarckii.total.bis != 0)
setdiff(Spirobranchus$Nb.Spirobranchus.lamarckii.total, Spirobranchus$Nb.Spirobranchus.lamarckii.total.bis)
df. <- filter(Spirobranchus, is.na(Spirobranchus$Nb.Spirobranchus.lamarckii.total) & Nb.Spirobranchus.lamarckii.total.bis != 0)

spirorbis <- qecb[, c("Nb.spirorbis.1A", "Nb.spirorbis.2A", "Nb.spirorbis.3A", "Nb.spirorbis.4A", "Nb.spirorbis.5A", "Nb.spirorbis.total")]
spirorbis$Nb.spirorbis.total.bis <- rowSums(spirorbis[, c(1:5)], na.rm = T)
unique(spirorbis$Nb.spirorbis.total/spirorbis$Nb.spirorbis.total.bis)
table(spirorbis$Nb.spirorbis.total/spirorbis$Nb.spirorbis.total.bis)
spirorbis$Nb.spirorbis.total.bis <- rowSums(spirorbis[, c(1:5)], na.rm = T)*40
table(spirorbis$Nb.spirorbis.total/spirorbis$Nb.spirorbis.total.bis)
df. <- filter(spirorbis, Nb.spirorbis.total.bis != 0)
setdiff(spirorbis$Nb.spirorbis.total, spirorbis$Nb.spirorbis.total.bis)
df. <- filter(spirorbis, is.na(spirorbis$Nb.spirorbis.total) & Nb.spirorbis.total.bis != 0)

# when I do sum quadrat 1-5 data, there are some differences with total values ... ?? I will only replace NAs in total when rowSums != 0, and I will consider actual totals as the correct ones.

qecb$Nb.Spirobranchus.lamarckii.total <- ifelse(is.na(qecb$Nb.Spirobranchus.lamarckii.total) & rowSums(qecb[, c("Nb.Spirobranchus.lamarckii.1B", "Nb.Spirobranchus.lamarckii.2B", "Nb.Spirobranchus.lamarckii.3B", "Nb.Spirobranchus.lamarckii.4B", "Nb.Spirobranchus.lamarckii.5B")], na.rm = T) != 0, rowSums(qecb[, c("Nb.Spirobranchus.lamarckii.1B", "Nb.Spirobranchus.lamarckii.2B", "Nb.Spirobranchus.lamarckii.3B", "Nb.Spirobranchus.lamarckii.4B", "Nb.Spirobranchus.lamarckii.5B")], na.rm = T)*20, qecb$Nb.Spirobranchus.lamarckii.total)

qecb$Nb.spirorbis.total <- ifelse(is.na(qecb$Nb.spirorbis.total) & rowSums(qecb[, c("Nb.spirorbis.1A", "Nb.spirorbis.2A", "Nb.spirorbis.3A", "Nb.spirorbis.4A", "Nb.spirorbis.5A", "Nb.spirorbis.total")], na.rm = T) != 0, rowSums(qecb[, c("Nb.spirorbis.1A", "Nb.spirorbis.2A", "Nb.spirorbis.3A", "Nb.spirorbis.4A", "Nb.spirorbis.5A", "Nb.spirorbis.total")], na.rm = T)*40, qecb$Nb.spirorbis.total)

rm(df., Spirobranchus, spirorbis)

#####################################################################

# NB: les infos surface d'accolement sont dupliquées de la face inf vers la face sup de blocs mobiles (même si peu de sens d'avoir cette info pour les face sup ...)
# NB: les data "Abondance ressources ciblées par pêcheurs à pied" présentes uniquement pour les blocs mobiles sont dupliquées entre face inf et face sup. 


#######################################

## SCRIPT I - NAs <- 0

unique(qecb$Region)
unique(qecb$Type.Bloc)
Bretagne.bm <- filter(qecb, Region == "Bretagne" & Type.Bloc == "Bloc mobile")
Bretagne.bf <- filter(qecb, Region == "Bretagne" & Type.Bloc %in% c("Bloc fixé", "Roche en place"))
EGMP.BASQ.bm <- filter(qecb, Region == "EGMP.BASQ" & Type.Bloc == "Bloc mobile")
EGMP.BASQ.bf <- filter(qecb, Region == "EGMP.BASQ" & Type.Bloc %in% c("Bloc fixé", "Roche en place"))

# replace NAs by "0" for variables used in qecb determination

{

# Bretagne.bm
Bretagne.bm[,c(
           "X..algues.brunes"                       ,            
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
           #.."Nb.Crassostrea.gigas"                   ,           
           #.."Nb.Ostrea.edulis"                       ,           
           #.."X..Mytilus.sp."                         ,           
           #.."X..Hermelles"                           ,           
           #.."X..Hydraires"                           ,           
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
           #.."Nb.Eriphia.verrucosa..Crabe.verruqueux.",           
           #.."Nb.Octopus.vulgaris..Poulpe."           ,           
           "Nb.Galathea..Galathées."                ,          
           #.."Nb.Paracentrotus.lividus..Oursin."      ,            
           "Nb.Lophozozymus.incisus..ancien.Xantho.incisus."         ,
           "Nb.Palaemon.sp..Crevette.bouquet.ou.crevette.rose."      ,
           "Nb.Haliotis.tuberculata..Ormeau."                        ,
           #"Nb.Stramonita.haemastoma..Pourpre.bouche.de.sang."       ,
           "Nb.Littorina.littorea..Bigorneau."      ,           
           "Nb.Xantho.pilipes..Xanthe.poilu."       ,           
           "Nb.Mimachlamys.varia..Pétoncle.noir."
           )
        ] <- lapply(Bretagne.bm[, c(
          "X..algues.brunes"                       ,            
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
          #.."Nb.Crassostrea.gigas"                   ,           
          #.."Nb.Ostrea.edulis"                       ,           
          #.."X..Mytilus.sp."                         ,           
          #.."X..Hermelles"                           ,           
          #.."X..Hydraires"                           ,           
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
          #.."Nb.Eriphia.verrucosa..Crabe.verruqueux.",           
          #.."Nb.Octopus.vulgaris..Poulpe."           ,           
          "Nb.Galathea..Galathées."                ,          
          #.."Nb.Paracentrotus.lividus..Oursin."      ,            
          "Nb.Lophozozymus.incisus..ancien.Xantho.incisus."         ,
          "Nb.Palaemon.sp..Crevette.bouquet.ou.crevette.rose."      ,
          "Nb.Haliotis.tuberculata..Ormeau."                        ,
          #"Nb.Stramonita.haemastoma..Pourpre.bouche.de.sang."       ,
          "Nb.Littorina.littorea..Bigorneau."      ,           
          "Nb.Xantho.pilipes..Xanthe.poilu."       ,           
          "Nb.Mimachlamys.varia..Pétoncle.noir."
        )
          ], function(x) replace(x, is.na(x), 0))


# Bretagne.bf
Bretagne.bf[,c(
  "X..algues.brunes"                       ,            
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
  #.."Nb.Crassostrea.gigas"                   ,           
  #.."Nb.Ostrea.edulis"                       ,           
  #.."X..Mytilus.sp."                         ,           
  #.."X..Hermelles"                           ,           
  #.."X..Hydraires"                           ,           
  "X..Eponges"                             ,           
  "X..Ascidies.Coloniales"                 ,           
  "X..Ascidies.Solitaires"                 ,           
  "X..Bryozoaires.Dresses"                 ,           
  "X..Balanes.Vivantes"                    ,           
  #"Commentaires.Avant"                     ,            
  "X..Surface.Accolement"                  #, # is NA, then replace by 0 as well because no sense to have a NA value for "% surface accolement" as well.              
  #"Type.sustrat.observé"                   ,           
  #"Commentaires"                           ,            
  #."Nb.Cancer.pagurus..Tourteau."           ,           
  #.."Nb.Necora.puber..Etrille."              ,           
  #."Nb.Carcinus.maenas..Crabe.vert."        ,           
  #."Nb.Nucella.lapilus..Pourpre."           ,           
  #.."Nb.Eriphia.verrucosa..Crabe.verruqueux.",           
  #.."Nb.Octopus.vulgaris..Poulpe."           ,           
  #."Nb.Galathea..Galathées."                ,          
  #.."Nb.Paracentrotus.lividus..Oursin."      ,            
  #."Nb.Lophozozymus.incisus..ancien.Xantho.incisus."         ,
  #."Nb.Palaemon.sp..Crevette.bouquet.ou.crevette.rose."      ,
  #."Nb.Haliotis.tuberculata..Ormeau."                        ,
  #."Nb.Stramonita.haemastoma..Pourpre.bouche.de.sang."       ,
  #."Nb.Littorina.littorea..Bigorneau."      ,           
  #."Nb.Xantho.pilipes..Xanthe.poilu."       ,           
  #."Nb.Mimachlamys.varia..Pétoncle.noir."
)
] <- lapply(Bretagne.bf[, c(
  "X..algues.brunes"                       ,            
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
  #.."Nb.Crassostrea.gigas"                   ,           
  #.."Nb.Ostrea.edulis"                       ,           
  #.."X..Mytilus.sp."                         ,           
  #.."X..Hermelles"                           ,           
  #.."X..Hydraires"                           ,           
  "X..Eponges"                             ,           
  "X..Ascidies.Coloniales"                 ,           
  "X..Ascidies.Solitaires"                 ,           
  "X..Bryozoaires.Dresses"                 ,           
  "X..Balanes.Vivantes"                    ,           
  #"Commentaires.Avant"                     ,            
  "X..Surface.Accolement"                  #, # is NA, then replace by 0 as well because no sense to have a NA value for "% surface accolement" as well.              
  #"Type.sustrat.observé"                   ,           
  #"Commentaires"                           ,            
  #."Nb.Cancer.pagurus..Tourteau."           ,           
  #.."Nb.Necora.puber..Etrille."              ,           
  #."Nb.Carcinus.maenas..Crabe.vert."        ,           
  #."Nb.Nucella.lapilus..Pourpre."           ,           
  #.."Nb.Eriphia.verrucosa..Crabe.verruqueux.",           
  #.."Nb.Octopus.vulgaris..Poulpe."           ,           
  #."Nb.Galathea..Galathées."                ,          
  #.."Nb.Paracentrotus.lividus..Oursin."      ,            
  #."Nb.Lophozozymus.incisus..ancien.Xantho.incisus."         ,
  #."Nb.Palaemon.sp..Crevette.bouquet.ou.crevette.rose."      ,
  #."Nb.Haliotis.tuberculata..Ormeau."                        ,
  #."Nb.Stramonita.haemastoma..Pourpre.bouche.de.sang."       ,
  #."Nb.Littorina.littorea..Bigorneau."      ,           
  #."Nb.Xantho.pilipes..Xanthe.poilu."       ,           
  #."Nb.Mimachlamys.varia..Pétoncle.noir."
)
], function(x) replace(x, is.na(x), 0))


# EGMP.BASQ.bm
EGMP.BASQ.bm[,c(
  "X..algues.brunes"                       ,            
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
] <- lapply(EGMP.BASQ.bm[, c(
  "X..algues.brunes"                       ,            
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
], function(x) replace(x, is.na(x), 0))


# EGMP.BASQ.bf
EGMP.BASQ.bf[,c(
  "X..algues.brunes"                       ,            
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
  "X..Surface.Accolement"                  #, # is NA, then replace by 0 as well because no sense to have a NA value for "% surface accolement" as well.              
  #"Type.sustrat.observé"                   ,           
  #"Commentaires"                           ,            
  #."Nb.Cancer.pagurus..Tourteau."           ,           
  #.."Nb.Necora.puber..Etrille."              ,           
  #."Nb.Carcinus.maenas..Crabe.vert."        ,           
  #."Nb.Nucella.lapilus..Pourpre."           ,           
  #.."Nb.Eriphia.verrucosa..Crabe.verruqueux.",           
  #.."Nb.Octopus.vulgaris..Poulpe."           ,           
  #."Nb.Galathea..Galathées."                ,          
  #.."Nb.Paracentrotus.lividus..Oursin."      ,            
  #."Nb.Lophozozymus.incisus..ancien.Xantho.incisus."         ,
  #."Nb.Palaemon.sp..Crevette.bouquet.ou.crevette.rose."      ,
  #."Nb.Haliotis.tuberculata..Ormeau."                        ,
  #."Nb.Stramonita.haemastoma..Pourpre.bouche.de.sang."       ,
  #."Nb.Littorina.littorea..Bigorneau."      ,           
  #."Nb.Xantho.pilipes..Xanthe.poilu."       ,           
  #."Nb.Mimachlamys.varia..Pétoncle.noir."
)
] <- lapply(EGMP.BASQ.bf[, c(
  "X..algues.brunes"                       ,            
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
  "X..Surface.Accolement"                  #, # is NA, then replace by 0 as well because no sense to have a NA value for "% surface accolement" as well.              
  #"Type.sustrat.observé"                   ,           
  #"Commentaires"                           ,            
  #."Nb.Cancer.pagurus..Tourteau."           ,           
  #.."Nb.Necora.puber..Etrille."              ,           
  #."Nb.Carcinus.maenas..Crabe.vert."        ,           
  #."Nb.Nucella.lapilus..Pourpre."           ,           
  #.."Nb.Eriphia.verrucosa..Crabe.verruqueux.",           
  #.."Nb.Octopus.vulgaris..Poulpe."           ,           
  #."Nb.Galathea..Galathées."                ,          
  #.."Nb.Paracentrotus.lividus..Oursin."      ,            
  #."Nb.Lophozozymus.incisus..ancien.Xantho.incisus."         ,
  #."Nb.Palaemon.sp..Crevette.bouquet.ou.crevette.rose."      ,
  #."Nb.Haliotis.tuberculata..Ormeau."                        ,
  #."Nb.Stramonita.haemastoma..Pourpre.bouche.de.sang."       ,
  #."Nb.Littorina.littorea..Bigorneau."      ,           
  #."Nb.Xantho.pilipes..Xanthe.poilu."       ,           
  #."Nb.Mimachlamys.varia..Pétoncle.noir."
)
], function(x) replace(x, is.na(x), 0))

}

# merge dfs.
qecbNato0 <- bind_rows(Bretagne.bm, Bretagne.bf)
qecbNato0 <- bind_rows(qecbNato0, EGMP.BASQ.bm)
qecbNato0 <- bind_rows(qecbNato0, EGMP.BASQ.bf)

qecbNato0 <- arrange(qecbNato0, Region, Site.Year.Month.Day, Type.Bloc, Numéro.Bloc.échantillon, Face)


# before I go further ahead in the ecology script analysis, I have to correct for surface d'accollement for several variable for BM.FI !! See qecb script for more info. 

# accolement function according to recent 'retournement'

# not the same file name between script qecb script (qecbNew) and this script (qecbNAto0); doesn't matter, only appears here in the first filter lines.

unique(qecbNato0$Region)
qecbNato0 <- add_column(qecbNato0, terri. = substr(qecbNato0$Site, 1, 4), .after = "Site.bis")

qecbNato0$X..Eponges_ini <- qecbNato0$X..Eponges
qecbNato0$X..Ascidies.Coloniales_ini <- qecbNato0$X..Ascidies.Coloniales
qecbNato0$X..Ascidies.Solitaires_ini <- qecbNato0$X..Ascidies.Solitaires
qecbNato0$X..Bryozoaires.Dresses_ini <- qecbNato0$X..Bryozoaires.Dresses
qecbNato0$X..Lithophyllum_ini <- qecbNato0$X..Lithophyllum
qecbNato0$X..Balanes.Vivantes_ini <- qecbNato0$X..Balanes.Vivantes

unique(qecbNato0$Type.Bloc)
qecbNato0 %>% filter(Type.Bloc == "Bloc mobile" & Face == "face supérieure") -> df.BM.FS
qecbNato0 %>% filter(Type.Bloc == "Bloc mobile" & Face == "face inférieure") -> df.BM.FI
qecbNato0 %>% filter(Type.Bloc != "Bloc mobile") -> df.BF
head(df.BM.FS[, c("Region", "Site.Year.Month.Day", "Type.Bloc", "Numéro.Bloc.échantillon", "Face")])
head(df.BM.FI[, c("Region", "Site.Year.Month.Day", "Type.Bloc", "Numéro.Bloc.échantillon", "Face")])
tail(df.BM.FS[, c("Region", "Site.Year.Month.Day", "Type.Bloc", "Numéro.Bloc.échantillon", "Face")])
tail(df.BM.FI[, c("Region", "Site.Year.Month.Day", "Type.Bloc", "Numéro.Bloc.échantillon", "Face")])
#setdiff(paste0(df.BM.FI$Site.Year.Month.Day, "-", df.BM.FI$Type.Bloc, "-", df.BM.FI$Numéro.Bloc.échantillon), paste0(df.BM.FS$Site.Year.Month.Day, "-", df.BM.FS$Type.Bloc, "-", df.BM.FS$Numéro.Bloc.échantillon)) # setdiff function not usefull here because it doesn't take into account the order of character string; only all elements considered together for comparison purpose, so the order can differ !! that's why I had an issues previsouly when comparing car.var.acco. function results between this qecb scipt and ecology script !!!
#df.BM.FI$X..Surface.Accolement <- ifelse(is.na(df.BM.FI$X..Surface.Accolement) == TRUE & df.BM.FI$Type.Bloc == "Bloc mobile", 0, df.BM.FI$X..Surface.Accolement) # not needed anymore because surface accolement Nato0 


acco.fct <- function(var.) {
  
  df.BM.FI$var.cor.acco. <<- NA
  
  for (i in c(1:nrow(df.BM.FI))) {
    
    df.BM.FI$var.cor.acco.[[i]] <<- if (df.BM.FI$terri.[[i]] %notin% c("EGMP", "BASQ")) {
      ifelse(#df.$Couleur.dominante %in% c("Rouge", "Brune", "Brune-Rouge") | 
        df.BM.FS$Couleur.dominante[[i]] %in% c("Blanche", "Verte", "Blanche-Verte", "Colorée"), df.BM.FI[i, var.] / (100 - df.BM.FI$X..Surface.Accolement[[i]]) * 100, df.BM.FI[i, var.])
    } else {
      ifelse(df.BM.FS$Couleur.dominante[[i]] %in% c("Blanche", "Verte", "Blanche-Verte", "Colorée") 
             & df.BM.FI$X..Surface.Accolement[[i]] != 0 # I have to use it in filter this time as well for EGMP- BASQ (but not for Bretagne, although could be added, same result); identical/repeated measure for BM.FI and BM.FS
             & df.BM.FS$X..Mytilus.sp.[[i]] == 0, df.BM.FI[i, var.] / (100 - df.BM.FI$X..Surface.Accolement[[i]]) * 100, df.BM.FI[i, var.])
    }
    
  }
  
}

# I would only consider colors in c("Rouge", "Brune", "Brune-Rouge") for BM.FI correction [ and not the series c("Blanche-Brune", "Rouge", "Brune", "Blanche-Rouge", "Brune-Rouge", "Rouge-Verte", "Brune-Verte") ] ; and for BM.FS, the list c("Blanche", "Verte", "Colorée") => we do the correction for BM.FI accollement based on BM.FS color !!!

# apply acco.fct to BM.FI variables

apply.acco.fct <- function(var.) {
  
  #var. <- "X..Eponges"
  
  show(sort(df.BM.FI[, var.], decreasing = T, index.return = F)[1:50])
  pre. <- as.vector(df.BM.FI[, var.])
  acco.fct(var.)
  library(tibble)
  df.BM.FI <<- add_column(df.BM.FI, var.cor. = df.BM.FI$var.cor.acco., .after = var.)
  show(sort(df.BM.FI$var.cor., decreasing = T, index.return = F)[1:50])
  df.BM.FI$var.cor. <<- as.numeric(ifelse(as.character(df.BM.FI$var.cor.) %in% c(NA, "NaN", "-Inf", "Inf"), "0", as.character(df.BM.FI$var.cor.)))
  df.BM.FI$var.cor. <<- ifelse(df.BM.FI$var.cor. > 100, 100, df.BM.FI$var.cor.)
  show(sort(df.BM.FI$var.cor., decreasing = T, index.return = F)[1:50])
  show(length(na.omit(which(abs(as.vector(df.BM.FI$var.cor.) - pre.) != 0)))/na.omit(length(df.BM.FI$var.cor.))*100)
  par(mfrow=c(1,3))
  hist(pre., main = var., xlab = "pre-corection")
  hist(df.BM.FI$var.cor., main = var., xlab = "post-corection")
  hist(df.BM.FI[as.vector(which(abs(as.vector(df.BM.FI$var.cor.) - pre.) != 0)), var.], main = var., xlab = "diff. post-pre != 0")
  par(mfrow=c(1,1))
  df.BM.FI <<- df.BM.FI[ , -which(names(df.BM.FI) %in% c(var., "var.cor.acco."))]
  colnames(df.BM.FI)[colnames(df.BM.FI) == "var.cor."] <<- var.
  
  rm(pre.)
  
}

apply.acco.fct("X..Eponges")
apply.acco.fct("X..Ascidies.Coloniales")
apply.acco.fct("X..Ascidies.Solitaires")
apply.acco.fct("X..Bryozoaires.Dresses")
apply.acco.fct("X..Lithophyllum")
apply.acco.fct("X..Balanes.Vivantes")

qecbNato0 <- bind_rows(df.BM.FS, df.BM.FI)
qecbNato0 <- bind_rows(qecbNato0, df.BF)

qecbNato0 <- arrange(qecbNato0, Region, Site.Year.Month.Day, Type.Bloc, Numéro.Bloc.échantillon, Face)

#saveRDS(qecbNato0, "results/Ecology/qecbNato0.RDS")

rm(df.BF, df.BM.FI, df.BM.FS)

#rm(Bretagne.bf, Bretagne.bm, EGMP.BASQ.bf, EGMP.BASQ.bm)

#######################################


# makes no sense script below because see above some observations are NAs !!
#######################################

# SCRIPT 2

# VERY IMPORTANT here, I removed for below variables any rows with NAs !! Unactivate this line if I replace NAs by 0 next.
# na.omit is nicer for just removing all NA's. complete.cases allows partial selection by including only certain columns of the dataframe.

qecbNAomit <- qecb
qecbNAomit <- qecbNAomit[complete.cases(qecbNAomit[ , c(
  "X..algues.brunes"                       ,            
  "Strate.algues.brunes"                   ,          
  "X..algues.rouges"                       ,           
  "Strate.algues.rouges"                   ,           
  "X..algues.vertes"                       ,            
  "Strate.algues.vertes"                   ,            
  "X..Cladophora"                          ,           
  "X..Lithophyllum"                        ,           
  #"X..Recouvrement.Sediment"               , # not considered, cfr in order not to loose data on organisms                        
  #"Type.Sediment"                          ,           
  #"X..Roche.Nue"                           , # not considered, cfr in order not to loose data on organisms                        
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
  #"X..Surface.Accolement"                  , # not considered, cfr in order not to loose data on organisms              
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
)]),]

#rm(qecbNAomit)

#######################################


## hist

# function eg. with iris

data(iris)
iris
# loop over column names
sapply(names(iris[, c("Sepal.Length", "Sepal.Width")]), function(cname){
  print(hist(iris[[cname]], main=cname))
})

rm(iris)

unique(qecbNato0$Type.Bloc)
unique(qecbNato0$Face)

# activate
# for now on, we'll keep all the obs BM vs BF vs FS vs FI ... ; see later on if Isa & Christian want some other patters/filtering
qecbNato0hist. <- qecbNato0
ylab. = "fréquence"
#qecbNato0hist. <- filter(qecbNato0, Type.Bloc == "Bloc mobile")
#ylab. = "fréquence bloc mobile"
# activate
#qecbNato0hist. <- filter(qecbNato0, Type.Bloc == "Bloc mobile" & Face == "face supérieure")
#ylab. = "fréquence bloc mobile face sup."
# or
#qecbNato0hist. <- filter(qecbNato0, Type.Bloc == "Bloc mobile" & Face == "face inférieure")
#ylab. = "fréquence bloc mobile face inf."
# or
#qecbNato0hist. <- filter(qecbNato0, Type.Bloc == c("Roche en place", "Bloc fixé"))
#ylab. = "fréquence bloc fixé / roche en place"

hist. <- qecbNato0hist.[, c(
  "Type.Bloc"                              ,
  "Face"                                   ,
  "X..algues.brunes"                       ,            
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
)]

par(mfrow = c(2,3))

sapply(names(hist.[, c(3:ncol(hist.))]), 
  function(cname){
  print(hist(hist.[, c(3:ncol(hist.))][[cname]], main = "", xlab = cname, ylab = ylab., breaks = length(unique(hist.[, c(3:ncol(hist.))][[cname]]))))
})

par(mfrow=c(1,1))

# spirorbis & Spirobranchus data investigation for protocol update.

hist(qecbNato0hist.$Nb.Spirobranchus.lamarckii.total)
hist(log10(qecbNato0hist.$Nb.Spirobranchus.lamarckii.total))
sum(is.na(qecbNato0hist.$Nb.Spirobranchus.lamarckii.total))
summary(qecbNato0hist.$Nb.Spirobranchus.lamarckii.total)
quantile(qecbNato0hist.$Nb.Spirobranchus.lamarckii.total, probs = c(0,10,20,30,40,50,60,70,80,90,91,92,93,94,95,96,97,98,98.5,99,99.5,100)/100, type = 4)
length(qecbNato0hist.$Nb.Spirobranchus.lamarckii.total)
nrow(filter(qecbNato0hist., Nb.Spirobranchus.lamarckii.total!=0))

hist(qecbNato0hist.$Nb.spirorbis.total)
hist(log10(qecbNato0hist.$Nb.spirorbis.total))
sum(is.na(qecbNato0hist.$Nb.spirorbis.total))
summary(qecbNato0hist.$Nb.spirorbis.total)
quantile(qecbNato0hist.$Nb.spirorbis.total, probs = c(0,10,20,30,40,50,60,70,75,80,85,90,91,92,93,94,95,96,97,98,98.5,99,99.5,100)/100, type = 4)
length(qecbNato0hist.$Nb.spirorbis.total)
nrow(filter(qecbNato0hist., Nb.spirorbis.total!=0))


rm(qecbNato0hist., ylab., hist.)


# variables investigation
# were related to BM initially, with the wrong overall data NA to 0 correction (and not applied to specific sub(4)datasets); so no more need to run these lines anymore since we consider all the observations

# blocs mobiles face sup.
#sort(unique(filter(qecbNato0, Type.Bloc == "Bloc mobile" & Face == "face supérieure")[, "X..algues.rouges"]))
#sum(filter(qecbNato0, Type.Bloc == "Bloc mobile" & Face == "face supérieure")[, "X..Surface.Accolement"] > 0)/length(filter(qecbNato0, Type.Bloc == "Bloc mobile" & Face == "face supérieure")[, "X..Surface.Accolement"])
#unique(filter(qecbNato0, Type.Bloc == "Bloc mobile" & Face == "face supérieure")[, "Nb.Octopus.vulgaris..Poulpe."])

# blocs mobiles face sup.
#unique(filter(qecbNato0, Type.Bloc == "Bloc mobile" & Face == "face supérieure")[, "Nb.Octopus.vulgaris..Poulpe."])


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
], function(x) replace(x, x > 0, 1))

saveRDS(qecbNato0.0vs1, "results/Ecology/qecbNato0.0vs1.RDS")

# activate
# for now on, we'll keep all the obs BM vs BF vs FS vs FI ... ; see later on if Isa & Christian want some other patters/filtering
qecbNato0.0vs1hist. <- qecbNato0.0vs1
ylab. = "fréquence"
#qecbNato0.0vs1hist. <- filter(qecbNato0.0vs1, Type.Bloc == "Bloc mobile")
#ylab. = "fréquence bloc mobile"
# activate
#qecbNato0.0vs1hist. <- filter(qecbNato0.0vs1, Type.Bloc == "Bloc mobile" & Face == "face supérieure")
#ylab. = "fréquence bloc mobile face sup."
# or
#qecbNato0.0vs1hist. <- filter(qecbNato0.0vs1, Type.Bloc == "Bloc mobile" & Face == "face inférieure")
#ylab. = "fréquence bloc mobile face inf."
# or
#qecbNato0.0vs1hist. <- filter(qecbNato0.0vs1, Type.Bloc == c("Roche en place", "Bloc fixé"))
#ylab. = "fréquence bloc fixé / roche en place"


hist. <- qecbNato0.0vs1hist.[, c(
  "Type.Bloc"                              ,
  "Face"                                   ,
  "X..algues.brunes"                       ,            
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
)]

par(mfrow = c(2,3))

sapply(names(hist.[, c(3:ncol(hist.))]), 
       function(cname){
         print(hist(hist.[, c(3:ncol(hist.))][[cname]], main = "", xlab = cname, ylab = ylab., breaks = length(unique(hist.[, c(3:ncol(hist.))][[cname]]))))
       })

par(mfrow=c(1,1))

rm(qecbNato0.0vs1hist., ylab., hist.)


## ratio présence absence

CB.ratio <- function(df., name.) {
  
  ratio. <- lapply(df.[, c(  "X..algues.brunes"                       ,            
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
  ], function(x) ((round(count(filter(df., x == 1))/count(filter(df., x %in% c(0,1)))*100, digits = 1))))
  
  ratio.
  df.ratio. <- data.frame(matrix(ncol = 2, nrow = length(ratio.)))
  colnames(df.ratio.) <- c("variable", name.)
  names(ratio.)
  df.ratio.$variable <- names(ratio.)
  df.ratio.[, 2] <- do.call("rbind", ratio.)[,1]
  df.ratio. <<- df.ratio.

}

CB.ratio(df. = qecbNato0.0vs1, name. = "BM.FS.FI_BF")
ratio_BM.FS.FI_BF <- df.ratio.

# check
unclass(round(count(filter(qecbNato0.0vs1, X..Eponges == 1))/count(filter(qecbNato0.0vs1,X..Eponges %in% c(0,1)))*100, digits = 1))[1]

unique(qecbNato0.0vs1$Type.Bloc)
unique(qecbNato0.0vs1$Face)

df.0vs1 <- filter(qecbNato0.0vs1, Type.Bloc == "Bloc mobile", Face == "face supérieure")
CB.ratio(df. = df.0vs1, name. = "BM.FS")
ratio_BM.FS <- df.ratio.

df.0vs1 <- filter(qecbNato0.0vs1, Type.Bloc == "Bloc mobile", Face == "face inférieure")
CB.ratio(df. = df.0vs1, name. = "BM.FI")
ratio_BM.FI <- df.ratio.

df.0vs1 <- filter(qecbNato0.0vs1, Type.Bloc %in% c("Bloc fixé", "Roche en place"))
CB.ratio(df. = df.0vs1, name. = "BF")
ratio_BF <- df.ratio.

df.ratio.full <- left_join(ratio_BM.FS.FI_BF, ratio_BM.FS)
df.ratio.full <- left_join(df.ratio.full, ratio_BM.FI)
df.ratio.full <- left_join(df.ratio.full, ratio_BF)
df.ratio.full

saveRDS(df.ratio.full, "results/Ecology/df.ratio.full.RDS")

df.ratio.full <- arrange(df.ratio.full, desc(BM.FS))
df.ratio.full

rm(df.ratio., df.0vs1, ratio_BF, ratio_BM.FI, ratio_BM.FS, ratio_BM.FS.FI_BF)


# and by region

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
  "X..Balanes.Vivantes"                    ,
  "X..Recouvrement.Sediment"               ,
  "X..Roche.Nue"                           ,
  "X..Surface.Accolement")                   

EGMP.BASQ_qecb <- c("Nb.Crassostrea.gigas", "Nb.Ostrea.edulis", "X..Mytilus.sp.", "X..Hermelles" , "X..Hydraires")

Bret_EGMP.BASQ_fishing <- c("Nb.Cancer.pagurus..Tourteau.", 
                            "Nb.Necora.puber..Etrille.", 
                            "Nb.Carcinus.maenas..Crabe.vert.", 
                            "Nb.Nucella.lapilus..Pourpre.", 
                            "Nb.Galathea..Galathées.", 
                            "Nb.Lophozozymus.incisus..ancien.Xantho.incisus.", 
                            "Nb.Palaemon.sp..Crevette.bouquet.ou.crevette.rose.", 
                            "Nb.Haliotis.tuberculata..Ormeau.", 
                            "Nb.Littorina.littorea..Bigorneau.", 
                            "Nb.Xantho.pilipes..Xanthe.poilu.", 
                            "Nb.Mimachlamys.varia..Pétoncle.noir.")

EGMP.BASQ_fishing <- c("Nb.Eriphia.verrucosa..Crabe.verruqueux.", "Nb.Octopus.vulgaris..Poulpe." , "Nb.Paracentrotus.lividus..Oursin.", "Nb.Stramonita.haemastoma..Pourpre.bouche.de.sang." )   


# Bretagne first

unique(qecbNato0.0vs1$Type.Bloc)
unique(qecbNato0.0vs1$Face)
unique(qecbNato0.0vs1$Region)

qecbNato0.0vs1.Bretagne <- filter(qecbNato0.0vs1, Region == "Bretagne")

table(qecbNato0.0vs1.Bretagne$Nb.Crassostrea.gigas)
table(qecbNato0.0vs1.Bretagne$Nb.Ostrea.edulis)
table(qecbNato0.0vs1.Bretagne$X..Mytilus.sp.)
table(qecbNato0.0vs1.Bretagne$X..Hermelles)
table(qecbNato0.0vs1.Bretagne$X..Hydraires)
table(qecbNato0.0vs1.Bretagne$Nb.Eriphia.verrucosa..Crabe.verruqueux.)         
table(qecbNato0.0vs1.Bretagne$Nb.Octopus.vulgaris..Poulpe.)
table(qecbNato0.0vs1.Bretagne$Nb.Paracentrotus.lividus..Oursin.)
table(qecbNato0.0vs1.Bretagne$Nb.Stramonita.haemastoma..Pourpre.bouche.de.sang.)

#qecbNato0.0vs1.Bretagne <- select(qecbNato0.0vs1.Bretagne, -c("Nb.Crassostrea.gigas", "Nb.Ostrea.edulis", "X..Mytilus.sp.", "X..Hermelles", "X..Hydraires", "Nb.Eriphia.verrucosa..Crabe.verruqueux.", "Nb.Octopus.vulgaris..Poulpe.", "Nb.Paracentrotus.lividus..Oursin."))

CB.ratio(df. = qecbNato0.0vs1.Bretagne, name. = "BM.FS.FI_BF")
ratio_BM.FS.FI_BF <- df.ratio.

df.0vs1 <- filter(qecbNato0.0vs1.Bretagne, Type.Bloc == "Bloc mobile", Face == "face supérieure")
CB.ratio(df. = df.0vs1, name. = "BM.FS")
ratio_BM.FS <- df.ratio.

df.0vs1 <- filter(qecbNato0.0vs1.Bretagne, Type.Bloc == "Bloc mobile", Face == "face inférieure")
CB.ratio(df. = df.0vs1, name. = "BM.FI")
ratio_BM.FI <- df.ratio.

df.0vs1 <- filter(qecbNato0.0vs1.Bretagne, Type.Bloc %in% c("Bloc fixé", "Roche en place"))
CB.ratio(df. = df.0vs1, name. = "BF")
ratio_BF <- df.ratio.

df.ratio.Bretagne <- left_join(ratio_BM.FS.FI_BF, ratio_BM.FS)
df.ratio.Bretagne <- left_join(df.ratio.Bretagne, ratio_BM.FI)
df.ratio.Bretagne <- left_join(df.ratio.Bretagne, ratio_BF)
df.ratio.Bretagne

`%notin%` <- Negate(`%in%`)

# I have to filter for variables after running the function, not before
df.ratio.Bretagne <- filter(df.ratio.Bretagne, variable %notin% c("Nb.Crassostrea.gigas", "Nb.Ostrea.edulis", "X..Mytilus.sp.", "X..Hermelles", "X..Hydraires", "Nb.Eriphia.verrucosa..Crabe.verruqueux.", "Nb.Octopus.vulgaris..Poulpe.", "Nb.Paracentrotus.lividus..Oursin.", "Nb.Stramonita.haemastoma..Pourpre.bouche.de.sang."))

saveRDS(df.ratio.Bretagne, "results/Ecology/df.ratio.Bretagne.RDS")

df.ratio.Bretagne <- arrange(df.ratio.Bretagne, desc(BM.FS))
df.ratio.Bretagne

filter(df.ratio.Bretagne, BM.FS >= 5 | BM.FI >= 5 | BF >= 5) #  | symbol is for OR argument in filter
as.vector(unlist(filter(df.ratio.Bretagne, BM.FS >= 5 | BM.FI >= 5 | BF >= 5)["variable"])) -> var.ratio.Bretagne
setdiff(var.ratio.Bretagne, c("X..Surface.Accolement", "Strate.algues.rouges", "Strate.algues.vertes", "X..Roche.Nue", "X..Recouvrement.Sediment", "Strate.algues.brunes")) -> var.ratio.Bretagne 

setdiff(var.ratio.Bretagne, Bret_EGMP.BASQ_fishing) -> var.ratio.Bretagne.qecb 
setdiff(Bret_EGMP.BASQ_qecb, var.ratio.Bretagne.qecb)

setdiff(var.ratio.Bretagne, Bret_EGMP.BASQ_qecb) -> var.ratio.Bretagne.fishing 
setdiff(Bret_EGMP.BASQ_fishing, var.ratio.Bretagne.fishing)

rm(df.ratio., df.0vs1, ratio_BF, ratio_BM.FI, ratio_BM.FS, ratio_BM.FS.FI_BF, var.ratio.Bretagne, var.ratio.Bretagne.qecb, var.ratio.Bretagne.fishing)


# EGMP.BASQ next

qecbNato0.0vs1.EGMP.BASQ <- filter(qecbNato0.0vs1, Region == "EGMP.BASQ")

CB.ratio(df. = qecbNato0.0vs1.EGMP.BASQ, name. = "BM.FS.FI_BF")
ratio_BM.FS.FI_BF <- df.ratio.

df.0vs1 <- filter(qecbNato0.0vs1.EGMP.BASQ, Type.Bloc == "Bloc mobile", Face == "face supérieure")
CB.ratio(df. = df.0vs1, name. = "BM.FS")
ratio_BM.FS <- df.ratio.

df.0vs1 <- filter(qecbNato0.0vs1.EGMP.BASQ, Type.Bloc == "Bloc mobile", Face == "face inférieure")
CB.ratio(df. = df.0vs1, name. = "BM.FI")
ratio_BM.FI <- df.ratio.

df.0vs1 <- filter(qecbNato0.0vs1.EGMP.BASQ, Type.Bloc %in% c("Bloc fixé", "Roche en place"))
CB.ratio(df. = df.0vs1, name. = "BF")
ratio_BF <- df.ratio.

df.ratio.EGMP.BASQ <- left_join(ratio_BM.FS.FI_BF, ratio_BM.FS)
df.ratio.EGMP.BASQ <- left_join(df.ratio.EGMP.BASQ, ratio_BM.FI)
df.ratio.EGMP.BASQ <- left_join(df.ratio.EGMP.BASQ, ratio_BF)
df.ratio.EGMP.BASQ

saveRDS(df.ratio.EGMP.BASQ, "results/Ecology/df.ratio.EGMP.BASQ.RDS")

df.ratio.EGMP.BASQ <- arrange(df.ratio.EGMP.BASQ, desc(BM.FS))
df.ratio.EGMP.BASQ

filter(df.ratio.EGMP.BASQ, BM.FS >= 5 | BM.FI >= 5 | BF >= 5) #  | symbol is for OR argument in filter
as.vector(unlist(filter(df.ratio.EGMP.BASQ, BM.FS >= 5 | BM.FI >= 5 | BF >= 5)["variable"])) -> var.ratio.EGMP.BASQ
setdiff(var.ratio.EGMP.BASQ, c("X..Surface.Accolement", "Strate.algues.rouges", "Strate.algues.vertes", "X..Roche.Nue", "X..Recouvrement.Sediment", "Strate.algues.brunes")) -> var.ratio.EGMP.BASQ 

setdiff(var.ratio.EGMP.BASQ, c(Bret_EGMP.BASQ_fishing, EGMP.BASQ_fishing)) -> var.ratio.EGMP.BASQ.qecb 
setdiff(c(Bret_EGMP.BASQ_qecb, EGMP.BASQ_qecb), var.ratio.EGMP.BASQ.qecb)

setdiff(var.ratio.EGMP.BASQ, c(Bret_EGMP.BASQ_qecb, EGMP.BASQ_qecb)) -> var.ratio.EGMP.BASQ.fishing 
setdiff(c(Bret_EGMP.BASQ_fishing, EGMP.BASQ_fishing), var.ratio.EGMP.BASQ.fishing)

rm(df.ratio., df.0vs1, ratio_BF, ratio_BM.FI, ratio_BM.FS, ratio_BM.FS.FI_BF, var.ratio.EGMP.BASQ, var.ratio.EGMP.BASQ.qecb, var.ratio.EGMP.BASQ.fishing)


## when present (i.e. data > 0), what is the median value

CB.median <- function(df., name.) {
  
  med. <- lapply(df.[, c(  "X..algues.brunes"                       ,            
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
  ], function(x) if (sum(x, na.rm = TRUE) == 0) NA else median(x[x != 0], na.rm = T)
  )
  
  med. <<- med.
  df.med. <- data.frame(matrix(ncol = 2, nrow = length(med.)))
  colnames(df.med.) <- c("variable", name.)
  names(med.)
  df.med.$variable <- names(med.)
  df.med.[, 2] <- do.call("rbind", med.)[,1]
  df.med. <<- df.med.
  
}

CB.median(df. = qecbNato0, name. = "BM.FS.FI_BF")
median_BM.FS.FI_BF <- df.med.

# check
median(filter(qecbNato0, X..Eponges > 0)[, "X..Eponges"], na.rm = T)

unique(qecbNato0$Type.Bloc)
unique(qecbNato0$Face)

df. <- filter(qecbNato0, Type.Bloc == "Bloc mobile", Face == "face supérieure")
CB.median(df. = df., name. = "BM.FS")
median_BM.FS <- df.med.

df. <- filter(qecbNato0, Type.Bloc == "Bloc mobile", Face == "face inférieure")
CB.median(df. = df., name. = "BM.FI")
median_BM.FI <- df.med.

df. <- filter(qecbNato0, Type.Bloc %in% c("Bloc fixé", "Roche en place"))
CB.median(df. = df., name. = "BF")
median_BF <- df.med.

df.median.full <- left_join(median_BM.FS.FI_BF, median_BM.FS)
df.median.full <- left_join(df.median.full, median_BM.FI)
df.median.full <- left_join(df.median.full, median_BF)
df.median.full

saveRDS(df.median.full, "results/Ecology/df.median.full.RDS")

df.median.full <- arrange(df.median.full, desc(BM.FS))
df.median.full

rm(df.med., df., median_BF, median_BM.FI, median_BM.FS, median_BM.FS.FI_BF)


# and by region


# Bretagne first

unique(qecbNato0$Type.Bloc)
unique(qecbNato0$Face)
unique(qecbNato0$Region)

qecbNato0.Bretagne <- filter(qecbNato0, Region == "Bretagne")

table(qecbNato0.Bretagne$Nb.Crassostrea.gigas)
table(qecbNato0.Bretagne$Nb.Ostrea.edulis)
table(qecbNato0.Bretagne$X..Mytilus.sp.)
table(qecbNato0.Bretagne$X..Hermelles)
table(qecbNato0.Bretagne$X..Hydraires)
table(qecbNato0.Bretagne$Nb.Eriphia.verrucosa..Crabe.verruqueux.)         
table(qecbNato0.Bretagne$Nb.Octopus.vulgaris..Poulpe.)
table(qecbNato0.Bretagne$Nb.Paracentrotus.lividus..Oursin.)
table(qecbNato0.Bretagne$Nb.Stramonita.haemastoma..Pourpre.bouche.de.sang.)

CB.median(df. = qecbNato0.Bretagne, name. = "BM.FS.FI_BF")
median_BM.FS.FI_BF <- df.med.

df. <- filter(qecbNato0.Bretagne, Type.Bloc == "Bloc mobile", Face == "face supérieure")
CB.median(df. = df., name. = "BM.FS")
median_BM.FS <- df.med.

df. <- filter(qecbNato0.Bretagne, Type.Bloc == "Bloc mobile", Face == "face inférieure")
CB.median(df. = df., name. = "BM.FI")
median_BM.FI <- df.med.

df. <- filter(qecbNato0.Bretagne, Type.Bloc %in% c("Bloc fixé", "Roche en place"))
CB.median(df. = df., name. = "BF")
median_BF <- df.med.

df.median.Bretagne <- left_join(median_BM.FS.FI_BF, median_BM.FS)
df.median.Bretagne <- left_join(df.median.Bretagne, median_BM.FI)
df.median.Bretagne <- left_join(df.median.Bretagne, median_BF)
df.median.Bretagne

`%notin%` <- Negate(`%in%`)

# I have to filter for variables after running the function, not before
df.median.Bretagne <- filter(df.median.Bretagne, variable %notin% c("Nb.Crassostrea.gigas", "Nb.Ostrea.edulis", "X..Mytilus.sp.", "X..Hermelles", "X..Hydraires", "Nb.Eriphia.verrucosa..Crabe.verruqueux.", "Nb.Octopus.vulgaris..Poulpe.", "Nb.Paracentrotus.lividus..Oursin.", "Nb.Stramonita.haemastoma..Pourpre.bouche.de.sang."))

saveRDS(df.median.Bretagne, "results/Ecology/df.median.Bretagne.RDS")

df.median.Bretagne <- arrange(df.median.Bretagne, desc(BM.FS))
df.median.Bretagne

rm(df.med., df., median_BF, median_BM.FI, median_BM.FS, median_BM.FS.FI_BF)


# EGMP.BASQ next

qecbNato0.EGMP.BASQ <- filter(qecbNato0, Region == "EGMP.BASQ")

CB.median(df. = qecbNato0.EGMP.BASQ, name. = "BM.FS.FI_BF")
median_BM.FS.FI_BF <- df.med.

df. <- filter(qecbNato0.EGMP.BASQ, Type.Bloc == "Bloc mobile", Face == "face supérieure")
CB.median(df. = df., name. = "BM.FS")
median_BM.FS <- df.med.

df. <- filter(qecbNato0.EGMP.BASQ, Type.Bloc == "Bloc mobile", Face == "face inférieure")
CB.median(df. = df., name. = "BM.FI")
median_BM.FI <- df.med.

df. <- filter(qecbNato0.EGMP.BASQ, Type.Bloc %in% c("Bloc fixé", "Roche en place"))
CB.median(df. = df., name. = "BF")
median_BF <- df.med.

df.median.EGMP.BASQ <- left_join(median_BM.FS.FI_BF, median_BM.FS)
df.median.EGMP.BASQ <- left_join(df.median.EGMP.BASQ, median_BM.FI)
df.median.EGMP.BASQ <- left_join(df.median.EGMP.BASQ, median_BF)
df.median.EGMP.BASQ

saveRDS(df.median.EGMP.BASQ, "results/Ecology/df.median.EGMP.BASQ.RDS")

df.median.EGMP.BASQ <- arrange(df.median.EGMP.BASQ, desc(BM.FS))
df.median.EGMP.BASQ

rm(df.med., df., median_BF, median_BM.FI, median_BM.FS, median_BM.FS.FI_BF)


# prepare ratio and median table for the paper

names(df.ratio.full)[2:5] <- c(paste0(names(df.ratio.full[, 2:5]), "_Atlantique"))
names(df.ratio.Bretagne)[2:5] <- paste0(names(df.ratio.Bretagne[, 2:5]), "_Bretagne")
names(df.ratio.EGMP.BASQ)[2:5] <- paste0(names(df.ratio.EGMP.BASQ[, 2:5]), "_EGMP.BASQ")

tbl.ratio.paper <- full_join(df.ratio.full, df.ratio.Bretagne, by = "variable")
tbl.ratio.paper <- full_join(tbl.ratio.paper, df.ratio.EGMP.BASQ, by = "variable")
write.csv(tbl.ratio.paper, "tbl.ratio.paper.CSV")

names(df.median.full)[2:5] <- c(paste0(names(df.median.full[, 2:5]), "_Atlantique"))
names(df.median.Bretagne)[2:5] <- paste0(names(df.median.Bretagne[, 2:5]), "_Bretagne")
names(df.median.EGMP.BASQ)[2:5] <- paste0(names(df.median.EGMP.BASQ[, 2:5]), "_EGMP.BASQ")

tbl.median.paper <- full_join(df.median.full, df.median.Bretagne, by = "variable")
tbl.median.paper <- full_join(tbl.median.paper, df.median.EGMP.BASQ, by = "variable")
write.csv(tbl.median.paper, "tbl.median.paper.CSV")


# basic statistics for the paper

unique(qecbNato0$Type.Bloc)
unique(qecbNato0$Face)

unique(qecbNato0$Site.Year.Month.Day)

nrow(filter(na.omit(qecbNato0[, c("Type.Bloc", "Face")]), Type.Bloc == "Bloc mobile", Face == "face supérieure"))
nrow(filter(na.omit(qecbNato0[, c("Type.Bloc", "Face")]), Type.Bloc == "Bloc mobile", Face == "face inférieure"))
nrow(filter(na.omit(qecbNato0[, c("Type.Bloc", "Face")]), Type.Bloc %in% c("Bloc fixé", "Roche en place"), Face == "face supérieure"))

nrow(unique(na.omit(qecbNato0[, c("Site.Year.Month.Day", "Quadrat.bis")])))
table(unique(na.omit(qecbNato0[, c("Site.Year.Month.Day", "Quadrat.bis")])))

# presence-absence (frequency) & median (better than average) by survey

unique(qecbNato0$Site.Year.Month.Day)
library(tibble)
Region.Site.Year.Month.Day <- paste0(qecbNato0$Region, "_", qecbNato0$Site.Year.Month.Day)
qecbNato0 <- add_column(qecbNato0, Region.Site.Year.Month.Day, .after = "Site.Year.Month.Day")
unique((qecbNato0$Region.Site.Year.Month.Day))

# table frequency - median Bretagne

qecbNato0.Bretagne <- filter(qecbNato0, Region == "Bretagne")

tbl.freq.Bretagne <- data.frame(matrix(ncol = length(c(Bret_EGMP.BASQ_qecb, Bret_EGMP.BASQ_fishing))+1, nrow = length(unique(qecbNato0.Bretagne$Region.Site.Year.Month.Day))))
colnames(tbl.freq.Bretagne) <- c("Region.Site.Year.Month.Day", Bret_EGMP.BASQ_qecb, Bret_EGMP.BASQ_fishing)
tbl.median.Bretagne <- tbl.freq.Bretagne

for(i in c(1:length(unique(qecbNato0.Bretagne$Region.Site.Year.Month.Day)))){
  
  survey. <- filter(qecbNato0.Bretagne, Region.Site.Year.Month.Day == unique(qecbNato0.Bretagne$Region.Site.Year.Month.Day)[i])
  survey. <- survey.[,c("Region.Site.Year.Month.Day", Bret_EGMP.BASQ_qecb, Bret_EGMP.BASQ_fishing)]
  
  tbl.freq.Bretagne[i,2:ncol(survey.)] <- colSums(survey.[,2:ncol(survey.)] != 0, na.rm = T)/colSums(!is.na(survey.[,2:ncol(survey.)]))
  tbl.freq.Bretagne[i,1] <- unique(survey.$Region.Site.Year.Month.Day)
  
  tbl.median.Bretagne[i,2:ncol(survey.)] <- sapply(survey.[,2:ncol(survey.)], median, na.rm = T)
  tbl.median.Bretagne[i,1] <- unique(survey.$Region.Site.Year.Month.Day)
  
}

tbl.freq.Bretagne[,2:ncol(tbl.freq.Bretagne)] <- format(round(tbl.freq.Bretagne[,2:ncol(tbl.freq.Bretagne)], digits = 2), nsmall = 2)

# table frequency - median EGMP.BASQ

qecbNato0.EGMP.BASQ <- filter(qecbNato0, Region == "EGMP.BASQ")

tbl.freq.EGMP.BASQ <- data.frame(matrix(ncol = length(c(Bret_EGMP.BASQ_qecb, Bret_EGMP.BASQ_fishing, EGMP.BASQ_qecb, EGMP.BASQ_fishing))+1, nrow = length(unique(qecbNato0.EGMP.BASQ$Region.Site.Year.Month.Day))))
colnames(tbl.freq.EGMP.BASQ) <- c("Region.Site.Year.Month.Day", Bret_EGMP.BASQ_qecb, Bret_EGMP.BASQ_fishing, EGMP.BASQ_qecb, EGMP.BASQ_fishing)
tbl.median.EGMP.BASQ <- tbl.freq.EGMP.BASQ

for(i in c(1:length(unique(qecbNato0.EGMP.BASQ$Region.Site.Year.Month.Day)))){
  i<-1
  survey. <- filter(qecbNato0.EGMP.BASQ, Region.Site.Year.Month.Day == unique(qecbNato0.EGMP.BASQ$Region.Site.Year.Month.Day)[i])
  survey. <- survey.[,c("Region.Site.Year.Month.Day", Bret_EGMP.BASQ_qecb, Bret_EGMP.BASQ_fishing, EGMP.BASQ_qecb, EGMP.BASQ_fishing)]
  
  tbl.freq.EGMP.BASQ[i,2:ncol(survey.)] <- colSums(survey.[,2:ncol(survey.)] != 0, na.rm = T)/colSums(!is.na(survey.[,2:ncol(survey.)]))
  tbl.freq.EGMP.BASQ[i,1] <- unique(survey.$Region.Site.Year.Month.Day)
  
  tbl.median.EGMP.BASQ[i,2:ncol(survey.)] <- sapply(survey.[,2:ncol(survey.)], median, na.rm = T)
  tbl.median.EGMP.BASQ[i,1] <- unique(survey.$Region.Site.Year.Month.Day)
  
}

tbl.freq.EGMP.BASQ[,2:ncol(tbl.freq.EGMP.BASQ)] <- format(round(tbl.freq.EGMP.BASQ[,2:ncol(tbl.freq.EGMP.BASQ)], digits = 2), nsmall = 2)

# table frequency - median Atlantique

tbl.freq.Bretagne[setdiff(names(tbl.freq.EGMP.BASQ), names(tbl.freq.Bretagne))] <- NA
tbl.freq.Atlantique <- rbind(tbl.freq.Bretagne, tbl.freq.EGMP.BASQ)

tbl.median.Bretagne[setdiff(names(tbl.median.EGMP.BASQ), names(tbl.median.Bretagne))] <- NA
tbl.median.Atlantique <- rbind(tbl.median.Bretagne, tbl.median.EGMP.BASQ)

saveRDS(tbl.freq.Atlantique, "results/Ecology/tbl.freq.Atlantique.RDS")
saveRDS(tbl.median.Atlantique, "results/Ecology/tbl.median.Atlantique.RDS")


## analyse cumulée

unique(qecbNato0$Site.Year.Month.Day)
unique(qecbNato0$Site)

# test

filter(qecbNato0, Site == "GDMO_Locmariaquer") -> Loc.
unique(Loc.$Site.Year.Month.Day)
filter(qecbNato0, Site.Year.Month.Day == "GDMO_Locmariaquer.2016.03.09" &  Type.Bloc == "Bloc mobile" & Face == "face supérieure") -> Loc.
Loc.$Numéro.Bloc.échantillon <- as.factor(Loc.$Numéro.Bloc.échantillon)
unique(Loc.$Numéro.Bloc.échantillon)

Loc.sp_accu. <- Loc.[, c(
  "Site.Year.Month.Day"                    ,
  "Numéro.Bloc.échantillon"                ,
  "X..algues.brunes"                       ,            
  #"Strate.algues.brunes"                   ,          
  "X..algues.rouges"                       ,           
  #"Strate.algues.rouges"                   ,           
  "X..algues.vertes"                       ,            
  #"Strate.algues.vertes"                   ,            
  "X..Cladophora"                          ,           
  "X..Lithophyllum"                        ,           
  #"X..Recouvrement.Sediment"               , # is NA, then replace by 0 as well because no sense to have a NA value for "% recouvrement sédiment" as well.          
  #"Type.Sediment"                          ,           
  #"X..Roche.Nue"                           , # is NA, then replace by 0 as well because no sense to have a NA value for "% roche nue" as well.          
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
  #"Nb.Crassostrea.gigas"                   ,           
  #"Nb.Ostrea.edulis"                       ,           
  #"X..Mytilus.sp."                         ,           
  #"X..Hermelles"                           ,           
  #"X..Hydraires"                           ,           
  "X..Eponges"                             ,           
  "X..Ascidies.Coloniales"                 ,           
  "X..Ascidies.Solitaires"                 ,           
  "X..Bryozoaires.Dresses"                 ,           
  "X..Balanes.Vivantes"                    #,           
  #"Commentaires.Avant"                     ,            
  #"X..Surface.Accolement"                  , # is NA, then replace by 0 as well because no sense to have a NA value for "% surface accolement" as well.              
  #"Type.sustrat.observé"                   ,           
  #"Commentaires"                           ,            
  #"Nb.Cancer.pagurus..Tourteau."           ,           
  #"Nb.Necora.puber..Etrille."              ,           
  #"Nb.Carcinus.maenas..Crabe.vert."        ,           
  #"Nb.Nucella.lapilus..Pourpre."           ,           
  #"Nb.Eriphia.verrucosa..Crabe.verruqueux.",           
  #"Nb.Octopus.vulgaris..Poulpe."           ,           
  #"Nb.Galathea..Galathées."                ,          
  #"Nb.Paracentrotus.lividus..Oursin."      ,            
  #"Nb.Lophozozymus.incisus..ancien.Xantho.incisus."         ,
  #"Nb.Palaemon.sp..Crevette.bouquet.ou.crevette.rose."      ,
  #"Nb.Haliotis.tuberculata..Ormeau."                        ,
  #"Nb.Stramonita.haemastoma..Pourpre.bouche.de.sang."       ,
  #"Nb.Littorina.littorea..Bigorneau."      ,           
  #"Nb.Xantho.pilipes..Xanthe.poilu."       ,           
  #"Nb.Mimachlamys.varia..Pétoncle.noir."
)]

library(vegan)
(speca. <- specaccum(Loc.sp_accu.[, c(3:ncol(Loc.sp_accu.))], method = "exact"))
plot(speca., xlab = "Blocs mobiles (face supérieure)", main = "Test for 1 survey")

rm(Loc., Loc.sp_accu., speca.)


# test

sp_accu. <- filter(qecbNato0, Type.Bloc == "Bloc mobile" & Face == "face supérieure" & Region == "Bretagne")
sp_accu. <- sp_accu.[, c("Site", "Site.Year.Month.Day", "Type.Bloc", "Face", "Numéro.Bloc.échantillon", Bret_EGMP.BASQ_qecb, Bret_EGMP.BASQ_fishing)]

sp_accu. %>% filter(Site.Year.Month.Day == unique(sp_accu.$Site.Year.Month.Day)[[19]]) -> qecb.i
(speca. <- specaccum(qecb.i[, c(6:ncol(qecb.i))], method = "exact"))
show(speca.)
plot(speca., xlab = "Blocs", main = "Test for fct.")

(ratio. <- speca.$richness/speca.$richness[length(speca.$richness)])
ratio.[ratio. >= 0.80][1]
(ratio./(ratio.[ratio. >= 0.80][1]))
chge. <- round((ratio.[2:10] - ratio.[1:9])*100, digits = 0)
length(chge.) - length(chge.[chge. < 5]) + 1

rm(qecb.i, sp_accu., speca., chge., ratio.)


# write a function

speca.fct <- function(method., main.) {
  
  speca.list <- vector("list", length(unique(sp_accu.$Site.Year.Month.Day)))
  inf5.list <- vector("list", length(unique(sp_accu.$Site.Year.Month.Day)))
  
  for (i in c(1:length(unique(sp_accu.$Site.Year.Month.Day)))) {
    
    sp_accu.  %>% filter(Site.Year.Month.Day == unique(sp_accu.$Site.Year.Month.Day)[[i]]) -> qecb.i
    qecb.i <- with(qecb.i, qecb.i[order(Numéro.Bloc.échantillon),])
    
    (speca. <- specaccum(qecb.i[, c(6:ncol(qecb.i))], method = method.))
    show(speca.)
    speca.list[[i]] <- speca.
    speca.list <<- speca.list
  
    (ratio. <- speca.$richness/speca.$richness[length(speca.$richness)])
    chge. <- round((ratio.[2:10] - ratio.[1:9])*100, digits = 0)
    inf5. <- length(chge.) - length(chge.[chge. < 5]) + 1
    inf5.list[[i]] <- inf5.
    inf5.list <<- inf5.list
    
  }
  
  ylim. <- range(unlist(sapply(speca.list, '[[', 'richness')) 
                 #+ unlist(sapply(speca.list, '[[', 'sd'))
                 )
  ylim. <- c(0, max(ylim.)+2)
  
  plot(speca.list[[1]], ylim = ylim., ci = 0, xlab = "Blocs", main = main.)
  for (i in 2:length(speca.list)) plot(speca.list[[i]], ci = 0, add = T, col = i)
  
  library(reshape2)
  melt. <- melt(lapply(speca.list, '[[', 'richness'))

  library(dplyr)
  melt.  %>% 
    group_by(L1) %>% 
    mutate(Number = seq_along(L1)) %>% 
    ungroup() -> melt.
  melt. %>% group_by(Number) %>% summarize(meanL1 = mean(value), sdL1 = sd(value), nb. = n()) -> stats.
  #stats.$Number <- as.factor(stats.$Number)
  
  points(stats.$Number, stats.$meanL1, type = "b", pch = 19, col = "red", cex = 2, lwd = 3)
  arrows(stats.$Number, stats.$meanL1 + stats.$sdL1, stats.$Number, stats.$meanL1 - stats.$sdL1, code = 3, angle = 90, length = 0.00, col = "red", lwd = 2)
  
  unlist(inf5.list)[unlist(inf5.list) != 0]
  legend("topleft", c("boulder nb for <5% change:", paste0("mean = " , round(mean(unlist(inf5.list)[unlist(inf5.list) > 1], na.rm = T), digits = 0)), paste0("median = " , round(median(unlist(inf5.list)[unlist(inf5.list) > 1], na.rm = T), digits = 0))), bty = "n")
  
}


# Bretagne

sp_accu. <- filter(qecbNato0, Type.Bloc == "Bloc mobile" & Face == "face supérieure" & Region == "Bretagne")
sp_accu. <- sp_accu.[, c("Site", "Site.Year.Month.Day", "Type.Bloc", "Face", "Numéro.Bloc.échantillon", Bret_EGMP.BASQ_qecb
                         #, Bret_EGMP.BASQ_fishing
                         )]
sp_accu.$Numéro.Bloc.échantillon <- as.factor(sp_accu.$Numéro.Bloc.échantillon)
unique(sp_accu.$Numéro.Bloc.échantillon)
speca.fct(method. = "exact", main. = "Blocs mobiles - face supérieure - Bretagne")
speca.fct(method. = "collector", main. = "Blocs mobiles - face supérieure - Bretagne")
rm(sp_accu.)

sp_accu. <- filter(qecbNato0, Type.Bloc == "Bloc mobile" & Face == "face supérieure" & Region == "Bretagne")
sp_accu. <- sp_accu.[, c("Site", "Site.Year.Month.Day", "Type.Bloc", "Face", "Numéro.Bloc.échantillon", Bret_EGMP.BASQ_qecb
                         #, Bret_EGMP.BASQ_fishing
                         )]
sp_accu.$Numéro.Bloc.échantillon <- factor(sp_accu.$Numéro.Bloc.échantillon, levels = c("1","3","5","7","9","2","4","6","8","10"))
unique(sp_accu.$Numéro.Bloc.échantillon)
speca.fct(method. = "collector", main. = "Blocs mobiles odd nbs first - face supérieure - Bretagne")
rm(sp_accu.)

sp_accu. <- filter(qecbNato0, Type.Bloc == "Bloc mobile" & Face == "face supérieure" & Region == "Bretagne")
sp_accu. <- sp_accu.[, c("Site", "Site.Year.Month.Day", "Type.Bloc", "Face", "Numéro.Bloc.échantillon", Bret_EGMP.BASQ_qecb
                         #, Bret_EGMP.BASQ_fishing
)]
sp_accu.$Numéro.Bloc.échantillon <- factor(sp_accu.$Numéro.Bloc.échantillon, levels = c("2","4","6","8","10", "1","3","5","7","9"))
unique(sp_accu.$Numéro.Bloc.échantillon)
speca.fct(method. = "collector", main. = "Blocs mobiles even nbs first - face supérieure - Bretagne")
rm(sp_accu.)

sp_accu. <- filter(qecbNato0, Type.Bloc == "Bloc mobile" & Face == "face inférieure" & Region == "Bretagne")
sp_accu. <- sp_accu.[, c("Site", "Site.Year.Month.Day", "Type.Bloc", "Face", "Numéro.Bloc.échantillon", Bret_EGMP.BASQ_qecb
                         #, Bret_EGMP.BASQ_fishing
                         )]
sp_accu.$Numéro.Bloc.échantillon <- as.factor(sp_accu.$Numéro.Bloc.échantillon)
unique(sp_accu.$Numéro.Bloc.échantillon)
speca.fct(method. = "exact", main. = "Blocs mobiles - face inférieures - Bretagne")
speca.fct(method. = "collector", main. = "Blocs mobiles - face inférieures - Bretagne")
rm(sp_accu.)

sp_accu. <- filter(qecbNato0, Type.Bloc == "Bloc mobile" & Face == "face inférieure" & Region == "Bretagne")
sp_accu. <- sp_accu.[, c("Site", "Site.Year.Month.Day", "Type.Bloc", "Face", "Numéro.Bloc.échantillon", Bret_EGMP.BASQ_qecb
                         #, Bret_EGMP.BASQ_fishing
                         )]
sp_accu.$Numéro.Bloc.échantillon <- factor(sp_accu.$Numéro.Bloc.échantillon, levels = c("1","3","5","7","9","2","4","6","8","10"))
unique(sp_accu.$Numéro.Bloc.échantillon)
speca.fct(method. = "collector", main. = "Blocs mobiles odd nbs first - face inférieure - Bretagne")
rm(sp_accu.)

sp_accu. <- filter(qecbNato0, Type.Bloc == "Bloc mobile" & Face == "face inférieure" & Region == "Bretagne")
sp_accu. <- sp_accu.[, c("Site", "Site.Year.Month.Day", "Type.Bloc", "Face", "Numéro.Bloc.échantillon", Bret_EGMP.BASQ_qecb
                         #, Bret_EGMP.BASQ_fishing
)]
sp_accu.$Numéro.Bloc.échantillon <- factor(sp_accu.$Numéro.Bloc.échantillon, levels = c("2","4","6","8","10","1","3","5","7","9"))
unique(sp_accu.$Numéro.Bloc.échantillon)
speca.fct(method. = "collector", main. = "Blocs mobiles even nbs first - face inférieure - Bretagne")
rm(sp_accu.)

unique(qecbNato0$Type.Bloc)
sp_accu. <- filter(qecbNato0, Type.Bloc %in% c("Bloc fixé", "Roche en place") & Region == "Bretagne")
sp_accu. <- sp_accu.[, c("Site", "Site.Year.Month.Day", "Type.Bloc", "Face", "Numéro.Bloc.échantillon", Bret_EGMP.BASQ_qecb)]
sp_accu.$Numéro.Bloc.échantillon <- as.factor(sp_accu.$Numéro.Bloc.échantillon)
unique(sp_accu.$Numéro.Bloc.échantillon)
speca.fct(method. = "exact", main. = "Blocs fixés et Roche en place - Bretagne")
rm(sp_accu.)


# EGMP.BASQ

sp_accu. <- filter(qecbNato0, Type.Bloc == "Bloc mobile" & Face == "face supérieure" & Region == "EGMP.BASQ")
sp_accu. <- sp_accu.[, c("Site", "Site.Year.Month.Day", "Type.Bloc", "Face", "Numéro.Bloc.échantillon", Bret_EGMP.BASQ_qecb, EGMP.BASQ_qecb
                         #, Bret_EGMP.BASQ_fishing, EGMP.BASQ_fishing
                         )]
sp_accu.$Numéro.Bloc.échantillon <- as.factor(sp_accu.$Numéro.Bloc.échantillon)
unique(sp_accu.$Numéro.Bloc.échantillon)
speca.fct(method. = "exact", main. = "Blocs mobiles - face supérieure - EGMP.BASQ")
speca.fct(method. = "collector", main. = "Blocs mobiles - face supérieure - EGMP.BASQ")
rm(sp_accu.)

sp_accu. <- filter(qecbNato0, Type.Bloc == "Bloc mobile" & Face == "face supérieure" & Region == "EGMP.BASQ")
sp_accu. <- sp_accu.[, c("Site", "Site.Year.Month.Day", "Type.Bloc", "Face", "Numéro.Bloc.échantillon", Bret_EGMP.BASQ_qecb, EGMP.BASQ_qecb
                         #, Bret_EGMP.BASQ_fishing, EGMP.BASQ_fishing
                         )]
sp_accu.$Numéro.Bloc.échantillon <- factor(sp_accu.$Numéro.Bloc.échantillon, levels = c("1","3","5","7","9","2","4","6","8","10"))
unique(sp_accu.$Numéro.Bloc.échantillon)
speca.fct(method. = "collector", main. = "Blocs mobiles odd nbs first - face supérieure - EGMP.BASQ")
rm(sp_accu.)

sp_accu. <- filter(qecbNato0, Type.Bloc == "Bloc mobile" & Face == "face supérieure" & Region == "EGMP.BASQ")
sp_accu. <- sp_accu.[, c("Site", "Site.Year.Month.Day", "Type.Bloc", "Face", "Numéro.Bloc.échantillon", Bret_EGMP.BASQ_qecb, EGMP.BASQ_qecb
                         #, Bret_EGMP.BASQ_fishing, EGMP.BASQ_fishing
)]
sp_accu.$Numéro.Bloc.échantillon <- factor(sp_accu.$Numéro.Bloc.échantillon, levels = c("2","4","6","8","10", "1","3","5","7","9"))
unique(sp_accu.$Numéro.Bloc.échantillon)
speca.fct(method. = "collector", main. = "Blocs mobiles even nbs first - face supérieure - EGMP.BASQ")
rm(sp_accu.)

sp_accu. <- filter(qecbNato0, Type.Bloc == "Bloc mobile" & Face == "face inférieure" & Region == "EGMP.BASQ")
sp_accu. <- sp_accu.[, c("Site", "Site.Year.Month.Day", "Type.Bloc", "Face", "Numéro.Bloc.échantillon", Bret_EGMP.BASQ_qecb, EGMP.BASQ_qecb
                         #, Bret_EGMP.BASQ_fishing, EGMP.BASQ_fishing
                         )]
sp_accu.$Numéro.Bloc.échantillon <- as.factor(sp_accu.$Numéro.Bloc.échantillon)
unique(sp_accu.$Numéro.Bloc.échantillon)
speca.fct(method. = "exact", main. = "Blocs mobiles - face inférieures - EGMP.BASQ")
speca.fct(method. = "collector", main. = "Blocs mobiles - face inférieures - EGMP.BASQ")
rm(sp_accu.)

sp_accu. <- filter(qecbNato0, Type.Bloc == "Bloc mobile" & Face == "face inférieure" & Region == "EGMP.BASQ")
sp_accu. <- sp_accu.[, c("Site", "Site.Year.Month.Day", "Type.Bloc", "Face", "Numéro.Bloc.échantillon", Bret_EGMP.BASQ_qecb, EGMP.BASQ_qecb
                         #Bret_EGMP.BASQ_fishing, EGMP.BASQ_fishing
                         )]
sp_accu.$Numéro.Bloc.échantillon <- factor(sp_accu.$Numéro.Bloc.échantillon, levels = c("1","3","5","7","9","2","4","6","8","10"))
unique(sp_accu.$Numéro.Bloc.échantillon)
speca.fct(method. = "collector", main. = "Blocs mobiles odd nbs first - face inférieure - EGMP.BASQ")
rm(sp_accu.)

sp_accu. <- filter(qecbNato0, Type.Bloc == "Bloc mobile" & Face == "face inférieure" & Region == "EGMP.BASQ")
sp_accu. <- sp_accu.[, c("Site", "Site.Year.Month.Day", "Type.Bloc", "Face", "Numéro.Bloc.échantillon", Bret_EGMP.BASQ_qecb, EGMP.BASQ_qecb
                         #Bret_EGMP.BASQ_fishing, EGMP.BASQ_fishing
)]
sp_accu.$Numéro.Bloc.échantillon <- factor(sp_accu.$Numéro.Bloc.échantillon, levels = c("2","4","6","8","10","1","3","5","7","9"))
unique(sp_accu.$Numéro.Bloc.échantillon)
speca.fct(method. = "collector", main. = "Blocs mobiles even nbs first - face inférieure - EGMP.BASQ")
rm(sp_accu.)

unique(qecbNato0$Type.Bloc)
sp_accu. <- filter(qecbNato0, Type.Bloc %in% c("Bloc fixé", "Roche en place") & Region == "EGMP.BASQ")
sp_accu. <- sp_accu.[, c("Site", "Site.Year.Month.Day", "Type.Bloc", "Face", "Numéro.Bloc.échantillon", Bret_EGMP.BASQ_qecb, EGMP.BASQ_qecb)]
sp_accu.$Numéro.Bloc.échantillon <- as.factor(sp_accu.$Numéro.Bloc.échantillon)
unique(sp_accu.$Numéro.Bloc.échantillon)
speca.fct(method. = "exact", main. = "Blocs fixés et Roche en place - EGMP.BASQ")
rm(sp_accu.)

rm(inf5.list, speca.list)


## mobile stats

#################################################

sp_stats. <- filter(qecbNato0, Type.Bloc == "Bloc mobile" & Face == "face supérieure" & Region == "Bretagne")
sp_stats. <- sp_stats.[, c("Site", "Site.Year.Month.Day", "Type.Bloc", "Face", "Numéro.Bloc.échantillon", Bret_EGMP.BASQ_qecb
                         #, Bret_EGMP.BASQ_fishing
)]
sp_stats. %>% filter(Site.Year.Month.Day == unique(sp_stats.$Site.Year.Month.Day)[[36]]) -> qecb.i


# test
# no issue with NAs since all NAs were replace by 0 (! differed by Region)

{

qecb.i[1, 6:23] %>%
  summarise(across(everything(), list(mean = mean, sd = sd, median = median))) -> sp.stats.res.

if (nrow(qecb.i) >= 2) {
  qecb.i[1:2, 6:23] %>%
  summarise(across(everything(), list(mean = mean, sd = sd, median = median))) -> sp.stats.res.[2,]
} else {
    NA -> sp.stats.res.[2,]
}

if (nrow(qecb.i) >= 3) {
  qecb.i[1:3, 6:23] %>%
    summarise(across(everything(), list(mean = mean, sd = sd, median = median))) -> sp.stats.res.[3,]
} else {
  NA -> sp.stats.res.[3,]
}

if (nrow(qecb.i) >= 4) {
  qecb.i[1:4, 6:23] %>%
    summarise(across(everything(), list(mean = mean, sd = sd, median = median))) -> sp.stats.res.[4,]
} else {
  NA -> sp.stats.res.[4,]
}

if (nrow(qecb.i) >= 5) {
  qecb.i[1:5, 6:23] %>%
    summarise(across(everything(), list(mean = mean, sd = sd, median = median))) -> sp.stats.res.[5,]
} else {
  NA -> sp.stats.res.[5,]
}

if (nrow(qecb.i) >= 6) {
  qecb.i[1:6, 6:23] %>%
    summarise(across(everything(), list(mean = mean, sd = sd, median = median))) -> sp.stats.res.[6,]
} else {
  NA -> sp.stats.res.[6,]
}

if (nrow(qecb.i) >= 7) {
  qecb.i[1:7, 6:23] %>%
    summarise(across(everything(), list(mean = mean, sd = sd, median = median))) -> sp.stats.res.[7,]
} else {
  NA -> sp.stats.res.[7,]
}

if (nrow(qecb.i) >= 8) {
  qecb.i[1:8, 6:23] %>%
    summarise(across(everything(), list(mean = mean, sd = sd, median = median))) -> sp.stats.res.[8,]
} else {
  NA -> sp.stats.res.[8,]
}

if (nrow(qecb.i) >= 9) {
  qecb.i[1:9, 6:23] %>%
    summarise(across(everything(), list(mean = mean, sd = sd, median = median))) -> sp.stats.res.[9,]
} else {
  NA -> sp.stats.res.[9,]
}

if (nrow(qecb.i) == 10) {
  qecb.i[1:10, 6:23] %>%
    summarise(across(everything(), list(mean = mean, sd = sd, median = median))) -> sp.stats.res.[10,]
} else {
  NA -> sp.stats.res.[10,]
}

sp.stats.res.

sp.stats.res.$nb <- c(1, rep(NA,9))
sp.stats.res.$nb[2] <- ifelse(nrow(qecb.i) >= 2, 2, NA)
sp.stats.res.$nb[3] <- ifelse(nrow(qecb.i) >= 3, 3, NA)
sp.stats.res.$nb[4] <- ifelse(nrow(qecb.i) >= 4, 4, NA)
sp.stats.res.$nb[5] <- ifelse(nrow(qecb.i) >= 5, 5, NA)
sp.stats.res.$nb[6] <- ifelse(nrow(qecb.i) >= 6, 6, NA)
sp.stats.res.$nb[7] <- ifelse(nrow(qecb.i) >= 7, 7, NA)
sp.stats.res.$nb[8] <- ifelse(nrow(qecb.i) >= 8, 8, NA)
sp.stats.res.$nb[9] <- ifelse(nrow(qecb.i) >= 9, 9, NA)
sp.stats.res.$nb[10] <- ifelse(nrow(qecb.i) == 10, 10, NA)

sp.stats.res. <- subset(sp.stats.res., (!is.na(sp.stats.res.[,"nb"])))

list. <- vector("list", ncol(sp.stats.res.)-1)

for(i in c(1:(ncol(sp.stats.res.)-1))) {
  
  var.i. <- as.vector(sp.stats.res.[[i]])
  
  var.i.norm. <- c(rep(NA), length(var.i.))
  
  for(j in c(1:length(var.i.))) {
    
    x <- (var.i.[[j]] - min(var.i., na.rm = T)) / (max(var.i., na.rm = T) - min(var.i., na.rm = T))
    var.i.norm.[[j]] <- x
    
  }
  
  list.[[i]] <- var.i.norm.
  
}

sp.stats.res.norm. <- data.frame(matrix(unlist(list.), ncol = length(list.), byrow = F))
sp.stats.res.norm. <- data.frame(sapply(sp.stats.res.norm., function(x) ifelse(is.nan(x), 0, x)))
names(sp.stats.res.norm.) <- names(sp.stats.res.[, 1:(ncol(sp.stats.res.)-1)])

library(tibble)
nb <- sp.stats.res.$nb
sp.stats.res.norm. <- add_column(sp.stats.res.norm., nb, .before = names(sp.stats.res.norm.[1]))
rm(nb)
Site.Year.Month.Day <- qecb.i[, "Site.Year.Month.Day"]
sp.stats.res.norm. <- add_column(sp.stats.res.norm., Site.Year.Month.Day, .before = names(sp.stats.res.norm.[1]))
rm(Site.Year.Month.Day)

}

# then plot

library(tidyr)
library(ggplot2)

#p.mean. <- select(sp.stats.res.norm., c(grep("mean", names(sp.stats.res.norm.), value=TRUE)))
p.mean. <- sp.stats.res.norm.[, c(grep("mean", names(sp.stats.res.norm.), value=TRUE))]
p.mean.$nb <- sp.stats.res.norm.$nb 

#p.sd. <- select(sp.stats.res.norm., c(grep("sd", names(sp.stats.res.norm.), value=TRUE)))
p.sd. <- sp.stats.res.norm.[, c(grep("sd", names(sp.stats.res.norm.), value=TRUE))]
p.sd.$nb <- sp.stats.res.norm.$nb 

#p.median. <- select(sp.stats.res.norm., c(grep("median", names(sp.stats.res.norm.), value=TRUE)))
p.median. <- sp.stats.res.norm.[, c(grep("median", names(sp.stats.res.norm.), value=TRUE))]
p.median.$nb <- sp.stats.res.norm.$nb 

df. <- p.mean.
ylab. = "mean"

mdf <- melt(df., id = "nb")  # convert to long format
p.mean. <- ggplot(mdf, aes(x = nb, y = value, colour = variable)) +
  labs(x = "blocs", y = ylab.) +
  coord_cartesian(ylim = c(0,1)) +
  scale_x_continuous(limits=c(0.75,10.25), breaks = c(1,2,3,4,5,6,7,8,9,10)) +
  geom_point() + 
  theme_bw() +
  theme(legend.key.size = unit(0.5, 'lines')) +
  stat_summary(fun = mean, geom = "point", color = "black", size = 3) +
  stat_summary(fun = mean, geom = "line", color = "black", size = 1) +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), 
               geom = "errorbar", color = "black", width = 0.5)

df. <- p.sd.
ylab. = "sd"

mdf <- melt(df., id = "nb")  # convert to long format
p.sd. <- ggplot(mdf, aes(x = nb, y = value, colour = variable)) +
  labs(x = "blocs", y = ylab.) +
  coord_cartesian(ylim = c(0,1)) +
  scale_x_continuous(limits=c(0.75,10.25), breaks = c(1,2,3,4,5,6,7,8,9,10)) +
  geom_point() + 
  theme_bw() +
  theme(legend.key.size = unit(0.5, 'lines')) +
  stat_summary(fun = mean, geom = "point", color = "black", size = 3) +
  stat_summary(fun = mean, geom = "line", color = "black", size = 1) +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), 
               geom = "errorbar", color = "black", width = 0.5)

df. <- p.median.
ylab. = "median"

mdf <- melt(df., id = "nb")  # convert to long format
p.median. <- ggplot(mdf, aes(x = nb, y = value, colour = variable)) +
  labs(x = "blocs", y = ylab.) +
  coord_cartesian(ylim = c(0,1)) +
  scale_x_continuous(limits=c(0.75,10.25), breaks = c(1,2,3,4,5,6,7,8,9,10)) +
  geom_point() + 
  theme_bw() +
  theme(legend.key.size = unit(0.5, 'lines')) +
  stat_summary(fun = mean, geom = "point", color = "black", size = 3) +
  stat_summary(fun = mean, geom = "line", color = "black", size = 1) +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), 
               geom = "errorbar", color = "black", width = 0.5) +
  stat_summary(fun = median, geom = "point", color = "red", size = 3) +
  stat_summary(fun = median, geom = "line", color = "red", size = 1)
  
library("gridExtra")
grid.arrange(p.mean., p.sd., p.median., ncol=1, nrow = 3)

rm(df., list., mdf, p.mean., p.sd., p.median., qecb.i, sp_stats., i, j, var.i., var.i.norm., x, ylab.)
rm(sp.stats.res., sp.stats.res.norm.)

#################################################


# in a loop

#################################################

# loop, with normalisation by survey, e.g. on Bretagne bloc mobile face supérieure

sp_stats. <- filter(qecbNato0, Type.Bloc == "Bloc mobile" & Face == "face supérieure" & Region == "Bretagne")
sp_stats. <- sp_stats.[, c("Site", "Site.Year.Month.Day", "Type.Bloc", "Face", "Numéro.Bloc.échantillon", Bret_EGMP.BASQ_qecb
                           #, Bret_EGMP.BASQ_fishing
)]

stats.list <- vector("list", length(unique(sp_stats.$Site.Year.Month.Day)))

for(k in c(1:length(unique(sp_stats.$Site.Year.Month.Day)))) {

  sp_stats. %>% filter(Site.Year.Month.Day == unique(sp_stats.$Site.Year.Month.Day)[[k]]) -> qecb.i

{
  
  qecb.i[1, 6:23] %>%
    summarise(across(everything(), list(mean = mean, sd = sd, median = median))) -> sp.stats.res.
  
  if (nrow(qecb.i) >= 2) {
    qecb.i[1:2, 6:23] %>%
      summarise(across(everything(), list(mean = mean, sd = sd, median = median))) -> sp.stats.res.[2,]
  } else {
    NA -> sp.stats.res.[2,]
  }
  
  if (nrow(qecb.i) >= 3) {
    qecb.i[1:3, 6:23] %>%
      summarise(across(everything(), list(mean = mean, sd = sd, median = median))) -> sp.stats.res.[3,]
  } else {
    NA -> sp.stats.res.[3,]
  }
  
  if (nrow(qecb.i) >= 4) {
    qecb.i[1:4, 6:23] %>%
      summarise(across(everything(), list(mean = mean, sd = sd, median = median))) -> sp.stats.res.[4,]
  } else {
    NA -> sp.stats.res.[4,]
  }
  
  if (nrow(qecb.i) >= 5) {
    qecb.i[1:5, 6:23] %>%
      summarise(across(everything(), list(mean = mean, sd = sd, median = median))) -> sp.stats.res.[5,]
  } else {
    NA -> sp.stats.res.[5,]
  }
  
  if (nrow(qecb.i) >= 6) {
    qecb.i[1:6, 6:23] %>%
      summarise(across(everything(), list(mean = mean, sd = sd, median = median))) -> sp.stats.res.[6,]
  } else {
    NA -> sp.stats.res.[6,]
  }
  
  if (nrow(qecb.i) >= 7) {
    qecb.i[1:7, 6:23] %>%
      summarise(across(everything(), list(mean = mean, sd = sd, median = median))) -> sp.stats.res.[7,]
  } else {
    NA -> sp.stats.res.[7,]
  }
  
  if (nrow(qecb.i) >= 8) {
    qecb.i[1:8, 6:23] %>%
      summarise(across(everything(), list(mean = mean, sd = sd, median = median))) -> sp.stats.res.[8,]
  } else {
    NA -> sp.stats.res.[8,]
  }
  
  if (nrow(qecb.i) >= 9) {
    qecb.i[1:9, 6:23] %>%
      summarise(across(everything(), list(mean = mean, sd = sd, median = median))) -> sp.stats.res.[9,]
  } else {
    NA -> sp.stats.res.[9,]
  }
  
  if (nrow(qecb.i) == 10) {
    qecb.i[1:10, 6:23] %>%
      summarise(across(everything(), list(mean = mean, sd = sd, median = median))) -> sp.stats.res.[10,]
  } else {
    NA -> sp.stats.res.[10,]
  }
  
  sp.stats.res.
  
  library(tibble)
  nb <- c(rep(NA,10))
  sp.stats.res. <- add_column(sp.stats.res., nb, .before = names(sp.stats.res.[1]))
  rm(nb)
  
  sp.stats.res.$nb[1] <- ifelse(nrow(qecb.i) >= 1, 1, NA)
  sp.stats.res.$nb[2] <- ifelse(nrow(qecb.i) >= 2, 2, NA)
  sp.stats.res.$nb[3] <- ifelse(nrow(qecb.i) >= 3, 3, NA)
  sp.stats.res.$nb[4] <- ifelse(nrow(qecb.i) >= 4, 4, NA)
  sp.stats.res.$nb[5] <- ifelse(nrow(qecb.i) >= 5, 5, NA)
  sp.stats.res.$nb[6] <- ifelse(nrow(qecb.i) >= 6, 6, NA)
  sp.stats.res.$nb[7] <- ifelse(nrow(qecb.i) >= 7, 7, NA)
  sp.stats.res.$nb[8] <- ifelse(nrow(qecb.i) >= 8, 8, NA)
  sp.stats.res.$nb[9] <- ifelse(nrow(qecb.i) >= 9, 9, NA)
  sp.stats.res.$nb[10] <- ifelse(nrow(qecb.i) == 10, 10, NA)
  
  sp.stats.res. <- subset(sp.stats.res., (!is.na(sp.stats.res.[,"nb"])))
  
  list. <- vector("list", ncol(sp.stats.res.)-1)
  
  for(i in c(2:ncol(sp.stats.res.))) {
    
    var.i. <- as.vector(sp.stats.res.[[i]])
    
    var.i.norm. <- c(rep(NA), length(var.i.))
    
    for(j in c(1:length(var.i.))) {
      
      x <- (var.i.[[j]] - min(var.i., na.rm = T)) / (max(var.i., na.rm = T) - min(var.i., na.rm = T))
      var.i.norm.[[j]] <- x
      
    }
    
    list.[[i]] <- var.i.norm.
    
  }

  sp.stats.res.norm. <- data.frame(matrix(unlist(list.), ncol = length(list.)-1, byrow = F))
  sp.stats.res.norm. <- data.frame(sapply(sp.stats.res.norm., function(x) ifelse(is.nan(x), 0, x)))
  names(sp.stats.res.norm.) <- names(sp.stats.res.[, 2:(ncol(sp.stats.res.))])
  
  nb <- sp.stats.res.$nb
  sp.stats.res.norm. <- add_column(sp.stats.res.norm., nb, .before = names(sp.stats.res.norm.[1]))
  rm(nb)
  
  Site.Year.Month.Day <- qecb.i[, "Site.Year.Month.Day"]
  sp.stats.res.norm. <- add_column(sp.stats.res.norm., Site.Year.Month.Day, .before = sp.stats.res.norm.[, 1])
  rm(Site.Year.Month.Day)
  
}
  
  stats.list[[k]] <- sp.stats.res.norm.
  
}

stats.df.norm. <- do.call("rbind", stats.list)
#stats.df. <<- stats.df.

# then plot

#p.mean. <- select(stats.df.norm., c(grep("mean", names(stats.df.norm.), value=TRUE)))
p.mean. <- stats.df.norm.[, c(grep("mean", names(stats.df.norm.), value=TRUE))]
p.mean.$nb <- stats.df.norm.$nb 

#p.sd. <- select(stats.df.norm., c(grep("sd", names(stats.df.norm.), value=TRUE)))
p.sd. <- stats.df.norm.[, c(grep("sd", names(stats.df.norm.), value=TRUE))]
p.sd.$nb <- stats.df.norm.$nb 

#p.median. <- select(stats.df.norm., c(grep("median", names(stats.df.norm.), value=TRUE)))
p.median. <- stats.df.norm.[, c(grep("median", names(stats.df.norm.), value=TRUE))]
p.median.$nb <- stats.df.norm.$nb 

df. <- p.mean.
ylab. = "mean"

mdf <- melt(df., id = "nb")  # convert to long format
p.mean. <- ggplot(mdf, aes(x = nb, y = value, colour = variable)) +
  labs(x = "blocs", y = ylab.) +
  coord_cartesian(ylim = c(0,1)) +
  scale_x_continuous(limits=c(0.75,10.25), breaks = c(1,2,3,4,5,6,7,8,9,10)) +
  geom_point() + 
  theme_bw() +
  theme(legend.key.size = unit(0.5, 'lines')) +
  stat_summary(fun = mean, geom = "point", color = "black", size = 3) +
  stat_summary(fun = mean, geom = "line", color = "black", size = 1) +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), 
               geom = "errorbar", color = "black", width = 0.5)

df. <- p.sd.
ylab. = "sd"

mdf <- melt(df., id = "nb")  # convert to long format
p.sd. <- ggplot(mdf, aes(x = nb, y = value, colour = variable)) +
  labs(x = "blocs", y = ylab.) +
  coord_cartesian(ylim = c(0,1)) +
  scale_x_continuous(limits=c(0.75,10.25), breaks = c(1,2,3,4,5,6,7,8,9,10)) +
  geom_point() + 
  theme_bw() +
  theme(legend.key.size = unit(0.5, 'lines')) +
  stat_summary(fun = mean, geom = "point", color = "black", size = 3) +
  stat_summary(fun = mean, geom = "line", color = "black", size = 1) +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), 
               geom = "errorbar", color = "black", width = 0.5)

df. <- p.median.
ylab. = "median"

mdf <- melt(df., id = "nb")  # convert to long format
p.median. <- ggplot(mdf, aes(x = nb, y = value, colour = variable)) +
  labs(x = "blocs", y = ylab.) +
  coord_cartesian(ylim = c(0,1)) +
  scale_x_continuous(limits=c(0.75,10.25), breaks = c(1,2,3,4,5,6,7,8,9,10)) +
  geom_point() + 
  theme_bw() +
  theme(legend.key.size = unit(0.5, 'lines')) +
  stat_summary(fun = mean, geom = "point", color = "black", size = 3) +
  stat_summary(fun = mean, geom = "line", color = "black", size = 1) +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), 
               geom = "errorbar", color = "black", width = 0.5) +
  stat_summary(fun = median, geom = "point", color = "red", size = 3) +
  stat_summary(fun = median, geom = "line", color = "red", size = 1)


library("gridExtra")
grid.arrange(p.mean., p.sd., p.median., ncol=1, nrow = 3)

rm(df., list., mdf, p.mean., p.sd., p.median., qecb.i, sp_stats., i, j, var.i., var.i.norm., x, ylab., sp.stats.res., sp.stats.res.norm., stats.list, k)
rm(stats.df.norm.)

#################################################


# loop - in a fct - with normalisation for all survey data

mob.fct. <- function(df.) {

sp_stats. <- df.
sp_stats. <- with(sp_stats., sp_stats.[order( Site.Year.Month.Day, Numéro.Bloc.échantillon),]) # to order according to the levels of variable Numéro.Bloc.échantillon
stats.list <- vector("list", length(unique(sp_stats.$Site.Year.Month.Day)))

{
  
  for(k in c(1:length(unique(sp_stats.$Site.Year.Month.Day)))) {
    
    sp_stats. %>% filter(Site.Year.Month.Day == unique(sp_stats.$Site.Year.Month.Day)[[k]]) -> qecb.i
    
    qecb.i[1, 6:23] %>%
      summarise(across(everything(), list(mean = mean, sd = sd, median = median))) -> sp.stats.res.
    
    if (nrow(qecb.i) >= 2) {
      qecb.i[1:2, 6:23] %>%
        summarise(across(everything(), list(mean = mean, sd = sd, median = median))) -> sp.stats.res.[2,]
    } else {
      NA -> sp.stats.res.[2,]
    }
    
    if (nrow(qecb.i) >= 3) {
      qecb.i[1:3, 6:23] %>%
        summarise(across(everything(), list(mean = mean, sd = sd, median = median))) -> sp.stats.res.[3,]
    } else {
      NA -> sp.stats.res.[3,]
    }
    
    if (nrow(qecb.i) >= 4) {
      qecb.i[1:4, 6:23] %>%
        summarise(across(everything(), list(mean = mean, sd = sd, median = median))) -> sp.stats.res.[4,]
    } else {
      NA -> sp.stats.res.[4,]
    }
    
    if (nrow(qecb.i) >= 5) {
      qecb.i[1:5, 6:23] %>%
        summarise(across(everything(), list(mean = mean, sd = sd, median = median))) -> sp.stats.res.[5,]
    } else {
      NA -> sp.stats.res.[5,]
    }
    
    if (nrow(qecb.i) >= 6) {
      qecb.i[1:6, 6:23] %>%
        summarise(across(everything(), list(mean = mean, sd = sd, median = median))) -> sp.stats.res.[6,]
    } else {
      NA -> sp.stats.res.[6,]
    }
    
    if (nrow(qecb.i) >= 7) {
      qecb.i[1:7, 6:23] %>%
        summarise(across(everything(), list(mean = mean, sd = sd, median = median))) -> sp.stats.res.[7,]
    } else {
      NA -> sp.stats.res.[7,]
    }
    
    if (nrow(qecb.i) >= 8) {
      qecb.i[1:8, 6:23] %>%
        summarise(across(everything(), list(mean = mean, sd = sd, median = median))) -> sp.stats.res.[8,]
    } else {
      NA -> sp.stats.res.[8,]
    }
    
    if (nrow(qecb.i) >= 9) {
      qecb.i[1:9, 6:23] %>%
        summarise(across(everything(), list(mean = mean, sd = sd, median = median))) -> sp.stats.res.[9,]
    } else {
      NA -> sp.stats.res.[9,]
    }
    
    if (nrow(qecb.i) == 10) {
      qecb.i[1:10, 6:23] %>%
        summarise(across(everything(), list(mean = mean, sd = sd, median = median))) -> sp.stats.res.[10,]
    } else {
      NA -> sp.stats.res.[10,]
    }
    
    sp.stats.res.
    
    library(tibble)
    nb <- c(rep(NA,10))
    sp.stats.res. <- add_column(sp.stats.res., nb, .before = names(sp.stats.res.[1]))
    rm(nb)
    
    sp.stats.res.$nb[1] <- ifelse(nrow(qecb.i) >= 1, 1, NA)
    sp.stats.res.$nb[2] <- ifelse(nrow(qecb.i) >= 2, 2, NA)
    sp.stats.res.$nb[3] <- ifelse(nrow(qecb.i) >= 3, 3, NA)
    sp.stats.res.$nb[4] <- ifelse(nrow(qecb.i) >= 4, 4, NA)
    sp.stats.res.$nb[5] <- ifelse(nrow(qecb.i) >= 5, 5, NA)
    sp.stats.res.$nb[6] <- ifelse(nrow(qecb.i) >= 6, 6, NA)
    sp.stats.res.$nb[7] <- ifelse(nrow(qecb.i) >= 7, 7, NA)
    sp.stats.res.$nb[8] <- ifelse(nrow(qecb.i) >= 8, 8, NA)
    sp.stats.res.$nb[9] <- ifelse(nrow(qecb.i) >= 9, 9, NA)
    sp.stats.res.$nb[10] <- ifelse(nrow(qecb.i) == 10, 10, NA)
    
    Site.Year.Month.Day <- rep(unique(qecb.i[, "Site.Year.Month.Day"], 10))
    sp.stats.res. <- add_column(sp.stats.res., Site.Year.Month.Day, .before = names(sp.stats.res.[1]))
    rm(Site.Year.Month.Day)
    
    stats.list[[k]] <- sp.stats.res.
    
  }    
  
  stats.df. <- do.call("rbind", stats.list)
  stats.df. <- subset(stats.df., (!is.na(stats.df.[,"nb"])))
  
  
  list. <- vector("list", ncol(stats.df.)-2)
  
  for(i in c(3:ncol(stats.df.))) {
    
    var.i. <- as.vector(stats.df.[[i]])
    
    var.i.norm. <- c(rep(NA), length(var.i.))
    
    for(j in c(1:length(var.i.))) {
      
      x <- (var.i.[[j]] - min(var.i., na.rm = T)) / (max(var.i., na.rm = T) - min(var.i., na.rm = T))
      var.i.norm.[[j]] <- x
      
    }
    
    list.[[i]] <- var.i.norm.
    
  }
  
  list. <- list.[3:length(list.)]
  
  stats.df.norm. <- data.frame(matrix(unlist(list.), ncol = length(list.), byrow = F))
  stats.df.norm. <- data.frame(sapply(stats.df.norm., function(x) ifelse(is.nan(x), 0, x)))
  names(stats.df.norm.) <- names(stats.df.[, 3:ncol(stats.df.)])
  
  nb <- stats.df.$nb
  stats.df.norm. <- add_column(stats.df.norm., nb, .before = names(stats.df.norm.[1]))
  rm(nb)
  
  Site.Year.Month.Day <- stats.df.$Site.Year.Month.Day
  stats.df.norm. <- add_column(stats.df.norm., Site.Year.Month.Day, .before = names(stats.df.norm.[1]))
  rm(Site.Year.Month.Day)
  
}

stats.df.norm. <<- stats.df.norm.
stats.df. <<- stats.df. 

}

# then plot - in a fct -

plot.fct. <- function(df.) {

# test
#df. <- stats.df.norm.
  
library(tidyr)  
library(ggplot2)  
  
stats.df.norm. <- df.

x. <- c(as.vector(grep("mean", names(stats.df.norm.), value=TRUE)))
c("1", "2")

p.mean. <- stats.df.norm.[, c(names(stats.df.norm.) %in% c(grep("mean", names(stats.df.norm.), value=TRUE)))] 
#p.mean. <- select(stats.df.norm., c(grep("mean", names(stats.df.norm.), value=TRUE)))
p.mean.$nb <- stats.df.norm.$nb 

p.sd. <- stats.df.norm.[, c(names(stats.df.norm.) %in% c(grep("sd", names(stats.df.norm.), value=TRUE)))] 
#p.sd. <- select(stats.df.norm., c(grep("sd", names(stats.df.norm.), value=TRUE)))
p.sd.$nb <- stats.df.norm.$nb 

p.median. <- stats.df.norm.[, c(names(stats.df.norm.) %in% c(grep("median", names(stats.df.norm.), value=TRUE)))] 
#p.median. <- select(stats.df.norm., c(grep("median", names(stats.df.norm.), value=TRUE)))
p.median.$nb <- stats.df.norm.$nb 

df. <- p.mean.
ylab. = "mean"
df. %>% group_by(nb) %>% summarise(across(c(1:(ncol(df.)-1)), mean)) -> stats.mean
rowMeans(stats.mean[2:ncol(stats.mean)]) -> lim.
ylim. = c(min(lim., na.rm = T)-(max(lim., na.rm = T)-min(lim., na.rm = T)), max(lim., na.rm = T)+(max(lim., na.rm = T)-min(lim., na.rm = T)))

mdf <- melt(df., id = "nb")  # convert to long format

p. <- ggplot(mdf, aes(x = nb, y = value, colour = variable)) +
  labs(x = "blocs", y = ylab.) +
  coord_cartesian(ylim = c(0,1)) +
  scale_x_continuous(limits=c(0.75,10.25), breaks = c(1,2,3,4,5,6,7,8,9,10)) +
  geom_point() + 
  theme_bw() +
  theme(legend.key.size = unit(0.75, 'lines')) +
  stat_summary(fun = mean, geom = "point", color = "black", size = 3) +
  stat_summary(fun = mean, geom = "line", color = "black", size = 1) +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), 
               geom = "errorbar", color = "black", width = 0.5)

p.lim. <- ggplot(mdf, aes(x = nb, y = value, colour = variable)) +
  labs(x = "blocs", y = ylab.) +
  coord_cartesian(ylim = ylim.) +
  scale_x_continuous(limits=c(0.75,10.25), breaks = c(1,2,3,4,5,6,7,8,9,10)) +
  geom_point() + 
  theme_bw() +
  theme(legend.key.size = unit(0.75, 'lines')) +
  stat_summary(fun = mean, geom = "point", color = "black", size = 3) +
  stat_summary(fun = mean, geom = "line", color = "black", size = 1) +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), 
               geom = "errorbar", color = "black", width = 0.5)

library("gridExtra")
grid.arrange(p., p.lim., ncol = 1, nrow = 2)

df. <- p.sd.
ylab. = "sd"
df. %>% group_by(nb) %>% summarise(across(c(1:(ncol(df.)-1)), mean)) -> stats.sd
rowMeans(stats.sd[2:ncol(stats.sd)]) -> lim.
ylim. = c(min(lim., na.rm = T)-(max(lim., na.rm = T)-min(lim., na.rm = T)), max(lim., na.rm = T)+(max(lim., na.rm = T)-min(lim., na.rm = T)))

mdf <- melt(df., id = "nb")  # convert to long format

p. <- ggplot(mdf, aes(x = nb, y = value, colour = variable)) +
  labs(x = "blocs", y = ylab.) +
  coord_cartesian(ylim = c(0,1)) +
  scale_x_continuous(limits=c(0.75,10.25), breaks = c(1,2,3,4,5,6,7,8,9,10)) +
  geom_point() + 
  theme_bw() +
  theme(legend.key.size = unit(0.75, 'lines')) +
  stat_summary(fun = mean, geom = "point", color = "black", size = 3) +
  stat_summary(fun = mean, geom = "line", color = "black", size = 1) +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), 
               geom = "errorbar", color = "black", width = 0.5)

p.lim. <- ggplot(mdf, aes(x = nb, y = value, colour = variable)) +
  labs(x = "blocs", y = ylab.) +
  coord_cartesian(ylim = ylim.) +
  scale_x_continuous(limits=c(0.75,10.25), breaks = c(1,2,3,4,5,6,7,8,9,10)) +
  geom_point() + 
  theme_bw() +
  theme(legend.key.size = unit(0.75, 'lines')) +
  stat_summary(fun = mean, geom = "point", color = "black", size = 3) +
  stat_summary(fun = mean, geom = "line", color = "black", size = 1) +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), 
               geom = "errorbar", color = "black", width = 0.5)

library("gridExtra")
grid.arrange(p., p.lim., ncol = 1, nrow = 2)

df. <- p.median.
ylab. = "median"
df. %>% group_by(nb) %>% summarise(across(c(1:(ncol(df.)-1)), mean)) -> stats.median
rowMeans(stats.median[2:ncol(stats.median)]) -> lim.
ylim. = c(min(lim., na.rm = T)-(max(lim., na.rm = T)-min(lim., na.rm = T)), max(lim., na.rm = T)+(max(lim., na.rm = T)-min(lim., na.rm = T)))

mdf <- melt(df., id = "nb")  # convert to long format

p. <- ggplot(mdf, aes(x = nb, y = value, colour = variable)) +
  labs(x = "blocs", y = ylab.) +
  coord_cartesian(ylim = c(0,1)) +
  scale_x_continuous(limits=c(0.75,10.25), breaks = c(1,2,3,4,5,6,7,8,9,10)) +
  geom_point() + 
  theme_bw() +
  theme(legend.key.size = unit(0.75, 'lines')) +
  stat_summary(fun = mean, geom = "point", color = "black", size = 3) +
  stat_summary(fun = mean, geom = "line", color = "black", size = 1) +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), 
               geom = "errorbar", color = "black", width = 0.5) +
  stat_summary(fun = median, geom = "point", color = "red", size = 3) +
  stat_summary(fun = median, geom = "line", color = "red", size = 1)

p.lim. <- ggplot(mdf, aes(x = nb, y = value, colour = variable)) +
  labs(x = "blocs", y = ylab.) +
  coord_cartesian(ylim = ylim.) +
  scale_x_continuous(limits=c(0.75,10.25), breaks = c(1,2,3,4,5,6,7,8,9,10)) +
  geom_point() + 
  theme_bw() +
  theme(legend.key.size = unit(0.75, 'lines')) +
  stat_summary(fun = mean, geom = "point", color = "black", size = 3) +
  stat_summary(fun = mean, geom = "line", color = "black", size = 1) +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), 
               geom = "errorbar", color = "black", width = 0.5) +
  stat_summary(fun = median, geom = "point", color = "red", size = 3) +
  stat_summary(fun = median, geom = "line", color = "red", size = 1)

library("gridExtra")
grid.arrange(p., p.lim., ncol = 1, nrow = 2)

}


# Bretagne

df. <- filter(qecbNato0, Type.Bloc == "Bloc mobile" & Face == "face supérieure" & Region == "Bretagne")
df. <- df.[, c("Site", "Site.Year.Month.Day", "Type.Bloc", "Face", "Numéro.Bloc.échantillon", Bret_EGMP.BASQ_qecb
                         #, Bret_EGMP.BASQ_fishing
)]
df.$Numéro.Bloc.échantillon <- as.factor(df.$Numéro.Bloc.échantillon)
unique(df.$Numéro.Bloc.échantillon)
mob.fct.(df. = df.)
plot.fct.(df. = stats.df.norm.)
rm(df., stats.df., stats.df.norm.)

df. <- filter(qecbNato0, Type.Bloc == "Bloc mobile" & Face == "face supérieure" & Region == "Bretagne")
df. <- df.[, c("Site", "Site.Year.Month.Day", "Type.Bloc", "Face", "Numéro.Bloc.échantillon", Bret_EGMP.BASQ_qecb
                         #, Bret_EGMP.BASQ_fishing
)]
df.$Numéro.Bloc.échantillon <- factor(df.$Numéro.Bloc.échantillon, levels = c("1","3","5","7","9","2","4","6","8","10"))
unique(df.$Numéro.Bloc.échantillon)
mob.fct.(df. = df.)
plot.fct.(df. = stats.df.norm.)
rm(df., stats.df., stats.df.norm.)

df. <- filter(qecbNato0, Type.Bloc == "Bloc mobile" & Face == "face supérieure" & Region == "Bretagne")
df. <- df.[, c("Site", "Site.Year.Month.Day", "Type.Bloc", "Face", "Numéro.Bloc.échantillon", Bret_EGMP.BASQ_qecb
                         #, Bret_EGMP.BASQ_fishing
)]
df.$Numéro.Bloc.échantillon <- factor(df.$Numéro.Bloc.échantillon, levels = c("2","4","6","8","10", "1","3","5","7","9"))
unique(df.$Numéro.Bloc.échantillon)
mob.fct.(df. = df.)
plot.fct.(df. = stats.df.norm.)
rm(df., stats.df., stats.df.norm.)

df. <- filter(qecbNato0, Type.Bloc == "Bloc mobile" & Face == "face inférieure" & Region == "Bretagne")
df. <- df.[, c("Site", "Site.Year.Month.Day", "Type.Bloc", "Face", "Numéro.Bloc.échantillon", Bret_EGMP.BASQ_qecb
                         #, Bret_EGMP.BASQ_fishing
)]
df.$Numéro.Bloc.échantillon <- as.factor(df.$Numéro.Bloc.échantillon)
unique(df.$Numéro.Bloc.échantillon)
mob.fct.(df. = df.)
plot.fct.(df. = stats.df.norm.)
rm(df., stats.df., stats.df.norm.)

df. <- filter(qecbNato0, Type.Bloc == "Bloc mobile" & Face == "face inférieure" & Region == "Bretagne")
df. <- df.[, c("Site", "Site.Year.Month.Day", "Type.Bloc", "Face", "Numéro.Bloc.échantillon", Bret_EGMP.BASQ_qecb
                         #, Bret_EGMP.BASQ_fishing
)]
df.$Numéro.Bloc.échantillon <- factor(df.$Numéro.Bloc.échantillon, levels = c("1","3","5","7","9","2","4","6","8","10"))
unique(df.$Numéro.Bloc.échantillon)
mob.fct.(df. = df.)
plot.fct.(df. = stats.df.norm.)
rm(df., stats.df., stats.df.norm.)

df. <- filter(qecbNato0, Type.Bloc == "Bloc mobile" & Face == "face inférieure" & Region == "Bretagne")
df. <- df.[, c("Site", "Site.Year.Month.Day", "Type.Bloc", "Face", "Numéro.Bloc.échantillon", Bret_EGMP.BASQ_qecb
                         #, Bret_EGMP.BASQ_fishing
)]
df.$Numéro.Bloc.échantillon <- factor(df.$Numéro.Bloc.échantillon, levels = c("2","4","6","8","10","1","3","5","7","9"))
unique(df.$Numéro.Bloc.échantillon)
mob.fct.(df. = df.)
plot.fct.(df. = stats.df.norm.)
rm(df., stats.df., stats.df.norm.)

unique(qecbNato0$Type.Bloc)
df. <- filter(qecbNato0, Type.Bloc %in% c("Bloc fixé", "Roche en place") & Region == "Bretagne")
df. <- df.[, c("Site", "Site.Year.Month.Day", "Type.Bloc", "Face", "Numéro.Bloc.échantillon", Bret_EGMP.BASQ_qecb)]
df.$Numéro.Bloc.échantillon <- as.factor(df.$Numéro.Bloc.échantillon)
unique(df.$Numéro.Bloc.échantillon)
mob.fct.(df. = df.)
plot.fct.(df. = stats.df.norm.)
rm(df., stats.df., stats.df.norm.)


# EGMP.BASQ

df. <- filter(qecbNato0, Type.Bloc == "Bloc mobile" & Face == "face supérieure" & Region == "EGMP.BASQ")
df. <- df.[, c("Site", "Site.Year.Month.Day", "Type.Bloc", "Face", "Numéro.Bloc.échantillon", Bret_EGMP.BASQ_qecb, EGMP.BASQ_qecb
                         #, Bret_EGMP.BASQ_fishing, EGMP.BASQ_fishing
)]
df.$Numéro.Bloc.échantillon <- as.factor(df.$Numéro.Bloc.échantillon)
unique(df.$Numéro.Bloc.échantillon)
mob.fct.(df. = df.)
plot.fct.(df. = stats.df.norm.)
rm(df., stats.df., stats.df.norm.)

df. <- filter(qecbNato0, Type.Bloc == "Bloc mobile" & Face == "face supérieure" & Region == "EGMP.BASQ")
df. <- df.[, c("Site", "Site.Year.Month.Day", "Type.Bloc", "Face", "Numéro.Bloc.échantillon", Bret_EGMP.BASQ_qecb, EGMP.BASQ_qecb
                         #, Bret_EGMP.BASQ_fishing, EGMP.BASQ_fishing
)]
df.$Numéro.Bloc.échantillon <- factor(df.$Numéro.Bloc.échantillon, levels = c("1","3","5","7","9","2","4","6","8","10"))
unique(df.$Numéro.Bloc.échantillon)
mob.fct.(df. = df.)
plot.fct.(df. = stats.df.norm.)
rm(df., stats.df., stats.df.norm.)

df. <- filter(qecbNato0, Type.Bloc == "Bloc mobile" & Face == "face supérieure" & Region == "EGMP.BASQ")
df. <- df.[, c("Site", "Site.Year.Month.Day", "Type.Bloc", "Face", "Numéro.Bloc.échantillon", Bret_EGMP.BASQ_qecb, EGMP.BASQ_qecb
                         #, Bret_EGMP.BASQ_fishing, EGMP.BASQ_fishing
)]
df.$Numéro.Bloc.échantillon <- factor(df.$Numéro.Bloc.échantillon, levels = c("2","4","6","8","10", "1","3","5","7","9"))
unique(df.$Numéro.Bloc.échantillon)
mob.fct.(df. = df.)
plot.fct.(df. = stats.df.norm.)
rm(df., stats.df., stats.df.norm.)

df. <- filter(qecbNato0, Type.Bloc == "Bloc mobile" & Face == "face inférieure" & Region == "EGMP.BASQ")
df. <- df.[, c("Site", "Site.Year.Month.Day", "Type.Bloc", "Face", "Numéro.Bloc.échantillon", Bret_EGMP.BASQ_qecb, EGMP.BASQ_qecb
                         #, Bret_EGMP.BASQ_fishing, EGMP.BASQ_fishing
)]
df.$Numéro.Bloc.échantillon <- as.factor(df.$Numéro.Bloc.échantillon)
unique(df.$Numéro.Bloc.échantillon)
mob.fct.(df. = df.)
plot.fct.(df. = stats.df.norm.)
rm(df., stats.df., stats.df.norm.)

df. <- filter(qecbNato0, Type.Bloc == "Bloc mobile" & Face == "face inférieure" & Region == "EGMP.BASQ")
df. <- df.[, c("Site", "Site.Year.Month.Day", "Type.Bloc", "Face", "Numéro.Bloc.échantillon", Bret_EGMP.BASQ_qecb, EGMP.BASQ_qecb
                         #Bret_EGMP.BASQ_fishing, EGMP.BASQ_fishing
)]
df.$Numéro.Bloc.échantillon <- factor(df.$Numéro.Bloc.échantillon, levels = c("1","3","5","7","9","2","4","6","8","10"))
unique(df.$Numéro.Bloc.échantillon)
mob.fct.(df. = df.)
plot.fct.(df. = stats.df.norm.)
rm(df., stats.df., stats.df.norm.)

df. <- filter(qecbNato0, Type.Bloc == "Bloc mobile" & Face == "face inférieure" & Region == "EGMP.BASQ")
df. <- df.[, c("Site", "Site.Year.Month.Day", "Type.Bloc", "Face", "Numéro.Bloc.échantillon", Bret_EGMP.BASQ_qecb, EGMP.BASQ_qecb
                         #Bret_EGMP.BASQ_fishing, EGMP.BASQ_fishing
)]
df.$Numéro.Bloc.échantillon <- factor(df.$Numéro.Bloc.échantillon, levels = c("2","4","6","8","10","1","3","5","7","9"))
unique(df.$Numéro.Bloc.échantillon)
mob.fct.(df. = df.)
plot.fct.(df. = stats.df.norm.)
rm(df., stats.df., stats.df.norm.)

unique(qecbNato0$Type.Bloc)
df. <- filter(qecbNato0, Type.Bloc %in% c("Bloc fixé", "Roche en place") & Region == "EGMP.BASQ")
df. <- df.[, c("Site", "Site.Year.Month.Day", "Type.Bloc", "Face", "Numéro.Bloc.échantillon", Bret_EGMP.BASQ_qecb, EGMP.BASQ_qecb)]
df.$Numéro.Bloc.échantillon <- as.factor(df.$Numéro.Bloc.échantillon)
unique(df.$Numéro.Bloc.échantillon)
mob.fct.(df. = df.)
plot.fct.(df. = stats.df.norm.)
rm(df., stats.df., stats.df.norm.)



## analyse matricielle

qecbNato0 <- readRDS("results/Ecology/qecbNato0.RDS")
qecbNato0 <- add_column(qecbNato0, Region.Site.Year.Month.Day = paste0(qecbNato0$Region, qecbNato0$Site.Year.Month.Day), .before = "Region")
ivr <- readRDS("results/IVR/ivr.val.qu.RDS")

names(ivr)
#filter(ivr, Site == "GDMO_Locmariaquer" & Date == "2018-09-10") -> issue.Locma

ivr <- rename(ivr, id = id.ivr)
library(stringr)
#Site.Year.Month.Day <- str_sub(ivr$Site.Year.Month.Day.QdNb, end = -3)
library(tibble)
#ivr <- add_column(ivr, Site.Year.Month.Day, .after = "Site.bis")
#rm(Site.Year.Month.Day)

qecbNato0 <- rename(qecbNato0, id = id_qecb)
Numero.Quadrat <- str_sub(qecbNato0$Quadrat.bis, start = -1)
qecbNato0 <- add_column(qecbNato0, Numero.Quadrat, .after = "Quadrat.bis")
rm(Numero.Quadrat)
qecbNato0$Numero.Quadrat <- as.integer(qecbNato0$Numero.Quadrat)
#qecbNato0 <- rename(qecbNato0, Date = Date.fiche)

qecbNato0 <- rename(qecbNato0, Numero.Photo.qecb = Numero.Photo)
ivr <- rename(ivr, Numero.Photo.ivr = Numero.Photo)

qecbNato0 <- rename(qecbNato0, n.qecb = n)
ivr <- rename(ivr, n.ivr = n)

qecbNato0$Year <- as.integer(qecbNato0$Year)
qecbNato0$Month <- as.integer(qecbNato0$Month)
qecbNato0$Day <- as.integer(qecbNato0$Day)

qecb.ivr <- left_join(qecbNato0, ivr)
#, by = c("type.de.suivi", "date.sortie", "Date.fiche", "Year", "Month", "Day", "heure.début", "heure.fin", "coefficient.marée", "heure.marée.basse", "hauteur.basse.mer", "période", "libellé.campagne", "libellé.sortie", "référent.sortie", "équipe.terrain", "territoire", "code.site", "site", "sous.site", "zone.habitat", "type.protocole", "version.protocole", "auteur.saisie", "organisme.suivi", "couverture.nuageuse", "précipitations", "pression.atmosphérique", "température", "force.du.vent", "force.du.vent.en.rafale", "direction.du.vent", "état.de.la.mer", "commentaires", "id", "ID.Fiche", "Site", "Site.bis", "Site.Year.Month.Day", "Numero.Quadrat") # I could use only variables "Site.Year.Month.Day" and "Numero.Quadrat", leads to the same left_joined df/result.

qecb.ivr <- rename(qecb.ivr, Date = Date.fiche)

qecb.ivr <- arrange(qecb.ivr, Site, Date, Numero.Quadrat, Type.Bloc, Face)

filter(qecb.ivr, Site == "GDMO_Locmariaquer" & Date == "2018-09-10") -> issue.Locma

saveRDS(qecb.ivr, "results/Ecology/qecb.ivr.RDS")

qecb.ivr <- readRDS("results/Ecology/qecb.ivr.RDS")

############################################################
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# Anna still hasn't corrected for boulder nb in FINS_Quemenes.2020.10.16 data encoding ! removed from the df.
qecb.ivr %>% filter(Site.Year.Month.Day != "FINS_Quemenes.2020.10.16") -> qecb.ivr
############################################################


# first, create vector (4) for qecb and fishing by region (same as above)

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
  "X..Balanes.Vivantes"                    
  #, "X..Recouvrement.Sediment"               
  #, "X..Roche.Nue"                           
  #, "X..Surface.Accolement"
  )                   

EGMP.BASQ_qecb <- c("Nb.Crassostrea.gigas", "Nb.Ostrea.edulis", "X..Mytilus.sp.", "X..Hermelles" , "X..Hydraires")

Bret_EGMP.BASQ_fishing <- c("Nb.Cancer.pagurus..Tourteau.", 
                            "Nb.Necora.puber..Etrille.", 
                            "Nb.Carcinus.maenas..Crabe.vert.", 
                            "Nb.Nucella.lapilus..Pourpre.", 
                            "Nb.Galathea..Galathées.", 
                            "Nb.Lophozozymus.incisus..ancien.Xantho.incisus.", 
                            "Nb.Palaemon.sp..Crevette.bouquet.ou.crevette.rose.", 
                            "Nb.Haliotis.tuberculata..Ormeau.", 
                            "Nb.Littorina.littorea..Bigorneau.", 
                            "Nb.Xantho.pilipes..Xanthe.poilu.", 
                            "Nb.Mimachlamys.varia..Pétoncle.noir.")

EGMP.BASQ_fishing <- c("Nb.Eriphia.verrucosa..Crabe.verruqueux.", "Nb.Octopus.vulgaris..Poulpe." , "Nb.Paracentrotus.lividus..Oursin.", "Nb.Stramonita.haemastoma..Pourpre.bouche.de.sang." )   


# what to do with spirorbes & Nb.Spirobranchus.lamarckii.total?

hist(qecb.ivr$Nb.spirorbis.total)
hist(qecb.ivr$Nb.spirorbis.total, xlim = c(0,10000), breaks = 1000)
hist(qecb.ivr$Nb.spirorbis.total, xlim = c(0,1000), breaks = 10000)
hist(qecb.ivr$Nb.spirorbis.total, xlim = c(0,100), breaks = 10000)
hist(log10(qecb.ivr$Nb.spirorbis.total+1))
hist(filter(qecb.ivr, Face == "face inférieure")[, "Nb.spirorbis.total"])
hist(filter(qecb.ivr, Face == "face inférieure")[, "Nb.spirorbis.total"], xlim = c(0,1000), breaks = 10000)
hist(log10(filter(qecb.ivr, Face == "face inférieure")[, "Nb.spirorbis.total"]+1))

hist(qecb.ivr$Nb.Spirobranchus.lamarckii.total)
hist(qecb.ivr$Nb.Spirobranchus.lamarckii.total, xlim = c(0,10000), breaks = 1000)
hist(qecb.ivr$Nb.Spirobranchus.lamarckii.total, xlim = c(0,1000), breaks = 10000)
hist(qecb.ivr$Nb.Spirobranchus.lamarckii.total, xlim = c(0,100), breaks = 10000)
hist(log10(qecb.ivr$Nb.Spirobranchus.lamarckii.total+1))
hist(filter(qecb.ivr, Face == "face inférieure")[, "Nb.Spirobranchus.lamarckii.total"])
hist(filter(qecb.ivr, Face == "face inférieure")[, "Nb.Spirobranchus.lamarckii.total"], xlim = c(0,1000), breaks = 10000)
hist(log10(filter(qecb.ivr, Face == "face inférieure")[, "Nb.Spirobranchus.lamarckii.total"]+1))

hist(qecb.ivr$X..Balanes.Vivantes)

qecb.ivr <- add_column(qecb.ivr, log10.Nb.spirorbis.total = log10(qecb.ivr$Nb.spirorbis.total+1), .after = "Nb.spirorbis.total")
qecb.ivr <- add_column(qecb.ivr, log10.Nb.Spirobranchus.lamarckii.total = log10(qecb.ivr$Nb.Spirobranchus.lamarckii.total+1), .after = "Nb.Spirobranchus.lamarckii.total")

# here I can choose to either replace spirorbis and/or spirobranchus by their log10 transformation in Bret_EGMP.BASQ_qecb vector
Bret_EGMP.BASQ_qecb
Bret_EGMP.BASQ_qecb <- replace(Bret_EGMP.BASQ_qecb, Bret_EGMP.BASQ_qecb == "Nb.spirorbis.total", "log10.Nb.spirorbis.total")
#Bret_EGMP.BASQ_qecb <- replace(Bret_EGMP.BASQ_qecb, Bret_EGMP.BASQ_qecb == "Nb.Spirobranchus.lamarckii.total", "log10.Nb.Spirobranchus.lamarckii.total") 


## face supérieure bloc mobile vs roche en place

# test

filter(qecb.ivr, Region == "Bretagne") -> Loc.
filter(qecb.ivr, Site == "GDMO_Locmariaquer") -> Loc.
unique(Loc.$Site.Year.Month.Day)
# activate required row
filter(Loc., Site.Year.Month.Day == "GDMO_Locmariaquer.2016.03.09" &  Face == "face supérieure") -> Loc.
#filter(Loc., Site.Year.Month.Day == "GDMO_Locmariaquer.2016.03.09" &  Type.Bloc == "Bloc mobile") -> Loc.

# or test for EGMP.BASQ
filter(qecb.ivr, Region == "EGMP.BASQ") -> Loc.
filter(qecb.ivr, Site == "EGMP_BreeBains") -> Loc.
unique(Loc.$Site.Year.Month.Day)
# activate required row
filter(Loc., Site.Year.Month.Day == "EGMP_BreeBains.2016.04.06" &  Face == "face supérieure") -> Loc.
#filter(Loc., Site.Year.Month.Day == "GDMO_Locmariaquer.2016.03.09" &  Type.Bloc == "Bloc mobile") -> Loc.


# NB: survey that made problem for Bretagne blocs face supérieure

filter(qecb.ivr, Region == "Bretagne" & Face == "face supérieure") -> Loc.

unique(Loc.$Site.Year.Month.Day)[36]
unique(Loc.$Site.Year.Month.Day)[38]
unique(Loc.$Site.Year.Month.Day)[39]
unique(Loc.$Site.Year.Month.Day)[41]
unique(Loc.$Site.Year.Month.Day)[48]
unique(Loc.$Site.Year.Month.Day)[49]
unique(Loc.$Site.Year.Month.Day)[50]
unique(Loc.$Site.Year.Month.Day)[53]
unique(Loc.$Site.Year.Month.Day)[74]
unique(Loc.$Site.Year.Month.Day)[79]

# were only issue of <10 blocs mobiles vs blocs fixes paired dissimilarities; so was changed in the formula.

filter(Loc., Site.Year.Month.Day == unique(Loc.$Site.Year.Month.Day)[79]) -> Loc.
Loc.$Type.Bloc <- ifelse(as.character(Loc.$Type.Bloc) == "Roche en place", "Bloc fixé", as.character(Loc.$Type.Bloc))

Loc.$Type.Bloc <- ifelse(as.character(Loc.$Type.Bloc) == "Roche en place", "Bloc fixé", as.character(Loc.$Type.Bloc))
rownames(Loc.) <- paste0(Loc.$Type.Bloc, "_", Loc.$Face,  "_", Loc.$Numéro.Bloc.échantillon, "_", Loc.$Quadrat.bis)

#install.packages("remotes")
#remotes::install_github("phytomosaic/ecole")

{ 
  
#library(vegan)
#mtxdis. <- vegdist(
  #sqrt
  #(Loc.[,c(Bret_EGMP.BASQ_qecb)]), #Transform your species abundance data. Typically, raw abundances are transformed prior to analysis. Usually you will use square root, fourth-root, log(X+1), or presence-absence (square root being least extreme, P/A being most). I would start with square root. (https://stats.stackexchange.com/questions/234495/double-zeroes-problem-with-euclidean-distance-and-abundance-data-is-the-proble)
  #na.rm = T,
  #method = "bray" #Construct species abundance dissimilarity matrices with Bray-Curtis. If your data contains samples that are all-zero you will run into the double zero problem. This can be overcome by using a zero-adjusted Bray-Curtis coefficient, which is sometimes referred to as a 'dummy variable' which damps down the similarity fluctuations between samples that are both zero (undefined). => see below for zero-adjusted Bray-Curtis coefficient ; #another possibility, sqrt + 1 ??
#)

# code below not needed work with or without
#Loc. %>% group_by(Quadrat.bis) %>% summarize(n()) -> boulders  
#Loc. %>% filter(Quadrat.bis %in% unlist(filter(boulders, `n()` == 3)["Quadrat.bis"])) -> Loc.
#rm(boulders)
    
library(ecole) #https://rdrr.io/github/phytomosaic/ecole/
mtxdis. <- bray0(
  sqrt
  (Loc.[,c(Bret_EGMP.BASQ_qecb)])) # to be changed to 'conca' in function.

mtxdis.
mtxdis.
expand.grid(mtxdis.)

mtxdis.df. <- as.data.frame(as.matrix(mtxdis.))

a. <- NA
b. <- NA
c. <- NA
d. <- NA
e. <- NA
f. <- NA
g. <- NA
h. <- NA
i. <- NA
j. <- NA
k. <- NA
l. <- NA
m. <- NA
n. <- NA

for (z in c(1:nrow(mtxdis.df.))) {

  a.[[z]] <- (paste0(rownames(mtxdis.df.[z+1,]), " & ", ifelse(nrow(mtxdis.df.) >= 1, colnames(mtxdis.df.[1]), NA)))
  b.[[z]] <- (paste0(rownames(mtxdis.df.[z+2,]), " & ", ifelse(nrow(mtxdis.df.) >= 2, colnames(mtxdis.df.[2]), NA)))
  c.[[z]] <- (paste0(rownames(mtxdis.df.[z+3,]), " & ", ifelse(nrow(mtxdis.df.) >= 3, colnames(mtxdis.df.[3]), NA)))
  d.[[z]] <- (paste0(rownames(mtxdis.df.[z+4,]), " & ", ifelse(nrow(mtxdis.df.) >= 4, colnames(mtxdis.df.[4]), NA)))
  e.[[z]] <- (paste0(rownames(mtxdis.df.[z+5,]), " & ", ifelse(nrow(mtxdis.df.) >= 5, colnames(mtxdis.df.[5]), NA)))
  f.[[z]] <- (paste0(rownames(mtxdis.df.[z+6,]), " & ", ifelse(nrow(mtxdis.df.) >= 6, colnames(mtxdis.df.[6]), NA)))
  g.[[z]] <- (paste0(rownames(mtxdis.df.[z+7,]), " & ", ifelse(nrow(mtxdis.df.) >= 7, colnames(mtxdis.df.[7]), NA)))
  h.[[z]] <- (paste0(rownames(mtxdis.df.[z+8,]), " & ", ifelse(nrow(mtxdis.df.) >= 8, colnames(mtxdis.df.[8]), NA)))
  i.[[z]] <- (paste0(rownames(mtxdis.df.[z+9,]), " & ", ifelse(nrow(mtxdis.df.) >= 9, colnames(mtxdis.df.[9]), NA)))
  j.[[z]] <- (paste0(rownames(mtxdis.df.[z+10,]), " & ",  ifelse(nrow(mtxdis.df.) >= 10, colnames(mtxdis.df.[10]), NA)))
  k.[[z]] <- (paste0(rownames(mtxdis.df.[z+11,]), " & ",  ifelse(nrow(mtxdis.df.) >= 11, colnames(mtxdis.df.[11]), NA)))
  l.[[z]] <- (paste0(rownames(mtxdis.df.[z+12,]), " & ",  ifelse(nrow(mtxdis.df.) >= 12, colnames(mtxdis.df.[12]), NA)))
  m.[[z]] <- (paste0(rownames(mtxdis.df.[z+13,]), " & ", ifelse(nrow(mtxdis.df.) >= 13, colnames(mtxdis.df.[13]), NA)))
  n.[[z]] <- (paste0(rownames(mtxdis.df.[z+14,]), " & ", ifelse(nrow(mtxdis.df.) >= 14, colnames(mtxdis.df.[14]), NA)))

}

rm(z)

y <- length(a.)-(ifelse(nrow(mtxdis.df.) >= 1, 1, nrow(mtxdis.df.)))
a. <- a.[1:y]
y <- length(b.)-(ifelse(nrow(mtxdis.df.) >= 2, 2, nrow(mtxdis.df.)))
b. <- b.[1:y]
y <- length(c.)-(ifelse(nrow(mtxdis.df.) >= 3, 3, nrow(mtxdis.df.)))
c. <- c.[1:y]
y <- length(d.)-(ifelse(nrow(mtxdis.df.) >= 4, 4, nrow(mtxdis.df.)))
d. <- d.[1:y]
y <- length(e.)-(ifelse(nrow(mtxdis.df.) >= 5, 5, nrow(mtxdis.df.)))
e. <- e.[1:y]
y <- length(f.)-(ifelse(nrow(mtxdis.df.) >= 6, 6, nrow(mtxdis.df.)))
f. <- f.[1:y]
y <- length(g.)-(ifelse(nrow(mtxdis.df.) >= 7, 7, nrow(mtxdis.df.)))
g. <- g.[1:y]
y <- length(h.)-(ifelse(nrow(mtxdis.df.) >= 8, 8, nrow(mtxdis.df.)))
h. <- h.[1:y]
y <- length(i.)-(ifelse(nrow(mtxdis.df.) >= 9, 9, nrow(mtxdis.df.)))
i. <- i.[1:y]
y <- length(j.)-(ifelse(nrow(mtxdis.df.) >= 10, 10, nrow(mtxdis.df.)))
j. <- j.[1:y]
y <- length(k.)-(ifelse(nrow(mtxdis.df.) >= 11, 11, nrow(mtxdis.df.)))
k. <- k.[1:y]
y <- length(l.)-(ifelse(nrow(mtxdis.df.) >= 12, 12, nrow(mtxdis.df.)))
l. <- l.[1:y]
y <- length(m.)-(ifelse(nrow(mtxdis.df.) >= 13, 13, nrow(mtxdis.df.)))
m. <- m.[1:y]
y <- length(n.)-(ifelse(nrow(mtxdis.df.) >= 14, 14, nrow(mtxdis.df.)))
n. <- n.[1:y]

rm(y)

name. <- c(a.,b.,c.,d.,e.,f.,g.,h.,i.,j.,k.,l.,m.,n.)
df. <- data.frame(expand.grid(mtxdis.), name.[1:nrow(expand.grid(mtxdis.))])
names(df.) <- c("dist.", "name.")

rm(a.,b.,c.,d.,e.,f.,g.,h.,i.,j.,k.,l.,m.,n.)
rm(name.)

Q. <- strsplit(df.$name., " & ")
mat.  <- matrix(unlist(Q.), ncol = 2, byrow = TRUE)
Q.df.   <- as.data.frame(matrix(unlist(Q.), ncol = 2, byrow = TRUE))
df. <- bind_cols(df., Q.df.)

rm(Q., mat., Q.df.)

split. <- strsplit(df.$V1, "_")
V1.split. <- as.data.frame(matrix(unlist(split.), ncol = 4, byrow = TRUE))
split. <- strsplit(df.$V2, "_")
V2.split. <- as.data.frame(matrix(unlist(split.), ncol = 4, byrow = TRUE))

df. <- bind_cols(df., V1.split.)
df. <- bind_cols(df., V2.split.)
df.red. <- subset(df., V4...8 == V4...12 & V1...5 != V1...9)
Site.Year.Month.Day <- rep(unique(Loc.$Site.Year.Month.Day), nrow(df.red.))
library(tibble)
df.red. <- add_column(df.red., Site.Year.Month.Day, .before = "dist.")

rm(split., V1.split., V2.split.)
rm(Loc., mtxdis., mtxdis.df., df., Site.Year.Month.Day)
rm(df.red.)

}


# loop in a fct

matri.fct.BMF <- function(data, conca) {
  
  matri.df. <- data
  
  for (x in c(1:length(unique(matri.df.$Site.Year.Month.Day)))) {
    
    matri.df. %>% filter(Site.Year.Month.Day == unique(matri.df.$Site.Year.Month.Day)[[x]]) -> qecb.ivr.x
    
    rownames(qecb.ivr.x) <- paste0(qecb.ivr.x$Type.Bloc, "_", qecb.ivr.x$Face,  "_", qecb.ivr.x$Numéro.Bloc.échantillon, "_", qecb.ivr.x$Quadrat.bis)
  
  #library(vegan)
  #mtxdis. <- vegdist(
  #  sqrt
  #  (qecb.ivr.x[,conca]), #Transform your species abundance data. Typically, raw abundances are transformed prior to analysis. Usually you will use square root, fourth-root, log(X+1), or presence-absence (square root being least extreme, P/A being most). I would start with square root. (https://stats.stackexchange.com/questions/234495/double-zeroes-problem-with-euclidean-distance-and-abundance-data-is-the-proble)
  #  na.rm = T,
  #  method = "bray" #Construct species abundance dissimilarity matrices with Bray-Curtis. If your data contains samples that are all-zero you will run into the double zero problem. This can be overcome by using a zero-adjusted Bray-Curtis coefficient, which is sometimes referred to as a 'dummy variable' which damps down the similarity fluctuations between samples that are both zero (undefined). => see below for zero-adjusted Bray-Curtis coefficient ; #another possibility, sqrt + 1 ??
  #)
  
  library(ecole) #https://rdrr.io/github/phytomosaic/ecole/man/bray0.html
  mtxdis. <- bray0(
    sqrt
    (qecb.ivr.x[,conca]), na.rm = T)
  
  mtxdis.
  expand.grid(mtxdis.)
  
  mtxdis.df. <- as.data.frame(as.matrix(mtxdis.))
  
  a. <- NA
  b. <- NA
  c. <- NA
  d. <- NA
  e. <- NA
  f. <- NA
  g. <- NA
  h. <- NA
  i. <- NA
  j. <- NA
  k. <- NA
  l. <- NA
  m. <- NA
  n. <- NA
  
  for (z in c(1:nrow(mtxdis.df.))) {
    
    a.[[z]] <- (paste0(rownames(mtxdis.df.[z+1,]), " & ", ifelse(nrow(mtxdis.df.) >= 1, colnames(mtxdis.df.[1]), NA)))
    b.[[z]] <- (paste0(rownames(mtxdis.df.[z+2,]), " & ", ifelse(nrow(mtxdis.df.) >= 2, colnames(mtxdis.df.[2]), NA)))
    c.[[z]] <- (paste0(rownames(mtxdis.df.[z+3,]), " & ", ifelse(nrow(mtxdis.df.) >= 3, colnames(mtxdis.df.[3]), NA)))
    d.[[z]] <- (paste0(rownames(mtxdis.df.[z+4,]), " & ", ifelse(nrow(mtxdis.df.) >= 4, colnames(mtxdis.df.[4]), NA)))
    e.[[z]] <- (paste0(rownames(mtxdis.df.[z+5,]), " & ", ifelse(nrow(mtxdis.df.) >= 5, colnames(mtxdis.df.[5]), NA)))
    f.[[z]] <- (paste0(rownames(mtxdis.df.[z+6,]), " & ", ifelse(nrow(mtxdis.df.) >= 6, colnames(mtxdis.df.[6]), NA)))
    g.[[z]] <- (paste0(rownames(mtxdis.df.[z+7,]), " & ", ifelse(nrow(mtxdis.df.) >= 7, colnames(mtxdis.df.[7]), NA)))
    h.[[z]] <- (paste0(rownames(mtxdis.df.[z+8,]), " & ", ifelse(nrow(mtxdis.df.) >= 8, colnames(mtxdis.df.[8]), NA)))
    i.[[z]] <- (paste0(rownames(mtxdis.df.[z+9,]), " & ", ifelse(nrow(mtxdis.df.) >= 9, colnames(mtxdis.df.[9]), NA)))
    j.[[z]] <- (paste0(rownames(mtxdis.df.[z+10,]), " & ",  ifelse(nrow(mtxdis.df.) >= 10, colnames(mtxdis.df.[10]), NA)))
    k.[[z]] <- (paste0(rownames(mtxdis.df.[z+11,]), " & ",  ifelse(nrow(mtxdis.df.) >= 11, colnames(mtxdis.df.[11]), NA)))
    l.[[z]] <- (paste0(rownames(mtxdis.df.[z+12,]), " & ",  ifelse(nrow(mtxdis.df.) >= 12, colnames(mtxdis.df.[12]), NA)))
    m.[[z]] <- (paste0(rownames(mtxdis.df.[z+13,]), " & ", ifelse(nrow(mtxdis.df.) >= 13, colnames(mtxdis.df.[13]), NA)))
    n.[[z]] <- (paste0(rownames(mtxdis.df.[z+14,]), " & ", ifelse(nrow(mtxdis.df.) >= 14, colnames(mtxdis.df.[14]), NA)))
    
  }
  
  rm(z)
  
  y <- length(a.)-(ifelse(nrow(mtxdis.df.) >= 1, 1, nrow(mtxdis.df.)))
  a. <- a.[1:y]
  y <- length(b.)-(ifelse(nrow(mtxdis.df.) >= 2, 2, nrow(mtxdis.df.)))
  b. <- b.[1:y]
  y <- length(c.)-(ifelse(nrow(mtxdis.df.) >= 3, 3, nrow(mtxdis.df.)))
  c. <- c.[1:y]
  y <- length(d.)-(ifelse(nrow(mtxdis.df.) >= 4, 4, nrow(mtxdis.df.)))
  d. <- d.[1:y]
  y <- length(e.)-(ifelse(nrow(mtxdis.df.) >= 5, 5, nrow(mtxdis.df.)))
  e. <- e.[1:y]
  y <- length(f.)-(ifelse(nrow(mtxdis.df.) >= 6, 6, nrow(mtxdis.df.)))
  f. <- f.[1:y]
  y <- length(g.)-(ifelse(nrow(mtxdis.df.) >= 7, 7, nrow(mtxdis.df.)))
  g. <- g.[1:y]
  y <- length(h.)-(ifelse(nrow(mtxdis.df.) >= 8, 8, nrow(mtxdis.df.)))
  h. <- h.[1:y]
  y <- length(i.)-(ifelse(nrow(mtxdis.df.) >= 9, 9, nrow(mtxdis.df.)))
  i. <- i.[1:y]
  y <- length(j.)-(ifelse(nrow(mtxdis.df.) >= 10, 10, nrow(mtxdis.df.)))
  j. <- j.[1:y]
  y <- length(k.)-(ifelse(nrow(mtxdis.df.) >= 11, 11, nrow(mtxdis.df.)))
  k. <- k.[1:y]
  y <- length(l.)-(ifelse(nrow(mtxdis.df.) >= 12, 12, nrow(mtxdis.df.)))
  l. <- l.[1:y]
  y <- length(m.)-(ifelse(nrow(mtxdis.df.) >= 13, 13, nrow(mtxdis.df.)))
  m. <- m.[1:y]
  y <- length(n.)-(ifelse(nrow(mtxdis.df.) >= 14, 14, nrow(mtxdis.df.)))
  n. <- n.[1:y]
  
  rm(y)
  
  name. <- c(a.,b.,c.,d.,e.,f.,g.,h.,i.,j.,k.,l.,m.,n.)
  df. <- data.frame(expand.grid(mtxdis.), name.[1:nrow(expand.grid(mtxdis.))])
  names(df.) <- c("dist.", "name.")
  
  rm(a.,b.,c.,d.,e.,f.,g.,h.,i.,j.,k.,l.,m.,n.)
  rm(name.)
  
  Q. <- strsplit(df.$name., " & ")
  mat.  <- matrix(unlist(Q.), ncol = 2, byrow = TRUE)
  Q.df.   <- as.data.frame(matrix(unlist(Q.), ncol = 2, byrow = TRUE))
  df. <- bind_cols(df., Q.df.)
  
  rm(Q., mat., Q.df.)
  
  split. <- strsplit(df.$V1, "_")
  V1.split. <- as.data.frame(matrix(unlist(split.), ncol = 4, byrow = TRUE))
  split. <- strsplit(df.$V2, "_")
  V2.split. <- as.data.frame(matrix(unlist(split.), ncol = 4, byrow = TRUE))
  
  df. <- bind_cols(df., V1.split.)
  df. <- bind_cols(df., V2.split.)
  df.red. <- subset(df., V4...8 == V4...12 & V1...5 != V1...9)
  Site.Year.Month.Day <- rep(unique(qecb.ivr.x$Site.Year.Month.Day), nrow(df.red.))
  library(tibble)
  df.red. <- add_column(df.red., Site.Year.Month.Day, .before = "dist.")
  
  rm(split., V1.split., V2.split.)
  rm(mtxdis., mtxdis.df., df., Site.Year.Month.Day)
  #rm(df.red.)
  
  matri.list[[x]] <- df.red. 
  matri.list <<- matri.list
  
  rm(df.red., qecb.ivr.x, x)
  
  }
  
  matri.df. <- do.call("rbind", matri.list)
  
  names(matri.df.) <- c("Site.Year.Month.Day", "dist.", "name.", "name.left", "name.right", "Type.Bloc.left", "Face.left", "Numéro.Bloc.échantillon.left", "Quadrat.left", "Type.Bloc.right", "Face.right", "Numéro.Bloc.échantillon.right", "Quadrat.right")
  
  matri.df. <<- matri.df.
  
  hist(matri.df.$dist.)

}


# Full dataset

filter(qecb.ivr, Face == "face supérieure") -> data.
data.$Type.Bloc <- ifelse(as.character(data.$Type.Bloc) == "Roche en place", "Bloc fixé", as.character(data.$Type.Bloc))
matri.list <- vector("list", length(unique(data.$Site.Year.Month.Day)))
#matri.fct.BMF(data = data., conca = c(Bret_EGMP.BASQ_qecb))
matri.fct.BMF(data = data., conca = c(Bret_EGMP.BASQ_qecb, EGMP.BASQ_qecb))
hist(matri.df.$dist., main = c(paste("Histo. of Bray (0-adjusted) dist. dissimilarity measures"), paste(" (sqrt transfo) - BMfs vs BF -")))

matri.full.BM.BF_FS <- matri.df.
#saveRDS(matri.full.BM.BF_FS, "results/Ecology/matri.full.BM.BF_FS.RDS")
saveRDS(matri.full.BM.BF_FS, "results/Ecology/matri.full_log.spi_BM.BF_FS.RDS")
rm(data., matri.df., matri.list)

library(stringr)
ivr$Quadrat.left <- str_sub(ivr$Site.Year.Month.Day.QdNb, -1)
ivr$Quadrat.left <- paste0("Q", ivr$Quadrat.left)

fct.ivr.dist. <- function(df., title.) {
  # test
  #df. <- matri.full.BM_FS.FI
  #title. <- "dissimilarité BM.FI vs BM.FI Atlantique"
  df.ivr <- left_join(df., ivr, c("Site.Year.Month.Day", "Quadrat.left"))
  library(ggplot2)
  show(ggplot(df.ivr, aes(blocs.retournes.fr., dist.)) + geom_point() + ggtitle(title.) +
         geom_smooth(method = "gam", formula = y ~ poly(x, 2)) )
  show(ggplot(df.ivr, aes(blocs.retournes.fr., dist.)) + geom_point() + ggtitle(title.) +
         geom_smooth(method = "loess", span = 0.3, se = FALSE) )
  show(ggplot(df.ivr, aes(blocs.retournes.fr., dist.)) + geom_point() + ggtitle(title.) +
         geom_smooth(method = "lm", formula = y ~ x) )
}

fct.ivr.dist.(df. = matri.full.BM.BF_FS, title. = "dissimilarité BM.FS vs BF Atlantique")

# Bretagne

filter(qecb.ivr, Region == "Bretagne" & Face == "face supérieure") -> data.
data.$Type.Bloc <- ifelse(as.character(data.$Type.Bloc) == "Roche en place", "Bloc fixé", as.character(data.$Type.Bloc))
matri.list <- vector("list", length(unique(data.$Site.Year.Month.Day)))
matri.fct.BMF(data = data., conca = c(Bret_EGMP.BASQ_qecb))
hist(matri.df.$dist., main = c(paste("Histo. of Bray (0-adjusted) dist. dissimilarity measures"), paste(" (sqrt transfo) - Bretagne BMfs vs BF -")))

matri.Bretagne.BM.BF_FS <- matri.df.
#saveRDS(matri.Bretagne.BM.BF_FS, "results/Ecology/matri.Bretagne.BM.BF_FS.RDS")
saveRDS(matri.Bretagne.BM.BF_FS, "results/Ecology/matri.Bretagne_log.spi_BM.BF_FS.RDS")
rm(data., matri.df., matri.list)

fct.ivr.dist.(df. = matri.Bretagne.BM.BF_FS, title. = "dissimilarité BM.FS vs BF Bretagne")

# EGMP.BASQ

filter(qecb.ivr, Region == "EGMP.BASQ" & Face == "face supérieure") -> data.
data.$Type.Bloc <- ifelse(as.character(data.$Type.Bloc) == "Roche en place", "Bloc fixé", as.character(data.$Type.Bloc))
matri.list <- vector("list", length(unique(data.$Site.Year.Month.Day)))
matri.fct.BMF(data = data., conca = c(Bret_EGMP.BASQ_qecb, EGMP.BASQ_qecb))
hist(matri.df.$dist., main = c(paste("Histo. of Bray (0-adjusted) dist. dissimilarity measures"), paste(" (sqrt transfo) - EGMP & BASQ BMfs vs BF -")))

matri.EGMP.BASQ.BM.BF_FS <- matri.df.
#saveRDS(matri.EGMP.BASQ.BM.BF_FS, "results/Ecology/matri.EGMP.BASQ.BM.BF_FS.RDS")
saveRDS(matri.EGMP.BASQ.BM.BF_FS, "results/Ecology/matri.EGMP.BASQ_log.spi_BM.BF_FS.RDS")
rm(data., matri.df., matri.list)

fct.ivr.dist.(df. = matri.EGMP.BASQ.BM.BF_FS, title. = "dissimilarité BM.FS vs BF EGMP-BASQ")


# don't run anymore below code for now on; we'll need something similar later on when calculating distance by removing var by var
###########################################################
# recalculate dissimilarity distances, after removal of "modeling" vectors of variables.
# this section should be run after executing the "CB_ecology" R script.

(Atlantique.rem. <- readRDS("results/Ecology/Atlantique.rem.RDS"))
(Bretagne.rem. <- readRDS("results/Ecology/Bretagne.rem.RDS"))
(EGMP.BASQ.rem. <- readRDS("results/Ecology/EGMP.BASQ.rem.RDS"))

`%notin%` <- Negate(`%in%`)

# Full dataset

qecb.ivr[, which(names(qecb.ivr) %notin% Atlantique.rem.)] -> data.
filter(data., Face == "face supérieure") -> data.
data.$Type.Bloc <- ifelse(as.character(data.$Type.Bloc) == "Roche en place", "Bloc fixé", as.character(data.$Type.Bloc))
matri.list <- vector("list", length(unique(data.$Site.Year.Month.Day)))
#matri.fct.BMF(data = data., conca = c(Bret_EGMP.BASQ_qecb))
matri.fct.BMF(data = data., conca = c(setdiff(c(Bret_EGMP.BASQ_qecb, EGMP.BASQ_qecb), Atlantique.rem.)))
hist(matri.df.$dist., main = c(paste("Histo. of Bray (0-adjusted) dist. dissimilarity measures"), paste(" (sqrt transfo) - BMfs vs BF -")))

matri.full.rem.BM.BF_FS <- matri.df.
saveRDS(matri.full.rem.BM.BF_FS, "results/Ecology/matri.full.rem.BM.BF_FS.RDS")
rm(data., matri.df., matri.list)

fct.ivr.dist.(df. = matri.full.rem.BM.BF_FS, title. = "dissimilarité BM.FS vs BF Atlantique")

# Bretagne

qecb.ivr[, which(names(qecb.ivr) %notin% Bretagne.rem.)] -> data.
filter(data., Region == "Bretagne" & Face == "face supérieure") -> data.
data.$Type.Bloc <- ifelse(as.character(data.$Type.Bloc) == "Roche en place", "Bloc fixé", as.character(data.$Type.Bloc))
matri.list <- vector("list", length(unique(data.$Site.Year.Month.Day)))
matri.fct.BMF(data = data., conca = c(setdiff(Bret_EGMP.BASQ_qecb, Bretagne.rem.)))
hist(matri.df.$dist., main = c(paste("Histo. of Bray (0-adjusted) dist. dissimilarity measures"), paste(" (sqrt transfo) - Bretagne BMfs vs BF -")))

matri.Bretagne.rem.BM.BF_FS <- matri.df.
saveRDS(matri.Bretagne.rem.BM.BF_FS, "results/Ecology/matri.Bretagne.rem.BM.BF_FS.RDS")
rm(data., matri.df., matri.list)

fct.ivr.dist.(df. = matri.Bretagne.rem.BM.BF_FS, title. = "dissimilarité BM.FS vs BF Bretagne")

# EGMP.BASQ

qecb.ivr[, which(names(qecb.ivr) %notin% EGMP.BASQ.rem.)] -> data.
filter(data., Region == "EGMP.BASQ" & Face == "face supérieure") -> data.
data.$Type.Bloc <- ifelse(as.character(data.$Type.Bloc) == "Roche en place", "Bloc fixé", as.character(data.$Type.Bloc))
matri.list <- vector("list", length(unique(data.$Site.Year.Month.Day)))
matri.fct.BMF(data = data., conca = c(setdiff(c(Bret_EGMP.BASQ_qecb, EGMP.BASQ_qecb), EGMP.BASQ.rem.)))
hist(matri.df.$dist., main = c(paste("Histo. of Bray (0-adjusted) dist. dissimilarity measures"), paste(" (sqrt transfo) - EGMP & BASQ BMfs vs BF -")))

matri.EGMP.BASQ.rem.BM.BF_FS <- matri.df.
saveRDS(matri.EGMP.BASQ.rem.BM.BF_FS, "results/Ecology/matri.EGMP.BASQ.rem.BM.BF_FS.RDS")
rm(data., matri.df., matri.list)

fct.ivr.dist.(df. = matri.EGMP.BASQ.rem.BM.BF_FS, title. = "dissimilarité BM.FS vs BF EGMP.BASQ")
###########################################################


## blocs mobiles face sup vs face inf.

filter(qecb.ivr, Region == "Bretagne" & Type.Bloc == "Bloc mobile") -> Loc.
filter(Loc., Site.Year.Month.Day == unique(Loc.$Site.Year.Month.Day)[1]) -> Loc.

rownames(Loc.) <- paste0(Loc.$Type.Bloc, "_", Loc.$Face,  "_", Loc.$Numéro.Bloc.échantillon, "_", Loc.$Quadrat.bis)

{ 
  
  #library(vegan)
  #mtxdis. <- vegdist(
  #  sqrt
  #  (Loc.[,c(Bret_EGMP.BASQ_qecb)]), #Transform your species abundance data. Typically, raw abundances are transformed prior to analysis. Usually you will use square root, fourth-root, log(X+1), or presence-absence (square root being least extreme, P/A being most). I would start with square root. (https://stats.stackexchange.com/questions/234495/double-zeroes-problem-with-euclidean-distance-and-abundance-data-is-the-proble)
  #  na.rm = T,
  #  method = "bray" #Construct species abundance dissimilarity matrices with Bray-Curtis. If your data contains samples that are all-zero you will run into the double zero problem. This can be overcome by using a zero-adjusted Bray-Curtis coefficient, which is sometimes referred to as a 'dummy variable' which damps down the similarity fluctuations between samples that are both zero (undefined). => see below for zero-adjusted Bray-Curtis coefficient ; #another possibility, sqrt + 1 ??
  #)
  
  library(ecole)
  mtxdis. <- bray0(
    sqrt
    (Loc.[,c(Bret_EGMP.BASQ_qecb)])) # to be changed to 'conca' in function
  
  mtxdis.
  mtxdis.
  expand.grid(mtxdis.)
  
  mtxdis.df. <- as.data.frame(as.matrix(mtxdis.))
  
  a. <- NA
  b. <- NA
  c. <- NA
  d. <- NA
  e. <- NA
  f. <- NA
  g. <- NA
  h. <- NA
  i. <- NA
  j. <- NA
  k. <- NA
  l. <- NA
  m. <- NA
  n. <- NA
  o. <- NA
  p. <- NA
  q. <- NA
  r. <- NA
  s. <- NA
  
  for (z in c(1:nrow(mtxdis.df.))) {
    
    a.[[z]] <- (paste0(rownames(mtxdis.df.[z+1,]), " & ", ifelse(nrow(mtxdis.df.) >= 1, colnames(mtxdis.df.[1]), NA)))
    b.[[z]] <- (paste0(rownames(mtxdis.df.[z+2,]), " & ", ifelse(nrow(mtxdis.df.) >= 2, colnames(mtxdis.df.[2]), NA)))
    c.[[z]] <- (paste0(rownames(mtxdis.df.[z+3,]), " & ", ifelse(nrow(mtxdis.df.) >= 3, colnames(mtxdis.df.[3]), NA)))
    d.[[z]] <- (paste0(rownames(mtxdis.df.[z+4,]), " & ", ifelse(nrow(mtxdis.df.) >= 4, colnames(mtxdis.df.[4]), NA)))
    e.[[z]] <- (paste0(rownames(mtxdis.df.[z+5,]), " & ", ifelse(nrow(mtxdis.df.) >= 5, colnames(mtxdis.df.[5]), NA)))
    f.[[z]] <- (paste0(rownames(mtxdis.df.[z+6,]), " & ", ifelse(nrow(mtxdis.df.) >= 6, colnames(mtxdis.df.[6]), NA)))
    g.[[z]] <- (paste0(rownames(mtxdis.df.[z+7,]), " & ", ifelse(nrow(mtxdis.df.) >= 7, colnames(mtxdis.df.[7]), NA)))
    h.[[z]] <- (paste0(rownames(mtxdis.df.[z+8,]), " & ", ifelse(nrow(mtxdis.df.) >= 8, colnames(mtxdis.df.[8]), NA)))
    i.[[z]] <- (paste0(rownames(mtxdis.df.[z+9,]), " & ", ifelse(nrow(mtxdis.df.) >= 9, colnames(mtxdis.df.[9]), NA)))
    j.[[z]] <- (paste0(rownames(mtxdis.df.[z+10,]), " & ",  ifelse(nrow(mtxdis.df.) >= 10, colnames(mtxdis.df.[10]), NA)))
    k.[[z]] <- (paste0(rownames(mtxdis.df.[z+11,]), " & ",  ifelse(nrow(mtxdis.df.) >= 11, colnames(mtxdis.df.[11]), NA)))
    l.[[z]] <- (paste0(rownames(mtxdis.df.[z+12,]), " & ",  ifelse(nrow(mtxdis.df.) >= 12, colnames(mtxdis.df.[12]), NA)))
    m.[[z]] <- (paste0(rownames(mtxdis.df.[z+13,]), " & ", ifelse(nrow(mtxdis.df.) >= 13, colnames(mtxdis.df.[13]), NA)))
    n.[[z]] <- (paste0(rownames(mtxdis.df.[z+14,]), " & ", ifelse(nrow(mtxdis.df.) >= 14, colnames(mtxdis.df.[14]), NA)))
    o.[[z]] <- (paste0(rownames(mtxdis.df.[z+15,]), " & ", ifelse(nrow(mtxdis.df.) >= 15, colnames(mtxdis.df.[15]), NA)))
    p.[[z]] <- (paste0(rownames(mtxdis.df.[z+16,]), " & ", ifelse(nrow(mtxdis.df.) >= 16, colnames(mtxdis.df.[16]), NA)))
    q.[[z]] <- (paste0(rownames(mtxdis.df.[z+17,]), " & ", ifelse(nrow(mtxdis.df.) >= 17, colnames(mtxdis.df.[17]), NA)))
    r.[[z]] <- (paste0(rownames(mtxdis.df.[z+18,]), " & ", ifelse(nrow(mtxdis.df.) >= 18, colnames(mtxdis.df.[18]), NA)))
    s.[[z]] <- (paste0(rownames(mtxdis.df.[z+19,]), " & ", ifelse(nrow(mtxdis.df.) >= 19, colnames(mtxdis.df.[19]), NA)))
    
  }
  
  rm(z)
  
  y <- length(a.)-(ifelse(nrow(mtxdis.df.) >= 1, 1, nrow(mtxdis.df.)))
  a. <- a.[1:y]
  y <- length(b.)-(ifelse(nrow(mtxdis.df.) >= 2, 2, nrow(mtxdis.df.)))
  b. <- b.[1:y]
  y <- length(c.)-(ifelse(nrow(mtxdis.df.) >= 3, 3, nrow(mtxdis.df.)))
  c. <- c.[1:y]
  y <- length(d.)-(ifelse(nrow(mtxdis.df.) >= 4, 4, nrow(mtxdis.df.)))
  d. <- d.[1:y]
  y <- length(e.)-(ifelse(nrow(mtxdis.df.) >= 5, 5, nrow(mtxdis.df.)))
  e. <- e.[1:y]
  y <- length(f.)-(ifelse(nrow(mtxdis.df.) >= 6, 6, nrow(mtxdis.df.)))
  f. <- f.[1:y]
  y <- length(g.)-(ifelse(nrow(mtxdis.df.) >= 7, 7, nrow(mtxdis.df.)))
  g. <- g.[1:y]
  y <- length(h.)-(ifelse(nrow(mtxdis.df.) >= 8, 8, nrow(mtxdis.df.)))
  h. <- h.[1:y]
  y <- length(i.)-(ifelse(nrow(mtxdis.df.) >= 9, 9, nrow(mtxdis.df.)))
  i. <- i.[1:y]
  y <- length(j.)-(ifelse(nrow(mtxdis.df.) >= 10, 10, nrow(mtxdis.df.)))
  j. <- j.[1:y]
  y <- length(k.)-(ifelse(nrow(mtxdis.df.) >= 11, 11, nrow(mtxdis.df.)))
  k. <- k.[1:y]
  y <- length(l.)-(ifelse(nrow(mtxdis.df.) >= 12, 12, nrow(mtxdis.df.)))
  l. <- l.[1:y]
  y <- length(m.)-(ifelse(nrow(mtxdis.df.) >= 13, 13, nrow(mtxdis.df.)))
  m. <- m.[1:y]
  y <- length(n.)-(ifelse(nrow(mtxdis.df.) >= 14, 14, nrow(mtxdis.df.)))
  n. <- n.[1:y]
  y <- length(o.)-(ifelse(nrow(mtxdis.df.) >= 15, 15, nrow(mtxdis.df.)))
  o. <- o.[1:y]
  y <- length(p.)-(ifelse(nrow(mtxdis.df.) >= 16, 16, nrow(mtxdis.df.)))
  p. <- p.[1:y]
  y <- length(q.)-(ifelse(nrow(mtxdis.df.) >= 17, 17, nrow(mtxdis.df.)))
  q. <- q.[1:y]
  y <- length(r.)-(ifelse(nrow(mtxdis.df.) >= 18, 18, nrow(mtxdis.df.)))
  r. <- r.[1:y]
  y <- length(s.)-(ifelse(nrow(mtxdis.df.) >= 19, 19, nrow(mtxdis.df.)))
  s. <- s.[1:y]
  
  rm(y)
  
  name. <- c(a.,b.,c.,d.,e.,f.,g.,h.,i.,j.,k.,l.,m.,n.,o.,p.,q.,r.,s.)
  df. <- data.frame(expand.grid(mtxdis.), name.[1:nrow(expand.grid(mtxdis.))])
  names(df.) <- c("dist.", "name.")
  
  rm(a.,b.,c.,d.,e.,f.,g.,h.,i.,j.,k.,l.,m.,n.,o.,p.,q.,r.,s.)
  rm(name.)
  
  Q. <- strsplit(df.$name., " & ")
  mat.  <- matrix(unlist(Q.), ncol = 2, byrow = TRUE)
  Q.df.   <- as.data.frame(matrix(unlist(Q.), ncol = 2, byrow = TRUE))
  df. <- bind_cols(df., Q.df.)
  
  rm(Q., mat., Q.df.)
  
  split. <- strsplit(df.$V1, "_")
  V1.split. <- as.data.frame(matrix(unlist(split.), ncol = 4, byrow = TRUE))
  split. <- strsplit(df.$V2, "_")
  V2.split. <- as.data.frame(matrix(unlist(split.), ncol = 4, byrow = TRUE))
  
  df. <- bind_cols(df., V1.split.)
  df. <- bind_cols(df., V2.split.)
  df.red. <- subset(df., V4...8 == V4...12 & V3...7 == V3...11)
  
  rm(split., V1.split., V2.split.)
  rm(Loc., mtxdis., mtxdis.df., df.)
  rm(df.red.)
  
}


# loop in a fct

matri.fct.BMM <- function(data, conca) {
  
  matri.df. <- data

  for (x in c(1:length(unique(matri.df.$Site.Year.Month.Day)))) {
    
    matri.df. %>% filter(Site.Year.Month.Day == unique(matri.df.$Site.Year.Month.Day)[[x]]) -> qecb.ivr.x
    
    rownames(qecb.ivr.x) <- paste0(qecb.ivr.x$Type.Bloc, "_", qecb.ivr.x$Face,  "_", qecb.ivr.x$Numéro.Bloc.échantillon, "_", qecb.ivr.x$Quadrat.bis)
  
  #library(vegan)
  #mtxdis. <- vegdist(
  #  sqrt
  #  (qecb.ivr.x[,c(Bret_EGMP.BASQ_qecb)]), #Transform your species abundance data. Typically, raw abundances are transformed prior to analysis. Usually you will use square root, fourth-root, log(X+1), or presence-absence (square root being least extreme, P/A being most). I would start with square root. (https://stats.stackexchange.com/questions/234495/double-zeroes-problem-with-euclidean-distance-and-abundance-data-is-the-proble)
  #  na.rm = T,
  #  method = "bray" #Construct species abundance dissimilarity matrices with Bray-Curtis. If your data contains samples that are all-zero you will run into the double zero problem. This can be overcome by using a zero-adjusted Bray-Curtis coefficient, which is sometimes referred to as a 'dummy variable' which damps down the similarity fluctuations between samples that are both zero (undefined). => see below for zero-adjusted Bray-Curtis coefficient ; #another possibility, sqrt + 1 ??
  #)
  
  library(ecole)
  mtxdis. <- bray0(
    sqrt
    (qecb.ivr.x[,conca]), na.rm = T)
  
  mtxdis.
  mtxdis.
  expand.grid(mtxdis.)
  
  mtxdis.df. <- as.data.frame(as.matrix(mtxdis.))
  
  a. <- NA
  b. <- NA
  c. <- NA
  d. <- NA
  e. <- NA
  f. <- NA
  g. <- NA
  h. <- NA
  i. <- NA
  j. <- NA
  k. <- NA
  l. <- NA
  m. <- NA
  n. <- NA
  o. <- NA
  p. <- NA
  q. <- NA
  r. <- NA
  s. <- NA
  
  for (z in c(1:nrow(mtxdis.df.))) {
    
    a.[[z]] <- (paste0(rownames(mtxdis.df.[z+1,]), " & ", ifelse(nrow(mtxdis.df.) >= 1, colnames(mtxdis.df.[1]), NA)))
    b.[[z]] <- (paste0(rownames(mtxdis.df.[z+2,]), " & ", ifelse(nrow(mtxdis.df.) >= 2, colnames(mtxdis.df.[2]), NA)))
    c.[[z]] <- (paste0(rownames(mtxdis.df.[z+3,]), " & ", ifelse(nrow(mtxdis.df.) >= 3, colnames(mtxdis.df.[3]), NA)))
    d.[[z]] <- (paste0(rownames(mtxdis.df.[z+4,]), " & ", ifelse(nrow(mtxdis.df.) >= 4, colnames(mtxdis.df.[4]), NA)))
    e.[[z]] <- (paste0(rownames(mtxdis.df.[z+5,]), " & ", ifelse(nrow(mtxdis.df.) >= 5, colnames(mtxdis.df.[5]), NA)))
    f.[[z]] <- (paste0(rownames(mtxdis.df.[z+6,]), " & ", ifelse(nrow(mtxdis.df.) >= 6, colnames(mtxdis.df.[6]), NA)))
    g.[[z]] <- (paste0(rownames(mtxdis.df.[z+7,]), " & ", ifelse(nrow(mtxdis.df.) >= 7, colnames(mtxdis.df.[7]), NA)))
    h.[[z]] <- (paste0(rownames(mtxdis.df.[z+8,]), " & ", ifelse(nrow(mtxdis.df.) >= 8, colnames(mtxdis.df.[8]), NA)))
    i.[[z]] <- (paste0(rownames(mtxdis.df.[z+9,]), " & ", ifelse(nrow(mtxdis.df.) >= 9, colnames(mtxdis.df.[9]), NA)))
    j.[[z]] <- (paste0(rownames(mtxdis.df.[z+10,]), " & ",  ifelse(nrow(mtxdis.df.) >= 10, colnames(mtxdis.df.[10]), NA)))
    k.[[z]] <- (paste0(rownames(mtxdis.df.[z+11,]), " & ",  ifelse(nrow(mtxdis.df.) >= 11, colnames(mtxdis.df.[11]), NA)))
    l.[[z]] <- (paste0(rownames(mtxdis.df.[z+12,]), " & ",  ifelse(nrow(mtxdis.df.) >= 12, colnames(mtxdis.df.[12]), NA)))
    m.[[z]] <- (paste0(rownames(mtxdis.df.[z+13,]), " & ", ifelse(nrow(mtxdis.df.) >= 13, colnames(mtxdis.df.[13]), NA)))
    n.[[z]] <- (paste0(rownames(mtxdis.df.[z+14,]), " & ", ifelse(nrow(mtxdis.df.) >= 14, colnames(mtxdis.df.[14]), NA)))
    o.[[z]] <- (paste0(rownames(mtxdis.df.[z+15,]), " & ", ifelse(nrow(mtxdis.df.) >= 15, colnames(mtxdis.df.[15]), NA)))
    p.[[z]] <- (paste0(rownames(mtxdis.df.[z+16,]), " & ", ifelse(nrow(mtxdis.df.) >= 16, colnames(mtxdis.df.[16]), NA)))
    q.[[z]] <- (paste0(rownames(mtxdis.df.[z+17,]), " & ", ifelse(nrow(mtxdis.df.) >= 17, colnames(mtxdis.df.[17]), NA)))
    r.[[z]] <- (paste0(rownames(mtxdis.df.[z+18,]), " & ", ifelse(nrow(mtxdis.df.) >= 18, colnames(mtxdis.df.[18]), NA)))
    s.[[z]] <- (paste0(rownames(mtxdis.df.[z+19,]), " & ", ifelse(nrow(mtxdis.df.) >= 19, colnames(mtxdis.df.[19]), NA)))
    
  }
  
  rm(z)
  
  y <- length(a.)-(ifelse(nrow(mtxdis.df.) >= 1, 1, nrow(mtxdis.df.)))
  a. <- a.[1:y]
  y <- length(b.)-(ifelse(nrow(mtxdis.df.) >= 2, 2, nrow(mtxdis.df.)))
  b. <- b.[1:y]
  y <- length(c.)-(ifelse(nrow(mtxdis.df.) >= 3, 3, nrow(mtxdis.df.)))
  c. <- c.[1:y]
  y <- length(d.)-(ifelse(nrow(mtxdis.df.) >= 4, 4, nrow(mtxdis.df.)))
  d. <- d.[1:y]
  y <- length(e.)-(ifelse(nrow(mtxdis.df.) >= 5, 5, nrow(mtxdis.df.)))
  e. <- e.[1:y]
  y <- length(f.)-(ifelse(nrow(mtxdis.df.) >= 6, 6, nrow(mtxdis.df.)))
  f. <- f.[1:y]
  y <- length(g.)-(ifelse(nrow(mtxdis.df.) >= 7, 7, nrow(mtxdis.df.)))
  g. <- g.[1:y]
  y <- length(h.)-(ifelse(nrow(mtxdis.df.) >= 8, 8, nrow(mtxdis.df.)))
  h. <- h.[1:y]
  y <- length(i.)-(ifelse(nrow(mtxdis.df.) >= 9, 9, nrow(mtxdis.df.)))
  i. <- i.[1:y]
  y <- length(j.)-(ifelse(nrow(mtxdis.df.) >= 10, 10, nrow(mtxdis.df.)))
  j. <- j.[1:y]
  y <- length(k.)-(ifelse(nrow(mtxdis.df.) >= 11, 11, nrow(mtxdis.df.)))
  k. <- k.[1:y]
  y <- length(l.)-(ifelse(nrow(mtxdis.df.) >= 12, 12, nrow(mtxdis.df.)))
  l. <- l.[1:y]
  y <- length(m.)-(ifelse(nrow(mtxdis.df.) >= 13, 13, nrow(mtxdis.df.)))
  m. <- m.[1:y]
  y <- length(n.)-(ifelse(nrow(mtxdis.df.) >= 14, 14, nrow(mtxdis.df.)))
  n. <- n.[1:y]
  y <- length(o.)-(ifelse(nrow(mtxdis.df.) >= 15, 15, nrow(mtxdis.df.)))
  o. <- o.[1:y]
  y <- length(p.)-(ifelse(nrow(mtxdis.df.) >= 16, 16, nrow(mtxdis.df.)))
  p. <- p.[1:y]
  y <- length(q.)-(ifelse(nrow(mtxdis.df.) >= 17, 17, nrow(mtxdis.df.)))
  q. <- q.[1:y]
  y <- length(r.)-(ifelse(nrow(mtxdis.df.) >= 18, 18, nrow(mtxdis.df.)))
  r. <- r.[1:y]
  y <- length(s.)-(ifelse(nrow(mtxdis.df.) >= 19, 19, nrow(mtxdis.df.)))
  s. <- s.[1:y]
  
  rm(y)
  
  name. <- c(a.,b.,c.,d.,e.,f.,g.,h.,i.,j.,k.,l.,m.,n.,o.,p.,q.,r.,s.)
  df. <- data.frame(expand.grid(mtxdis.), name.[1:nrow(expand.grid(mtxdis.))])
  names(df.) <- c("dist.", "name.")
  
  rm(a.,b.,c.,d.,e.,f.,g.,h.,i.,j.,k.,l.,m.,n.,o.,p.,q.,r.,s.)
  rm(name.)
  
  Q. <- strsplit(df.$name., " & ")
  mat.  <- matrix(unlist(Q.), ncol = 2, byrow = TRUE)
  Q.df.   <- as.data.frame(matrix(unlist(Q.), ncol = 2, byrow = TRUE))
  df. <- bind_cols(df., Q.df.)
  
  rm(Q., mat., Q.df.)
  
  split. <- strsplit(df.$V1, "_")
  V1.split. <- as.data.frame(matrix(unlist(split.), ncol = 4, byrow = TRUE))
  split. <- strsplit(df.$V2, "_")
  V2.split. <- as.data.frame(matrix(unlist(split.), ncol = 4, byrow = TRUE))
  
  df. <- bind_cols(df., V1.split.)
  df. <- bind_cols(df., V2.split.)
  df.red. <- subset(df., V4...8 == V4...12 & V3...7 == V3...11)
  Site.Year.Month.Day <- rep(unique(qecb.ivr.x$Site.Year.Month.Day), nrow(df.red.))
  library(tibble)
  df.red. <- add_column(df.red., Site.Year.Month.Day, .before = "dist.")
  
  rm(split., V1.split., V2.split.)
  rm(mtxdis., mtxdis.df., df., Site.Year.Month.Day)
  #rm(df.red.)
  
  matri.list[[x]] <- df.red. 
  matri.list <<- matri.list
  
  rm(df.red., qecb.ivr.x, x)
  
  }
  
  matri.df. <- do.call("rbind", matri.list)
  
  names(matri.df.) <- c("Site.Year.Month.Day", "dist.", "name.", "name.left", "name.right", "Type.Bloc.left", "Face.left", "Numéro.Bloc.échantillon.left", "Quadrat.left", "Type.Bloc.right", "Face.right", "Numéro.Bloc.échantillon.right", "Quadrat.right")
  
  matri.df. <<- matri.df.
  
  hist(matri.df.$dist.)
  
}


# Full dataset

filter(qecb.ivr, Type.Bloc == "Bloc mobile") -> data.
matri.list <- vector("list", length(unique(data.$Site.Year.Month.Day)))
#matri.fct.BMM(data = data., conca = c(Bret_EGMP.BASQ_qecb))
matri.fct.BMM(data = data., conca = c(Bret_EGMP.BASQ_qecb, EGMP.BASQ_qecb))
hist(matri.df.$dist., main = c(paste("Histo. of Bray (0-adjusted) dist. dissimilarity measures"), paste(" (sqrt transfo) - BMfs vs BMfi -")))

matri.full.BM_FS.FI <- matri.df.
#saveRDS(matri.full.BM_FS.FI, "results/Ecology/matri.full.BM_FS.FI.RDS")
saveRDS(matri.full.BM_FS.FI, "results/Ecology/matri.full_log.spi_BM_FS.FI.RDS")
rm(data., matri.df., matri.list)
hist(matri.full.BM_FS.FI$dist., main = c(paste("Histo. of Bray (0-adjusted) dist. dissimilarity measures"), paste(" (sqrt transfo) - BMfs vs BMfi -")))

fct.ivr.dist.(df. = matri.full.BM_FS.FI, title. = "dissimilarité BM.FS vs BM.FI Atlantique") # 2 plot click previous in plots window to see gam model

# Bretagne

filter(qecb.ivr, Region == "Bretagne" & Type.Bloc == "Bloc mobile") -> data.
matri.list <- vector("list", length(unique(data.$Site.Year.Month.Day)))
matri.fct.BMM(data = data., conca = c(Bret_EGMP.BASQ_qecb))
hist(matri.df.$dist., main = c(paste("Histo. of Bray (0-adjusted) dist. dissimilarity measures"), paste(" (sqrt transfo) - Bretagne BMfs vs BMfi -")))

matri.Bretagne.BM_FS.FI <- matri.df.
#saveRDS(matri.Bretagne.BM_FS.FI, "results/Ecology/matri.Bretagne.BM_FS.FI.RDS")
saveRDS(matri.Bretagne.BM_FS.FI, "results/Ecology/matri.Bretagne_log.spi_BM_FS.FI.RDS")
rm(data., matri.df., matri.list)

fct.ivr.dist.(df. = matri.Bretagne.BM_FS.FI, title. = "dissimilarité BM.FS vs BM.FI Bretagne")

# EGMP.BASQ

filter(qecb.ivr, Region == "EGMP.BASQ" & Type.Bloc == "Bloc mobile") -> data.
matri.list <- vector("list", length(unique(data.$Site.Year.Month.Day)))
matri.fct.BMM(data = data., conca = c(Bret_EGMP.BASQ_qecb, EGMP.BASQ_qecb))
hist(matri.df.$dist., main = c(paste("Histo. of Bray (0-adjusted) dist. dissimilarity measures"), paste(" (sqrt transfo) - EGMP.BASQ BMfs vs BMfi -")))

matri.EGMP.BASQ.BM_FS.FI <- matri.df.
#saveRDS(matri.EGMP.BASQ.BM_FS.FI, "results/Ecology/matri.EGMP.BASQ.BM_FS.FI.RDS")
saveRDS(matri.EGMP.BASQ.BM_FS.FI, "results/Ecology/matri.EGMP.BASQ_log.spi_BM_FS.FI.RDS")
rm(data., matri.df., matri.list)

fct.ivr.dist.(df. = matri.EGMP.BASQ.BM_FS.FI, title. = "dissimilarité BM.FS vs BM.FI EGMP-BASQ")


# don't run anymore below code for now on; we'll need something similar later on when calculating distance by removing var by var
###########################################################
# recalculate dissimilarity distances, after removal of "modeling" vectors of variables.
# this section should be run after executing the "CB_ecology" R script.

(Atlantique.rem. <- readRDS("results/Ecology/Atlantique.rem.RDS"))
(Bretagne.rem. <- readRDS("results/Ecology/Bretagne.rem.RDS"))
(EGMP.BASQ.rem. <- readRDS("results/Ecology/EGMP.BASQ.rem.RDS"))

`%notin%` <- Negate(`%in%`)

# Full dataset

qecb.ivr[, which(names(qecb.ivr) %notin% Atlantique.rem.)] -> data.
filter(data., Type.Bloc == "Bloc mobile") -> data.
matri.list <- vector("list", length(unique(data.$Site.Year.Month.Day)))
#matri.fct.BMM(data = data., conca = c(setdiff(Bret_EGMP.BASQ_qecb, Atlantique.rem.)))
matri.fct.BMM(data = data., conca = c(setdiff(c(Bret_EGMP.BASQ_qecb, EGMP.BASQ_qecb), Atlantique.rem.)))
hist(matri.df.$dist., main = c(paste("Histo. of Bray (0-adjusted) dist. dissimilarity measures"), paste(" (sqrt transfo) - BMfs vs BMfi -")))

matri.full.rem.BM_FS.FI <- matri.df.
saveRDS(matri.full.rem.BM_FS.FI, "results/Ecology/matri.full.rem.BM_FS.FI.RDS")
rm(data., matri.df., matri.list)

fct.ivr.dist.(df. = matri.full.rem.BM_FS.FI, title. = "dissimilarité BM.FS vs BM.FI Atlantique") 

# Bretagne

qecb.ivr[, which(names(qecb.ivr) %notin% Bretagne.rem.)] -> data.
filter(data., Region == "Bretagne" & Type.Bloc == "Bloc mobile") -> data.
matri.list <- vector("list", length(unique(data.$Site.Year.Month.Day)))
matri.fct.BMM(data = data., conca = c(setdiff(Bret_EGMP.BASQ_qecb, Bretagne.rem.)))
hist(matri.df.$dist., main = c(paste("Histo. of Bray (0-adjusted) dist. dissimilarity measures"), paste(" (sqrt transfo) - Bretagne BMfs vs BMfi -")))

matri.Bretagne.rem.BM_FS.FI <- matri.df.
saveRDS(matri.Bretagne.rem.BM_FS.FI, "results/Ecology/matri.Bretagne.rem.BM_FS.FI.RDS")
rm(data., matri.df., matri.list)

fct.ivr.dist.(df. = matri.Bretagne.rem.BM_FS.FI, title. = "dissimilarité BM.FS vs BM.FI Bretagne") 

# EGMP.BASQ

qecb.ivr[, which(names(qecb.ivr) %notin% EGMP.BASQ.rem.)] -> data.
filter(data., Region == "EGMP.BASQ" & Type.Bloc == "Bloc mobile") -> data.
matri.list <- vector("list", length(unique(data.$Site.Year.Month.Day)))
matri.fct.BMM(data = data., conca = c(setdiff(c(Bret_EGMP.BASQ_qecb, EGMP.BASQ_qecb), EGMP.BASQ.rem.)))
hist(matri.df.$dist., main = c(paste("Histo. of Bray (0-adjusted) dist. dissimilarity measures"), paste(" (sqrt transfo) - EGMP.BASQ BMfs vs BMfi -")))

matri.EGMP.BASQ.rem.BM_FS.FI <- matri.df.
saveRDS(matri.EGMP.BASQ.rem.BM_FS.FI, "results/Ecology/matri.EGMP.BASQ.rem.BM_FS.FI.RDS")
rm(data., matri.df., matri.list)

fct.ivr.dist.(df. = matri.EGMP.BASQ.rem.BM_FS.FI, title. = "dissimilarité BM.FS vs BM.FI EGMP-BASQ")
###########################################################


# let's check that matri full = cbi of matri.Bretagne & matri.EGMP.BASQ, i.e. that var. with NA data are not considered for dist. determination

bind_rows(matri.Bretagne.BM_FS.FI, matri.EGMP.BASQ.BM_FS.FI) -> matri.Region_bind.BM_FS.FI

par(mfrow=c(1,1))
hist(matri.full.BM_FS.FI$dist.)
hist(matri.Region_bind.BM_FS.FI$dist.)
matri.compa <- (left_join(matri.full.BM_FS.FI, matri.Region_bind.BM_FS.FI, by = c("Site.Year.Month.Day", "name.", "name.left", "name.right", "Type.Bloc.left", "Face.left", "Numéro.Bloc.échantillon.left", "Quadrat.left", "Type.Bloc.right", "Face.right", "Numéro.Bloc.échantillon.right", "Quadrat.right")))
unique(matri.compa$dist..x - matri.compa$dist..y)
# same result, therefore just calculating matri full is enough !
# not needed anymore to calculate distance for both Bretagne and EGMP.BASQ separately
# logical, there is a na.rm = TRUE in the Bray dist function
rm(matri.compa)

bind_rows(matri.Bretagne.BM.BF_FS, matri.EGMP.BASQ.BM.BF_FS) -> matri.Region_bind.BM.BF_FS

par(mfrow=c(1,1))
hist(matri.full.BM.BF_FS$dist.)
hist(matri.Region_bind.BM.BF_FS$dist.)
matri.compa <- (left_join(matri.full.BM_FS.FI, matri.Region_bind.BM_FS.FI, by = c("Site.Year.Month.Day", "name.", "name.left", "name.right", "Type.Bloc.left", "Face.left", "Numéro.Bloc.échantillon.left", "Quadrat.left", "Type.Bloc.right", "Face.right", "Numéro.Bloc.échantillon.right", "Quadrat.right")))
unique(matri.compa$dist..x - matri.compa$dist..y)
# same result, therefore just calculating matri full is enough !
# not needed anymore to calculate distance for both Bretagne and EGMP.BASQ separately
# logical, there is a na.rm = TRUE in the Bray dist function
rm(matri.compa)


## plot
# activate line
#matri.full.BM.BF_FS <- readRDS("results/Ecology/matri.full.BM.BF_FS.RDS")
matri.full.BM.BF_FS <- readRDS("results/Ecology/matri.full_log.spi_BM.BF_FS.RDS")
#matri.full.BM_FS.FI <- readRDS("results/Ecology/matri.full.BM_FS.FI.RDS")
matri.full.BM_FS.FI <- readRDS("results/Ecology/matri.full_log.spi_BM_FS.FI.RDS")


library(tidyr)
library(tibble)

matri.full.BM.BF_FS <- separate(matri.full.BM.BF_FS, "Site.Year.Month.Day", into = c("departement", "Site", "Year", "Month", "Day"), remove = F)
matri.full.BM.BF_FS$Site <- paste0(matri.full.BM.BF_FS$departement, "_", matri.full.BM.BF_FS$Site)
matri.full.BM.BF_FS <- subset(matri.full.BM.BF_FS, select = - c(departement)) 
matri.full.BM.BF_FS <- add_column(matri.full.BM.BF_FS, Date = as.Date(paste0(matri.full.BM.BF_FS$Year, "-", matri.full.BM.BF_FS$Month, "-", matri.full.BM.BF_FS$Day), origin = "1970-01-01"), .after = "Site")
matri.full.BM.BF_FS$Site <- as.factor(matri.full.BM.BF_FS$Site)

matri.full.BM_FS.FI <- separate(matri.full.BM_FS.FI, "Site.Year.Month.Day", into = c("departement", "Site", "Year", "Month", "Day"), remove = F)
matri.full.BM_FS.FI$Site <- paste0(matri.full.BM_FS.FI$departement, "_", matri.full.BM_FS.FI$Site)
matri.full.BM_FS.FI <- subset(matri.full.BM_FS.FI, select = - c(departement)) 
matri.full.BM_FS.FI <- add_column(matri.full.BM_FS.FI, Date = as.Date(paste0(matri.full.BM_FS.FI$Year, "-", matri.full.BM_FS.FI$Month, "-", matri.full.BM_FS.FI$Day), origin = "1970-01-01"), .after = "Site")
matri.full.BM_FS.FI$Site <- as.factor(matri.full.BM_FS.FI$Site)

#dev.off() # if error message "Error in .Call.graphics(C_palette2, .Call(C_palette2, NULL)) : invalid graphics state"

library(ggplot2)

ggplot(matri.full.BM.BF_FS, aes(x = Site, y = dist.)) +
  geom_boxplot() + 
  #geom_jitter(shape = 16, position=position_jitter(0.2)) +
  xlab("") +
  ylab("distance diss. BM.BF_FS") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(matri.full.BM_FS.FI, aes(x = Site, y = dist.)) +
  geom_boxplot() + 
  #geom_jitter(shape = 16, position=position_jitter(0.2)) +
  xlab("") +
  ylab("distance diss. BM_FS.FI") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# issue with type de bloc, numéro de bloc and quadrat for df. BM.BF_FS, cfr left vs right doesn't give the right combination.
matri.full.BM.BF_FS$Quadrat <- NA
for (i in c(1:nrow(matri.full.BM.BF_FS))) { 
  ifelse(matri.full.BM.BF_FS$Type.Bloc.left[i] == "Bloc mobile", matri.full.BM.BF_FS$Quadrat[i] <- matri.full.BM.BF_FS$Quadrat.left[i], matri.full.BM.BF_FS$Quadrat[i] <- matri.full.BM.BF_FS$Quadrat.right[i]) }
matri.full.BM.BF_FS$Numéro.Bloc <- NA
for (i in c(1:nrow(matri.full.BM.BF_FS))) { 
  ifelse(matri.full.BM.BF_FS$Type.Bloc.left[i] == "Bloc mobile", matri.full.BM.BF_FS$Numéro.Bloc[i] <- matri.full.BM.BF_FS$Numéro.Bloc.échantillon.left[i], matri.full.BM.BF_FS$Numéro.Bloc[i] <- matri.full.BM.BF_FS$Numéro.Bloc.échantillon.right[i]) }

matri.full.BM.BF_FS <- add_column(matri.full.BM.BF_FS, Site.Year.Month.Day.Q.BMnb = paste0(matri.full.BM.BF_FS$Site.Year.Month.Day, "_",  matri.full.BM.BF_FS$Quadrat, "_", matri.full.BM.BF_FS$Numéro.Bloc), .after = "Site.Year.Month.Day")
matri.full.BM_FS.FI <- add_column(matri.full.BM_FS.FI, Site.Year.Month.Day.Q.BMnb = paste0(matri.full.BM_FS.FI$Site.Year.Month.Day, "_",  matri.full.BM_FS.FI$Quadrat.left, "_", matri.full.BM_FS.FI$Numéro.Bloc.échantillon.left), .after = "Site.Year.Month.Day")

colnames(matri.full.BM.BF_FS) <- paste("BM.BF_FS", colnames(matri.full.BM.BF_FS), sep = "_")
matri.full.BM.BF_FS <- rename(matri.full.BM.BF_FS, Site.Year.Month.Day.Q.BMnb = BM.BF_FS_Site.Year.Month.Day.Q.BMnb )
colnames(matri.full.BM_FS.FI) <- paste("BM_FS.FI", colnames(matri.full.BM_FS.FI), sep = "_")
matri.full.BM_FS.FI <- rename(matri.full.BM_FS.FI, Site.Year.Month.Day.Q.BMnb = BM_FS.FI_Site.Year.Month.Day.Q.BMnb )

matri.full <- full_join(matri.full.BM.BF_FS[, c("Site.Year.Month.Day.Q.BMnb", "BM.BF_FS_dist.")], matri.full.BM_FS.FI[, c("Site.Year.Month.Day.Q.BMnb", "BM_FS.FI_dist.")])

matri.full <- separate_(matri.full, "Site.Year.Month.Day.Q.BMnb", into = c("departement", "Site", "Year", "Month", "Day", "Quadrat", "Bloc Mobile Number"), remove = F)
matri.full$Site <- paste0(matri.full$departement, "_", matri.full$Site)
matri.full <- subset(matri.full, select = - c(departement)) 
matri.full <- add_column(matri.full, Date = as.Date(paste0(matri.full$Year, "-", matri.full$Month, "-", matri.full$Day), origin = "1970-01-01"), .after = "Site")

names(qecb.ivr)
duplicated(qecb.ivr[, c("Region.Site.Year.Month.Day", "Type.Bloc", "Numéro.Bloc.échantillon", "Face", "Quadrat.bis")])
qecb.ivr %>% group_by(Region.Site.Year.Month.Day, Type.Bloc, Numéro.Bloc.échantillon, Face, Quadrat.bis) %>% filter(n() > 1) -> dupl.
dupl.
rm(dupl.)

names(matri.full)
duplicated(matri.full[, "Site.Year.Month.Day.Q.BMnb"])
matri.full %>% group_by(Site.Year.Month.Day.Q.BMnb) %>% filter(n() > 1) -> dupl.
dupl.

filter(qecb.ivr, Site.Year.Month.Day == "GDMO_Locmariaquer.2018.09.10", Quadrat.bis == "Q3") -> dupl.


#saveRDS(matri.full, "results/Ecology/matri.full.RDS")
saveRDS(matri.full, "results/Ecology/matri.full_log.spi.RDS")
#matri.full <- readRDS("results/Ecology/matri.full.RDS")
matri.full <- readRDS("results/Ecology/matri.full_log.spi.RDS")

par(mfrow= c(1,1))

matri.full.red <- na.omit(matri.full)
plot(x = matri.full.red$BM.BF_FS_dist., y = matri.full.red$BM_FS.FI_dist., xlim = c(0,1), ylim = c(0,1), xlab = "distance diss. BM.FS vs BF", ylab = "distance diss. BM.FS vs BM.FI")
matri.full.red <- arrange(matri.full.red, BM.BF_FS_dist.) 
plot(x = matri.full.red$BM.BF_FS_dist., y = matri.full.red$BM_FS.FI_dist., xlim = c(0,1), ylim = c(0,1), xlab = "distance diss. BM.FS vs BF", ylab = "distance diss. BM.FS vs BM.FI")
lw. <- loess(matri.full.red$BM_FS.FI_dist. ~ matri.full.red$BM.BF_FS_dist., span = 1)
lines(matri.full.red$BM.BF_FS_dist., lw.$fitted, col = "black",lwd = 3, lty = "dashed")
lm. <- lm(matri.full.red$BM_FS.FI_dist. ~ matri.full.red$BM.BF_FS_dist.)
lines(matri.full.red$BM.BF_FS_dist., lm.$fitted, col = "red",lwd = 3, lty = "dashed")
cor(matri.full.red$BM.BF_FS_dist., matri.full.red$BM_FS.FI_dist., method="spearman")
text(0.4,0, paste0("Coefficient de corrélation de Spearman = ", round(cor(matri.full.red$BM.BF_FS_dist., matri.full.red$BM_FS.FI_dist., method="spearman"), digits = 2)))
cor(matri.full.red$BM.BF_FS_dist., matri.full.red$BM_FS.FI_dist., method="spearman")
text(0.4,0.075, paste0("Coefficient de corrélation de Pearson = ", round(cor(matri.full.red$BM.BF_FS_dist., matri.full.red$BM_FS.FI_dist., method="pearson"), digits = 2)), col = "red")

matri.full.red <- filter(matri.full.red, BM.BF_FS_dist. !=0)
plot(x = matri.full.red$BM.BF_FS_dist., y = matri.full.red$BM_FS.FI_dist., xlim = c(0,1), ylim = c(0,1), xlab = "distance diss. BM.FS vs BF", ylab = "distance diss. BM.FS vs BM.FI")
matri.full.red <- arrange(matri.full.red, BM.BF_FS_dist.) 
plot(x = matri.full.red$BM.BF_FS_dist., y = matri.full.red$BM_FS.FI_dist., xlim = c(0,1), ylim = c(0,1), xlab = "distance diss. BM.FS vs BF", ylab = "distance diss. BM.FS vs BM.FI")
lw. <- loess(matri.full.red$BM_FS.FI_dist. ~ matri.full.red$BM.BF_FS_dist., span = 1)
lines(matri.full.red$BM.BF_FS_dist., lw.$fitted, col = "black",lwd = 3, lty = "dashed")
lm. <- lm(matri.full.red$BM_FS.FI_dist. ~ matri.full.red$BM.BF_FS_dist.)
lines(matri.full.red$BM.BF_FS_dist., lm.$fitted, col = "red",lwd = 3, lty = "dashed")
cor(matri.full.red$BM.BF_FS_dist., matri.full.red$BM_FS.FI_dist., method="spearman")
text(0.4,0, paste0("Coefficient de corrélation de Spearman = ", round(cor(matri.full.red$BM.BF_FS_dist., matri.full.red$BM_FS.FI_dist., method="spearman"), digits = 2)))
cor(matri.full.red$BM.BF_FS_dist., matri.full.red$BM_FS.FI_dist., method="spearman")
text(0.4,0.075, paste0("Coefficient de corrélation de Pearson = ", round(cor(matri.full.red$BM.BF_FS_dist., matri.full.red$BM_FS.FI_dist., method="pearson"), digits = 2)), col = "red")

matri.full.red <- filter(matri.full.red, BM.BF_FS_dist. >= 0.3 | BM_FS.FI_dist. >= 0.3)
matri.full.red <- filter(matri.full.red, BM.BF_FS_dist. <= 0.9 | BM_FS.FI_dist. <= 0.9)
plot(x = matri.full.red$BM.BF_FS_dist., y = matri.full.red$BM_FS.FI_dist., xlim = c(0,1), ylim = c(0,1), xlab = "distance diss. BM.FS vs BF", ylab = "distance diss. BM.FS vs BM.FI")
matri.full.red <- arrange(matri.full.red, BM.BF_FS_dist.) 
plot(x = matri.full.red$BM.BF_FS_dist., y = matri.full.red$BM_FS.FI_dist., xlim = c(0,1), ylim = c(0,1), xlab = "distance diss. BM.FS vs BF", ylab = "distance diss. BM.FS vs BM.FI")
lw. <- loess(matri.full.red$BM_FS.FI_dist. ~ matri.full.red$BM.BF_FS_dist., span = 1)
lines(matri.full.red$BM.BF_FS_dist., lw.$fitted, col = "black",lwd = 3, lty = "dashed")
lm. <- lm(matri.full.red$BM_FS.FI_dist. ~ matri.full.red$BM.BF_FS_dist.)
lines(matri.full.red$BM.BF_FS_dist., lm.$fitted, col = "red",lwd = 3, lty = "dashed")
cor(matri.full.red$BM.BF_FS_dist., matri.full.red$BM_FS.FI_dist., method="spearman")
text(0.4,0, paste0("Coefficient de corrélation de Spearman = ", round(cor(matri.full.red$BM.BF_FS_dist., matri.full.red$BM_FS.FI_dist., method="spearman"), digits = 2)))
cor(matri.full.red$BM.BF_FS_dist., matri.full.red$BM_FS.FI_dist., method="spearman")
text(0.4,0.075, paste0("Coefficient de corrélation de Pearson = ", round(cor(matri.full.red$BM.BF_FS_dist., matri.full.red$BM_FS.FI_dist., method="pearson"), digits = 2)), col = "red")



## Do calculate dissimilarity distances by removing one var. at a time


# Full dataset BM.BF_FS


filter(qecb.ivr, Face == "face supérieure") -> data.
data.$Type.Bloc <- ifelse(as.character(data.$Type.Bloc) == "Roche en place", "Bloc fixé", as.character(data.$Type.Bloc))
matri.list <- vector("list", length(unique(data.$Site.Year.Month.Day)))

par(mfrow=c(1,1))

for(i in c(1:length(c(Bret_EGMP.BASQ_qecb, EGMP.BASQ_qecb)))) {

  matri.fct.BMF(data = data., conca = c(Bret_EGMP.BASQ_qecb, EGMP.BASQ_qecb)[-i])
  
  names(matri.df.)[names(matri.df.) == 'dist.'] <- paste0("dist.", c(Bret_EGMP.BASQ_qecb, EGMP.BASQ_qecb)[i])
  
  assign(paste0("matri.fullRED.BM.BF_FS", ".", c(Bret_EGMP.BASQ_qecb, EGMP.BASQ_qecb)[i]), matri.df., envir = .GlobalEnv)
  
  rm(matri.df., i)
  
}

par(mfrow=c(1,1))

require(purrr)
#matri.full.BM.BF_FS.saved <- matri.full.BM.BF_FS
#matri.full.BM.BF_FS <- matri.full.BM.BF_FS.saved
names(matri.full.BM.BF_FS) = gsub(pattern = "BM.BF_FS_", replacement = "", x = names(matri.full.BM.BF_FS))

Pattern1 <- c("matri.full.BM.BF_FS", grep("fullRED.BM.BF_FS", names(.GlobalEnv), value = TRUE))
#rm("fullRED.BM.BF_FS", "matri.fullRED.BM.BF_FS", "fullRED.BM.BF_FS.i")
do.call("list", mget(Pattern1)) %>% reduce(full_join, by = c("Site.Year.Month.Day", "name.", "name.left", "name.right", "Type.Bloc.left", "Face.left", "Numéro.Bloc.échantillon.left", "Quadrat.left", "Type.Bloc.right", "Face.right", "Numéro.Bloc.échantillon.right", "Quadrat.right")) -> matri.fullRED.BM.BF_FS

fullRED.BM.BF_FS <- matri.fullRED.BM.BF_FS[, c("Site.Year.Month.Day",  grep("dist.", names(matri.fullRED.BM.BF_FS), value=TRUE))]

# here below we create as many dfs as they are statistics to calculate

moy.list <- vector("list", length(unique(fullRED.BM.BF_FS$Site.Year.Month.Day)))
et.list <- vector("list", length(unique(fullRED.BM.BF_FS$Site.Year.Month.Day)))
nb.list <- vector("list", length(unique(fullRED.BM.BF_FS$Site.Year.Month.Day)))

for (i in c(1:length(unique(fullRED.BM.BF_FS$Site.Year.Month.Day)))) {
  
  fullRED.BM.BF_FS  %>% filter(Site.Year.Month.Day == unique(fullRED.BM.BF_FS$Site.Year.Month.Day)[[i]]) -> fullRED.BM.BF_FS.i
  
  moy.list[[i]]  <- lapply(fullRED.BM.BF_FS.i[,2:ncol(fullRED.BM.BF_FS.i)], mean, na.rm = TRUE)
  
  et.list[[i]] <- lapply(fullRED.BM.BF_FS.i[,2:ncol(fullRED.BM.BF_FS.i)], sd, na.rm = TRUE)
  
  nb.list[[i]] <- apply(fullRED.BM.BF_FS.i[,2:ncol(fullRED.BM.BF_FS.i)], 2, function(x) { length(which(!is.na(x))) } )
  
  #fullRED.BM.BF_FS.med <- lapply(fullRED.BM.BF_FS.i[,2:ncol(fullRED.BM.BF_FS.i)], median)
  #fullRED.BM.BF_FS.min <- lapply(fullRED.BM.BF_FS.i[,2:ncol(fullRED.BM.BF_FS.i)], min)
  #fullRED.BM.BF_FS.max <- lapply(fullRED.BM.BF_FS.i[,2:ncol(fullRED.BM.BF_FS.i)], max)
  
}

moy. <- data.frame(do.call("rbind", moy.list))
for(i in c(1:ncol(moy.))){
  moy.[[i]] <- unlist(moy.[[i]])
}
moy. <- add_column(moy., unique(fullRED.BM.BF_FS["Site.Year.Month.Day"]), .before = "dist.")

names(moy.)[2:ncol(moy.)]

# and do the same if other dfs eg. below sd and nb are needed
et. <- data.frame(do.call("rbind", et.list))
nb. <- data.frame(do.call("rbind", nb.list))

# here below we work on moy. data; but don't forget we also have the sd, and we can calculate median, min, max etc.

ratio <- (moy.[, 2] - moy.[, 3:ncol(moy.)]) / moy.[, 2] 
ratio <- add_column(ratio, unique(moy.["Site.Year.Month.Day"]), .before = names(ratio[1]))
ratio <- add_column(ratio, moy.["dist."], .after = "Site.Year.Month.Day")

par(mfrow = c(1,1))
par(mar=c(15,5,1,1))
boxplot((abs(ratio[, c(2:length(ratio))])), xaxt = "n", ylab = "rapport dist. BM.BF_FS Atlantique")
tick <- seq_along(ratio[, c(2:length(ratio))])
axis(1, at = tick, labels = FALSE)
text(tick, par("usr")[3] - 0.45, names(ratio[, c(2:length(ratio))]), srt = 90, xpd = T)

par(mfrow = c(1,1))
par(mar=c(15,5,1,1))
boxplot(log10(abs(ratio[, c(2:length(ratio))])+1), xaxt = "n", ylab = "log10(rapport dist. BM.BF_FS Atlantique + 1)")
tick <- seq_along(ratio[, c(2:length(ratio))])
axis(1, at = tick, labels = FALSE)
text(tick, par("usr")[3] - 0.12, names(ratio[, c(2:length(ratio))]), srt = 90, xpd = T)

box.BM.BF_FS.Atlantique <- boxplot((abs(ratio[, c(2:length(ratio))])))
box.stats.BM.BF_FS.Atlantique <- data.frame(box.BM.BF_FS.Atlantique$stats)
colnames(box.stats.BM.BF_FS.Atlantique) <- box.BM.BF_FS.Atlantique$names
rownames(box.stats.BM.BF_FS.Atlantique) <- c("Min", "First Quartile", "Median", "Third Quartile", "Maximum")
box.stats.BM.BF_FS.Atlantique <- data.frame(t(box.stats.BM.BF_FS.Atlantique))
box.stats.BM.BF_FS.Atlantique <- box.stats.BM.BF_FS.Atlantique[with(box.stats.BM.BF_FS.Atlantique, order(-Median, -Third.Quartile)),][2:nrow(box.stats.BM.BF_FS.Atlantique),]
median.BM.BF_FS.Atlantique<- rownames(filter(box.stats.BM.BF_FS.Atlantique, Median > 0))
Third.Quartile.BM.BF_FS.Atlantique<- rownames(filter(box.stats.BM.BF_FS.Atlantique, Third.Quartile > 0))


# Bretagne dataset BM.BF_FS


filter(qecb.ivr, Region == "Bretagne", Face == "face supérieure") -> data.
data.$Type.Bloc <- ifelse(as.character(data.$Type.Bloc) == "Roche en place", "Bloc fixé", as.character(data.$Type.Bloc))
matri.list <- vector("list", length(unique(data.$Site.Year.Month.Day)))

par(mfrow=c(1,1))

for(i in c(1:length(c(Bret_EGMP.BASQ_qecb)))) { #, EGMP.BASQ_qecb
  
  matri.fct.BMF(data = data., conca = c(Bret_EGMP.BASQ_qecb)[-i]) #, EGMP.BASQ_qecb
  
  names(matri.df.)[names(matri.df.) == 'dist.'] <- paste0("dist.", c(Bret_EGMP.BASQ_qecb)[i]) #, EGMP.BASQ_qecb
  
  assign(paste0("matri.BretagneRED.BM.BF_FS", ".", c(Bret_EGMP.BASQ_qecb)[i]), matri.df., envir = .GlobalEnv) #, EGMP.BASQ_qecb
  
  rm(matri.df., i)
  
}

par(mfrow=c(1,1))

require(purrr)
#matri.Bretagne.BM.BF_FS.saved <- matri.Bretagne.BM.BF_FS
#matri.Bretagne.BM.BF_FS <- matri.Bretagne.BM.BF_FS.saved
#names(matri.Bretagne.BM.BF_FS) = gsub(pattern = "BM.BF_FS_", replacement = "", x = names(matri.Bretagne.BM.BF_FS))

Pattern1 <- c("matri.Bretagne.BM.BF_FS", grep("BretagneRED.BM.BF_FS", names(.GlobalEnv), value = TRUE))
#rm("BretagneRED.BM.BF_FS.i", "matri.BretagneRED.BM.BF_FS", "BretagneRED.BM.BF_FS")
do.call("list", mget(Pattern1)) %>% reduce(full_join, by = c("Site.Year.Month.Day", "name.", "name.left", "name.right", "Type.Bloc.left", "Face.left", "Numéro.Bloc.échantillon.left", "Quadrat.left", "Type.Bloc.right", "Face.right", "Numéro.Bloc.échantillon.right", "Quadrat.right")) -> matri.BretagneRED.BM.BF_FS

BretagneRED.BM.BF_FS <- matri.BretagneRED.BM.BF_FS[, c("Site.Year.Month.Day",  grep("dist.", names(matri.BretagneRED.BM.BF_FS), value=TRUE))]

# here below we create as many dfs as they are statistics to calculate

moy.list <- vector("list", length(unique(BretagneRED.BM.BF_FS$Site.Year.Month.Day)))
et.list <- vector("list", length(unique(BretagneRED.BM.BF_FS$Site.Year.Month.Day)))
nb.list <- vector("list", length(unique(BretagneRED.BM.BF_FS$Site.Year.Month.Day)))

for (i in c(1:length(unique(BretagneRED.BM.BF_FS$Site.Year.Month.Day)))) {
  
  BretagneRED.BM.BF_FS  %>% filter(Site.Year.Month.Day == unique(BretagneRED.BM.BF_FS$Site.Year.Month.Day)[[i]]) -> BretagneRED.BM.BF_FS.i
  
  moy.list[[i]]  <- lapply(BretagneRED.BM.BF_FS.i[,2:ncol(BretagneRED.BM.BF_FS.i)], mean, na.rm = TRUE)
  
  et.list[[i]] <- lapply(BretagneRED.BM.BF_FS.i[,2:ncol(BretagneRED.BM.BF_FS.i)], sd, na.rm = TRUE)
  
  nb.list[[i]] <- apply(BretagneRED.BM.BF_FS.i[,2:ncol(BretagneRED.BM.BF_FS.i)], 2, function(x) { length(which(!is.na(x))) } )
  
  #BretagneRED.BM.BF_FS.med <- lapply(BretagneRED.BM.BF_FS.i[,2:ncol(BretagneRED.BM.BF_FS.i)], median)
  #BretagneRED.BM.BF_FS.min <- lapply(BretagneRED.BM.BF_FS.i[,2:ncol(BretagneRED.BM.BF_FS.i)], min)
  #BretagneRED.BM.BF_FS.max <- lapply(BretagneRED.BM.BF_FS.i[,2:ncol(BretagneRED.BM.BF_FS.i)], max)
  
}

moy. <- data.frame(do.call("rbind", moy.list))
for(i in c(1:ncol(moy.))){
  moy.[[i]] <- unlist(moy.[[i]])
}
moy. <- add_column(moy., unique(BretagneRED.BM.BF_FS["Site.Year.Month.Day"]), .before = "dist.")

names(moy.)[2:ncol(moy.)]

# and do the same if other dfs eg. below sd and nb are needed
et. <- data.frame(do.call("rbind", et.list))
nb. <- data.frame(do.call("rbind", nb.list))

# here below we work on moy. data; but don't forget we also have the sd, and we can calculate median, min, max etc.

ratio <- (moy.[, 2] - moy.[, 3:ncol(moy.)]) / moy.[, 2] 
ratio <- add_column(ratio, unique(moy.["Site.Year.Month.Day"]), .before = names(ratio[1]))
ratio <- add_column(ratio, moy.["dist."], .after = "Site.Year.Month.Day")

par(mfrow = c(1,1))
par(mar=c(15,5,1,1))
boxplot((abs(ratio[, c(2:length(ratio))])), xaxt = "n", ylab = "rapport dist. BM.BF_FS Bretagne")
tick <- seq_along(ratio[, c(2:length(ratio))])
axis(1, at = tick, labels = FALSE)
text(tick, par("usr")[3] - 0.45, names(ratio[, c(2:length(ratio))]), srt = 90, xpd = T)

par(mfrow = c(1,1))
par(mar=c(15,5,1,1))
boxplot(log10(abs(ratio[, c(2:length(ratio))])+1), xaxt = "n", ylab = "log10(rapport dist. BM.BF_FS Bretagne + 1)")
tick <- seq_along(ratio[, c(2:length(ratio))])
axis(1, at = tick, labels = FALSE)
text(tick, par("usr")[3] - 0.12, names(ratio[, c(2:length(ratio))]), srt = 90, xpd = T)

box.BM.BF_FS.Bretagne <- boxplot((abs(ratio[, c(2:length(ratio))])))
box.stats.BM.BF_FS.Bretagne <- data.frame(box.BM.BF_FS.Bretagne$stats)
colnames(box.stats.BM.BF_FS.Bretagne) <- box.BM.BF_FS.Bretagne$names
rownames(box.stats.BM.BF_FS.Bretagne) <- c("Min", "First Quartile", "Median", "Third Quartile", "Maximum")
box.stats.BM.BF_FS.Bretagne <- data.frame(t(box.stats.BM.BF_FS.Bretagne))
box.stats.BM.BF_FS.Bretagne <- box.stats.BM.BF_FS.Bretagne[with(box.stats.BM.BF_FS.Bretagne, order(-Median, -Third.Quartile)),][2:nrow(box.stats.BM.BF_FS.Bretagne),]
median.BM.BF_FS.Bretagne<- rownames(filter(box.stats.BM.BF_FS.Bretagne, Median > 0))
Third.Quartile.BM.BF_FS.Bretagne<- rownames(filter(box.stats.BM.BF_FS.Bretagne, Third.Quartile > 0))


# EGMP.BASQ dataset BM.BF_FS


filter(qecb.ivr, Region == "EGMP.BASQ", Face == "face supérieure") -> data.
data.$Type.Bloc <- ifelse(as.character(data.$Type.Bloc) == "Roche en place", "Bloc fixé", as.character(data.$Type.Bloc))
matri.list <- vector("list", length(unique(data.$Site.Year.Month.Day)))

par(mfrow=c(1,1))

for(i in c(1:length(c(Bret_EGMP.BASQ_qecb, EGMP.BASQ_qecb)))) {
  
  matri.fct.BMF(data = data., conca = c(Bret_EGMP.BASQ_qecb, EGMP.BASQ_qecb)[-i]) 
  
  names(matri.df.)[names(matri.df.) == 'dist.'] <- paste0("dist.", c(Bret_EGMP.BASQ_qecb, EGMP.BASQ_qecb)[i]) 
  
  assign(paste0("matri.EGMP.BASQRED.BM.BF_FS", ".", c(Bret_EGMP.BASQ_qecb, EGMP.BASQ_qecb)[i]), matri.df., envir = .GlobalEnv) 
  
  rm(matri.df., i)
  
}

par(mfrow=c(1,1))

require(purrr)
#matri.EGMP.BASQ.BM.BF_FS.saved <- matri.EGMP.BASQ.BM.BF_FS
#matri.EGMP.BASQ.BM.BF_FS <- matri.EGMP.BASQ.BM.BF_FS.saved
#names(matri.EGMP.BASQ.BM.BF_FS) = gsub(pattern = "BM.BF_FS_", replacement = "", x = names(matri.EGMP.BASQ.BM.BF_FS))

Pattern1 <- c("matri.EGMP.BASQ.BM.BF_FS", grep("EGMP.BASQRED.BM.BF_FS", names(.GlobalEnv), value = TRUE))
#rm(matri.EGMP.BASQRED.BM.BF_FS, EGMP.BASQRED.BM.BF_FS, EGMP.BASQRED.BM.BF_FS.i)
do.call("list", mget(Pattern1)) %>% reduce(full_join, by = c("Site.Year.Month.Day", "name.", "name.left", "name.right", "Type.Bloc.left", "Face.left", "Numéro.Bloc.échantillon.left", "Quadrat.left", "Type.Bloc.right", "Face.right", "Numéro.Bloc.échantillon.right", "Quadrat.right")) -> matri.EGMP.BASQRED.BM.BF_FS

EGMP.BASQRED.BM.BF_FS <- matri.EGMP.BASQRED.BM.BF_FS[, c("Site.Year.Month.Day",  grep("dist.", names(matri.EGMP.BASQRED.BM.BF_FS), value=TRUE))]

# here below we create as many dfs as they are statistics to calculate

moy.list <- vector("list", length(unique(EGMP.BASQRED.BM.BF_FS$Site.Year.Month.Day)))
et.list <- vector("list", length(unique(EGMP.BASQRED.BM.BF_FS$Site.Year.Month.Day)))
nb.list <- vector("list", length(unique(EGMP.BASQRED.BM.BF_FS$Site.Year.Month.Day)))

for (i in c(1:length(unique(EGMP.BASQRED.BM.BF_FS$Site.Year.Month.Day)))) {
  
  EGMP.BASQRED.BM.BF_FS  %>% filter(Site.Year.Month.Day == unique(EGMP.BASQRED.BM.BF_FS$Site.Year.Month.Day)[[i]]) -> EGMP.BASQRED.BM.BF_FS.i
  
  moy.list[[i]]  <- lapply(EGMP.BASQRED.BM.BF_FS.i[,2:ncol(EGMP.BASQRED.BM.BF_FS.i)], mean, na.rm = TRUE)
  
  et.list[[i]] <- lapply(EGMP.BASQRED.BM.BF_FS.i[,2:ncol(EGMP.BASQRED.BM.BF_FS.i)], sd, na.rm = TRUE)
  
  nb.list[[i]] <- apply(EGMP.BASQRED.BM.BF_FS.i[,2:ncol(EGMP.BASQRED.BM.BF_FS.i)], 2, function(x) { length(which(!is.na(x))) } )
  
  #EGMP.BASQRED.BM.BF_FS.med <- lapply(EGMP.BASQRED.BM.BF_FS.i[,2:ncol(EGMP.BASQRED.BM.BF_FS.i)], median)
  #EGMP.BASQRED.BM.BF_FS.min <- lapply(EGMP.BASQRED.BM.BF_FS.i[,2:ncol(EGMP.BASQRED.BM.BF_FS.i)], min)
  #EGMP.BASQRED.BM.BF_FS.max <- lapply(EGMP.BASQRED.BM.BF_FS.i[,2:ncol(EGMP.BASQRED.BM.BF_FS.i)], max)
  
}

moy. <- data.frame(do.call("rbind", moy.list))
for(i in c(1:ncol(moy.))){
  moy.[[i]] <- unlist(moy.[[i]])
}
moy. <- add_column(moy., unique(EGMP.BASQRED.BM.BF_FS["Site.Year.Month.Day"]), .before = "dist.")

names(moy.)[2:ncol(moy.)]

# and do the same if other dfs eg. below sd and nb are needed
et. <- data.frame(do.call("rbind", et.list))
nb. <- data.frame(do.call("rbind", nb.list))

# here below we work on moy. data; but don't forget we also have the sd, and we can calculate median, min, max etc.

ratio <- (moy.[, 2] - moy.[, 3:ncol(moy.)]) / moy.[, 2] 
ratio <- add_column(ratio, unique(moy.["Site.Year.Month.Day"]), .before = names(ratio[1]))
ratio <- add_column(ratio, moy.["dist."], .after = "Site.Year.Month.Day")

par(mfrow = c(1,1))
par(mar=c(15,5,1,1))
boxplot((abs(ratio[, c(2:length(ratio))])), xaxt = "n", ylab = "rapport dist. BM.BF_FS EGMP.BASQ")
tick <- seq_along(ratio[, c(2:length(ratio))])
axis(1, at = tick, labels = FALSE)
text(tick, par("usr")[3] - 0.45, names(ratio[, c(2:length(ratio))]), srt = 90, xpd = T)

par(mfrow = c(1,1))
par(mar=c(15,5,1,1))
boxplot(log10(abs(ratio[, c(2:length(ratio))])+1), xaxt = "n", ylab = "log10(rapport dist. BM.BF_FS EGMP.BASQ + 1)")
tick <- seq_along(ratio[, c(2:length(ratio))])
axis(1, at = tick, labels = FALSE)
text(tick, par("usr")[3] - 0.06, names(ratio[, c(2:length(ratio))]), srt = 90, xpd = T)

box.BM.BF_FS.EGMP.BASQ <- boxplot((abs(ratio[, c(2:length(ratio))])))
box.stats.BM.BF_FS.EGMP.BASQ <- data.frame(box.BM.BF_FS.EGMP.BASQ$stats)
colnames(box.stats.BM.BF_FS.EGMP.BASQ) <- box.BM.BF_FS.EGMP.BASQ$names
rownames(box.stats.BM.BF_FS.EGMP.BASQ) <- c("Min", "First Quartile", "Median", "Third Quartile", "Maximum")
box.stats.BM.BF_FS.EGMP.BASQ <- data.frame(t(box.stats.BM.BF_FS.EGMP.BASQ))
box.stats.BM.BF_FS.EGMP.BASQ <- box.stats.BM.BF_FS.EGMP.BASQ[with(box.stats.BM.BF_FS.EGMP.BASQ, order(-Median, -Third.Quartile)),][2:nrow(box.stats.BM.BF_FS.EGMP.BASQ),]
median.BM.BF_FS.EGMP.BASQ<- rownames(filter(box.stats.BM.BF_FS.EGMP.BASQ, Median > 0))
Third.Quartile.BM.BF_FS.EGMP.BASQ<- rownames(filter(box.stats.BM.BF_FS.EGMP.BASQ, Third.Quartile > 0))


# Full dataset BM_FS.FI


filter(qecb.ivr, Type.Bloc == "Bloc mobile") -> data.
matri.list <- vector("list", length(unique(data.$Site.Year.Month.Day)))

par(mfrow=c(1,1))

for(i in c(1:length(c(Bret_EGMP.BASQ_qecb, EGMP.BASQ_qecb)))) {
  
  matri.fct.BMM(data = data., conca = c(Bret_EGMP.BASQ_qecb, EGMP.BASQ_qecb)[-i])
  
  names(matri.df.)[names(matri.df.) == 'dist.'] <- paste0("dist.", c(Bret_EGMP.BASQ_qecb, EGMP.BASQ_qecb)[i])
  
  assign(paste0("matri.fullRED.BM_FS.FI", ".", c(Bret_EGMP.BASQ_qecb, EGMP.BASQ_qecb)[i]), matri.df., envir = .GlobalEnv)
  
  rm(matri.df., i)
  
}

par(mfrow=c(1,1))

require(purrr)
#matri.full.BM_FS.FI.saved <- matri.full.BM_FS.FI
#matri.full.BM_FS.FI <- matri.full.BM_FS.FI.saved
names(matri.full.BM_FS.FI) = gsub(pattern = "BM_FS.FI_", replacement = "", x = names(matri.full.BM_FS.FI))

Pattern1 <- c("matri.full.BM_FS.FI", grep("fullRED.BM_FS.FI", names(.GlobalEnv), value = TRUE))
#rm("fullRED.BM_FS.FI", "matri.fullRED.BM_FS.FI", "fullRED.BM_FS.FI.i")
do.call("list", mget(Pattern1)) %>% reduce(full_join, by = c("Site.Year.Month.Day", "name.", "name.left", "name.right", "Type.Bloc.left", "Face.left", "Numéro.Bloc.échantillon.left", "Quadrat.left", "Type.Bloc.right", "Face.right", "Numéro.Bloc.échantillon.right", "Quadrat.right")) -> matri.fullRED.BM_FS.FI
fullRED.BM_FS.FI <- matri.fullRED.BM_FS.FI[, c("Site.Year.Month.Day",  grep("dist.", names(matri.fullRED.BM_FS.FI), value=TRUE))]

# here below we create as many dfs as they are statistics to calculate

moy.list <- vector("list", length(unique(fullRED.BM_FS.FI$Site.Year.Month.Day)))
et.list <- vector("list", length(unique(fullRED.BM_FS.FI$Site.Year.Month.Day)))
nb.list <- vector("list", length(unique(fullRED.BM_FS.FI$Site.Year.Month.Day)))

for (i in c(1:length(unique(fullRED.BM_FS.FI$Site.Year.Month.Day)))) {
  
  fullRED.BM_FS.FI  %>% filter(Site.Year.Month.Day == unique(fullRED.BM_FS.FI$Site.Year.Month.Day)[[i]]) -> fullRED.BM_FS.FI.i
  
  moy.list[[i]]  <- lapply(fullRED.BM_FS.FI.i[,2:ncol(fullRED.BM_FS.FI.i)], mean, na.rm = TRUE)
  
  et.list[[i]] <- lapply(fullRED.BM_FS.FI.i[,2:ncol(fullRED.BM_FS.FI.i)], sd, na.rm = TRUE)
  
  nb.list[[i]] <- apply(fullRED.BM_FS.FI.i[,2:ncol(fullRED.BM_FS.FI.i)], 2, function(x) { length(which(!is.na(x))) } )
  
  #fullRED.BM_FS.FI.med <- lapply(fullRED.BM_FS.FI.i[,2:ncol(fullRED.BM_FS.FI.i)], median)
  #fullRED.BM_FS.FI.min <- lapply(fullRED.BM_FS.FI.i[,2:ncol(fullRED.BM_FS.FI.i)], min)
  #fullRED.BM_FS.FI.max <- lapply(dfullRED.BM_FS.FI.i[,2:ncol(fullRED.BM_FS.FI.i)], max)
  
}

moy. <- data.frame(do.call("rbind", moy.list))
for(i in c(1:ncol(moy.))){
  moy.[[i]] <- unlist(moy.[[i]])
}
moy. <- add_column(moy., unique(fullRED.BM_FS.FI["Site.Year.Month.Day"]), .before = "dist.")

names(moy.)[2:ncol(moy.)]

# and do the same if other dfs eg. below sd and nb are needed
et. <- data.frame(do.call("rbind", et.list))
nb. <- data.frame(do.call("rbind", nb.list))

# here below we work on moy. data; but don't forget we also have the sd, and we can calculate median, min, max etc.

ratio <- (moy.[, 2] - moy.[, 3:ncol(moy.)]) / moy.[, 2] 
ratio <- add_column(ratio, unique(moy.["Site.Year.Month.Day"]), .before = names(ratio[1]))
ratio <- add_column(ratio, moy.["dist."], .after = "Site.Year.Month.Day")

par(mfrow = c(1,1))
par(mar=c(15,5,1,1))
boxplot((abs(ratio[, c(2:length(ratio))])), xaxt = "n", ylab = "rapport dist. BM_FS.FI Atlantique")
tick <- seq_along(ratio[, c(2:length(ratio))])
axis(1, at = tick, labels = FALSE)
text(tick, par("usr")[3] - 0.11, names(ratio[, c(2:length(ratio))]), srt = 90, xpd = T)

par(mfrow = c(1,1))
par(mar=c(15,5,1,1))
boxplot(log10(abs(ratio[, c(2:length(ratio))])+1), xaxt = "n", ylab = "log10(rapport dist. BM_FS.FI Atlantique + 1)")
tick <- seq_along(ratio[, c(2:length(ratio))])
axis(1, at = tick, labels = FALSE)
text(tick, par("usr")[3] - 0.10, names(ratio[, c(2:length(ratio))]), srt = 90, xpd = T)

box.BM_FS.FI.Atlantique <- boxplot((abs(ratio[, c(2:length(ratio))])))
box.stats.BM_FS.FI.Atlantique <- data.frame(box.BM_FS.FI.Atlantique$stats)
colnames(box.stats.BM_FS.FI.Atlantique) <- box.BM_FS.FI.Atlantique$names
rownames(box.stats.BM_FS.FI.Atlantique) <- c("Min", "First Quartile", "Median", "Third Quartile", "Maximum")
box.stats.BM_FS.FI.Atlantique <- data.frame(t(box.stats.BM_FS.FI.Atlantique))
box.stats.BM_FS.FI.Atlantique <- box.stats.BM_FS.FI.Atlantique[with(box.stats.BM_FS.FI.Atlantique, order(-Median, -Third.Quartile)),][2:nrow(box.stats.BM_FS.FI.Atlantique),]
median.BM_FS.FI.Atlantique<- rownames(filter(box.stats.BM_FS.FI.Atlantique, Median > 0))
Third.Quartile.BM_FS.FI.Atlantique<- rownames(filter(box.stats.BM_FS.FI.Atlantique, Third.Quartile > 0))


# Bretagne dataset BM_FS.FI


filter(qecb.ivr, Region == "Bretagne", Type.Bloc == "Bloc mobile") -> data.
matri.list <- vector("list", length(unique(data.$Site.Year.Month.Day)))

par(mfrow=c(1,1))

for(i in c(1:length(c(Bret_EGMP.BASQ_qecb)))) { #, EGMP.BASQ_qecb
  
  matri.fct.BMM(data = data., conca = c(Bret_EGMP.BASQ_qecb)[-i]) #, EGMP.BASQ_qecb
  
  names(matri.df.)[names(matri.df.) == 'dist.'] <- paste0("dist.", c(Bret_EGMP.BASQ_qecb)[i]) #, EGMP.BASQ_qecb
  
  assign(paste0("matri.BretagneRED.BM_FS.FI", ".", c(Bret_EGMP.BASQ_qecb)[i]), matri.df., envir = .GlobalEnv) #, EGMP.BASQ_qecb
  
  rm(matri.df., i)
  
}

par(mfrow=c(1,1))

require(purrr)
#matri.Bretagne.BM_FS.FI.saved <- matri.Bretagne.BM_FS.FI
#matri.Bretagne.BM_FS.FI <- matri.Bretagne.BM_FS.FI.saved
names(matri.Bretagne.BM_FS.FI) = gsub(pattern = "BM_FS.FI_", replacement = "", x = names(matri.Bretagne.BM_FS.FI))

Pattern1 <- c("matri.Bretagne.BM_FS.FI", grep("BretagneRED.BM_FS.FI", names(.GlobalEnv), value = TRUE))
#rm("BretagneRED.BM_FS.FI", "matri.BretagneRED.BM_FS.FI", "BretagneRED.BM_FS.FI.i")
do.call("list", mget(Pattern1)) %>% reduce(full_join, by = c("Site.Year.Month.Day", "name.", "name.left", "name.right", "Type.Bloc.left", "Face.left", "Numéro.Bloc.échantillon.left", "Quadrat.left", "Type.Bloc.right", "Face.right", "Numéro.Bloc.échantillon.right", "Quadrat.right")) -> matri.BretagneRED.BM_FS.FI
BretagneRED.BM_FS.FI <- matri.BretagneRED.BM_FS.FI[, c("Site.Year.Month.Day",  grep("dist.", names(matri.BretagneRED.BM_FS.FI), value=TRUE))]

# here below we create as many dfs as they are statistics to calculate

moy.list <- vector("list", length(unique(BretagneRED.BM_FS.FI$Site.Year.Month.Day)))
et.list <- vector("list", length(unique(BretagneRED.BM_FS.FI$Site.Year.Month.Day)))
nb.list <- vector("list", length(unique(BretagneRED.BM_FS.FI$Site.Year.Month.Day)))

for (i in c(1:length(unique(BretagneRED.BM_FS.FI$Site.Year.Month.Day)))) {
  
  BretagneRED.BM_FS.FI  %>% filter(Site.Year.Month.Day == unique(BretagneRED.BM_FS.FI$Site.Year.Month.Day)[[i]]) -> BretagneRED.BM_FS.FI.i
  
  moy.list[[i]]  <- lapply(BretagneRED.BM_FS.FI.i[,2:ncol(BretagneRED.BM_FS.FI.i)], mean, na.rm = TRUE)
  
  et.list[[i]] <- lapply(BretagneRED.BM_FS.FI.i[,2:ncol(BretagneRED.BM_FS.FI.i)], sd, na.rm = TRUE)
  
  nb.list[[i]] <- apply(BretagneRED.BM_FS.FI.i[,2:ncol(BretagneRED.BM_FS.FI.i)], 2, function(x) { length(which(!is.na(x))) } )
  
  #BretagneRED.BM_FS.FI.med <- lapply(BretagneRED.BM_FS.FI.i[,2:ncol(BretagneRED.BM_FS.FI.i)], median)
  #BretagneRED.BM_FS.FI.min <- lapply(BretagneRED.BM_FS.FI.i[,2:ncol(BretagneRED.BM_FS.FI.i)], min)
  #BretagneRED.BM_FS.FI.max <- lapply(dBretagneRED.BM_FS.FI.i[,2:ncol(BretagneRED.BM_FS.FI.i)], max)
  
}

moy. <- data.frame(do.call("rbind", moy.list))
for(i in c(1:ncol(moy.))){
  moy.[[i]] <- unlist(moy.[[i]])
}
moy. <- add_column(moy., unique(BretagneRED.BM_FS.FI["Site.Year.Month.Day"]), .before = "dist.")

names(moy.)[2:ncol(moy.)]

# and do the same if other dfs eg. below sd and nb are needed
et. <- data.frame(do.call("rbind", et.list))
nb. <- data.frame(do.call("rbind", nb.list))

# here below we work on moy. data; but don't forget we also have the sd, and we can calculate median, min, max etc.

ratio <- (moy.[, 2] - moy.[, 3:ncol(moy.)]) / moy.[, 2] 
ratio <- add_column(ratio, unique(moy.["Site.Year.Month.Day"]), .before = names(ratio[1]))
ratio <- add_column(ratio, moy.["dist."], .after = "Site.Year.Month.Day")

par(mfrow = c(1,1))
par(mar=c(15,5,1,1))
boxplot((abs(ratio[, c(2:length(ratio))])), xaxt = "n", ylab = "rapport dist. BM_FS.FI Bretagne")
tick <- seq_along(ratio[, c(2:length(ratio))])
axis(1, at = tick, labels = FALSE)
text(tick, par("usr")[3] - 0.11, names(ratio[, c(2:length(ratio))]), srt = 90, xpd = T)

par(mfrow = c(1,1))
par(mar=c(15,5,1,1))
boxplot(log10(abs(ratio[, c(2:length(ratio))])+1), xaxt = "n", ylab = "log10(rapport dist. BM_FS.FI Bretagne + 1)")
tick <- seq_along(ratio[, c(2:length(ratio))])
axis(1, at = tick, labels = FALSE)
text(tick, par("usr")[3] - 0.11, names(ratio[, c(2:length(ratio))]), srt = 90, xpd = T)

box.BM_FS.FI.Bretagne <- boxplot((abs(ratio[, c(2:length(ratio))])))
box.stats.BM_FS.FI.Bretagne <- data.frame(box.BM_FS.FI.Bretagne$stats)
colnames(box.stats.BM_FS.FI.Bretagne) <- box.BM_FS.FI.Bretagne$names
rownames(box.stats.BM_FS.FI.Bretagne) <- c("Min", "First Quartile", "Median", "Third Quartile", "Maximum")
box.stats.BM_FS.FI.Bretagne <- data.frame(t(box.stats.BM_FS.FI.Bretagne))
box.stats.BM_FS.FI.Bretagne <- box.stats.BM_FS.FI.Bretagne[with(box.stats.BM_FS.FI.Bretagne, order(-Median, -Third.Quartile)),][2:nrow(box.stats.BM_FS.FI.Bretagne),]
median.BM_FS.FI.Bretagne<- rownames(filter(box.stats.BM_FS.FI.Bretagne, Median > 0))
Third.Quartile.BM_FS.FI.Bretagne<- rownames(filter(box.stats.BM_FS.FI.Bretagne, Third.Quartile > 0))


# EGMP.BASQ dataset BM_FS.FI


filter(qecb.ivr, Region == "EGMP.BASQ", Type.Bloc == "Bloc mobile") -> data.
matri.list <- vector("list", length(unique(data.$Site.Year.Month.Day)))

par(mfrow=c(1,1))

for(i in c(1:length(c(Bret_EGMP.BASQ_qecb, EGMP.BASQ_qecb)))) {
  
  matri.fct.BMM(data = data., conca = c(Bret_EGMP.BASQ_qecb, EGMP.BASQ_qecb)[-i])
  
  names(matri.df.)[names(matri.df.) == 'dist.'] <- paste0("dist.", c(Bret_EGMP.BASQ_qecb, EGMP.BASQ_qecb)[i])
  
  assign(paste0("matri.EGMP.BASQRED.BM_FS.FI", ".", c(Bret_EGMP.BASQ_qecb, EGMP.BASQ_qecb)[i]), matri.df., envir = .GlobalEnv)
  
  rm(matri.df., i)
  
}

par(mfrow=c(1,1))

require(purrr)
#matri.EGMP.BASQ.BM_FS.FI.saved <- matri.EGMP.BASQ.BM_FS.FI
#matri.EGMP.BASQ.BM_FS.FI <- matri.EGMP.BASQ.BM_FS.FI.saved
names(matri.EGMP.BASQ.BM_FS.FI) = gsub(pattern = "BM_FS.FI_", replacement = "", x = names(matri.EGMP.BASQ.BM_FS.FI))

Pattern1 <- c("matri.EGMP.BASQ.BM_FS.FI", grep("EGMP.BASQRED.BM_FS.FI", names(.GlobalEnv), value = TRUE))
#rm("EGMP.BASQRED.BM_FS.FI", "matri.EGMP.BASQRED.BM_FS.FI", "EGMP.BASQRED.BM_FS.FI.i")
do.call("list", mget(Pattern1)) %>% reduce(full_join, by = c("Site.Year.Month.Day", "name.", "name.left", "name.right", "Type.Bloc.left", "Face.left", "Numéro.Bloc.échantillon.left", "Quadrat.left", "Type.Bloc.right", "Face.right", "Numéro.Bloc.échantillon.right", "Quadrat.right")) -> matri.EGMP.BASQRED.BM_FS.FI
EGMP.BASQRED.BM_FS.FI <- matri.EGMP.BASQRED.BM_FS.FI[, c("Site.Year.Month.Day",  grep("dist.", names(matri.EGMP.BASQRED.BM_FS.FI), value=TRUE))]

# here below we create as many dfs as they are statistics to calculate

moy.list <- vector("list", length(unique(EGMP.BASQRED.BM_FS.FI$Site.Year.Month.Day)))
et.list <- vector("list", length(unique(EGMP.BASQRED.BM_FS.FI$Site.Year.Month.Day)))
nb.list <- vector("list", length(unique(EGMP.BASQRED.BM_FS.FI$Site.Year.Month.Day)))

for (i in c(1:length(unique(EGMP.BASQRED.BM_FS.FI$Site.Year.Month.Day)))) {
  
  EGMP.BASQRED.BM_FS.FI  %>% filter(Site.Year.Month.Day == unique(EGMP.BASQRED.BM_FS.FI$Site.Year.Month.Day)[[i]]) -> EGMP.BASQRED.BM_FS.FI.i
  
  moy.list[[i]]  <- lapply(EGMP.BASQRED.BM_FS.FI.i[,2:ncol(EGMP.BASQRED.BM_FS.FI.i)], mean, na.rm = TRUE)
  
  et.list[[i]] <- lapply(EGMP.BASQRED.BM_FS.FI.i[,2:ncol(EGMP.BASQRED.BM_FS.FI.i)], sd, na.rm = TRUE)
  
  nb.list[[i]] <- apply(EGMP.BASQRED.BM_FS.FI.i[,2:ncol(EGMP.BASQRED.BM_FS.FI.i)], 2, function(x) { length(which(!is.na(x))) } )
  
  #EGMP.BASQRED.BM_FS.FI.med <- lapply(EGMP.BASQRED.BM_FS.FI.i[,2:ncol(EGMP.BASQRED.BM_FS.FI.i)], median)
  #EGMP.BASQRED.BM_FS.FI.min <- lapply(EGMP.BASQRED.BM_FS.FI.i[,2:ncol(EGMP.BASQRED.BM_FS.FI.i)], min)
  #EGMP.BASQRED.BM_FS.FI.max <- lapply(dEGMP.BASQRED.BM_FS.FI.i[,2:ncol(EGMP.BASQRED.BM_FS.FI.i)], max)
  
}

moy. <- data.frame(do.call("rbind", moy.list))
for(i in c(1:ncol(moy.))){
  moy.[[i]] <- unlist(moy.[[i]])
}
moy. <- add_column(moy., unique(EGMP.BASQRED.BM_FS.FI["Site.Year.Month.Day"]), .before = "dist.")

names(moy.)[2:ncol(moy.)]

# and do the same if other dfs eg. below sd and nb are needed
et. <- data.frame(do.call("rbind", et.list))
nb. <- data.frame(do.call("rbind", nb.list))

# here below we work on moy. data; but don't forget we also have the sd, and we can calculate median, min, max etc.

ratio <- (moy.[, 2] - moy.[, 3:ncol(moy.)]) / moy.[, 2] 
ratio <- add_column(ratio, unique(moy.["Site.Year.Month.Day"]), .before = names(ratio[1]))
ratio <- add_column(ratio, moy.["dist."], .after = "Site.Year.Month.Day")

par(mfrow = c(1,1))
par(mar=c(15,5,1,1))
boxplot((abs(ratio[, c(2:length(ratio))])), xaxt = "n", ylab = "rapport dist. BM_FS.FI EGMP.BASQ")
tick <- seq_along(ratio[, c(2:length(ratio))])
axis(1, at = tick, labels = FALSE)
text(tick, par("usr")[3] - 0.11, names(ratio[, c(2:length(ratio))]), srt = 90, xpd = T)

par(mfrow = c(1,1))
par(mar=c(15,5,1,1))
boxplot(log10(abs(ratio[, c(2:length(ratio))])+1), xaxt = "n", ylab = "log10(rapport dist. BM_FS.FI EGMP.BASQ + 1)")
tick <- seq_along(ratio[, c(2:length(ratio))])
axis(1, at = tick, labels = FALSE)
text(tick, par("usr")[3] - 0.10, names(ratio[, c(2:length(ratio))]), srt = 90, xpd = T)

box.BM_FS.FI.EGMP.BASQ <- boxplot((abs(ratio[, c(2:length(ratio))])))
box.stats.BM_FS.FI.EGMP.BASQ <- data.frame(box.BM_FS.FI.EGMP.BASQ$stats)
colnames(box.stats.BM_FS.FI.EGMP.BASQ) <- box.BM_FS.FI.EGMP.BASQ$names
rownames(box.stats.BM_FS.FI.EGMP.BASQ) <- c("Min", "First Quartile", "Median", "Third Quartile", "Maximum")
box.stats.BM_FS.FI.EGMP.BASQ <- data.frame(t(box.stats.BM_FS.FI.EGMP.BASQ))
box.stats.BM_FS.FI.EGMP.BASQ <- box.stats.BM_FS.FI.EGMP.BASQ[with(box.stats.BM_FS.FI.EGMP.BASQ, order(-Median, -Third.Quartile)),][2:nrow(box.stats.BM_FS.FI.EGMP.BASQ),]
median.BM_FS.FI.EGMP.BASQ<- rownames(filter(box.stats.BM_FS.FI.EGMP.BASQ, Median > 0))
Third.Quartile.BM_FS.FI.EGMP.BASQ<- rownames(filter(box.stats.BM_FS.FI.EGMP.BASQ, Third.Quartile > 0))


# Which taxa are important ...

median.BM.BF_FS.Atlantique
median.BM.BF_FS.Bretagne
median.BM.BF_FS.EGMP.BASQ
(median.BM.BF_FS <- unique(c(median.BM.BF_FS.Atlantique, median.BM.BF_FS.Bretagne, median.BM.BF_FS.EGMP.BASQ)))
c(Bret_EGMP.BASQ_qecb, EGMP.BASQ_qecb)
setdiff(c(Bret_EGMP.BASQ_qecb, EGMP.BASQ_qecb), gsub("dist.", "", unique(c(median.BM.BF_FS.Atlantique, median.BM.BF_FS.Bretagne, median.BM.BF_FS.EGMP.BASQ))))

Third.Quartile.BM.BF_FS.Atlantique
Third.Quartile.BM.BF_FS.Bretagne
Third.Quartile.BM.BF_FS.EGMP.BASQ
(Third.Quartile.BM.BF_FS <- unique(c(Third.Quartile.BM.BF_FS.Atlantique, Third.Quartile.BM.BF_FS.Bretagne, Third.Quartile.BM.BF_FS.EGMP.BASQ)))
setdiff(c(Bret_EGMP.BASQ_qecb, EGMP.BASQ_qecb), gsub("dist.", "", unique(c(Third.Quartile.BM.BF_FS.Atlantique, Third.Quartile.BM.BF_FS.Bretagne, Third.Quartile.BM.BF_FS.EGMP.BASQ))))

median.BM_FS.FI.Atlantique
median.BM_FS.FI.Bretagne
median.BM_FS.FI.EGMP.BASQ
(median.BM_FS.FI <- unique(c(median.BM_FS.FI.Atlantique, median.BM_FS.FI.Bretagne, median.BM_FS.FI.EGMP.BASQ)))
c(Bret_EGMP.BASQ_qecb, EGMP.BASQ_qecb)
setdiff(c(Bret_EGMP.BASQ_qecb, EGMP.BASQ_qecb), gsub("dist.", "", unique(c(median.BM_FS.FI.Atlantique, median.BM_FS.FI.Bretagne, median.BM_FS.FI.EGMP.BASQ))))

Third.Quartile.BM_FS.FI.Atlantique
Third.Quartile.BM_FS.FI.Bretagne
Third.Quartile.BM_FS.FI.EGMP.BASQ
(Third.Quartile.BM_FS.FI <- unique(c(Third.Quartile.BM_FS.FI.Atlantique, Third.Quartile.BM_FS.FI.Bretagne, Third.Quartile.BM_FS.FI.EGMP.BASQ)))
setdiff(c(Bret_EGMP.BASQ_qecb, EGMP.BASQ_qecb), gsub("dist.", "", unique(c(Third.Quartile.BM_FS.FI.Atlantique, Third.Quartile.BM_FS.FI.Bretagne, Third.Quartile.BM_FS.FI.EGMP.BASQ))))


# prepare the final fullRED df for saving, just like previously for above dist df.

matri.fullRED.BM.BF_FS <- separate_(matri.fullRED.BM.BF_FS, "Site.Year.Month.Day", into = c("departement", "Site", "Year", "Month", "Day"), remove = F)
matri.fullRED.BM.BF_FS$Site <- paste0(matri.fullRED.BM.BF_FS$departement, "_", matri.fullRED.BM.BF_FS$Site)
matri.fullRED.BM.BF_FS <- subset(matri.fullRED.BM.BF_FS, select = - c(departement)) 
matri.fullRED.BM.BF_FS <- add_column(matri.fullRED.BM.BF_FS, Date = as.Date(paste0(matri.fullRED.BM.BF_FS$Year, "-", matri.fullRED.BM.BF_FS$Month, "-", matri.fullRED.BM.BF_FS$Day), origin = "1970-01-01"), .after = "Site")
matri.fullRED.BM.BF_FS$Site <- as.factor(matri.fullRED.BM.BF_FS$Site)

matri.fullRED.BM_FS.FI <- separate_(matri.fullRED.BM_FS.FI, "Site.Year.Month.Day", into = c("departement", "Site", "Year", "Month", "Day"), remove = F)
matri.fullRED.BM_FS.FI$Site <- paste0(matri.fullRED.BM_FS.FI$departement, "_", matri.fullRED.BM_FS.FI$Site)
matri.fullRED.BM_FS.FI <- subset(matri.fullRED.BM_FS.FI, select = - c(departement)) 
matri.fullRED.BM_FS.FI <- add_column(matri.fullRED.BM_FS.FI, Date = as.Date(paste0(matri.fullRED.BM_FS.FI$Year, "-", matri.fullRED.BM_FS.FI$Month, "-", matri.fullRED.BM_FS.FI$Day), origin = "1970-01-01"), .after = "Site")
matri.fullRED.BM_FS.FI$Site <- as.factor(matri.fullRED.BM_FS.FI$Site)

# issue with type de bloc, numéro de bloc and quadrat for df. BM.BF_FS, cfr left vs right doesn't give the right combination.
matri.fullRED.BM.BF_FS$Quadrat <- NA
for (i in c(1:nrow(matri.fullRED.BM.BF_FS))) { 
  ifelse(matri.fullRED.BM.BF_FS$Type.Bloc.left[i] == "Bloc mobile", matri.fullRED.BM.BF_FS$Quadrat[i] <- matri.fullRED.BM.BF_FS$Quadrat.left[i], matri.fullRED.BM.BF_FS$Quadrat[i] <- matri.fullRED.BM.BF_FS$Quadrat.right[i]) }
matri.fullRED.BM.BF_FS$Numéro.Bloc <- NA
for (i in c(1:nrow(matri.fullRED.BM.BF_FS))) { 
  ifelse(matri.fullRED.BM.BF_FS$Type.Bloc.left[i] == "Bloc mobile", matri.fullRED.BM.BF_FS$Numéro.Bloc[i] <- matri.fullRED.BM.BF_FS$Numéro.Bloc.échantillon.left[i], matri.fullRED.BM.BF_FS$Numéro.Bloc[i] <- matri.fullRED.BM.BF_FS$Numéro.Bloc.échantillon.right[i]) }

matri.fullRED.BM.BF_FS <- add_column(matri.fullRED.BM.BF_FS, Site.Year.Month.Day.Q.BMnb = paste0(matri.fullRED.BM.BF_FS$Site.Year.Month.Day, "_",  matri.fullRED.BM.BF_FS$Quadrat, "_", matri.fullRED.BM.BF_FS$Numéro.Bloc), .after = "Site.Year.Month.Day")
matri.fullRED.BM_FS.FI <- add_column(matri.fullRED.BM_FS.FI, Site.Year.Month.Day.Q.BMnb = paste0(matri.fullRED.BM_FS.FI$Site.Year.Month.Day, "_",  matri.fullRED.BM_FS.FI$Quadrat.left, "_", matri.fullRED.BM_FS.FI$Numéro.Bloc.échantillon.left), .after = "Site.Year.Month.Day")

colnames(matri.fullRED.BM.BF_FS) <- paste("BM.BF_FS", colnames(matri.fullRED.BM.BF_FS), sep = "_")
matri.fullRED.BM.BF_FS <- rename(matri.fullRED.BM.BF_FS, Site.Year.Month.Day.Q.BMnb = BM.BF_FS_Site.Year.Month.Day.Q.BMnb )
colnames(matri.fullRED.BM_FS.FI) <- paste("BM_FS.FI", colnames(matri.fullRED.BM_FS.FI), sep = "_")
matri.fullRED.BM_FS.FI <- rename(matri.fullRED.BM_FS.FI, Site.Year.Month.Day.Q.BMnb = BM_FS.FI_Site.Year.Month.Day.Q.BMnb )

matri.fullRED <- full_join(matri.fullRED.BM.BF_FS[, c("Site.Year.Month.Day.Q.BMnb", grep("BM.BF_FS.dist.", names(matri.fullRED.BM.BF_FS), value=TRUE))], matri.fullRED.BM_FS.FI[, c("Site.Year.Month.Day.Q.BMnb", grep("BM_FS.FI.dist.", names(matri.fullRED.BM_FS.FI), value=TRUE))], by = "Site.Year.Month.Day.Q.BMnb")

matri.fullRED <- separate_(matri.fullRED, "Site.Year.Month.Day.Q.BMnb", into = c("departement", "Site", "Year", "Month", "Day", "Quadrat", "Bloc Mobile Number"), remove = F)
matri.fullRED$Site <- paste0(matri.fullRED$departement, "_", matri.fullRED$Site)
matri.fullRED <- subset(matri.fullRED, select = - c(departement)) 
matri.fullRED <- add_column(matri.fullRED, Date = as.Date(paste0(matri.fullRED$Year, "-", matri.fullRED$Month, "-", matri.fullRED$Day), origin = "1970-01-01"), .after = "Site")

#saveRDS(matri.fullRED, "results/Ecology/matri.fullRED.RDS")
saveRDS(matri.fullRED, "results/Ecology/matri.fullRED_log.spi.RDS")


# New dissi dist by reducing the number of taxa

library("combinat")  
# unactivated because otherwise my computer freezes
c(Bret_EGMP.BASQ_qecb, EGMP.BASQ_qecb)
#cbi <- unlist(lapply(1:length(c(Bret_EGMP.BASQ_qecb, EGMP.BASQ_qecb)),
#                                 combinat::combn, 
#                                 x = c(Bret_EGMP.BASQ_qecb, EGMP.BASQ_qecb),
#                                 simplify = FALSE), 
#                          recursive = FALSE)
#cbi    
#rm(cbi)
# just not possible to test them all ...


## recalculate dissi. distances


# Full dataset BM.BF_FS

filter(qecb.ivr, Face == "face supérieure") -> data.
data.$Type.Bloc <- ifelse(as.character(data.$Type.Bloc) == "Roche en place", "Bloc fixé", as.character(data.$Type.Bloc))
matri.list <- vector("list", length(unique(data.$Site.Year.Month.Day)))
matri.fct.BMF(data = data., conca = gsub("dist.", "", median.BM.BF_FS))
matri.full.median.BM.BF_FS <- matri.df. ; rm(matri.df.)
matri.fct.BMF(data = data., conca = gsub("dist.", "", Third.Quartile.BM.BF_FS))
matri.full.Third.Quartile.BM.BF_FS <- matri.df. ; rm(matri.df.)

matri.full.median.BM.BF_FS <- rename(matri.full.median.BM.BF_FS, matri.full.median.BM.BF_FS_dist. = dist.)
matri.full.Third.Quartile.BM.BF_FS <- rename(matri.full.Third.Quartile.BM.BF_FS, matri.full.Third.Quartile.BM.BF_FS_dist. = dist.)


# Bretagne dataset BM.BF_FS

filter(qecb.ivr, Region == "Bretagne", Face == "face supérieure") -> data.
data.$Type.Bloc <- ifelse(as.character(data.$Type.Bloc) == "Roche en place", "Bloc fixé", as.character(data.$Type.Bloc))
matri.list <- vector("list", length(unique(data.$Site.Year.Month.Day)))
matri.fct.BMF(data = data., conca = gsub("dist.", "", median.BM.BF_FS.Bretagne))
matri.Bretagne.median.BM.BF_FS <- matri.df. ; rm(matri.df.)
matri.fct.BMF(data = data., conca = gsub("dist.", "", Third.Quartile.BM.BF_FS.Bretagne))
matri.Bretagne.Third.Quartile.BM.BF_FS <- matri.df. ; rm(matri.df.)

matri.Bretagne.median.BM.BF_FS <- rename(matri.Bretagne.median.BM.BF_FS, matri.Bretagne.median.BM.BF_FS_dist. = dist.)
matri.Bretagne.Third.Quartile.BM.BF_FS <- rename(matri.Bretagne.Third.Quartile.BM.BF_FS, matri.Bretagne.Third.Quartile.BM.BF_FS_dist. = dist.)


# EGMP.BASQ dataset BM.BF_FS

filter(qecb.ivr, Region == "EGMP.BASQ", Face == "face supérieure") -> data.
data.$Type.Bloc <- ifelse(as.character(data.$Type.Bloc) == "Roche en place", "Bloc fixé", as.character(data.$Type.Bloc))
matri.list <- vector("list", length(unique(data.$Site.Year.Month.Day)))
matri.fct.BMF(data = data., conca = gsub("dist.", "", median.BM.BF_FS.EGMP.BASQ))
matri.EGMP.BASQ.median.BM.BF_FS <- matri.df. ; rm(matri.df.)
matri.fct.BMF(data = data., conca = gsub("dist.", "", Third.Quartile.BM.BF_FS.EGMP.BASQ))
matri.EGMP.BASQ.Third.Quartile.BM.BF_FS <- matri.df. ; rm(matri.df.)

matri.EGMP.BASQ.median.BM.BF_FS <- rename(matri.EGMP.BASQ.median.BM.BF_FS, matri.EGMP.BASQ.median.BM.BF_FS_dist. = dist.)
matri.EGMP.BASQ.Third.Quartile.BM.BF_FS <- rename(matri.EGMP.BASQ.Third.Quartile.BM.BF_FS, matri.EGMP.BASQ.Third.Quartile.BM.BF_FS_dist. = dist.)


# Full dataset BM_FS.FI

filter(qecb.ivr, Type.Bloc == "Bloc mobile") -> data.
matri.list <- vector("list", length(unique(data.$Site.Year.Month.Day)))
matri.fct.BMM(data = data., conca = gsub("dist.", "", median.BM_FS.FI))
matri.full.median.BM_FS.FI <- matri.df. ; rm(matri.df.)
matri.fct.BMM(data = data., conca = gsub("dist.", "", Third.Quartile.BM_FS.FI))
matri.full.Third.Quartile.BM_FS.FI <- matri.df. ; rm(matri.df.)

matri.full.median.BM_FS.FI <- rename(matri.full.median.BM_FS.FI, matri.full.median.BM_FS.FI_dist. = dist.)
matri.full.Third.Quartile.BM_FS.FI <- rename(matri.full.Third.Quartile.BM_FS.FI, matri.full.Third.Quartile.BM_FS.FI_dist. = dist.)


# Bretagne dataset BM_FS.FI

filter(qecb.ivr, Region == "Bretagne", Type.Bloc == "Bloc mobile") -> data.
matri.list <- vector("list", length(unique(data.$Site.Year.Month.Day)))
matri.fct.BMM(data = data., conca = gsub("dist.", "", median.BM_FS.FI.Bretagne))
matri.Bretagne.median.BM_FS.FI <- matri.df. ; rm(matri.df.)
matri.fct.BMM(data = data., conca = gsub("dist.", "", Third.Quartile.BM_FS.FI.Bretagne))
matri.Bretagne.Third.Quartile.BM_FS.FI <- matri.df. ; rm(matri.df.)

matri.Bretagne.median.BM_FS.FI <- rename(matri.Bretagne.median.BM_FS.FI, matri.Bretagne.median.BM_FS.FI_dist. = dist.)
matri.Bretagne.Third.Quartile.BM_FS.FI <- rename(matri.Bretagne.Third.Quartile.BM_FS.FI, matri.Bretagne.Third.Quartile.BM_FS.FI_dist. = dist.)


# EGMP.BASQ dataset BM_FS.FI

filter(qecb.ivr, Region == "EGMP.BASQ", Type.Bloc == "Bloc mobile") -> data.
matri.list <- vector("list", length(unique(data.$Site.Year.Month.Day)))
matri.fct.BMM(data = data., conca = gsub("dist.", "", median.BM_FS.FI.EGMP.BASQ))
matri.EGMP.BASQ.median.BM_FS.FI <- matri.df. ; rm(matri.df.)
matri.fct.BMM(data = data., conca = gsub("dist.", "", Third.Quartile.BM_FS.FI.EGMP.BASQ))
matri.EGMP.BASQ.Third.Quartile.BM_FS.FI <- matri.df. ; rm(matri.df.)

matri.EGMP.BASQ.median.BM_FS.FI <- rename(matri.EGMP.BASQ.median.BM_FS.FI, matri.EGMP.BASQ.median.BM_FS.FI_dist. = dist.)
matri.EGMP.BASQ.Third.Quartile.BM_FS.FI <- rename(matri.EGMP.BASQ.Third.Quartile.BM_FS.FI, matri.EGMP.BASQ.Third.Quartile.BM_FS.FI_dist. = dist.)


# join them all, one by one for check

matri.red.BM.BF_FS <- full_join(matri.full.median.BM.BF_FS, matri.full.Third.Quartile.BM.BF_FS, by = c("Site.Year.Month.Day", "name.", "name.left", "name.right", "Type.Bloc.left", "Face.left", "Numéro.Bloc.échantillon.left", "Quadrat.left", "Type.Bloc.right", "Face.right", "Numéro.Bloc.échantillon.right", "Quadrat.right"))
matri.red.BM.BF_FS <- full_join(matri.red.BM.BF_FS, matri.Bretagne.median.BM.BF_FS, by = c("Site.Year.Month.Day", "name.", "name.left", "name.right", "Type.Bloc.left", "Face.left", "Numéro.Bloc.échantillon.left", "Quadrat.left", "Type.Bloc.right", "Face.right", "Numéro.Bloc.échantillon.right", "Quadrat.right"))
matri.red.BM.BF_FS <- full_join(matri.red.BM.BF_FS, matri.Bretagne.Third.Quartile.BM.BF_FS, by = c("Site.Year.Month.Day", "name.", "name.left", "name.right", "Type.Bloc.left", "Face.left", "Numéro.Bloc.échantillon.left", "Quadrat.left", "Type.Bloc.right", "Face.right", "Numéro.Bloc.échantillon.right", "Quadrat.right"))
matri.red.BM.BF_FS <- full_join(matri.red.BM.BF_FS, matri.EGMP.BASQ.median.BM.BF_FS, by = c("Site.Year.Month.Day", "name.", "name.left", "name.right", "Type.Bloc.left", "Face.left", "Numéro.Bloc.échantillon.left", "Quadrat.left", "Type.Bloc.right", "Face.right", "Numéro.Bloc.échantillon.right", "Quadrat.right"))
matri.red.BM.BF_FS <- full_join(matri.red.BM.BF_FS, matri.EGMP.BASQ.Third.Quartile.BM.BF_FS, by = c("Site.Year.Month.Day", "name.", "name.left", "name.right", "Type.Bloc.left", "Face.left", "Numéro.Bloc.échantillon.left", "Quadrat.left", "Type.Bloc.right", "Face.right", "Numéro.Bloc.échantillon.right", "Quadrat.right"))

matri.red.BM_FS.FI <- full_join(matri.full.median.BM_FS.FI, matri.full.Third.Quartile.BM_FS.FI, by = c("Site.Year.Month.Day", "name.", "name.left", "name.right", "Type.Bloc.left", "Face.left", "Numéro.Bloc.échantillon.left", "Quadrat.left", "Type.Bloc.right", "Face.right", "Numéro.Bloc.échantillon.right", "Quadrat.right"))
matri.red.BM_FS.FI <- full_join(matri.red.BM_FS.FI, matri.Bretagne.median.BM_FS.FI, by = c("Site.Year.Month.Day", "name.", "name.left", "name.right", "Type.Bloc.left", "Face.left", "Numéro.Bloc.échantillon.left", "Quadrat.left", "Type.Bloc.right", "Face.right", "Numéro.Bloc.échantillon.right", "Quadrat.right"))
matri.red.BM_FS.FI <- full_join(matri.red.BM_FS.FI, matri.Bretagne.Third.Quartile.BM_FS.FI, by = c("Site.Year.Month.Day", "name.", "name.left", "name.right", "Type.Bloc.left", "Face.left", "Numéro.Bloc.échantillon.left", "Quadrat.left", "Type.Bloc.right", "Face.right", "Numéro.Bloc.échantillon.right", "Quadrat.right"))
matri.red.BM_FS.FI <- full_join(matri.red.BM_FS.FI, matri.EGMP.BASQ.median.BM_FS.FI, by = c("Site.Year.Month.Day", "name.", "name.left", "name.right", "Type.Bloc.left", "Face.left", "Numéro.Bloc.échantillon.left", "Quadrat.left", "Type.Bloc.right", "Face.right", "Numéro.Bloc.échantillon.right", "Quadrat.right"))
matri.red.BM_FS.FI <- full_join(matri.red.BM_FS.FI, matri.EGMP.BASQ.Third.Quartile.BM_FS.FI, by = c("Site.Year.Month.Day", "name.", "name.left", "name.right", "Type.Bloc.left", "Face.left", "Numéro.Bloc.échantillon.left", "Quadrat.left", "Type.Bloc.right", "Face.right", "Numéro.Bloc.échantillon.right", "Quadrat.right"))


# prepare these final median and Third.QUartile dfs for saving, just like previously for above dist df.

library(tidyr)
library(tibble)

matri.red.BM.BF_FS <- separate_(matri.red.BM.BF_FS, "Site.Year.Month.Day", into = c("departement", "Site", "Year", "Month", "Day"), remove = F)
matri.red.BM.BF_FS$Site <- paste0(matri.red.BM.BF_FS$departement, "_", matri.red.BM.BF_FS$Site)
matri.red.BM.BF_FS <- subset(matri.red.BM.BF_FS, select = - c(departement)) 
matri.red.BM.BF_FS <- add_column(matri.red.BM.BF_FS, Date = as.Date(paste0(matri.red.BM.BF_FS$Year, "-", matri.red.BM.BF_FS$Month, "-", matri.red.BM.BF_FS$Day), origin = "1970-01-01"), .after = "Site")
matri.red.BM.BF_FS$Site <- as.factor(matri.red.BM.BF_FS$Site)

matri.red.BM_FS.FI <- separate_(matri.red.BM_FS.FI, "Site.Year.Month.Day", into = c("departement", "Site", "Year", "Month", "Day"), remove = F)
matri.red.BM_FS.FI$Site <- paste0(matri.red.BM_FS.FI$departement, "_", matri.red.BM_FS.FI$Site)
matri.red.BM_FS.FI <- subset(matri.red.BM_FS.FI, select = - c(departement)) 
matri.red.BM_FS.FI <- add_column(matri.red.BM_FS.FI, Date = as.Date(paste0(matri.red.BM_FS.FI$Year, "-", matri.red.BM_FS.FI$Month, "-", matri.red.BM_FS.FI$Day), origin = "1970-01-01"), .after = "Site")
matri.red.BM_FS.FI$Site <- as.factor(matri.red.BM_FS.FI$Site)

# issue with type de bloc, numéro de bloc and quadrat for df. BM.BF_FS, cfr left vs right doesn't give the right combination.
matri.red.BM.BF_FS$Quadrat <- NA
for (i in c(1:nrow(matri.red.BM.BF_FS))) { 
  ifelse(matri.red.BM.BF_FS$Type.Bloc.left[i] == "Bloc mobile", matri.red.BM.BF_FS$Quadrat[i] <- matri.red.BM.BF_FS$Quadrat.left[i], matri.red.BM.BF_FS$Quadrat[i] <- matri.red.BM.BF_FS$Quadrat.right[i]) }
matri.red.BM.BF_FS$Numéro.Bloc <- NA
for (i in c(1:nrow(matri.red.BM.BF_FS))) { 
  ifelse(matri.red.BM.BF_FS$Type.Bloc.left[i] == "Bloc mobile", matri.red.BM.BF_FS$Numéro.Bloc[i] <- matri.red.BM.BF_FS$Numéro.Bloc.échantillon.left[i], matri.red.BM.BF_FS$Numéro.Bloc[i] <- matri.red.BM.BF_FS$Numéro.Bloc.échantillon.right[i]) }

matri.red.BM.BF_FS <- add_column(matri.red.BM.BF_FS, Site.Year.Month.Day.Q.BMnb = paste0(matri.red.BM.BF_FS$Site.Year.Month.Day, "_",  matri.red.BM.BF_FS$Quadrat, "_", matri.red.BM.BF_FS$Numéro.Bloc), .after = "Site.Year.Month.Day")
matri.red.BM_FS.FI <- add_column(matri.red.BM_FS.FI, Site.Year.Month.Day.Q.BMnb = paste0(matri.red.BM_FS.FI$Site.Year.Month.Day, "_",  matri.red.BM_FS.FI$Quadrat.left, "_", matri.red.BM_FS.FI$Numéro.Bloc.échantillon.left), .after = "Site.Year.Month.Day")

colnames(matri.red.BM.BF_FS) <- paste("BM.BF_FS", colnames(matri.red.BM.BF_FS), sep = "_")
matri.red.BM.BF_FS <- rename(matri.red.BM.BF_FS, Site.Year.Month.Day.Q.BMnb = BM.BF_FS_Site.Year.Month.Day.Q.BMnb )
colnames(matri.red.BM_FS.FI) <- paste("BM_FS.FI", colnames(matri.red.BM_FS.FI), sep = "_")
matri.red.BM_FS.FI <- rename(matri.red.BM_FS.FI, Site.Year.Month.Day.Q.BMnb = BM_FS.FI_Site.Year.Month.Day.Q.BMnb )

matri.red <- full_join(matri.red.BM.BF_FS[, c("Site.Year.Month.Day.Q.BMnb", "BM.BF_FS_matri.full.median.BM.BF_FS_dist.", "BM.BF_FS_matri.full.Third.Quartile.BM.BF_FS_dist.", "BM.BF_FS_matri.Bretagne.median.BM.BF_FS_dist.", "BM.BF_FS_matri.Bretagne.Third.Quartile.BM.BF_FS_dist.", "BM.BF_FS_matri.EGMP.BASQ.median.BM.BF_FS_dist.", "BM.BF_FS_matri.EGMP.BASQ.Third.Quartile.BM.BF_FS_dist.")], matri.red.BM_FS.FI[, c("Site.Year.Month.Day.Q.BMnb", "BM_FS.FI_matri.full.median.BM_FS.FI_dist.", "BM_FS.FI_matri.full.Third.Quartile.BM_FS.FI_dist.", "BM_FS.FI_matri.Bretagne.median.BM_FS.FI_dist.", "BM_FS.FI_matri.Bretagne.Third.Quartile.BM_FS.FI_dist.", "BM_FS.FI_matri.EGMP.BASQ.median.BM_FS.FI_dist.", "BM_FS.FI_matri.EGMP.BASQ.Third.Quartile.BM_FS.FI_dist.")])

matri.red <- separate_(matri.red, "Site.Year.Month.Day.Q.BMnb", into = c("departement", "Site", "Year", "Month", "Day", "Quadrat", "Bloc Mobile Number"), remove = F)
matri.red$Site <- paste0(matri.red$departement, "_", matri.red$Site)
matri.red <- subset(matri.red, select = - c(departement)) 
matri.red <- add_column(matri.red, Date = as.Date(paste0(matri.red$Year, "-", matri.red$Month, "-", matri.red$Day), origin = "1970-01-01"), .after = "Site")

library(data.table)
setDT(matri.red)[, .SD[.N >1], Site.Year.Month.Day.Q.BMnb]
setDT(matri.fullRED)[, .SD[.N >1], Site.Year.Month.Day.Q.BMnb]

matri.fullREDred <- full_join(matri.fullRED, matri.red, by = c("Site.Year.Month.Day.Q.BMnb", "Site", "Date", "Year", "Month", "Day", "Quadrat", "Bloc Mobile Number"))

library(data.table)
setDT(matri.fullREDred)[, .SD[.N >1], Site.Year.Month.Day.Q.BMnb]

matri.fullREDred <- add_column(matri.fullREDred, BM_FS.FI_dist.inv. = (1-matri.fullREDred$BM_FS.FI_dist.), .after = "BM_FS.FI_dist.")

names(matri.fullREDred)
nrow(matri.fullREDred)

BM.BF_dist. <- na.omit(matri.fullREDred[, c("Site.Year.Month.Day.Q.BMnb", "Site", "Date", "Year", "Month", "Day", "Quadrat", "Bloc Mobile Number", "BM.BF_FS_dist.", "BM_FS.FI_dist.")])

hist(BM.BF_dist.$BM_FS.FI_dist.)
hist(BM.BF_dist.$BM.BF_FS_dist.)
#BM.BF_dist.$BF.norm.BM_dist. <- BM.BF_dist.$BM.BF_FS_dist./(BM.BF_dist.$BM_FS.FI_dist.+1)*100
BM.BF_dist.$BF.norm.BM_dist. <- (BM.BF_dist.$BM.BF_FS_dist.+1)/(BM.BF_dist.$BM_FS.FI_dist.+1)
hist(BM.BF_dist.$BF.norm.BM_dist.)
#BM.BF_dist.$BM.norm.BF_dist. <- BM.BF_dist.$BM_FS.FI_dist./(BM.BF_dist.$BM.BF_FS_dist.+1)*100
#hist(BM.BF_dist.$BM.norm.BF_dist.)
BM.BF_dist.$BM.norm.BF_dist. <- (BM.BF_dist.$BM_FS.FI_dist.+1)/(BM.BF_dist.$BM.BF_FS_dist.+1)
hist(BM.BF_dist.$BM.norm.BF_dist.)
#BM.BF_dist.$BM.sum.BF_dist. <- (BM.BF_dist.$BM.BF_FS_dist. + (1-BM.BF_dist.$BM_FS.FI_dist.))/2
#BM.BF_dist.$BM.sum.BF_dist.
BM.BF_dist.$BM.sum.BF_dist. <- ((1-BM.BF_dist.$BM.BF_FS_dist.) + BM.BF_dist.$BM_FS.FI_dist.)/2
hist(BM.BF_dist.$BM.sum.BF_dist.)
BM.BF_dist.$BM.prod.BF_dist. <- (1-BM.BF_dist.$BM.BF_FS_dist.) * BM.BF_dist.$BM_FS.FI_dist.
hist(BM.BF_dist.$BM.prod.BF_dist.)
BM.BF_dist.$BM.subnorm2.BF_dist. <- ((BM.BF_dist.$BM_FS.FI_dist.+1) - (BM.BF_dist.$BM.BF_FS_dist.+1)) / (BM.BF_dist.$BM.BF_FS_dist.+1)
hist(BM.BF_dist.$BM.subnorm2.BF_dist.)
BM.BF_dist.$BM.abssubnorm2.BF_dist. <- abs((BM.BF_dist.$BM_FS.FI_dist.+1) - (BM.BF_dist.$BM.BF_FS_dist.+1)) / (BM.BF_dist.$BM.BF_FS_dist.+1)
hist(BM.BF_dist.$BM.abssubnorm2.BF_dist.)
BM.BF_dist.$BM.subnorm1.BF_dist. <- ((BM.BF_dist.$BM_FS.FI_dist.) - (BM.BF_dist.$BM.BF_FS_dist.+1)) / (BM.BF_dist.$BM.BF_FS_dist.+1)
hist(BM.BF_dist.$BM.subnorm1.BF_dist.)
BM.BF_dist.$BM.subnorm0.BF_dist. <- ((BM.BF_dist.$BM_FS.FI_dist.) - (BM.BF_dist.$BM.BF_FS_dist.)) / (BM.BF_dist.$BM.BF_FS_dist.)
hist(BM.BF_dist.$BM.subnorm0.BF_dist.)

sort(BM.BF_dist.$BF.norm.BM_dist.)[1:25] ; sort(BM.BF_dist.$BF.norm.BM_dist., decreasing = T)[1:25]
sort(BM.BF_dist.$BM.norm.BF_dist.)[1:25] ; sort(BM.BF_dist.$BM.norm.BF_dist., decreasing = T)[1:25]
sort(BM.BF_dist.$BM.sum.BF_dist.)[1:25] ; sort(BM.BF_dist.$BM.sum.BF_dist., decreasing = T)[1:25]
sort(BM.BF_dist.$BM.prod.BF_dist.)[1:25] ; sort(BM.BF_dist.$BM.prod.BF_dist., decreasing = T)[1:25]
sort(BM.BF_dist.$BM.subnorm2.BF_dist.)[1:25] ; sort(BM.BF_dist.$BM.subnorm2.BF_dist., decreasing = T)[1:25]
sort(BM.BF_dist.$BM.abssubnorm2.BF_dist.)[1:25] ; sort(BM.BF_dist.$BM.abssubnorm2.BF_dist., decreasing = T)[1:25]
sort(BM.BF_dist.$BM.subnorm1.BF_dist.)[1:25] ; sort(BM.BF_dist.$BM.subnorm1.BF_dist., decreasing = T)[1:25]
sort(BM.BF_dist.$BM.subnorm0.BF_dist.)[1:25] ; sort(BM.BF_dist.$BM.subnorm0.BF_dist., decreasing = T)[1:25]


BM.BF_median_dist. <- na.omit(matri.fullREDred[, c("Site.Year.Month.Day.Q.BMnb", "Site", "Date", "Year", "Month", "Day", "Quadrat", "Bloc Mobile Number", "BM.BF_FS_matri.full.median.BM.BF_FS_dist.", "BM_FS.FI_matri.full.median.BM_FS.FI_dist.")])

hist(BM.BF_median_dist.$BM_FS.FI_matri.full.median.BM_FS.FI_dist.)
hist(BM.BF_median_dist.$BM.BF_FS_matri.full.median.BM.BF_FS_dist.)
#BM.BF_median_dist.$BF.norm.BM_median_dist. <- BM.BF_dist.$BM.BF_FS_matri.full.median.BM.BF_FS_dist./(BM.BF_dist.$BM_FS.FI_matri.full.median.BM_FS.FI_dist.+1)*100
#hist(BM.BF_median_dist.$BF.norm.BM_median_dist.)
BM.BF_median_dist.$BF.norm.BM_median_dist. <- (BM.BF_median_dist.$BM.BF_FS_matri.full.median.BM.BF_FS_dist.+1)/(BM.BF_median_dist.$BM_FS.FI_matri.full.median.BM_FS.FI_dist.+1)
hist(BM.BF_median_dist.$BF.norm.BM_median_dist.)
#BM.BF_median_dist.$BM.norm.BF_median_dist. <- BM.BF_median_dist.$BM_FS.FI_matri.full.median.BM_FS.FI_dist./(BM.BF_median_dist.$BM.BF_FS_matri.full.median.BM.BF_FS_dist.+1)*100
#hist(BM.BF_median_dist.$BM.norm.BF_median_dist.)
BM.BF_median_dist.$BM.norm.BF_median_dist. <- (BM.BF_median_dist.$BM_FS.FI_matri.full.median.BM_FS.FI_dist.+1)/(BM.BF_median_dist.$BM.BF_FS_matri.full.median.BM.BF_FS_dist.+1)
hist(BM.BF_median_dist.$BM.norm.BF_median_dist.)
#BM.BF_median_dist.$BM.sum.BF_median_dist. <- (BM.BF_median_dist.$BM.BF_FS_matri.full.median.BM.BF_FS_dist. + (1-BM.BF_median_dist.$BM_FS.FI_matri.full.median.BM_FS.FI_dist.))/2
#BM.BF_median_dist.$BM.sum.BF_median_dist.
BM.BF_median_dist.$BM.sum.BF_median_dist. <- ((1-BM.BF_median_dist.$BM.BF_FS_matri.full.median.BM.BF_FS_dist.) + BM.BF_median_dist.$BM_FS.FI_matri.full.median.BM_FS.FI_dist.)/2
hist(BM.BF_median_dist.$BM.sum.BF_median_dist.)
BM.BF_median_dist.$BM.prod.BF_median_dist. <- (1-BM.BF_median_dist.$BM.BF_FS_matri.full.median.BM.BF_FS_dist.) * BM.BF_median_dist.$BM_FS.FI_matri.full.median.BM_FS.FI_dist.
hist(BM.BF_median_dist.$BM.prod.BF_median_dist.)
BM.BF_median_dist.$BM.subnorm2.BF_median_dist. <- ((BM.BF_median_dist.$BM_FS.FI_matri.full.median.BM_FS.FI_dist.+1) - (BM.BF_median_dist.$BM.BF_FS_matri.full.median.BM.BF_FS_dist.+1)) / (BM.BF_median_dist.$BM.BF_FS_matri.full.median.BM.BF_FS_dist.+1)
hist(BM.BF_median_dist.$BM.subnorm2.BF_median_dist.)
BM.BF_median_dist.$BM.abssubnorm2.BF_median_dist. <- abs((BM.BF_median_dist.$BM_FS.FI_matri.full.median.BM_FS.FI_dist.+1) - (BM.BF_median_dist.$BM.BF_FS_matri.full.median.BM.BF_FS_dist.+1)) / (BM.BF_median_dist.$BM.BF_FS_matri.full.median.BM.BF_FS_dist.+1)
hist(BM.BF_median_dist.$BM.abssubnorm2.BF_median_dist.)
BM.BF_median_dist.$BM.subnorm1.BF_median_dist. <- ((BM.BF_median_dist.$BM_FS.FI_matri.full.median.BM_FS.FI_dist.) - (BM.BF_median_dist.$BM.BF_FS_matri.full.median.BM.BF_FS_dist.+1)) / (BM.BF_median_dist.$BM.BF_FS_matri.full.median.BM.BF_FS_dist.+1)
hist(BM.BF_median_dist.$BM.subnorm1.BF_median_dist.)
BM.BF_median_dist.$BM.subnorm0.BF_median_dist. <- ((BM.BF_median_dist.$BM_FS.FI_matri.full.median.BM_FS.FI_dist.) - (BM.BF_median_dist.$BM.BF_FS_matri.full.median.BM.BF_FS_dist.)) / (BM.BF_median_dist.$BM.BF_FS_matri.full.median.BM.BF_FS_dist.)
hist(BM.BF_median_dist.$BM.subnorm0.BF_median_dist.)

sort(BM.BF_median_dist.$BF.norm.BM_median_dist.)[1:25] ; sort(BM.BF_median_dist.$BF.norm.BM_median_dist., decreasing = T)[1:25]
sort(BM.BF_median_dist.$BM.norm.BF_median_dist.)[1:25] ; sort(BM.BF_median_dist.$BM.norm.BF_median_dist., decreasing = T)[1:25]
sort(BM.BF_median_dist.$BM.sum.BF_median_dist.)[1:25] ; sort(BM.BF_median_dist.$BM.sum.BF_median_dist., decreasing = T)[1:25]
sort(BM.BF_median_dist.$BM.prod.BF_median_dist.)[1:25] ; sort(BM.BF_median_dist.$BM.prod.BF_median_dist., decreasing = T)[1:25]
sort(BM.BF_median_dist.$BM.subnorm2.BF_median_dist.)[1:25] ; sort(BM.BF_median_dist.$BM.subnorm2.BF_median_dist., decreasing = T)[1:25]
sort(BM.BF_median_dist.$BM.abssubnorm2.BF_median_dist.)[1:25] ; sort(BM.BF_median_dist.$BM.abssubnorm2.BF_median_dist., decreasing = T)[1:25]
sort(BM.BF_median_dist.$BM.subnorm1.BF_median_dist.)[1:25] ; sort(BM.BF_median_dist.$BM.subnorm1.BF_median_dist., decreasing = T)[1:25]
sort(BM.BF_median_dist.$BM.subnorm0.BF_median_dist.)[1:25] ; sort(BM.BF_median_dist.$BM.subnorm0.BF_median_dist., decreasing = T)[1:25]

BM.BF_Third.Quartile_dist. <- na.omit(matri.fullREDred[, c("Site.Year.Month.Day.Q.BMnb", "Site", "Date", "Year", "Month", "Day", "Quadrat", "Bloc Mobile Number", "BM.BF_FS_matri.full.Third.Quartile.BM.BF_FS_dist.", "BM_FS.FI_matri.full.Third.Quartile.BM_FS.FI_dist.")])

hist(BM.BF_Third.Quartile_dist.$BM_FS.FI_matri.full.Third.Quartile.BM_FS.FI_dist.)
hist(BM.BF_Third.Quartile_dist.$BM.BF_FS_matri.full.Third.Quartile.BM.BF_FS_dist.)
#BM.BF_Third.Quartile_dist.$BF.norm.BM_Third.Quartile_dist. <- BM.BF_dist.$BM.BF_FS_matri.full.Third.Quartile.BM.BF_FS_dist./(BM.BF_dist.$BM_FS.FI_matri.full.Third.Quartile.BM_FS.FI_dist.+1)*100
#hist(BM.BF_Third.Quartile_dist.$BF.norm.BM_Third.Quartile_dist.)
BM.BF_Third.Quartile_dist.$BF.norm.BM_Third.Quartile_dist. <- (BM.BF_Third.Quartile_dist.$BM.BF_FS_matri.full.Third.Quartile.BM.BF_FS_dist.+1)/(BM.BF_Third.Quartile_dist.$BM_FS.FI_matri.full.Third.Quartile.BM_FS.FI_dist.+1)
hist(BM.BF_Third.Quartile_dist.$BF.norm.BM_Third.Quartile_dist.)
#BM.BF_Third.Quartile_dist.$BM.norm.BF_Third.Quartile_dist. <- BM.BF_Third.Quartile_dist.$BM_FS.FI_matri.full.Third.Quartile.BM_FS.FI_dist./(BM.BF_Third.Quartile_dist.$BM.BF_FS_matri.full.Third.Quartile.BM.BF_FS_dist.+1)*100
#hist(BM.BF_Third.Quartile_dist.$BM.norm.BF_Third.Quartile_dist.)
BM.BF_Third.Quartile_dist.$BM.norm.BF_Third.Quartile_dist. <- (BM.BF_Third.Quartile_dist.$BM_FS.FI_matri.full.Third.Quartile.BM_FS.FI_dist.+1)/(BM.BF_Third.Quartile_dist.$BM.BF_FS_matri.full.Third.Quartile.BM.BF_FS_dist.+1)
hist(BM.BF_Third.Quartile_dist.$BM.norm.BF_Third.Quartile_dist.)
#BM.BF_Third.Quartile_dist.$BM.sum.BF_Third.Quartile_dist. <- (BM.BF_Third.Quartile_dist.$BM.BF_FS_matri.full.Third.Quartile.BM.BF_FS_dist. + (1-BM.BF_Third.Quartile_dist.$BM_FS.FI_matri.full.Third.Quartile.BM_FS.FI_dist.))/2
#BM.BF_Third.Quartile_dist.$BM.sum.BF_Third.Quartile_dist.
BM.BF_Third.Quartile_dist.$BM.sum.BF_Third.Quartile_dist. <- ((1-BM.BF_Third.Quartile_dist.$BM.BF_FS_matri.full.Third.Quartile.BM.BF_FS_dist.) + BM.BF_Third.Quartile_dist.$BM_FS.FI_matri.full.Third.Quartile.BM_FS.FI_dist.)/2
hist(BM.BF_Third.Quartile_dist.$BM.sum.BF_Third.Quartile_dist.)
BM.BF_Third.Quartile_dist.$BM.prod.BF_Third.Quartile_dist. <- (1-BM.BF_Third.Quartile_dist.$BM.BF_FS_matri.full.Third.Quartile.BM.BF_FS_dist.) * BM.BF_Third.Quartile_dist.$BM_FS.FI_matri.full.Third.Quartile.BM_FS.FI_dist.
hist(BM.BF_Third.Quartile_dist.$BM.prod.BF_Third.Quartile_dist.)
BM.BF_Third.Quartile_dist.$BM.subnorm2.BF_Third.Quartile_dist. <- ((BM.BF_Third.Quartile_dist.$BM_FS.FI_matri.full.Third.Quartile.BM_FS.FI_dist.+1) - (BM.BF_Third.Quartile_dist.$BM.BF_FS_matri.full.Third.Quartile.BM.BF_FS_dist.+1)) / (BM.BF_Third.Quartile_dist.$BM.BF_FS_matri.full.Third.Quartile.BM.BF_FS_dist.+1)
hist(BM.BF_Third.Quartile_dist.$BM.subnorm2.BF_Third.Quartile_dist.)
BM.BF_Third.Quartile_dist.$BM.abssubnorm2.BF_Third.Quartile_dist. <- abs((BM.BF_Third.Quartile_dist.$BM_FS.FI_matri.full.Third.Quartile.BM_FS.FI_dist.+1) - (BM.BF_Third.Quartile_dist.$BM.BF_FS_matri.full.Third.Quartile.BM.BF_FS_dist.+1)) / (BM.BF_Third.Quartile_dist.$BM.BF_FS_matri.full.Third.Quartile.BM.BF_FS_dist.+1)
hist(BM.BF_Third.Quartile_dist.$BM.abssubnorm2.BF_Third.Quartile_dist.)
BM.BF_Third.Quartile_dist.$BM.subnorm1.BF_Third.Quartile_dist. <- ((BM.BF_Third.Quartile_dist.$BM_FS.FI_matri.full.Third.Quartile.BM_FS.FI_dist.) - (BM.BF_Third.Quartile_dist.$BM.BF_FS_matri.full.Third.Quartile.BM.BF_FS_dist.+1)) / (BM.BF_Third.Quartile_dist.$BM.BF_FS_matri.full.Third.Quartile.BM.BF_FS_dist.+1)
hist(BM.BF_Third.Quartile_dist.$BM.subnorm1.BF_Third.Quartile_dist.)
BM.BF_Third.Quartile_dist.$BM.subnorm0.BF_Third.Quartile_dist. <- ((BM.BF_Third.Quartile_dist.$BM_FS.FI_matri.full.Third.Quartile.BM_FS.FI_dist.) - (BM.BF_Third.Quartile_dist.$BM.BF_FS_matri.full.Third.Quartile.BM.BF_FS_dist.)) / (BM.BF_Third.Quartile_dist.$BM.BF_FS_matri.full.Third.Quartile.BM.BF_FS_dist.)
hist(BM.BF_Third.Quartile_dist.$BM.subnorm0.BF_Third.Quartile_dist.)

sort(BM.BF_Third.Quartile_dist.$BF.norm.BM_Third.Quartile_dist.)[1:25] ; sort(BM.BF_Third.Quartile_dist.$BF.norm.BM_Third.Quartile_dist., decreasing = T)[1:25]
sort(BM.BF_Third.Quartile_dist.$BM.norm.BF_Third.Quartile_dist.)[1:25] ; sort(BM.BF_Third.Quartile_dist.$BM.norm.BF_Third.Quartile_dist., decreasing = T)[1:25]
sort(BM.BF_Third.Quartile_dist.$BM.sum.BF_Third.Quartile_dist.)[1:25] ; sort(BM.BF_Third.Quartile_dist.$BM.sum.BF_Third.Quartile_dist., decreasing = T)[1:25]
sort(BM.BF_Third.Quartile_dist.$BM.prod.BF_Third.Quartile_dist.)[1:25] ; sort(BM.BF_Third.Quartile_dist.$BM.prod.BF_Third.Quartile_dist., decreasing = T)[1:25]
sort(BM.BF_Third.Quartile_dist.$BM.subnorm2.BF_Third.Quartile_dist.)[1:25] ; sort(BM.BF_Third.Quartile_dist.$BM.subnorm2.BF_Third.Quartile_dist., decreasing = T)[1:25]
sort(BM.BF_Third.Quartile_dist.$BM.abssubnorm2.BF_Third.Quartile_dist.)[1:25] ; sort(BM.BF_Third.Quartile_dist.$BM.abssubnorm2.BF_Third.Quartile_dist., decreasing = T)[1:25]
sort(BM.BF_Third.Quartile_dist.$BM.subnorm1.BF_Third.Quartile_dist.)[1:25] ; sort(BM.BF_Third.Quartile_dist.$BM.subnorm1.BF_Third.Quartile_dist., decreasing = T)[1:25]
sort(BM.BF_Third.Quartile_dist.$BM.subnorm0.BF_Third.Quartile_dist.)[1:25] ; sort(BM.BF_Third.Quartile_dist.$BM.subnorm0.BF_Third.Quartile_dist., decreasing = T)[1:25]


matri.fullREDred <- left_join(matri.fullREDred, BM.BF_dist.[, c("Site.Year.Month.Day.Q.BMnb", "Site", "Date", "Year", "Month", "Day", "Quadrat", "Bloc Mobile Number", "BF.norm.BM_dist.", "BM.norm.BF_dist.", "BM.sum.BF_dist.", "BM.prod.BF_dist.", "BM.subnorm2.BF_dist.", "BM.abssubnorm2.BF_dist.", "BM.subnorm1.BF_dist.", "BM.subnorm0.BF_dist.")], by = c("Site.Year.Month.Day.Q.BMnb", "Site", "Date", "Year", "Month", "Day", "Quadrat", "Bloc Mobile Number"))
matri.fullREDred <- left_join(matri.fullREDred, BM.BF_median_dist.[, c("Site.Year.Month.Day.Q.BMnb", "Site", "Date", "Year", "Month", "Day", "Quadrat", "Bloc Mobile Number", "BF.norm.BM_median_dist.", "BM.norm.BF_median_dist.", "BM.sum.BF_median_dist.", "BM.prod.BF_median_dist.", "BM.subnorm2.BF_median_dist.", "BM.abssubnorm2.BF_median_dist.", "BM.subnorm1.BF_median_dist.", "BM.subnorm0.BF_median_dist.")], by = c("Site.Year.Month.Day.Q.BMnb", "Site", "Date", "Year", "Month", "Day", "Quadrat", "Bloc Mobile Number"))
matri.fullREDred <- left_join(matri.fullREDred, BM.BF_Third.Quartile_dist.[, c("Site.Year.Month.Day.Q.BMnb", "Site", "Date", "Year", "Month", "Day", "Quadrat", "Bloc Mobile Number", "BF.norm.BM_Third.Quartile_dist.", "BM.norm.BF_Third.Quartile_dist.", "BM.sum.BF_Third.Quartile_dist.", "BM.prod.BF_Third.Quartile_dist.", "BM.subnorm2.BF_Third.Quartile_dist.", "BM.abssubnorm2.BF_Third.Quartile_dist.", "BM.subnorm1.BF_Third.Quartile_dist.", "BM.subnorm0.BF_Third.Quartile_dist.")], by = c("Site.Year.Month.Day.Q.BMnb", "Site", "Date", "Year", "Month", "Day", "Quadrat", "Bloc Mobile Number"))

pairs2(na.omit(matri.fullREDred[, c("BM.BF_FS_dist.", "BM_FS.FI_dist.", "BM_FS.FI_dist.inv.", "BF.norm.BM_dist.", "BM.norm.BF_dist.", "BM.sum.BF_dist.", "BM.prod.BF_dist.", "BM.subnorm2.BF_dist.", "BM.abssubnorm2.BF_dist.", "BM.subnorm1.BF_dist.", "BM.subnorm0.BF_dist.")]), scaleR = TRUE, Rmethod = 'spearman', reorder = FALSE)

pairs2(na.omit(matri.fullREDred[, c("BM.BF_FS_dist.", "BM_FS.FI_dist.", "BM_FS.FI_dist.inv.", "BF.norm.BM_median_dist.", "BM.norm.BF_median_dist.", "BM.sum.BF_median_dist.", "BM.prod.BF_median_dist.", "BM.subnorm2.BF_median_dist.", "BM.abssubnorm2.BF_median_dist.", "BM.subnorm1.BF_median_dist.", "BM.subnorm0.BF_median_dist.")]), scaleR = TRUE, Rmethod = 'spearman', reorder = FALSE)

pairs2(na.omit(matri.fullREDred[, c("BM.BF_FS_dist.", "BM_FS.FI_dist.", "BM_FS.FI_dist.inv.", "BF.norm.BM_Third.Quartile_dist.", "BM.norm.BF_Third.Quartile_dist.", "BM.sum.BF_Third.Quartile_dist.", "BM.prod.BF_Third.Quartile_dist.", "BM.subnorm2.BF_Third.Quartile_dist.", "BM.abssubnorm2.BF_Third.Quartile_dist.", "BM.subnorm1.BF_Third.Quartile_dist.", "BM.subnorm0.BF_Third.Quartile_dist.")]), scaleR = TRUE, Rmethod = 'spearman', reorder = FALSE)


#saveRDS(matri.fullREDred, "results/Ecology/matri.full.RDS")
saveRDS(matri.fullREDred, "results/Ecology/matri.full_log.spi.RDS")
#matri.full <- readRDS("results/Ecology/matri.full.RDS")
matri.full <- readRDS("results/Ecology/matri.full_log.spi.RDS")


## Diversity index

library(adiv)
# adiv contains two main functions for species diversity indices: speciesdiv, which includes widely used indices such as species richness and the Shannon index, and divparam, which includes indices that have a parameter to control the importance given to rare versus abundant species in diversity measurements (Pavoine (2020) - adiv: An r package to analyse biodiversity in ecology).

# NB: just like for dissimilarity distance matrices, no sense to use the "fishing" variable lists, because either they are present for the bloc mobile and not fo the bloc fixe (therefore false higher diversity for bloc mobile), either they are repeated between face supérieure and face inférieure of bloc mobile.

qecb.ivr.saved <- readRDS("results/Ecology/qecb.ivr.RDS")
qecb.ivr.Env. <- qecb.ivr # the one from above dissi distances determination and present in the Global Env ; some modifications might have been performed later on after saving the qecb.ivr.RDS

library(arsenal)

comparedf(qecb.ivr.Env., qecb.ivr.saved)
comparedf(qecb.ivr.saved, qecb.ivr.Env.)

summary(comparedf(qecb.ivr.Env., qecb.ivr.saved))

qecb.ivr <- qecb.ivr.saved

# log10 spirorbes & Nb.Spirobranchus.lamarckii.total
qecb.ivr <- add_column(qecb.ivr, log10.Nb.spirorbis.total = log10(qecb.ivr$Nb.spirorbis.total+1), .after = "Nb.spirorbis.total")
qecb.ivr <- add_column(qecb.ivr, log10.Nb.Spirobranchus.lamarckii.total = log10(qecb.ivr$Nb.Spirobranchus.lamarckii.total+1), .after = "Nb.Spirobranchus.lamarckii.total")

comparedf(qecb.ivr.Env., qecb.ivr)
summary(comparedf(qecb.ivr.Env., qecb.ivr))

# test

filter(qecb.ivr, Region == "Bretagne" & Site == "GDMO_Locmariaquer") -> Loc.
unique(Loc.$Site.Year.Month.Day)
filter(Loc., Site.Year.Month.Day == "GDMO_Locmariaquer.2016.03.09") -> Loc.

row.names(Loc.) <- c(paste0(Loc.$Region.Site.Year.Month.Day, "_", Loc.$Quadrat.bis, "_", Loc.$Type.Bloc, "_", Loc.$Numéro.Bloc.échantillon, "_", Loc.$Face))

# function speciesdiv for diversity indices that rely on relative or absolute species abundance, including richness 

speciesdiv(Loc.[, c(Bret_EGMP.BASQ_qecb)]) -> Loc.speciesdiv

adiv.df <- data.frame(Loc.speciesdiv)

hist(adiv.df[, 1], xlab = names(adiv.df)[1], main = "Loc. BM.FS.FI_BF")

par(mfrow = c(2,2))

sapply(names(adiv.df[, c(1:ncol(adiv.df))]), 
       function(cname){
         print(hist(adiv.df[, c(1:ncol(adiv.df))][[cname]], main = "", xlab = cname, breaks = length(unique(adiv.df[, c(1:ncol(adiv.df))][[cname]]))))
       })
par(mfrow=c(1,1))

# function divparam for parametric diversity indices; probably better because the parameter controls the relative importance given to rare versus abundant species in a communit

data(batcomm)
ab <- batcomm$ab
par(mfrow = c(2,1))
plot(divparam(ab))
plot(divparam(ab, q=0:4))
par(mfrow = c(1,1))

divparam(Loc.[, c(Bret_EGMP.BASQ_qecb)], q = c(0,0.25,0.5,1,2,4,8)) -> Loc.divparam # When q increases, abundant species are overweighted compared to rare species, we thus expect that the evenness in species weights decreases.
#par(mfrow = c(2,1))
plot(divparam(Loc.[, c(Bret_EGMP.BASQ_qecb)], q = 0), main = unique(Loc.$Site.Year.Month.Day))
plot(divparam(Loc.[, c(Bret_EGMP.BASQ_qecb)], q = 0:10), legend = F, main = unique(Loc.$Site.Year.Month.Day))
#par(mfrow = c(1,1))

adiv.df$x <- Loc.divparam$div$`1`
colnames(adiv.df)[which(colnames(adiv.df) == "x")] <- paste0("Para. ISD, q = ", Loc.divparam$q[1], " (equi. richness)")
adiv.df$x <- Loc.divparam$div$`2`
colnames(adiv.df)[which(colnames(adiv.df) == "x")] <- paste0("Para. ISD, q = ", Loc.divparam$q[2])
adiv.df$x <- Loc.divparam$div$`3`
colnames(adiv.df)[which(colnames(adiv.df) == "x")] <- paste0("Para. ISD, q = ", Loc.divparam$q[3])
adiv.df$x <- Loc.divparam$div$`4`
colnames(adiv.df)[which(colnames(adiv.df) == "x")] <- paste0("Para. ISD, q = ", Loc.divparam$q[4])
adiv.df$x <- Loc.divparam$div$`5`
colnames(adiv.df)[which(colnames(adiv.df) == "x")] <- paste0("Para. ISD, q = ", Loc.divparam$q[5], " (equi. Simpson)")
adiv.df$x <- Loc.divparam$div$`6`
colnames(adiv.df)[which(colnames(adiv.df) == "x")] <- paste0("Para. ISD, q = ", Loc.divparam$q[6])
adiv.df$x <- Loc.divparam$div$`7`
colnames(adiv.df)[which(colnames(adiv.df) == "x")] <- paste0("Para. ISD, q = ", Loc.divparam$q[7])

par(mfrow = c(2,2))
sapply(names(adiv.df[, c(1:ncol(adiv.df))]), 
       function(cname){
         print(hist(adiv.df[, c(1:ncol(adiv.df))][[cname]], main = "", xlab = cname, breaks = length(unique(adiv.df[, c(1:ncol(adiv.df))][[cname]]))))
       })
par(mfrow=c(1,1))

# reorder plots
par(mfrow = c(3,2))
sapply(names(adiv.df[, c(1,8,2,3,12,4:7,9:11,13:ncol(adiv.df))]), 
       function(cname){
         print(hist(adiv.df[, c(1,8,2,3,12,4:7,9:11,13:ncol(adiv.df))][[cname]], main = "", xlab = cname, breaks = length(unique(adiv.df[, c(1,8,2,3,12,4:7,9:11,13:ncol(adiv.df))][[cname]]))))
       })
par(mfrow=c(1,1))


# function in a loop

#qecb.ivr.saved <- qecb.ivr
#qecb.ivr <- qecb.ivr.saved

row.names(qecb.ivr) <- c(paste0(qecb.ivr$Region.Site.Year.Month.Day, "_", qecb.ivr$Quadrat.bis, "_", qecb.ivr$Type.Bloc, "_", qecb.ivr$Numéro.Bloc.échantillon, "_", qecb.ivr$Face))

# later on I can copy-paste above code to recreate variable names vector
Bret_EGMP.BASQ_qecb
EGMP.BASQ_qecb
Bret_EGMP.BASQ_fishing
EGMP.BASQ_fishing

# remove boulder variables
Bret_EGMP.BASQ_qecb <- Bret_EGMP.BASQ_qecb[! Bret_EGMP.BASQ_qecb %in% c("X..Recouvrement.Sediment", "X..Roche.Nue", "X..Surface.Accolement")]

comparedf(qecb.ivr.Env., qecb.ivr)
qecb.ivr$Period <- as.character(qecb.ivr$Period)
qecb.ivr$Face <- as.character(qecb.ivr$Face)
comparedf(qecb.ivr.Env., qecb.ivr)

div.list <- vector("list", length(unique(qecb.ivr$Site.Year.Month.Day)))

for (i in c(1:nrow(qecb.ivr))) {
  
  #i <- 7
  
  filter(qecb.ivr, Site.Year.Month.Day == unique(qecb.ivr$Site.Year.Month.Day)[i]) -> div.i
  
  ifelse(unique(div.i$Region) == "Bretagne", var. <- c(Bret_EGMP.BASQ_qecb), var. <- c(Bret_EGMP.BASQ_qecb, EGMP.BASQ_qecb)) # Qu. : Why can't R's ifelse statements return vectors? => you can circumvent the problem if you assign the result inside the ifelse.
  var.
  
  #8 remove empty row cfr: In speciesdiv(div.i[, var.]) & divparam(div.i[, var.]) : empty communities should be discarded
  div.i <- filter(div.i, rowSums(div.i[, var.]) > 0)
  
  speciesdiv(div.i[, var.]) -> div.i.speciesdiv
  adiv.i.df <- data.frame(div.i.speciesdiv)
  
  divparam(div.i[, var.], q = c(0,0.25,0.5,1,2,4,8)) -> div.i.divparam # When q increases, abundant species are overweighted compared to rare species, we thus expect that the evenness in species weights decreases.
  #par(mfrow = c(2,1))
  par(mfrow=c(1,1))
  plot(divparam(div.i[, var.], q = 0), main = unique(div.i$Site.Year.Month.Day))
  plot(divparam(div.i[, var.], q = 0:10), legend = F, main = unique(div.i$Site.Year.Month.Day))
  #par(mfrow = c(1,1))
  
  adiv.i.df$x <- div.i.divparam$div$`1`
  colnames(adiv.i.df)[which(colnames(adiv.i.df) == "x")] <- paste0("Para. ISD, q = ", div.i.divparam$q[1], " (equi. richness)")
  adiv.i.df$x <- div.i.divparam$div$`2`
  colnames(adiv.i.df)[which(colnames(adiv.i.df) == "x")] <- paste0("Para. ISD, q = ", div.i.divparam$q[2])
  adiv.i.df$x <- div.i.divparam$div$`3`
  colnames(adiv.i.df)[which(colnames(adiv.i.df) == "x")] <- paste0("Para. ISD, q = ", div.i.divparam$q[3])
  adiv.i.df$x <- div.i.divparam$div$`4`
  colnames(adiv.i.df)[which(colnames(adiv.i.df) == "x")] <- paste0("Para. ISD, q = ", div.i.divparam$q[4])
  adiv.i.df$x <- div.i.divparam$div$`5`
  colnames(adiv.i.df)[which(colnames(adiv.i.df) == "x")] <- paste0("Para. ISD, q = ", div.i.divparam$q[5], " (equi. Simpson)")
  adiv.i.df$x <- div.i.divparam$div$`6`
  colnames(adiv.i.df)[which(colnames(adiv.i.df) == "x")] <- paste0("Para. ISD, q = ", div.i.divparam$q[6])
  adiv.i.df$x <- div.i.divparam$div$`7`
  colnames(adiv.i.df)[which(colnames(adiv.i.df) == "x")] <- paste0("Para. ISD, q = ", div.i.divparam$q[7])
  
  # plot
  par(mfrow = c(3,2))
  sapply(names(adiv.i.df[, c(1,8,2,3,12,4:7,9:11,13:ncol(adiv.i.df))]), 
         function(cname){
           print(hist(adiv.i.df[, c(1,8,2,3,12,4:7,9:11,13:ncol(adiv.i.df))][[cname]], main = "", xlab = cname, breaks = length(unique(adiv.i.df[, c(1,8,2,3,12,4:7,9:11,13:ncol(adiv.i.df))][[cname]]))))
         })
  par(mfrow=c(1,1))
  
  div.list[[i]] <- adiv.i.df
  
  rm(div.i, adiv.i.df, div.i.speciesdiv, div.i.divparam)
  
}

# for the error message see below qecb.ivr.isnadiv. filtered for richness NA data

div.df. <- do.call("rbind", div.list)


# There is an issue with Region.terri that are merged with no "_" ...

library(tidyr)
div.df. <- add_column(div.df., rownames. = rownames(div.df.), .before = "richness")
div.df. <- separate(div.df., rownames., into = c("Region.terri.", "Site.Year.Month.Day", "Quadrat.bis", "Type.Bloc", "Numéro.Bloc.échantillon", "Face"), sep = "_")

# I therefore add these lines to solve that issue
unique(div.df.$Region.terri.)
unique(substring(div.df.$Region.terri., nchar(div.df.$Region.terri.)-3))
div.df. <- add_column(div.df., terri. = substring(div.df.$Region.terri., nchar(div.df.$Region.terri.)-3), .after = "Region.terri.")
unique(substring(div.df.$Region.terri., 1, nchar(div.df.$Region.terri.)-4))
div.df.$Region.terri. <- substring(div.df.$Region.terri., 1, nchar(div.df.$Region.terri)-4)
div.df. <- rename(div.df., Region = Region.terri.)

div.df.$Site.Year.Month.Day <- paste0(div.df.$terri., "_", div.df.$Site.Year.Month.Day)
div.df. <- subset(div.df., select = -c(terri.))

div.df.$Type.Bloc <- as.factor(div.df.$Type.Bloc)
div.df.$Face <- as.factor(div.df.$Face)
div.df.$Numéro.Bloc.échantillon <- as.integer(div.df.$Numéro.Bloc.échantillon)

saveRDS(div.df., "results/Ecology/div.df.RDS")

qecb.ivr.div. <- left_join(qecb.ivr, div.df.) # , by = c("Site.Year.Month.Day", "Region", "Type.Bloc", "Numéro.Bloc.échantillon", "Face", "Quadrat.bis")

qecb.ivr.isnadiv. <- filter(qecb.ivr.div., is.na(richness))
# lest's keep that info for boulder nacked with no organism ...
saveRDS(qecb.ivr.isnadiv., "results/Ecology/qecb.ivr.isnadiv.RDS")
rm(qecb.ivr.isnadiv.)

# check these NA div. data when compared to the matri data weird values

matri.full$Quadrat <- str_sub(matri.full$Quadrat, 2)
matri.full$Quadrat <- as.integer(matri.full$Quadrat)
matri.full <- rename(matri.full, Numero.Quadrat = Quadrat)
matri.full$`Bloc Mobile Number` <- as.integer(matri.full$`Bloc Mobile Number`)
matri.full <- rename(matri.full, Numéro.Bloc.échantillon = `Bloc Mobile Number`)
matri.full <- add_column(matri.full, Type.Bloc = "Bloc mobile", .after = "Numero.Quadrat")

matri.full$Year <- as.integer(matri.full$Year)
matri.full$Month <- as.integer(matri.full$Month)
matri.full$Day <- as.integer(matri.full$Day)

qecb.ivr.div.matri <- left_join(qecb.ivr.div., matri.full) # , by = c("Date", "Year", "Month", "Day", "Site", "Type.Bloc", "Numéro.Bloc.échantillon", "Numero.Quadrat")
qecb.ivr.div.matri$BM.BF_FS_dist. <- ifelse(qecb.ivr.div.matri$Type.Bloc == "Bloc mobile" & qecb.ivr.div.matri$Face == "face inférieure", NA, qecb.ivr.div.matri$BM.BF_FS_dist.)
check <- qecb.ivr.div.matri[, c("Site", "Date", "Numero.Quadrat", "Type.Bloc", "Face", "Numéro.Bloc.échantillon", "BM.BF_FS_dist.", "BM_FS.FI_dist.")]
rm(check)

qecb.ivr.isnadiv.matri <- filter(qecb.ivr.div.matri, is.na(richness))

plot(x = qecb.ivr.div.matri$BM.BF_FS_dist., y = qecb.ivr.div.matri$BM_FS.FI_dist., xlim = c(0,1), ylim = c(0,1), xlab = "distance diss. BM.FS vs BF", ylab = "distance diss. BM.FS vs BM.FI")
points(x = qecb.ivr.isnadiv.matri$BM.BF_FS_dist., y = qecb.ivr.isnadiv.matri$BM_FS.FI_dist., col = "red", pch = 19)
# (0,0) when diversity index value is.na; i.e. no species from qecb list ... and neither (or almost not) for fishing species)

qecb.ivr.div.matri.red <- filter(qecb.ivr.div.matri, BM.BF_FS_dist. != 0)
plot(x = qecb.ivr.div.matri.red$BM.BF_FS_dist., y = qecb.ivr.div.matri.red$BM_FS.FI_dist., xlim = c(0,1), ylim = c(0,1), xlab = "distance diss. BM.FS vs BF", ylab = "distance diss. BM.FS vs BM.FI")
points(x = qecb.ivr.isnadiv.matri$BM.BF_FS_dist., y = qecb.ivr.isnadiv.matri$BM_FS.FI_dist., col = "red", pch = 19)

qecb.ivr.div.matri.red <- filter(qecb.ivr.div.matri, BM.BF_FS_dist. != 0 & Type.Bloc == "Bloc mobile" & Face == "face supérieure")
plot(x = qecb.ivr.div.matri.red$BM.BF_FS_dist., y = qecb.ivr.div.matri.red$BM_FS.FI_dist., xlim = c(0,1), ylim = c(0,1), xlab = "distance diss. BM.FS vs BF", ylab = "distance diss. BM.FS vs BM.FI")
points(x = qecb.ivr.isnadiv.matri$BM.BF_FS_dist., y = qecb.ivr.isnadiv.matri$BM_FS.FI_dist., col = "red", pch = 19)

qecb.ivr.div.matri.redred <- filter(qecb.ivr.div.matri.red, BM.BF_FS_dist. >= 0.25 | BM_FS.FI_dist. >= 0.25)
qecb.ivr.div.matri.redred <- filter(qecb.ivr.div.matri.redred, BM.BF_FS_dist. <= 0.9 | BM_FS.FI_dist. <= 0.9)
plot.check <- qecb.ivr.div.matri.red[, c("Site", "Date", "Numero.Quadrat", "Type.Bloc", "Face", "Numéro.Bloc.échantillon", "BM.BF_FS_dist.", "BM_FS.FI_dist.")]
rm(plot.check)

plot(x = qecb.ivr.div.matri.redred$BM.BF_FS_dist., y = qecb.ivr.div.matri.redred$BM_FS.FI_dist., xlim = c(0,1), ylim = c(0,1), xlab = "distance diss. BM.FS vs BF", ylab = "distance diss. BM.FS vs BM.FI")
points(x = qecb.ivr.isnadiv.matri$BM.BF_FS_dist., y = qecb.ivr.isnadiv.matri$BM_FS.FI_dist., col = "red", pch = 19, cex = 1.5)

qecb.ivr.div.matri.remo <- setdiff(qecb.ivr.div.matri.red, qecb.ivr.div.matri.redred)
points(x = qecb.ivr.div.matri.remo$BM.BF_FS_dist., y = qecb.ivr.div.matri.remo$BM_FS.FI_dist., col = "orange", pch = 19, cex = 1)

qecb.ivr.div.matri.remo <- filter(qecb.ivr.div.matri.remo, Type.Bloc == "Bloc mobile")
#qecb.ivr.div.matri.remo <- filter(qecb.ivr.div.matri.remo, !is.na(qecb.ivr.div.matri.remo$richness)) # remove richness = NA, i.e. no qecb species
remo.check <- qecb.ivr.div.matri.remo[, c("Site", "Date", "Numero.Quadrat", "Type.Bloc", "Face", "Numéro.Bloc.échantillon", "richness", "BM.BF_FS_dist.", "BM_FS.FI_dist.")]
rm(remo.check)
unique(qecb.ivr.div.matri.remo$Site.Year.Month.Day)
par(mfrow=c(1,2))
hist(qecb.ivr.div.matri$richness)
hist(qecb.ivr.div.matri.remo$richness)
par(mfrow=c(1,1))
par(mfrow=c(1,2))
hist(qecb.ivr.div.matri$Simpson)
hist(qecb.ivr.div.matri.remo$Simpson)
par(mfrow=c(1,1))
table(qecb.ivr.div.matri.remo$Face)
table(qecb.ivr.div.matri.remo$Couleur.dominante)
library(pander)
pander(table(qecb.ivr.div.matri.remo[, c("Face", "Couleur.dominante")]))

# 35 observations in 21 surveys; no reason to remove these data "...remo"
