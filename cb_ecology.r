# author: "Jonathan Richir"
# date: "01 October 2022"


#Rscript

###############################
##  ##
###############################

#####Packages : dplyr
#               tidyr
#               readr
#               writexl
#               stringr
#               readxl
#               tibble
#               lubridate
#               cowplot
#               magrittr
#               rmarkdown
library(magrittr)
#####Load arguments

args <- commandArgs(trailingOnly = TRUE)

#####Import data

if (length(args) < 1) {
    stop("This tool needs at least 1 argument")
}else {
    ecology_input <- args[1]
#    input_data2 <- args[2]
#    fiche_val <- args[3]
#    fiche_term <- args[4]

}


## load qecbNew data ; df saved from CB_qecb script

qecb <- readRDS(ecology_input)
#read.table(ecology_input, sep = "\t", dec = ".", fileEncoding = "UTF-8")

`%notin%` <- Negate(`%in%`)

## reorder and/or create new variables

# variable site_year_month_day moved for clarity purpose, not needed necessarily
qecb <- tibble::add_column(qecb, qecb$site_year_month_day, .after = "Site_bis")
qecb <- qecb[ , -which(names(qecb) %in% c("site_year_month_day"))]
qecb <- dplyr::rename(qecb, site_year_month_day = `qecb$site_year_month_day`)

# new variable period (nothing to see with the existing périod variable)
period <- rep(NA, nrow(qecb))
qecb <- tibble::add_column(qecb, period, .after = "Day")
qecb$period <- ifelse(as.numeric(qecb$Month) < 7, "p1", "p2")
qecb$period <- as.factor(qecb$period)
rm(period)

qecb <- dplyr::arrange(qecb, region, site_year_month_day, Type.Bloc, Numéro.Bloc.échantillon, Face)

# NB: les infos surface d'accolement sont dupliquées de la face inf vers la face sup de blocs mobiles (même si peu de sens d'avoir cette info pour les face sup ...)
# NB: les data "Abondance ressources ciblées par pêcheurs à pied" présentes uniquement pour les blocs mobiles sont dupliquées entre face inf et face sup. 


## SCRIPT I - NAs <- 0

# already performed for part in the CB_qecb script ; but here I also consider mobile organisms, logical observation (or not) according to boulders, faces etc ... so more complete. Could be some kind of script fusion to only keep Na to 0 correction in one script, i.e. moving this script to the CB_qecb script ...

#unique(qecb$region)
#unique(qecb$Type.Bloc)
Bretagne.bm <- dplyr::filter(qecb, region == "Bretagne" & Type.Bloc == "Bloc mobile")
Bretagne.bf <- dplyr::filter(qecb, region == "Bretagne" & Type.Bloc %in% c("Bloc fixé", "Roche en place"))
EGMP.BASQ.bm <- dplyr::filter(qecb, region == "EGMP.BASQ" & Type.Bloc == "Bloc mobile")
EGMP.BASQ.bf <- dplyr::filter(qecb, region == "EGMP.BASQ" & Type.Bloc %in% c("Bloc fixé", "Roche en place"))

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
qecbNato0 <- dplyr::bind_rows(Bretagne.bm, Bretagne.bf)
qecbNato0 <- dplyr::bind_rows(qecbNato0, EGMP.BASQ.bm)
qecbNato0 <- dplyr::bind_rows(qecbNato0, EGMP.BASQ.bf)

qecbNato0 <- dplyr::arrange(qecbNato0, region, site_year_month_day, Type.Bloc, Numéro.Bloc.échantillon, Face)

rm(Bretagne.bm, Bretagne.bf, EGMP.BASQ.bm, EGMP.BASQ.bf)


## analyse matricielle

# NB some variables were dplyr::renamed or created, cfr I originally merged qecb and ivr data in below script to do some correlation analysis. This is not the case anymore, so no more merging anymore.

qecbNato0 <- tibble::add_column(qecbNato0, region.site_year_month_day = paste0(qecbNato0$region, qecbNato0$site_year_month_day), .before = "region")

#qecbNato0 <- dplyr::rename(qecbNato0, id = id_qecb)
Numero.Quadrat <- stringr::str_sub(qecbNato0$quadrat_bis, start = -1)
qecbNato0 <- tibble::add_column(qecbNato0, Numero.Quadrat, .after = "quadrat_bis")
rm(Numero.Quadrat)
qecbNato0$Numero.Quadrat <- as.integer(qecbNato0$Numero.Quadrat)

#qecbNato0 <- dplyr::rename(qecbNato0, Numero.Photo.qecb = Numero.Photo)
#qecbNato0 <- dplyr::rename(qecbNato0, n.qecb = n)

qecbNato0$Year <- as.integer(qecbNato0$Year)
qecbNato0$Month <- as.integer(qecbNato0$Month)
qecbNato0$Day <- as.integer(qecbNato0$Day)

############################################################
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# Anna still hasn't corrected for boulder nb in FINS_Quemenes.2020.10.16 data encoding ! removed from the df.
qecbNato0 %>% dplyr::filter(site_year_month_day != "FINS_Quemenes.2020.10.16") -> qecbNato0
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

# what to do with spirorbes & Nb.Spirobranchus.lamarckii.total? log10 transformation

qecbNato0 <- tibble::add_column(qecbNato0, log10.Nb.spirorbis.total = log10(qecbNato0$Nb.spirorbis.total+1), .after = "Nb.spirorbis.total")
qecbNato0 <- tibble::add_column(qecbNato0, log10.Nb.Spirobranchus.lamarckii.total = log10(qecbNato0$Nb.Spirobranchus.lamarckii.total+1), .after = "Nb.Spirobranchus.lamarckii.total")

# here I can choose to either replace spirorbis and/or spirobranchus by their log10 transformation in Bret_EGMP.BASQ_qecb vector
Bret_EGMP.BASQ_qecb <- replace(Bret_EGMP.BASQ_qecb, Bret_EGMP.BASQ_qecb == "Nb.spirorbis.total", "log10.Nb.spirorbis.total")
#Bret_EGMP.BASQ_qecb <- replace(Bret_EGMP.BASQ_qecb, Bret_EGMP.BASQ_qecb == "Nb.Spirobranchus.lamarckii.total", "log10.Nb.Spirobranchus.lamarckii.total") 


## determination of coefficient of dissimilarity face supérieure bloc mobile vs roche en place

# loop in a fct

matri.fct.BMF <- function(data, conca) {
  
  matri.df. <- data
  
  for (x in c(1:length(unique(matri.df.$site_year_month_day)))) {
    
    matri.df. %>% dplyr::filter(site_year_month_day == unique(matri.df.$site_year_month_day)[[x]]) -> qecbNato0.x
    
    rownames(qecbNato0.x) <- paste0(qecbNato0.x$Type.Bloc, "_", qecbNato0.x$Face,  "_", qecbNato0.x$Numéro.Bloc.échantillon, "_", qecbNato0.x$quadrat_bis)
  
  #library(vegan)
  mtxdis. <- vegan::vegdist(
    sqrt
    (qecbNato0.x[,conca]), #Transform your species abundance data. Typically, raw abundances are transformed prior to analysis. Usually you will use square root, fourth-root, log(X+1), or presence-absence (square root being least extreme, P/A being most). I would start with square root. (https://stats.stackexchange.com/questions/234495/double-zeroes-problem-with-euclidean-distance-and-abundance-data-is-the-proble)
    na.rm = T,
    method = "bray" #Construct species abundance dissimilarity matrices with Bray-Curtis. If your data contains samples that are all-zero you will run into the double zero problem. This can be overcome by using a zero-adjusted Bray-Curtis coefficient, which is sometimes referred to as a 'dummy variable' which damps down the similarity fluctuations between samples that are both zero (undefined). => see below for zero-adjusted Bray-Curtis coefficient ; #another possibility, sqrt + 1 ??
  )
#remotes::install_github("phytomosaic/ecole")
  
  #library(ecole) #https://rdrr.io/github/phytomosaic/ecole/man/bray0.html
 # mtxdis. <- ecole::bray0(
 #   sqrt
  #  (qecbNato0.x[,conca]), na.rm = T)
  
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
  df. <- dplyr::bind_cols(df., Q.df.)
  
  rm(Q., mat., Q.df.)
  
  split. <- strsplit(df.$V1, "_")
  V1.split. <- as.data.frame(matrix(unlist(split.), ncol = 4, byrow = TRUE))
  split. <- strsplit(df.$V2, "_")
  V2.split. <- as.data.frame(matrix(unlist(split.), ncol = 4, byrow = TRUE))
  
  df. <- dplyr::bind_cols(df., V1.split.)
  df. <- dplyr::bind_cols(df., V2.split.)
  df.red. <- subset(df., V4...8 == V4...12 & V1...5 != V1...9)
  site_year_month_day <- rep(unique(qecbNato0.x$site_year_month_day), nrow(df.red.))
  library(tibble)
  df.red. <- tibble::add_column(df.red., site_year_month_day, .before = "dist.")
  
  rm(split., V1.split., V2.split.)
  rm(mtxdis., mtxdis.df., df., site_year_month_day)
  #rm(df.red.)
  
  matri.list[[x]] <- df.red. 
  matri.list <<- matri.list
  
  rm(df.red., qecbNato0.x, x)
  
  }
  
  matri.df. <- do.call("rbind", matri.list)
  
  names(matri.df.) <- c("site_year_month_day", "dist.", "name.", "name.left", "name.right", "Type.Bloc.left", "Face.left", "Numéro.Bloc.échantillon.left", "Quadrat.left", "Type.Bloc.right", "Face.right", "Numéro.Bloc.échantillon.right", "Quadrat.right")
  
  matri.df. <<- matri.df.
  
  hist(matri.df.$dist.)

}

dplyr::filter(qecbNato0, Face == "face supérieure") -> data.
data.$Type.Bloc <- ifelse(as.character(data.$Type.Bloc) == "Roche en place", "Bloc fixé", as.character(data.$Type.Bloc))
matri.list <- vector("list", length(unique(data.$site_year_month_day)))
#matri.fct.BMF(data = data., conca = c(Bret_EGMP.BASQ_qecb))
matri.fct.BMF(data = data., conca = c(Bret_EGMP.BASQ_qecb, EGMP.BASQ_qecb))
hist(matri.df.$dist., main = c(paste("Histo. of Bray (0-adjusted) dist. dissimilarity measures"), paste(" (sqrt transfo) - BMfs vs BF -")))

matri.full.BM.BF_FS <- matri.df.
#saveRDS(matri.full.BM.BF_FS, "Ecology/matri.full_BM.BF_FS.RDS")
saveRDS(matri.full.BM.BF_FS, "matri.full_log.spi_BM.BF_FS.RDS")
rm(data., matri.df., matri.list)


## determination of coefficient of dissimilarity between blocs mobiles face sup vs face inf.

# loop in a fct

matri.fct.BMM <- function(data, conca) {
  
  matri.df. <- data

  for (x in c(1:length(unique(matri.df.$site_year_month_day)))) {
    
    matri.df. %>% dplyr::filter(site_year_month_day == unique(matri.df.$site_year_month_day)[[x]]) -> qecbNato0.x
    
    rownames(qecbNato0.x) <- paste0(qecbNato0.x$Type.Bloc, "_", qecbNato0.x$Face,  "_", qecbNato0.x$Numéro.Bloc.échantillon, "_", qecbNato0.x$quadrat_bis)
  
  #library(vegan)
  #mtxdis. <- vegdist(
  #  sqrt
  #  (qecbNato0.x[,c(Bret_EGMP.BASQ_qecb)]), #Transform your species abundance data. Typically, raw abundances are transformed prior to analysis. Usually you will use square root, fourth-root, log(X+1), or presence-absence (square root being least extreme, P/A being most). I would start with square root. (https://stats.stackexchange.com/questions/234495/double-zeroes-problem-with-euclidean-distance-and-abundance-data-is-the-proble)
  #  na.rm = T,
  #  method = "bray" #Construct species abundance dissimilarity matrices with Bray-Curtis. If your data contains samples that are all-zero you will run into the double zero problem. This can be overcome by using a zero-adjusted Bray-Curtis coefficient, which is sometimes referred to as a 'dummy variable' which damps down the similarity fluctuations between samples that are both zero (undefined). => see below for zero-adjusted Bray-Curtis coefficient ; #another possibility, sqrt + 1 ??
  #)
  
  library(ecole)
  mtxdis. <- bray0(
    sqrt
    (qecbNato0.x[,conca]), na.rm = T)
  
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
  df. <- dplyr::bind_cols(df., Q.df.)
  
  rm(Q., mat., Q.df.)
  
  split. <- strsplit(df.$V1, "_")
  V1.split. <- as.data.frame(matrix(unlist(split.), ncol = 4, byrow = TRUE))
  split. <- strsplit(df.$V2, "_")
  V2.split. <- as.data.frame(matrix(unlist(split.), ncol = 4, byrow = TRUE))
  
  df. <- dplyr::bind_cols(df., V1.split.)
  df. <- dplyr::bind_cols(df., V2.split.)
  df.red. <- subset(df., V4...8 == V4...12 & V3...7 == V3...11)
  site_year_month_day <- rep(unique(qecbNato0.x$site_year_month_day), nrow(df.red.))

  df.red. <- tibble::add_column(df.red., site_year_month_day, .before = "dist.")
  
  rm(split., V1.split., V2.split.)
  rm(mtxdis., mtxdis.df., df., site_year_month_day)
  #rm(df.red.)
  
  matri.list[[x]] <- df.red. 
  matri.list <<- matri.list
  
  rm(df.red., qecbNato0.x, x)
  
  }
  
  matri.df. <- do.call("rbind", matri.list)
  
  names(matri.df.) <- c("site_year_month_day", "dist.", "name.", "name.left", "name.right", "Type.Bloc.left", "Face.left", "Numéro.Bloc.échantillon.left", "Quadrat.left", "Type.Bloc.right", "Face.right", "Numéro.Bloc.échantillon.right", "Quadrat.right")
  
  matri.df. <<- matri.df.
  
  hist(matri.df.$dist.)
  
}

dplyr::filter(qecbNato0, Type.Bloc == "Bloc mobile") -> data.
matri.list <- vector("list", length(unique(data.$site_year_month_day)))
#matri.fct.BMM(data = data., conca = c(Bret_EGMP.BASQ_qecb))
matri.fct.BMM(data = data., conca = c(Bret_EGMP.BASQ_qecb, EGMP.BASQ_qecb))
hist(matri.df.$dist., main = c(paste("Histo. of Bray (0-adjusted) dist. dissimilarity measures"), paste(" (sqrt transfo) - BMfs vs BMfi -")))

matri.full.BM_FS.FI <- matri.df.
#saveRDS(matri.full.BM_FS.FI, "Ecology/matri.full.BM_FS.FI.RDS")
saveRDS(matri.full.BM_FS.FI, "matri.full_log.spi_BM_FS.FI.RDS")
rm(data., matri.df., matri.list)


## plot

# activate line
#matri.full.BM.BF_FS <- readRDS("Ecology/matri.full.BM.BF_FS.RDS")
#matri.full.BM.BF_FS <- readRDS("Ecology/matri.full_log.spi_BM.BF_FS.RDS")
#matri.full.BM_FS.FI <- readRDS("Ecology/matri.full.BM_FS.FI.RDS")
#matri.full.BM_FS.FI <- readRDS("Ecology/matri.full_log.spi_BM_FS.FI.RDS")


matri.full.BM.BF_FS <- tidyr::separate(matri.full.BM.BF_FS, "site_year_month_day", into = c("departement", "Site", "Year", "Month", "Day"), remove = F)
matri.full.BM.BF_FS$Site <- paste0(matri.full.BM.BF_FS$departement, "_", matri.full.BM.BF_FS$Site)
matri.full.BM.BF_FS <- subset(matri.full.BM.BF_FS, select = - c(departement)) 
matri.full.BM.BF_FS <- tibble::add_column(matri.full.BM.BF_FS, Date = as.Date(paste0(matri.full.BM.BF_FS$Year, "-", matri.full.BM.BF_FS$Month, "-", matri.full.BM.BF_FS$Day), origin = "1970-01-01"), .after = "Site")
matri.full.BM.BF_FS$Site <- as.factor(matri.full.BM.BF_FS$Site)

matri.full.BM_FS.FI <- tidyr::separate(matri.full.BM_FS.FI, "site_year_month_day", into = c("departement", "Site", "Year", "Month", "Day"), remove = F)
matri.full.BM_FS.FI$Site <- paste0(matri.full.BM_FS.FI$departement, "_", matri.full.BM_FS.FI$Site)
matri.full.BM_FS.FI <- subset(matri.full.BM_FS.FI, select = - c(departement)) 
matri.full.BM_FS.FI <- tibble::add_column(matri.full.BM_FS.FI, Date = as.Date(paste0(matri.full.BM_FS.FI$Year, "-", matri.full.BM_FS.FI$Month, "-", matri.full.BM_FS.FI$Day), origin = "1970-01-01"), .after = "Site")
matri.full.BM_FS.FI$Site <- as.factor(matri.full.BM_FS.FI$Site)

#dev.off() # if error message "Error in .Call.graphics(C_palette2, .Call(C_palette2, NULL)) : invalid graphics state"

library(ggplot2)

bf_fs_plot <- ggplot2::ggplot(matri.full.BM.BF_FS, ggplot2::aes(x = Site, y = dist.)) +
  ggplot2::geom_boxplot() + 
  #geom_jitter(shape = 16, position=position_jitter(0.2)) +
  ggplot2::xlab("") +
  ggplot2::ylab("distance diss. BM.BF_FS") +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot2::ggsave("distance_diss_BF_FS.png", bf_fs_plot)

fs_fi_plot <- ggplot2::ggplot(matri.full.BM_FS.FI, ggplot2::aes(x = Site, y = dist.)) +
  ggplot2::geom_boxplot() + 
  #geom_jitter(shape = 16, position=position_jitter(0.2)) +
  ggplot2::xlab("") +
  ggplot2::ylab("distance diss. BM_FS.FI") +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot2::ggsave("distance_diss_FS_FI.png", fs_fi_plot)

# issue with type de bloc, numéro de bloc and quadrat for df. BM.BF_FS, cfr df. left vs right variables doesn't give the right combination (variables with left vs right label in names come from the dissimilarity coefficient functions).
matri.full.BM.BF_FS$Quadrat <- NA
for (i in c(1:nrow(matri.full.BM.BF_FS))) { 
  ifelse(matri.full.BM.BF_FS$Type.Bloc.left[i] == "Bloc mobile", matri.full.BM.BF_FS$Quadrat[i] <- matri.full.BM.BF_FS$Quadrat.left[i], matri.full.BM.BF_FS$Quadrat[i] <- matri.full.BM.BF_FS$Quadrat.right[i]) }
matri.full.BM.BF_FS$Numéro.Bloc <- NA
for (i in c(1:nrow(matri.full.BM.BF_FS))) { 
  ifelse(matri.full.BM.BF_FS$Type.Bloc.left[i] == "Bloc mobile", matri.full.BM.BF_FS$Numéro.Bloc[i] <- matri.full.BM.BF_FS$Numéro.Bloc.échantillon.left[i], matri.full.BM.BF_FS$Numéro.Bloc[i] <- matri.full.BM.BF_FS$Numéro.Bloc.échantillon.right[i]) }

matri.full.BM.BF_FS <- tibble::add_column(matri.full.BM.BF_FS, site_year_month_day.Q.BMnb = paste0(matri.full.BM.BF_FS$site_year_month_day, "_",  matri.full.BM.BF_FS$Quadrat, "_", matri.full.BM.BF_FS$Numéro.Bloc), .after = "site_year_month_day")
matri.full.BM_FS.FI <- tibble::add_column(matri.full.BM_FS.FI, site_year_month_day.Q.BMnb = paste0(matri.full.BM_FS.FI$site_year_month_day, "_",  matri.full.BM_FS.FI$Quadrat.left, "_", matri.full.BM_FS.FI$Numéro.Bloc.échantillon.left), .after = "site_year_month_day")

colnames(matri.full.BM.BF_FS) <- paste("BM.BF_FS", colnames(matri.full.BM.BF_FS), sep = "_")
matri.full.BM.BF_FS <- dplyr::rename(matri.full.BM.BF_FS, site_year_month_day.Q.BMnb = BM.BF_FS_site_year_month_day.Q.BMnb )
colnames(matri.full.BM_FS.FI) <- paste("BM_FS.FI", colnames(matri.full.BM_FS.FI), sep = "_")
matri.full.BM_FS.FI <- dplyr::rename(matri.full.BM_FS.FI, site_year_month_day.Q.BMnb = BM_FS.FI_site_year_month_day.Q.BMnb )

matri.full <- dplyr::full_join(matri.full.BM.BF_FS[, c("site_year_month_day.Q.BMnb", "BM.BF_FS_dist.")], matri.full.BM_FS.FI[, c("site_year_month_day.Q.BMnb", "BM_FS.FI_dist.")])

matri.full <- tidyr::separate(matri.full, "site_year_month_day.Q.BMnb", into = c("departement", "Site", "Year", "Month", "Day", "Quadrat", "Bloc Mobile Number"), remove = F)
matri.full$Site <- paste0(matri.full$departement, "_", matri.full$Site)
matri.full <- subset(matri.full, select = - c(departement)) 
matri.full <- tibble::add_column(matri.full, Date = as.Date(paste0(matri.full$Year, "-", matri.full$Month, "-", matri.full$Day), origin = "1970-01-01"), .after = "Site")

# Name for report/plot

matri.full <- tibble::add_column(matri.full, Site_bis = NA, .after = "Site")

matri.full$Site_bis <- ifelse(matri.full$Site == "GDMO_Locmariaquer", "Locmariaquer", matri.full$Site_bis)
matri.full$Site_bis <- ifelse(matri.full$Site == "GDMO_BegLann", "Beg Lann", matri.full$Site_bis)
matri.full$Site_bis <- ifelse(matri.full$Site == "FOUR_PlateauFour", "Plateau du Four", matri.full$Site_bis)
matri.full$Site_bis <- ifelse(matri.full$Site == "EGMP_GroinCou", "Groin du Cou", matri.full$Site_bis)
matri.full$Site_bis <- ifelse(matri.full$Site == "EGMP_PasEmsembert", "Le Pas d'Emsembert", matri.full$Site_bis)
matri.full$Site_bis <- ifelse(matri.full$Site == "EGMP_BreeBains", "La Brée-les-Bains", matri.full$Site_bis)
matri.full$Site_bis <- ifelse(matri.full$Site == "EGMP_PerreAntiochat", "Le Perré d'Antiochat", matri.full$Site_bis)
matri.full$Site_bis <- ifelse(matri.full$Site == "EGMP_Chassiron", "Chassiron", matri.full$Site_bis)
matri.full$Site_bis <- ifelse(matri.full$Site == "BASQ_FlotsBleusZP", "Les Flots Bleus / zone pêcheurs", matri.full$Site_bis)
matri.full$Site_bis <- ifelse(matri.full$Site == "BASQ_FlotsBleusZF", "Les Flots Bleus / zone familles", matri.full$Site_bis)
matri.full$Site_bis <- ifelse(matri.full$Site == "GONB_IlotStMichel", "Îlot Saint-Michel", matri.full$Site_bis)
matri.full$Site_bis <- ifelse(matri.full$Site == "FINS_Quemenes", "Quéménès", matri.full$Site_bis)
matri.full$Site_bis <- ifelse(matri.full$Site == "FINS_SeinGoulenez", "Île de Sein - Goulenez", matri.full$Site_bis)
matri.full$Site_bis <- ifelse(matri.full$Site == "FINS_SeinKilaourou", "Île de Sein - Kilaourou", matri.full$Site_bis)
matri.full$Site_bis <- ifelse(matri.full$Site == "ARMO_Verdelet", "Îlot du Verdelet", matri.full$Site_bis)
matri.full$Site_bis <- ifelse(matri.full$Site == "ARMO_Piegu", "Piégu", matri.full$Site_bis)
matri.full$Site_bis <- ifelse(matri.full$Site == "ARMO_Bilfot", "Pointe de Bilfot", matri.full$Site_bis)
matri.full$Site_bis <- ifelse(matri.full$Site == "ARMO_IlePlate", "Île Plate", matri.full$Site_bis)
matri.full$Site_bis <- ifelse(matri.full$Site == "PDMO_Perharidy", "Perharidy", matri.full$Site_bis)
matri.full$Site_bis <- ifelse(matri.full$Site == "BRES_Keraliou", "Keraliou", matri.full$Site_bis)
matri.full$Site_bis <- ifelse(matri.full$Site == "FINS_Mousterlin", "Pointe de Mousterlin", matri.full$Site_bis)
matri.full$Site_bis <- ifelse(matri.full$Site == "FINS_StNicolasGlenan", "Saint-Nicolas des Glénan", matri.full$Site_bis)

unique(matri.full[, c("Site", "Site_bis")])

#saveRDS(matri.full, "Ecology/matri.full.RDS")
saveRDS(matri.full, "matri.full_log.spi.RDS")


## plot dissimilarity coefficient

matri.full$Year <- as.integer(matri.full$Year)
matri.full$Month <- as.integer(matri.full$Month)
matri.full$Day <- as.integer(matri.full$Day)


## BM_FS.FI_dist => mobile boulder upper vs lower faces

# Stats

matri.full %>% dplyr::group_by(Site, Site_bis, Date, Year, Month, Day) %>% dplyr::summarize(BM_FS.FI_dist.moy = mean(BM_FS.FI_dist., na.rm = T), BM_FS.FI_dist.et = sd(BM_FS.FI_dist., na.rm = T), BM_FS.FI_dist.med = median(BM_FS.FI_dist., na.rm = T), BM_FS.FI_dist.min = min(BM_FS.FI_dist., na.rm = T), BM_FS.FI_dist.max = max(BM_FS.FI_dist., na.rm = T), nb. = dplyr::n(), nb.notNa = sum(!is.na(BM_FS.FI_dist.))) -> BM_FS.FI_dist.stat.

dplyr::ungroup(BM_FS.FI_dist.stat.) -> BM_FS.FI_dist.stat.

# Quality scale based on quartiles

one <- round(mean(unlist(dplyr::filter(matri.full, BM_FS.FI_dist. <= quantile(matri.full$BM_FS.FI_dist., 0.25, na.rm = T))["BM_FS.FI_dist."])), digits = 3)
two <- round(mean(unlist(dplyr::filter(matri.full, BM_FS.FI_dist. > quantile(matri.full$BM_FS.FI_dist., 0.25, na.rm = T) & BM_FS.FI_dist. <= quantile(matri.full$BM_FS.FI_dist., 0.5, na.rm = T))["BM_FS.FI_dist."])), digits = 3)
three <- round(mean(unlist(dplyr::filter(matri.full, BM_FS.FI_dist. > quantile(matri.full$BM_FS.FI_dist., 0.5, na.rm = T) & BM_FS.FI_dist. <= quantile(matri.full$BM_FS.FI_dist., 0.75, na.rm = T))["BM_FS.FI_dist."])), digits = 3)
four <- round(mean(unlist(dplyr::filter(matri.full, BM_FS.FI_dist. > quantile(matri.full$BM_FS.FI_dist., 0.75, na.rm = T))["BM_FS.FI_dist."])), digits = 3)

# Plot

for (i in c(1:length(unique(BM_FS.FI_dist.stat.$Site)))) {
  
  dplyr::filter(BM_FS.FI_dist.stat., Site == unique(BM_FS.FI_dist.stat.$Site)[i]) -> df1
  
  xmin. <- as.Date(ifelse(min(df1$Year) >= 2014, "2014-01-01", paste0(min(matri.full$Year), "-01-01")), origin = "1970-01-01")
  xmax. <- as.Date(ifelse(max(df1$Year) <= 2017, "2018-01-01", #paste0(max(matri.full$Year)+1, 
                          "2022-01-01")
                   #)
                   , origin = "1970-01-01")
  
  ymin. = 0
  ymax. = 1
  
  png(paste0("diss_", unique(BM_FS.FI_dist.stat.$Site), ".png"))
  plot(BM_FS.FI_dist.stat.$Date, BM_FS.FI_dist.stat.$BM_FS.FI_dist.med, xlim = c(xmin., xmax.), ylim = c(ymin., ymax.), pch = 19, main = "", xlab = "", ylab = "", type = 'n', axes = F)
  
  rect(as.Date("2013-01-01", origin = "1970-01-01"), -0.1, as.Date("2023-01-01", origin = "1970-01-01"), one, col = "red", border = NA)
  rect(as.Date("2013-01-01", origin = "1970-01-01"), one, as.Date("2023-01-01", origin = "1970-01-01"), two, col = "orange", border = NA)
  rect(as.Date("2013-01-01", origin = "1970-01-01"), two, as.Date("2023-01-01", origin = "1970-01-01"), three, col = "yellow", border = NA)
  rect(as.Date("2013-01-01", origin = "1970-01-01"), three, as.Date("2023-01-01", origin = "1970-01-01"), four, col = "olivedrab", border = NA)
  rect(as.Date("2013-01-01", origin = "1970-01-01"), four, as.Date("2023-01-01", origin = "1970-01-01"), 1.1, col = "blue", border = NA)
  
  par(new = T)
  plot(BM_FS.FI_dist.stat.$Date, BM_FS.FI_dist.stat.$BM_FS.FI_dist.med, xlim = c(xmin., xmax.), ylim = c(ymin., ymax.), pch = 19, cex = 1, main = unique(df1$Site_bis), xlab = "Année",
       ylab = "Coef. dissi. BM_FS.FI", col = "grey")
  points(df1$Date, df1$BM_FS.FI_dist.med, pch = 19, cex = 1.5)
  arrows(df1$Date, df1$BM_FS.FI_dist.med, df1$Date, df1$BM_FS.FI_dist.max, code = 3, angle = 90, length = 0.00)
  arrows(df1$Date, df1$BM_FS.FI_dist.med, df1$Date, df1$BM_FS.FI_dist.min, code = 3, angle = 90, length = 0.00)
  
}

rm(df1, four, i, one, three, two, xmax., xmin., ymax., ymin.)


## BM.BF_FS_dist => mobile boulder vs fixed boulder upper faces

# Stats

matri.full %>% dplyr::group_by(Site, Site_bis, Date, Year, Month, Day) %>% dplyr::summarize(BM.BF_FS_dist.moy = mean(BM.BF_FS_dist., na.rm = T), BM.BF_FS_dist.et = sd(BM.BF_FS_dist., na.rm = T), BM.BF_FS_dist.med = median(BM.BF_FS_dist., na.rm = T), BM.BF_FS_dist.min = min(BM.BF_FS_dist., na.rm = T), BM.BF_FS_dist.max = max(BM.BF_FS_dist., na.rm = T), nb. = dplyr::n(), nb.notNa = sum(!is.na(BM.BF_FS_dist.))) -> BM.BF_FS_dist.stat.

dplyr::ungroup(BM.BF_FS_dist.stat.) -> BM.BF_FS_dist.stat.

# Quality scale based on quartiles

one <- round(mean(unlist(dplyr::filter(matri.full, BM.BF_FS_dist. <= quantile(matri.full$BM.BF_FS_dist., 0.25, na.rm = T))["BM.BF_FS_dist."])), digits = 3)
two <- round(mean(unlist(dplyr::filter(matri.full, BM.BF_FS_dist. > quantile(matri.full$BM.BF_FS_dist., 0.25, na.rm = T) & BM.BF_FS_dist. <= quantile(matri.full$BM.BF_FS_dist., 0.5, na.rm = T))["BM.BF_FS_dist."])), digits = 3)
three <- round(mean(unlist(dplyr::filter(matri.full, BM.BF_FS_dist. > quantile(matri.full$BM.BF_FS_dist., 0.5, na.rm = T) & BM.BF_FS_dist. <= quantile(matri.full$BM.BF_FS_dist., 0.75, na.rm = T))["BM.BF_FS_dist."])), digits = 3)
four <- round(mean(unlist(dplyr::filter(matri.full, BM.BF_FS_dist. > quantile(matri.full$BM.BF_FS_dist., 0.75, na.rm = T))["BM.BF_FS_dist."])), digits = 3)

# Plot

for (i in c(1:length(unique(BM.BF_FS_dist.stat.$Site)))) {
  
  dplyr::filter(BM.BF_FS_dist.stat., Site == unique(BM.BF_FS_dist.stat.$Site)[i]) -> df1
  
  xmin. <- as.Date(ifelse(min(df1$Year) >= 2014, "2014-01-01", paste0(min(matri.full$Year), "-01-01")), origin = "1970-01-01")
  xmax. <- as.Date(ifelse(max(df1$Year) <= 2017, "2018-01-01", #paste0(max(matri.full$Year)+1, 
                          "2022-01-01")
                   #)
                   , origin = "1970-01-01")
  
  ymin. = 0
  ymax. = 1
  
  png(paste0("diss_", unique(BM.BF_FS_dist.stat.$Site), ".png"))
  plot(BM.BF_FS_dist.stat.$Date, BM.BF_FS_dist.stat.$BM.BF_FS_dist.med, xlim = c(xmin., xmax.), ylim = c(ymin., ymax.), pch = 19, main = "", xlab = "", ylab = "", type = 'n', axes = F)
  
  rect(as.Date("2013-01-01", origin = "1970-01-01"), -0.1, as.Date("2023-01-01", origin = "1970-01-01"), one, col = "blue", border = NA)
  rect(as.Date("2013-01-01", origin = "1970-01-01"), one, as.Date("2023-01-01", origin = "1970-01-01"), two, col = "olivedrab", border = NA)
  rect(as.Date("2013-01-01", origin = "1970-01-01"), two, as.Date("2023-01-01", origin = "1970-01-01"), three, col = "yellow", border = NA)
  rect(as.Date("2013-01-01", origin = "1970-01-01"), three, as.Date("2023-01-01", origin = "1970-01-01"), four, col = "orange", border = NA)
  rect(as.Date("2013-01-01", origin = "1970-01-01"), four, as.Date("2023-01-01", origin = "1970-01-01"), 1.1, col = "red", border = NA)
  
  par(new = T)
  plot(BM.BF_FS_dist.stat.$Date, BM.BF_FS_dist.stat.$BM.BF_FS_dist.med, xlim = c(xmin., xmax.), ylim = c(ymin., ymax.), pch = 19, cex = 1, main = unique(df1$Site_bis), xlab = "Année", 
       ylab = "Coef. dissi. BM.BF_FS", col = "grey")
  points(df1$Date, df1$BM.BF_FS_dist.med, pch = 19, cex = 1.5)
  arrows(df1$Date, df1$BM.BF_FS_dist.med, df1$Date, df1$BM.BF_FS_dist.max, code = 3, angle = 90, length = 0.00)
  arrows(df1$Date, df1$BM.BF_FS_dist.med, df1$Date, df1$BM.BF_FS_dist.min, code = 3, angle = 90, length = 0.00)
  
}

rm(df1, four, i, one, three, two, xmax., xmin., ymax., ymin.)

write.table(BM.BF_FS_dist.stat., "Valeurs_stat.tabular", row.names = FALSE, quote = FALSE, sep = "\t", dec = ".", fileEncoding = "UTF-8")
#saveRDS(BM_FS.FI_dist.stat., "Ecology/matri.full_BM_FS.FI_dist.stat.RDS")
saveRDS(BM_FS.FI_dist.stat., "matri.full_log.spi_BM_FS.FI_dist.stat.RDS")

#saveRDS(BM.BF_FS_dist.stat., "Ecology/matri.full_BM.BF_FS_dist.stat.RDS")
saveRDS(BM.BF_FS_dist.stat., "matri.full_log.spi_BM.BF_FS_dist.stat.RDS")



