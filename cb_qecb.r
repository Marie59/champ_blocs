# author: "Jonathan Richir"
# date: "19 April 2021"


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
#               
#               
library(magrittr)
#####Load arguments

args <- commandArgs(trailingOnly = TRUE)

#####Import data

if (length(args) < 1) {
    stop("This tool needs at least 1 argument")
}else {
    input_data <- args[1]
    input_data2 <- args[2]
    fiche_val <- args[3]
    fiche_term <- args[4]

}


# load qecb data

qecb <- read.csv2(input_data, header = FALSE, fileEncoding = "Latin1")
names_ <- as.vector(unlist(qecb[1, ]))
names_ <- gsub(" ", ".", names_)
colnames(qecb) <- names_

qecb_next <- read.csv2(input_data2, header = FALSE, fileEncoding = "Latin1")
names_ <- as.vector(unlist(qecb_next[1, ]))
names_ <- gsub(" ", ".", names_)
colnames(qecb_next) <- names_
rm(names_)

# bind qecb dfs.

qecb <- dplyr::bind_rows(qecb, qecb_next)

# NB inversion between id and ID.Fiche variable names
qecb <- dplyr::rename(qecb, XX = ID.Fiche)
qecb <- dplyr::rename(qecb, ID.Fiche = id)
qecb <- dplyr::rename(qecb, id = XX)

# import csv files ficheterrain

fiche <- read.csv2(fiche_val, fileEncoding = "Latin1")

fiche_next <- read.csv2(fiche_term, fileEncoding = "Latin1")

# bind ficheterrain
fiche <- dplyr::bind_rows(fiche, fiche_next)

## work on "Fiche terrain"
date_fiche <- as.Date(stringr::str_sub(fiche$date.sortie, end = 10), origin = "1970-01-01")
fiche <- tibble::add_column(fiche, date_fiche, .after = "date.sortie")
rm(date_fiche)

## qecb vs fiche terrain

qecb$id <- as.numeric(qecb[, c("id")])
stop(qecb)
fiche_red <- dplyr::filter(fiche, fiche$ID.Fiche %in% unique(qecb[, c("id")]))

id_count <- qecb %>% dplyr::group_by(id) %>% dplyr::count() 
id_count <- dplyr::rename(id_count, "ID.Fiche" ="id")
id_count <- dplyr::ungroup(id_count)
id_count <- as.data.frame(id_count)

fiche_red <- dplyr::left_join(fiche_red, id_count)

# rep fiche terrain information
fiche_expanded <- fiche_red[rep(row.names(fiche_red), fiche_red$n), 1:ncol(fiche_red)]
fiche_expanded <- dplyr::rename(fiche_expanded, "id" = "ID.Fiche")

qecb <- dplyr::bind_cols(qecb, fiche_expanded) # finally keep all the fiche terrain info in the final qecb df.
qecb <- dplyr::rename(qecb, "id_qecb" = "id...1")
qecb <- dplyr::rename(qecb, "id_fiche" = "id...68")

rm(fiche_expanded, fiche_red, id_count)
rm(fiche_next, qecb_next)

saveRDS(fiche, "fiche.qecb.RDS")

# from previous script, when I had to compare 'home made' date with fiche date
# ok, we can use date_fiche instead of previous DateYmd in the rest of the script; we can easily explain the difference, see "ID.Fiche" details below in Console.

qecb %>% separate(date_fiche, c("Year", "Month", "Day"), sep = "-", remove = FALSE) -> qecb


## Quadrat nb

Quadrat <- stringr::str_extract(qecb$Numero.Photo, "Q[12345]")
qecb <- tibble::add_column(qecb, Quadrat, .after = "Numero.Photo")
rm(Quadrat)

# check
Quadrat.bis <- rep(NA, length = nrow(qecb))
qecb <- tibble::add_column(qecb, Quadrat.bis, .after = "Quadrat")
rm(Quadrat.bis)

qecb$Quadrat.bis <- ifelse(qecb$Numéro.Bloc.échantillon %in% c(1,2) & qecb$Type.Bloc == "Bloc mobile", "Q1", qecb$Quadrat.bis)
qecb$Quadrat.bis <- ifelse(qecb$Numéro.Bloc.échantillon %in% c(3,4) & qecb$Type.Bloc == "Bloc mobile", "Q2", qecb$Quadrat.bis)
qecb$Quadrat.bis <- ifelse(qecb$Numéro.Bloc.échantillon %in% c(5,6) & qecb$Type.Bloc == "Bloc mobile", "Q3", qecb$Quadrat.bis)
qecb$Quadrat.bis <- ifelse(qecb$Numéro.Bloc.échantillon %in% c(7,8) & qecb$Type.Bloc == "Bloc mobile", "Q4", qecb$Quadrat.bis)
qecb$Quadrat.bis <- ifelse(qecb$Numéro.Bloc.échantillon %in% c(9,10) & qecb$Type.Bloc == "Bloc mobile", "Q5", qecb$Quadrat.bis)
test. <- na.omit(qecb[, c("Quadrat", "Quadrat.bis")])

qecb$Quadrat.bis <- ifelse(qecb$Numéro.Bloc.échantillon == 1 & qecb$Type.Bloc %in% c("Bloc fixé", "Roche en place"), "Q1", qecb$Quadrat.bis)
qecb$Quadrat.bis <- ifelse(qecb$Numéro.Bloc.échantillon == 2 & qecb$Type.Bloc %in% c("Bloc fixé", "Roche en place"), "Q2", qecb$Quadrat.bis)
qecb$Quadrat.bis <- ifelse(qecb$Numéro.Bloc.échantillon == 3 & qecb$Type.Bloc %in% c("Bloc fixé", "Roche en place"), "Q3", qecb$Quadrat.bis)
qecb$Quadrat.bis <- ifelse(qecb$Numéro.Bloc.échantillon == 4 & qecb$Type.Bloc %in% c("Bloc fixé", "Roche en place"), "Q4", qecb$Quadrat.bis)
qecb$Quadrat.bis <- ifelse(qecb$Numéro.Bloc.échantillon == 5 & qecb$Type.Bloc %in% c("Bloc fixé", "Roche en place"), "Q5", qecb$Quadrat.bis)
test. <- na.omit(qecb[, c("Type.Bloc", "Quadrat", "Quadrat.bis")])

test.$check <- ifelse(test.$Quadrat == test.$Quadrat.bis, "Y", "N")
table(test.$check)
rm(test.)

# Site name as initially encoded in the previous home made Site name and Date search. 
unique(qecb$zone.habitat)
qecb <- tibble::add_column(qecb, Site = NA, .after = "ID.Fiche")
unique(qecb$Site)

qecb$Site <- ifelse(qecb$zone.habitat == unique(qecb$zone.habitat)[1], "GDMO_Locmariaquer", qecb$Site)
qecb$Site <- ifelse(qecb$zone.habitat == unique(qecb$zone.habitat)[2], "GDMO_BegLann", qecb$Site)
qecb$Site <- ifelse(qecb$zone.habitat == unique(qecb$zone.habitat)[3], "FOUR_PlateauFour", qecb$Site)
qecb$Site <- ifelse(qecb$zone.habitat == unique(qecb$zone.habitat)[4], "EGMP_GroinCou", qecb$Site)
qecb$Site <- ifelse(qecb$zone.habitat == unique(qecb$zone.habitat)[5], "EGMP_PasEmsembert", qecb$Site)
qecb$Site <- ifelse(qecb$zone.habitat == unique(qecb$zone.habitat)[6], "EGMP_BreeBains", qecb$Site)
qecb$Site <- ifelse(qecb$zone.habitat == unique(qecb$zone.habitat)[7], "EGMP_PerreAntiochat", qecb$Site)
qecb$Site <- ifelse(qecb$zone.habitat == unique(qecb$zone.habitat)[8], "EGMP_Chassiron", qecb$Site)
qecb$Site <- ifelse(qecb$zone.habitat == unique(qecb$zone.habitat)[9], "BASQ_FlotsBleusZP", qecb$Site)
qecb$Site <- ifelse(qecb$zone.habitat == unique(qecb$zone.habitat)[10], "BASQ_FlotsBleusZF", qecb$Site)
qecb$Site <- ifelse(qecb$zone.habitat == unique(qecb$zone.habitat)[11], "GONB_IlotStMichel", qecb$Site)
qecb$Site <- ifelse(qecb$zone.habitat == unique(qecb$zone.habitat)[12], "FINS_Quemenes", qecb$Site)
qecb$Site <- ifelse(qecb$zone.habitat == unique(qecb$zone.habitat)[13], "FINS_SeinGoulenez", qecb$Site)
qecb$Site <- ifelse(qecb$zone.habitat == unique(qecb$zone.habitat)[14], "FINS_SeinKilaourou", qecb$Site)
qecb$Site <- ifelse(qecb$zone.habitat == unique(qecb$zone.habitat)[15], "ARMO_Verdelet", qecb$Site)
qecb$Site <- ifelse(qecb$zone.habitat == unique(qecb$zone.habitat)[16], "ARMO_Piegu", qecb$Site)
qecb$Site <- ifelse(qecb$zone.habitat == unique(qecb$zone.habitat)[17], "ARMO_Bilfot", qecb$Site)
qecb$Site <- ifelse(qecb$zone.habitat == unique(qecb$zone.habitat)[18], "ARMO_IlePlate", qecb$Site)
qecb$Site <- ifelse(qecb$zone.habitat == unique(qecb$zone.habitat)[19], "PDMO_Perharidy", qecb$Site)
qecb$Site <- ifelse(qecb$zone.habitat == unique(qecb$zone.habitat)[20], "BRES_Keraliou", qecb$Site)
qecb$Site <- ifelse(qecb$zone.habitat == unique(qecb$zone.habitat)[21], "FINS_Mousterlin", qecb$Site)
qecb$Site <- ifelse(qecb$zone.habitat == unique(qecb$zone.habitat)[22], "FINS_StNicolasGlenan", qecb$Site)

unique(qecb$Site)
check. <- dplyr::filter(qecb, is.na(qecb$Site))
# Anne Boulet forgot to specify zone.habitat in 2020. I asked her to correct it in ESTAMP
qecb$Site <- ifelse(qecb$zone.habitat == unique(qecb$zone.habitat)[23], "GDMO_Locmariaquer", qecb$Site)
unique(qecb$Site)
unique(qecb[, c("Site", "zone.habitat")])
rm(check.)

# Name for report/plot

qecb <- tibble::add_column(qecb, Site_bis = NA, .after = "Site")
unique(qecb$Site)
dplyr::filter(qecb, is.na(qecb$Site))
check. <- data.frame(Site = qecb[, "Site"])
check. <- dplyr::arrange(check., Site)
rm(check.)
check. <- dplyr::arrange(qecb, Site)
rm(check.)

qecb$Site_bis <- ifelse(qecb$Site == "GDMO_Locmariaquer", "Locmariaquer", qecb$Site_bis)
qecb$Site_bis <- ifelse(qecb$Site == "GDMO_BegLann", "Beg Lann", qecb$Site_bis)
qecb$Site_bis <- ifelse(qecb$Site == "FOUR_PlateauFour", "Plateau du Four", qecb$Site_bis)
qecb$Site_bis <- ifelse(qecb$Site == "EGMP_GroinCou", "Groin du Cou", qecb$Site_bis)
qecb$Site_bis <- ifelse(qecb$Site == "EGMP_PasEmsembert", "Le Pas d'Emsembert", qecb$Site_bis)
qecb$Site_bis <- ifelse(qecb$Site == "EGMP_BreeBains", "La Brée-les-Bains", qecb$Site_bis)
qecb$Site_bis <- ifelse(qecb$Site == "EGMP_PerreAntiochat", "Le Perré d'Antiochat", qecb$Site_bis)
qecb$Site_bis <- ifelse(qecb$Site == "EGMP_Chassiron", "Chassiron", qecb$Site_bis)
qecb$Site_bis <- ifelse(qecb$Site == "BASQ_FlotsBleusZP", "Les Flots Bleus / zone pêcheurs", qecb$Site_bis)
qecb$Site_bis <- ifelse(qecb$Site == "BASQ_FlotsBleusZF", "Les Flots Bleus / zone familles", qecb$Site_bis)
qecb$Site_bis <- ifelse(qecb$Site == "GONB_IlotStMichel", "Îlot Saint-Michel", qecb$Site_bis)
qecb$Site_bis <- ifelse(qecb$Site == "FINS_Quemenes", "Quéménès", qecb$Site_bis)
qecb$Site_bis <- ifelse(qecb$Site == "FINS_SeinGoulenez", "Île de Sein - Goulenez", qecb$Site_bis)
qecb$Site_bis <- ifelse(qecb$Site == "FINS_SeinKilaourou", "Île de Sein - Kilaourou", qecb$Site_bis)
qecb$Site_bis <- ifelse(qecb$Site == "ARMO_Verdelet", "Îlot du Verdelet", qecb$Site_bis)
qecb$Site_bis <- ifelse(qecb$Site == "ARMO_Piegu", "Piégu", qecb$Site_bis)
qecb$Site_bis <- ifelse(qecb$Site == "ARMO_Bilfot", "Pointe de Bilfot", qecb$Site_bis)
qecb$Site_bis <- ifelse(qecb$Site == "ARMO_IlePlate", "Île Plate", qecb$Site_bis)
qecb$Site_bis <- ifelse(qecb$Site == "PDMO_Perharidy", "Perharidy", qecb$Site_bis)
qecb$Site_bis <- ifelse(qecb$Site == "BRES_Keraliou", "Keraliou", qecb$Site_bis)
qecb$Site_bis <- ifelse(qecb$Site == "FINS_Mousterlin", "Pointe de Mousterlin", qecb$Site_bis)
qecb$Site_bis <- ifelse(qecb$Site == "FINS_StNicolasGlenan", "Saint-Nicolas des Glénan", qecb$Site_bis)

unique(qecb[, c("Site", "Site_bis")])

# change some variables to factor

unique(qecb$Type.Bloc)
unique(qecb$Face)
unique(qecb$Numéro.Bloc.échantillon)


# change 'X..' variables that are indeed % to numeric; https://stackoverflow.com/questions/59410939/apply-function-to-all-variables-with-string-in-name
ix <- grep("^X..", names(qecb))
qecb[ix] <- lapply(qecb[ix], as.numeric)
rm(ix)


# save the final, commplete qecb df.

names(qecb)
qecb <- qecb[, c(72:107, 1:71)]

saveRDS(qecb, "qecb.RDS")

rm(qecb, fiche)

#################################################


## QECB determination test

#qecb <- readRDS("results/QECB/qecb.RDS")

unique(qecb$Site)
qecb  %>% dplyr::filter(qecb$Site == "GONB_IlotStMichel" && qecb$Year == "2016" & qecb$Month == "05") -> df.test

unique(df.test$Type.Bloc)
df.test$Type.Bloc <- factor(df.test$Type.Bloc, levels = c("Bloc mobile", "Bloc fixé", "Roche en place"))
unique(df.test$Face)
df.test$Face <- factor(df.test$Face, levels = c("face supérieure", "face inférieure"))
unique(df.test$Numéro.Bloc.échantillon)

df.test <- dplyr::arrange(df.test, Type.Bloc, Face, Numéro.Bloc.échantillon)


# formule developed below; but then updated and modified in the for loop; do not run anymore
#####################################################################

{

## QEBM.1


# VFS.BM Bloc mobile

df.test %>% dplyr::filter(df.test$Type.Bloc == "Bloc mobile" && df.test$Face == "face supérieure") -> df.
df. <- dplyr::arrange(df., Numéro.Bloc.échantillon)

(  (df.$X..algues.brunes + df.$X..algues.rouges)
 +  df.$X..Lithophyllum
 + (df.$Nb.Littorina.obtusata + df.$Nb.Gibbula.cineraria + df.$Nb.Gibbula.pennanti + df.$Nb.Gibbula.umbilicalis)
 + (df.$X..Eponges + df.$X..Ascidies.Coloniales + df.$X..Ascidies.Solitaires)
) - 
(   df.$X..algues.vertes
 +  df.$X..Roche.Nue
) -> VFS.BM
VFS.BM  

(A <- df.$X..algues.brunes + df.$X..algues.rouges)
(B <- df.$X..Lithophyllum)
(C <- df.$Nb.Littorina.obtusata + df.$Nb.Gibbula.cineraria + df.$Nb.Gibbula.pennanti + df.$Nb.Gibbula.umbilicalis)
(D <- df.$X..Eponges + df.$X..Ascidies.Coloniales + df.$X..Ascidies.Solitaires)
(E <- df.$X..algues.vertes)
(F <- df.$X..Roche.Nue)


# VFI.BM Bloc mobile         

df.test %>% dplyr::filter(df.test$Type.Bloc == "Bloc mobile" && df.test$Face == "face inférieure") -> df.
df. <- dplyr::arrange(df., Numéro.Bloc.échantillon)
(  (df.$X..Eponges + df.$X..Ascidies.Coloniales + df.$X..Ascidies.Solitaires)
  +  df.$X..Lithophyllum 
) - 
(  (df.$X..algues.brunes + df.$X..algues.rouges)
  + (df.$Nb.Littorina.obtusata + df.$Nb.Gibbula.cineraria + df.$Nb.Gibbula.pennanti + df.$Nb.Gibbula.umbilicalis)
  +  df.$X..algues.vertes
  +  df.$X..Roche.Nue
) -> VFI.BM            
VFI.BM

(Aprim <- df.$X..algues.brunes + df.$X..algues.rouges)
(Bprim <- df.$X..Lithophyllum) # !! differ !!
(Cprim <- df.$Nb.Littorina.obtusata + df.$Nb.Gibbula.cineraria + df.$Nb.Gibbula.pennanti + df.$Nb.Gibbula.umbilicalis) # !! differ !! 
(Dprim <- df.$X..Eponges + df.$X..Ascidies.Coloniales + df.$X..Ascidies.Solitaires) # !! differ !! for éponges + ascidies
(Eprim <- df.$X..algues.vertes)
(Fprim <- df.$X..Roche.Nue)
# for both values that differ, it comes from % Litophyllum and éponges + ascidies


# VFSI.BM Bloc mobile

df.test %>% dplyr::filter(df.test$Type.Bloc == "Bloc mobile") -> df.
num.bloc <- as.vector(sort(unique(df.$Numéro.Bloc.échantillon)))
VFSI.BM <- NA

for (i in c(1:length(num.bloc))) {

j. <- num.bloc[i]

VFSIn. <- unname(unlist(
 ( (dplyr::filter(df., df.$Numéro.Bloc.échantillon == j. && df.$Face == "face supérieure")["Nb.spirorbis.total"]
 + dplyr::filter(df., df.$Numéro.Bloc.échantillon == j. && df.$Face == "face inférieure")["Nb.spirorbis.total"]
 ) / 1000 )
 -
 ( 
  ( (dplyr::filter(df., df.$Numéro.Bloc.échantillon == j. && df.$Face == "face supérieure")["X..Balanes.Vivantes"]
   + dplyr::filter(df., df.$Numéro.Bloc.échantillon == j. && df.$Face == "face inférieure")["X..Balanes.Vivantes"]
  ) / 100 )
  + ( (dplyr::filter(df., df.$Numéro.Bloc.échantillon == j. && df.$Face == "face supérieure")["Nb.Spirobranchus.lamarckii.total"]
      + dplyr::filter(df., df.$Numéro.Bloc.échantillon == j. && df.$Face == "face inférieure")["Nb.Spirobranchus.lamarckii.total"]
  ) / 100 )
 )
)) 

VFSI.BM <<- c(VFSI.BM, VFSIn.)

}

VFSI.BM <- VFSI.BM[2:length(VFSI.BM)]
VFSI.BM


# QEBM.1

(QEBM.1 <- VFS.BM + VFI.BM + VFSI.BM)


# comparison with excel file
VFS.BM
VFI.BM #  differences come from % Litophyllum and éponges + ascidies
VFSI.BM
QEBM.1


## QEBM.2 


# VrFS.BF moyenne Bloc fixé ; = VDRmoyenne in excel file

df.test %>% dplyr::filter(df.test$Type.Bloc %in% c("Bloc fixé", "Roche en place")) -> df.
df. <- dplyr::arrange(df., Numéro.Bloc.échantillon)
(  (df.$X..algues.brunes + df.$X..algues.rouges)
  +  df.$X..Lithophyllum
  + (df.$Nb.Littorina.obtusata + df.$Nb.Gibbula.cineraria + df.$Nb.Gibbula.pennanti + df.$Nb.Gibbula.umbilicalis)
  + (df.$X..Eponges + df.$X..Ascidies.Coloniales + df.$X..Ascidies.Solitaires)
) - 
  (   df.$X..algues.vertes
      +  df.$X..Roche.Nue
  ) -> VrFS.BF # different from Pauline, check with her
VrFS.BF


# (G-(H+I)) Bloc fixé & Roche en place

( df.$Nb.spirorbis.total/1000
  - (df.$X..Balanes.Vivantes/100 + df.$Nb.Spirobranchus.lamarckii.total/100)
) -> `(G-(H+I))BF`
`(G-(H+I))BF`


# VrFS.BF.moy

(mean(VrFS.BF + `(G-(H+I))BF`) -> VrFS.BF.moy)


# (G-(H+I)S.BM) Bloc mobile face supérieure

df.test %>% dplyr::filter(df.test$Type.Bloc == "Bloc mobile" && df.test$Face == "face supérieure") -> df.
df. <- dplyr::arrange(df., Numéro.Bloc.échantillon)
( df.$Nb.spirorbis.total/1000
  - (df.$X..Balanes.Vivantes/100 + df.$Nb.Spirobranchus.lamarckii.total/100)
) -> `(G-(H+I))S.BM`
`(G-(H+I))S.BM`


# VrFS.BM

(VFS.BM + `(G-(H+I))S.BM` -> VrFS.BM)


# VrFS.BM.moy

(mean(VrFS.BM) -> VrFS.BM.moy)


# |VrFS.BM.moy/VrFS.BF.moy|

(abs(mean(VrFS.BM, na.rm = T)/VrFS.BF.moy) -> `|VrFS.BM.moy/VrFS.BF.moy|`)


# QEBM.2

(QEBM.1 * `|VrFS.BM.moy/VrFS.BF.moy|` -> QEBM.2)


## QECB

mean(QEBM.2, na.rm = T) -> QECB

mean(QEBM.2, na.rm = T) #-> mean.QEBM.2
sd(QEBM.2, na.rm = T) #-> sd.QEBM.2
median(QEBM.2, na.rm = T) #-> median.QEBM.2

(bp. <- boxplot(QEBM.2))
bp.$stats


QEBM.2
QEBM.1
VFI.BM
Bprim
Dprim
B
D
# all the differences come from % Litophyllum and éponges + ascidies

qecb.val.qu. <- data.frame(matrix(ncol = 9, nrow = 10))
colnames(qecb.val.qu.) <- c("VFS.BM", "VFI.BM", "VFSI.BM", "QEBM.1", "VrFS.BF", "(G-(H+I))S.BM", "(G-(H+I))BF", "VrFS.BM", "QEBM.2")

qecb.val.qu.$VFS.BM <- VFS.BM
qecb.val.qu.$VFI.BM <- VFI.BM
qecb.val.qu.$VFSI.BM <- VFSI.BM
qecb.val.qu.$QEBM.1 <- QEBM.1
qecb.val.qu.$VrFS.BF <- VrFS.BF
qecb.val.qu.$`(G-(H+I))S.BM` <- `(G-(H+I))S.BM`
qecb.val.qu.$`(G-(H+I))BF` <- `(G-(H+I))BF`
qecb.val.qu.$VrFS.BM <- VrFS.BM
qecb.val.qu.$QEBM.2 <- QEBM.2


qecb.val. <- data.frame(matrix(ncol = 4, nrow = 1))
colnames(qecb.val.) <- c("VrFS.BM.moy", "VrFS.BF.moy", "|VrFS.BM.moy/VrFS.BF.moy|", "QECB")

qecb.val.$VrFS.BM.moy <- VrFS.BM.moy
qecb.val.$VrFS.BF.moy <- VrFS.BF.moy
qecb.val.$`|VrFS.BM.moy/VrFS.BF.moy|` <- `|VrFS.BM.moy/VrFS.BF.moy|`
qecb.val.$QECB <- QECB

}

#####################################################################


## QECB in a for loop => several issues to solve


library(dplyr)
unique(qecb$Site)

unique(qecb$Type.Bloc)
qecb$Type.Bloc <- factor(qecb$Type.Bloc, levels = c("Bloc mobile", "Bloc fixé", "Roche en place"))
unique(qecb$Face)
qecb$Face <- factor(qecb$Face, levels = c("face supérieure", "face inférieure"))
unique(qecb$Numéro.Bloc.échantillon)

qecb <- dplyr::arrange(qecb, Type.Bloc, Face, Numéro.Bloc.échantillon)

qecb <- tibble::add_column(qecb, Site.Year.Month.Day = paste0(qecb$Site, ".", qecb$Year, ".", qecb$Month, ".", qecb$Day), .after = "Site_bis")

# save qecb as a new df. for analysis purpose => several changes to operate to run the code and functions

qecbNew <- qecb

qecb.val.qu.list <- vector("list", length(unique(qecbNew$Site.Year.Month.Day)))
qecb.val.list <- vector("list", length(unique(qecbNew$Site.Year.Month.Day)))


# df with list object nb and corresponding Site.Year.Month.Day value to solve for loop issues
df.list.loop <- data.frame("Site.Year.Month.Day" = unique(qecbNew$Site.Year.Month.Day),
     "loop nb" = c(1:length(unique(qecbNew$Site.Year.Month.Day))))


# dplyr::filter for df that makes problem, then eventually correct in the dataframe for wrong coding; brackets (xx) for nb because will change when qecb df. enlarged.


# list nb (28) - EGMP_BreeBains.2016.04.06
qecbNew$Face <- as.character(qecbNew$Face)
qecbNew$Face <- ifelse(qecbNew$ID.Fiche == "BDD_IVR&QECB_La Bree_20160406_VImport.xlsx" && qecbNew$Référence.bloc == "avr16-LaBreeB9sup", "face supérieure", qecbNew$Face)
qecbNew$Face <- ifelse(qecbNew$ID.Fiche == "BDD_IVR&QECB_La Bree_20160406_VImport.xlsx" && qecbNew$Référence.bloc == "avr16-LaBreeB10sup", "face supérieure", qecbNew$Face)
qecbNew$Face <- as.factor(qecbNew$Face)
unique(qecbNew$Face)

# list nb 33 - EGMP_PerreAntiochat.2016.04.07
qecbNew$Face <- as.character(qecbNew$Face)
qecbNew$Face <- ifelse(qecbNew$ID.Fiche == "BDD_IVR&QECB_PerAnt_20160407_VImport.xlsx" && qecbNew$Référence.bloc == "avr16-PerAntB9sup", "face supérieure", qecbNew$Face)
qecbNew$Face <- ifelse(qecbNew$ID.Fiche == "BDD_IVR&QECB_PerAnt_20160407_VImport.xlsx" && qecbNew$Référence.bloc == "avr16-PerAntB10sup", "face supérieure", qecbNew$Face)
qecbNew$Face <- as.factor(qecbNew$Face)
unique(qecbNew$Face)

# list nb 37 - EGMP_Chassiron.2016.03.09
qecbNew$Face <- as.character(qecbNew$Face)
qecbNew$Face <- ifelse(qecbNew$ID.Fiche == "BDD_IVR&QECB_Chassiron_20160309&10_VImport.xlsx" && qecbNew$Référence.bloc == "mars16-ChassB9sup", "face supérieure", qecbNew$Face)
qecbNew$Face <- ifelse(qecbNew$ID.Fiche == "BDD_IVR&QECB_Chassiron_20160309&10_VImport.xlsx" && qecbNew$Référence.bloc == "mars16-ChasB10sup", "face supérieure", qecbNew$Face)
qecbNew$Face <- as.factor(qecbNew$Face)
unique(qecbNew$Face)

# list nb 76 - ARMO_Verdelet.2015.03.23
qecbNew$Face <- as.character(qecbNew$Face)
qecbNew$Face <- ifelse(qecbNew$ID.Fiche == "BDD_IVR&QECB_Verdelet_20150323_VImport.xlsx" && qecbNew$Référence.bloc == "mar15-VerB10inf", "face inférieure", qecbNew$Face)
qecbNew$Face <- as.factor(qecbNew$Face)
unique(qecbNew$Face)

# list nb 116 - "GDMO_Locmariaquer.2018.09.10"
qecbNew$Type.Bloc <- as.character(qecbNew$Type.Bloc)
qecbNew$Type.Bloc <- ifelse(qecbNew$ID.Fiche == "2018-09-10-GDMO-CDB-001" && qecbNew$Numero.Photo == "2018-09-10_GDMO_01_CDB-5_sup_392578.jpg", "Roche en place", qecbNew$Type.Bloc)
qecbNew$Type.Bloc <- as.factor(qecbNew$Type.Bloc)
qecbNew$Quadrat.bis <- ifelse(qecbNew$ID.Fiche == "2018-09-10-GDMO-CDB-001" && qecbNew$Numero.Photo == "2018-09-10_GDMO_01_CDB-5_sup_392578.jpg", "Q5", qecbNew$Quadrat.bis)
qecbNew %>% dplyr::filter(!(qecbNew$ID.Fiche == "2018-09-10-GDMO-CDB-001" && qecbNew$Numero.Photo == "")) -> qecbNew
dplyr::filter(qecbNew, qecbNew$Site == "GDMO_Locmariaquer" && qecbNew$date_fiche == "2018-09-10") -> issue.Locma

#qecbNew  %>% dplyr::filter(Site.Year.Month.Day == unique(qecbNew$Site.Year.Month.Day)[[102]]) -> qecb.i
qecbNew  %>% dplyr::filter(qecbNew$Site.Year.Month.Day == "PDMO_Perharidy.2014.09.09") -> qecb.i

# We are facing an issues with NA observations, because either they were not measured/counted, then they are effectively NAs; or these NAs = indeed "0"; but I cannot have NAs for variables that are included in the index determination, cfr if 5+0 = 5, 5+NA = NA; see for example Site.Year.Month.Day == "ARMO_Bilfot.2014.04.28", Nb.Spirobranchus.lamarckii.total is NA, and therefore VFSI.BM ends up to be NA as well ...
# for now on, I'll change these NAs by 0
#qecbNew  %>% dplyr::filter(Site.Year.Month.Day == "ARMO_Bilfot.2014.04.28") -> qecb.i


# Few sites to remove prior running the for loop

qecbNew  %>% dplyr::filter(qecbNew$Site.Year.Month.Day == "FINS_StNicolasGlenan.2016.04.08") -> qecb.i # no bloc fixe !
qecbNew  %>% dplyr::filter(qecbNew$Site.Year.Month.Day != "FINS_StNicolasGlenan.2016.04.08") -> qecbNew

qecbNew  %>% dplyr::filter(qecbNew$Site.Year.Month.Day == "GDMO_Locmariaquer.2019.09.30") -> qecb.i # most faces of blocs mobiles do not correspond to each other; only 3 over 10 boulder have data for both face supérieure and face inférieure
qecbNew  %>% dplyr::filter(qecbNew$Site.Year.Month.Day != "GDMO_Locmariaquer.2019.09.30") -> qecbNew

#qecbNew  %>% dplyr::filter(Site.Year.Month.Day == unique(qecbNew$Site.Year.Month.Day)[[102]]) -> qecb.i

rm(df.list.loop, qecb.i
   #, qecb.val.list, qecb.val.qu.list
   )


# check for species with count within sub-0.1m^2-quadrat

# first for Spirobranchus 

qecbNew$Nb.Spirobranchus.lamarckii.total.ini <- qecbNew$Nb.Spirobranchus.lamarckii.total
qecbNew$Nb.Spirobranchus.lamarckii.total <- as.character(qecbNew$Nb.Spirobranchus.lamarckii.total)

table(qecbNew$Nb.Spirobranchus.lamarckii.total)
subset(qecbNew, is.na(qecbNew$Nb.Spirobranchus.lamarckii.total))
nrow(subset(qecbNew, is.na(qecbNew$Nb.Spirobranchus.lamarckii.total)))
subset(qecbNew, is.nan(qecbNew$Nb.Spirobranchus.lamarckii.total))
subset(qecbNew, is.finite(qecbNew$Nb.Spirobranchus.lamarckii.total))
nrow(dplyr::filter(qecbNew, qecbNew$Nb.Spirobranchus.lamarckii.total %in% c(NA, "NaN", "Inf", "-Inf")))

qecbNew.Spirobranchus <- (dplyr::filter(qecbNew, qecbNew$Nb.Spirobranchus.lamarckii.total %in% c(NA, "NaN", "Inf", "-Inf")))
qecbNew.Spirobranchus[, c("Nb.Spirobranchus.lamarckii.1B", "Nb.Spirobranchus.lamarckii.2B", "Nb.Spirobranchus.lamarckii.3B", "Nb.Spirobranchus.lamarckii.4B", "Nb.Spirobranchus.lamarckii.5B")] <- sapply(qecbNew.Spirobranchus[, c("Nb.Spirobranchus.lamarckii.1B", "Nb.Spirobranchus.lamarckii.2B", "Nb.Spirobranchus.lamarckii.3B", "Nb.Spirobranchus.lamarckii.4B", "Nb.Spirobranchus.lamarckii.5B")],as.character)
(Spirobranchus.data <- subset(qecbNew.Spirobranchus, !is.na(qecbNew.Spirobranchus$Nb.Spirobranchus.lamarckii.1B) | !is.na(qecbNew.Spirobranchus$Nb.Spirobranchus.lamarckii.2B) | !is.na(qecbNew.Spirobranchus$Nb.Spirobranchus.lamarckii.3B) | !is.na(qecbNew.Spirobranchus$Nb.Spirobranchus.lamarckii.4B) | !is.na(qecbNew.Spirobranchus$Nb.Spirobranchus.lamarckii.5B))[, c("Site.Year.Month.Day", "Nb.Spirobranchus.lamarckii.1B", "Nb.Spirobranchus.lamarckii.2B", "Nb.Spirobranchus.lamarckii.3B", "Nb.Spirobranchus.lamarckii.4B", "Nb.Spirobranchus.lamarckii.5B", "Nb.Spirobranchus.lamarckii.total")])
unique(Spirobranchus.data$Site.Year.Month.Day)

Quemenes <- dplyr::filter(qecbNew, qecbNew$Site == "FINS_Quemenes")
Quemenes <- dplyr::arrange(Quemenes, date_fiche)
# for Quemenes, issue because for sampling date "FINS_Quemenes.2015.09.30" the 5 counts of Spirobranchus were encoded in 1B instead of total !!! I noticed this issue when mining data (see below), therefore I corrected before running below script for Spirobranchus. 
qecbNew$Nb.Spirobranchus.lamarckii.total <- ifelse(qecbNew$Site.Year.Month.Day == "FINS_Quemenes.2015.09.30" & is.na(qecbNew$Nb.Spirobranchus.lamarckii.total), qecbNew$Nb.Spirobranchus.lamarckii.1B, qecbNew$Nb.Spirobranchus.lamarckii.total)
(Quemenes <- dplyr::filter(qecbNew, qecbNew$Site.Year.Month.Day == "FINS_Quemenes.2015.09.30")[, c("Site.Year.Month.Day", "Nb.Spirobranchus.lamarckii.1B", "Nb.Spirobranchus.lamarckii.2B", "Nb.Spirobranchus.lamarckii.3B", "Nb.Spirobranchus.lamarckii.4B", "Nb.Spirobranchus.lamarckii.5B", "Nb.Spirobranchus.lamarckii.total")])
rm(Quemenes)

Piegu <- dplyr::filter(qecbNew, qecbNew$Site == "ARMO_Piegu")
Piegu <- dplyr::arrange(Piegu, date_fiche)
rm(Piegu)

SeinKilaourou <- dplyr::filter(qecbNew, qecbNew$Site == "FINS_SeinKilaourou")
SeinKilaourou <- dplyr::arrange(SeinKilaourou, date_fiche)
# same issue with SeinKilaourou
qecbNew$Nb.Spirobranchus.lamarckii.total <- ifelse(qecbNew$Site.Year.Month.Day == "FINS_SeinKilaourou.2015.04.21" & is.na(qecbNew$Nb.Spirobranchus.lamarckii.total), qecbNew$Nb.Spirobranchus.lamarckii.1B, qecbNew$Nb.Spirobranchus.lamarckii.total)
(SeinKilaourou <- dplyr::filter(qecbNew, qecbNew$Site.Year.Month.Day == "FINS_SeinKilaourou.2015.04.21")[, c("Site.Year.Month.Day", "Nb.Spirobranchus.lamarckii.1B", "Nb.Spirobranchus.lamarckii.2B", "Nb.Spirobranchus.lamarckii.3B", "Nb.Spirobranchus.lamarckii.4B", "Nb.Spirobranchus.lamarckii.5B", "Nb.Spirobranchus.lamarckii.total")])
rm(SeinKilaourou)

# some more issues however with "x100"count data

Spirobranchus <- subset(qecbNew, !is.na(qecbNew$Nb.Spirobranchus.lamarckii.1B) & !is.na(qecbNew$Nb.Spirobranchus.lamarckii.2B) & !is.na(qecbNew$Nb.Spirobranchus.lamarckii.3B) & !is.na(qecbNew$Nb.Spirobranchus.lamarckii.4B) & !is.na(qecbNew$Nb.Spirobranchus.lamarckii.5B) & !is.na(qecbNew$Nb.Spirobranchus.lamarckii.total))[, c("Site.Year.Month.Day", "Nb.Spirobranchus.lamarckii.1B", "Nb.Spirobranchus.lamarckii.2B", "Nb.Spirobranchus.lamarckii.3B", "Nb.Spirobranchus.lamarckii.4B", "Nb.Spirobranchus.lamarckii.5B", "Nb.Spirobranchus.lamarckii.total")]
for (i in c(1:nrow(Spirobranchus))) {
  Spirobranchus$mean.x.100[[i]] <- sum(Spirobranchus[i, c(2:6)], na.rm = T )/sum(!is.na(Spirobranchus[i, c(2:6)])) * 100
} 
Spirobranchus$mean.x.100 <- unlist(Spirobranchus$mean.x.100)
Spirobranchus$Nb.Spirobranchus.lamarckii.total <- as.numeric(Spirobranchus$Nb.Spirobranchus.lamarckii.total)
for (i in c(1:nrow(Spirobranchus))) {
  Spirobranchus$diff[[i]] <- Spirobranchus[i, "Nb.Spirobranchus.lamarckii.total"] - Spirobranchus[i, "mean.x.100"]
} 
Spirobranchus$diff <- abs(as.integer(Spirobranchus$diff))
Spirobranchus <- dplyr::arrange(Spirobranchus, desc(diff), mean.x.100)
Spirobranchus <- dplyr::arrange(dplyr::filter(Spirobranchus, Spirobranchus$diff != 0 & Spirobranchus$mean.x.100 != 0), desc(diff))

# check it all in the qecbNew df

for (i in c(1:nrow(qecbNew))) { 
  qecbNew$mean.x.100[[i]] <- 
    #ifelse(qecbNew$Nb.Spirobranchus.lamarckii.total[[i]] %in% c(NA, "NaN", "Inf", "-Inf"), 
    sum(qecbNew[i, c("Nb.Spirobranchus.lamarckii.1B", "Nb.Spirobranchus.lamarckii.2B", "Nb.Spirobranchus.lamarckii.3B", "Nb.Spirobranchus.lamarckii.4B", "Nb.Spirobranchus.lamarckii.5B")], na.rm = T )/sum(!is.na(qecbNew[i, c("Nb.Spirobranchus.lamarckii.1B", "Nb.Spirobranchus.lamarckii.2B", "Nb.Spirobranchus.lamarckii.3B", "Nb.Spirobranchus.lamarckii.4B", "Nb.Spirobranchus.lamarckii.5B")]))*100
  #, qecbNew$Nb.Spirobranchus.lamarckii.total[[i]]) 
} # sum of only NAs/0 = NaN; so replace NaN by Na
qecbNew$mean.x.100 <- as.character(qecbNew$mean.x.100)
#subset(qecbNew, is.na(qecbNew$mean.x.200))[,"Nb.Spirobranchus.lamarckii.total"]
#subset(qecbNew, is.nan(qecbNew$mean.x.200))[,"Nb.Spirobranchus.lamarckii.total"]

sort(dplyr::filter(qecbNew, paste0(qecbNew$id_qecb, "_", qecbNew$Site.Year.Month.Day, "_", qecbNew$Type.Bloc, "_", qecbNew$Numéro.Bloc.échantillon, "_", qecbNew$Face) %in% paste0(qecbNew.Spirobranchus$id_qecb, "_", qecbNew.Spirobranchus$Site.Year.Month.Day, "_", qecbNew.Spirobranchus$Type.Bloc, "_", qecbNew.Spirobranchus$Numéro.Bloc.échantillon, "_", qecbNew.Spirobranchus$Face))[, "mean.x.100"])

for (i in c(1:nrow(qecbNew))) {
  qecbNew$mean.x.100[[i]] <- ifelse(qecbNew$mean.x.100[[i]] == "NaN", NA, qecbNew$mean.x.100[[i]])
}
nrow(subset(qecbNew, is.na(qecbNew$mean.x.100)))
qecbNew$mean.x.100 <- as.integer(qecbNew$mean.x.100)

qecbNew$Nb.Spirobranchus.lamarckii.total <- as.integer(qecbNew$Nb.Spirobranchus.lamarckii.total)
unique(qecbNew$Nb.Spirobranchus.lamarckii.total - qecbNew$mean.x.100) 
table(qecbNew$Nb.Spirobranchus.lamarckii.total - qecbNew$mean.x.100)
qecbNew$Nb.Spirobranchus.lamarckii.total.diff <- abs((qecbNew$Nb.Spirobranchus.lamarckii.total - qecbNew$mean.x.100))
Spirobranchus.diff <- qecbNew[, c("Nb.Spirobranchus.lamarckii.1B", "Nb.Spirobranchus.lamarckii.2B", "Nb.Spirobranchus.lamarckii.3B", "Nb.Spirobranchus.lamarckii.4B", "Nb.Spirobranchus.lamarckii.5B", "Nb.Spirobranchus.lamarckii.total", "Nb.Spirobranchus.lamarckii.total.ini", "mean.x.100", "Nb.Spirobranchus.lamarckii.total.diff")]
Spirobranchus.diff <- dplyr::arrange(Spirobranchus.diff, desc(Nb.Spirobranchus.lamarckii.total.diff), mean.x.100)
Spirobranchus.diff <- dplyr::arrange(dplyr::filter(Spirobranchus.diff, Nb.Spirobranchus.lamarckii.total.diff != 0 & mean.x.100 != 0), desc(Nb.Spirobranchus.lamarckii.total.diff))

qecbNew$Nb.Spirobranchus.lamarckii.total <- ifelse(qecbNew$Nb.Spirobranchus.lamarckii.total.diff != 0 & qecbNew$mean.x.100 != 0, qecbNew$mean.x.100, qecbNew$Nb.Spirobranchus.lamarckii.total)
Spirobranchus.diff <- qecbNew[, c("Nb.Spirobranchus.lamarckii.1B", "Nb.Spirobranchus.lamarckii.2B", "Nb.Spirobranchus.lamarckii.3B", "Nb.Spirobranchus.lamarckii.4B", "Nb.Spirobranchus.lamarckii.5B", "Nb.Spirobranchus.lamarckii.total", "Nb.Spirobranchus.lamarckii.total.ini", "mean.x.100", "Nb.Spirobranchus.lamarckii.total.diff")]
Spirobranchus.diff$Nb.Spirobranchus.lamarckii.total.diff <- abs(as.integer(Spirobranchus.diff$Nb.Spirobranchus.lamarckii.total.diff))
Spirobranchus.diff <- dplyr::arrange(Spirobranchus.diff, desc(Nb.Spirobranchus.lamarckii.total.diff), mean.x.100)
Spirobranchus.diff <- dplyr::arrange(dplyr::filter(Spirobranchus.diff, Nb.Spirobranchus.lamarckii.total.diff != 0 & mean.x.100 != 0), desc(Nb.Spirobranchus.lamarckii.total.diff))
# ok, change made when data x 100 was not correct.

# finally, change NA by mean.x100 for Spirobranchus total
qecbNew$Nb.Spirobranchus.lamarckii.total <- as.character(qecbNew$Nb.Spirobranchus.lamarckii.total)
for (i in c(1:nrow(qecbNew))) { 
  qecbNew$Nb.Spirobranchus.lamarckii.total[[i]] <- ifelse(qecbNew$Nb.Spirobranchus.lamarckii.total[[i]] %in% c(NA, "NaN", "Inf", "-Inf"), sum(qecbNew[i, c("Nb.Spirobranchus.lamarckii.1B", "Nb.Spirobranchus.lamarckii.2B", "Nb.Spirobranchus.lamarckii.3B", "Nb.Spirobranchus.lamarckii.4B", "Nb.Spirobranchus.lamarckii.5B")], na.rm = T )/sum(!is.na(qecbNew[i, c("Nb.Spirobranchus.lamarckii.1B", "Nb.Spirobranchus.lamarckii.2B", "Nb.Spirobranchus.lamarckii.3B", "Nb.Spirobranchus.lamarckii.4B", "Nb.Spirobranchus.lamarckii.5B")]))*100, qecbNew$Nb.Spirobranchus.lamarckii.total[[i]]) 
} # sum of only NAs/0 = NaN; so replace NaN by Na

sort(dplyr::filter(qecbNew, paste0(qecbNew$id_qecb, "_", qecbNew$Site.Year.Month.Day, "_", qecbNew$Type.Bloc, "_", qecbNew$Numéro.Bloc.échantillon, "_", qecbNew$Face) %in% paste0(qecbNew.Spirobranchus$id_qecb, "_", qecbNew.Spirobranchus$Site.Year.Month.Day, "_", qecbNew.Spirobranchus$Type.Bloc, "_", qecbNew.Spirobranchus$Numéro.Bloc.échantillon, "_", qecbNew.Spirobranchus$Face))[,"Nb.Spirobranchus.lamarckii.total"])

for (i in c(1:nrow(qecbNew))) {
  qecbNew$Nb.Spirobranchus.lamarckii.total[[i]] <- ifelse(qecbNew$Nb.Spirobranchus.lamarckii.total[[i]] == "NaN", NA, qecbNew$Nb.Spirobranchus.lamarckii.total[[i]])
}
nrow(subset(qecbNew, is.na(qecbNew$Nb.Spirobranchus.lamarckii.total)))
qecbNew$Nb.Spirobranchus.lamarckii.total <- as.integer(qecbNew$Nb.Spirobranchus.lamarckii.total)

unique(qecbNew$Nb.Spirobranchus.lamarckii.total - qecbNew$Nb.Spirobranchus.lamarckii.total.ini) 
table(qecbNew$Nb.Spirobranchus.lamarckii.total - qecbNew$Nb.Spirobranchus.lamarckii.total.ini)
qecbNew$Nb.Spirobranchus.lamarckii.total.diff <- abs(qecbNew$Nb.Spirobranchus.lamarckii.total - qecbNew$Nb.Spirobranchus.lamarckii.total.ini)
Spirobranchus.diff <- qecbNew[, c("Nb.Spirobranchus.lamarckii.1B", "Nb.Spirobranchus.lamarckii.2B", "Nb.Spirobranchus.lamarckii.3B", "Nb.Spirobranchus.lamarckii.4B", "Nb.Spirobranchus.lamarckii.5B", "Nb.Spirobranchus.lamarckii.total", "Nb.Spirobranchus.lamarckii.total.ini", "mean.x.100", "Nb.Spirobranchus.lamarckii.total.diff")]
Spirobranchus.diff <- dplyr::arrange(Spirobranchus.diff, desc(Nb.Spirobranchus.lamarckii.total.diff), mean.x.100)
Spirobranchus.diff <- dplyr::arrange(dplyr::filter(Spirobranchus.diff, Nb.Spirobranchus.lamarckii.total.diff != 0 & mean.x.100 != 0), desc(Nb.Spirobranchus.lamarckii.total.diff))
table(qecbNew$Nb.Spirobranchus.lamarckii.total.diff)
length(na.omit(qecbNew$Nb.Spirobranchus.lamarckii.total))
sum(is.na(qecbNew$Nb.Spirobranchus.lamarckii.total))
length(na.omit(qecbNew$Nb.Spirobranchus.lamarckii.total))+sum(is.na(qecbNew$Nb.Spirobranchus.lamarckii.total))

qecbNew <- subset(qecbNew, select = -c(Nb.Spirobranchus.lamarckii.total.ini, mean.x.100, Nb.Spirobranchus.lamarckii.total.diff))

rm(qecbNew.Spirobranchus, Spirobranchus, Spirobranchus.data, Spirobranchus.diff)


# do the same for spirorbis

qecbNew$Nb.spirorbis.total.ini <- qecbNew$Nb.spirorbis.total
qecbNew$Nb.spirorbis.total <- as.character(qecbNew$Nb.spirorbis.total)

table(qecbNew$Nb.spirorbis.total)
subset(qecbNew, is.na(qecbNew$Nb.spirorbis.total))
nrow(subset(qecbNew, is.na(qecbNew$Nb.spirorbis.total)))
subset(qecbNew, is.nan(qecbNew$Nb.spirorbis.total))
subset(qecbNew, is.finite(qecbNew$Nb.spirorbis.total))

nrow(dplyr::filter(qecbNew, qecbNew$Nb.spirorbis.total %in% c(NA, "NaN", "Inf", "-Inf")))

qecbNew.spirorbis <- (dplyr::filter(qecbNew, Nb.spirorbis.total %in% c(NA, "NaN", "Inf", "-Inf")))
qecbNew.spirorbis[, c("Nb.spirorbis.1A", "Nb.spirorbis.2A", "Nb.spirorbis.3A", "Nb.spirorbis.4A", "Nb.spirorbis.5A")] <- sapply(qecbNew.spirorbis[, c("Nb.spirorbis.1A", "Nb.spirorbis.2A", "Nb.spirorbis.3A", "Nb.spirorbis.4A", "Nb.spirorbis.5A")],as.character)
(spirorbis.data <- subset(qecbNew.spirorbis, !is.na(qecbNew.spirorbis$Nb.spirorbis.1A) | !is.na(qecbNew.spirorbis$Nb.spirorbis.2A) | !is.na(qecbNew.spirorbis$Nb.spirorbis.3A) | !is.na(qecbNew.spirorbis$Nb.spirorbis.4A) | !is.na(qecbNew.spirorbis$Nb.spirorbis.5A))[, c("Site.Year.Month.Day", "Nb.spirorbis.1A", "Nb.spirorbis.2A", "Nb.spirorbis.3A", "Nb.spirorbis.4A", "Nb.spirorbis.5A", "Nb.spirorbis.total")])
unique(spirorbis.data$Site.Year.Month.Day)

# In contrast to Spirobranchus data, no encoding issues for spirorbis data, cfr when sub-quadrat 1A-5A are ALL encoded, NA for total. 

# some more issues however with "x200"count data

spirorbis <- subset(qecbNew, !is.na(qecbNew$Nb.spirorbis.1A) & !is.na(qecbNew$Nb.spirorbis.2A) & !is.na(qecbNew$Nb.spirorbis.3A) & !is.na(qecbNew$Nb.spirorbis.4A) & !is.na(qecbNew$Nb.spirorbis.5A) & !is.na(qecbNew$Nb.spirorbis.total))[, c("Site.Year.Month.Day", "Nb.spirorbis.1A", "Nb.spirorbis.2A", "Nb.spirorbis.3A", "Nb.spirorbis.4A", "Nb.spirorbis.5A", "Nb.spirorbis.total")]
for (i in c(1:nrow(spirorbis))) {
    spirorbis$mean.x.200[[i]] <- sum(spirorbis[i, c(2:6)], na.rm = T )/sum(!is.na(spirorbis[i, c(2:6)])) * 200
} 
spirorbis$mean.x.200 <- unlist(spirorbis$mean.x.200)
spirorbis$Nb.spirorbis.total <- as.numeric(spirorbis$Nb.spirorbis.total)
for (i in c(1:nrow(spirorbis))) {
  spirorbis$diff[[i]] <- spirorbis[i, "Nb.spirorbis.total"] - spirorbis[i, "mean.x.200"]
} 
spirorbis$diff <- abs(as.integer(spirorbis$diff))
spirorbis <- dplyr::arrange(spirorbis, desc(diff), mean.x.200)
(GONB_IlotStMichel.2015.04.18 <- dplyr::filter(spirorbis, Site.Year.Month.Day == "GONB_IlotStMichel.2015.04.18"))
rm(GONB_IlotStMichel.2015.04.18)
spirorbis <- dplyr::arrange(dplyr::filter(spirorbis, diff != 0 & mean.x.200 != 0), desc(diff))

# check it all in the qecbNew df

for (i in c(1:nrow(qecbNew))) { 
    qecbNew$mean.x.200[[i]] <- 
      #ifelse(qecbNew$Nb.spirorbis.total[[i]] %in% c(NA, "NaN", "Inf", "-Inf"), 
      sum(qecbNew[i, c("Nb.spirorbis.1A", "Nb.spirorbis.2A", "Nb.spirorbis.3A", "Nb.spirorbis.4A", "Nb.spirorbis.5A")], na.rm = T )/sum(!is.na(qecbNew[i, c("Nb.spirorbis.1A", "Nb.spirorbis.2A", "Nb.spirorbis.3A", "Nb.spirorbis.4A", "Nb.spirorbis.5A")]))*200
    #, qecbNew$Nb.spirorbis.total[[i]]) 
} # sum of only NAs/0 = NaN; so replace NaN by Na
qecbNew$mean.x.200 <- as.character(qecbNew$mean.x.200)
#subset(qecbNew, is.na(qecbNew$mean.x.200))[,"Nb.spirorbis.total"]
#subset(qecbNew, is.nan(qecbNew$mean.x.200))[,"Nb.spirorbis.total"]

sort(dplyr::filter(qecbNew, paste0(qecbNew$id_qecb, "_", qecbNew$Site.Year.Month.Day, "_", qecbNew$Type.Bloc, "_", qecbNew$Numéro.Bloc.échantillon, "_", qecbNew$Face) %in% paste0(qecbNew.spirorbis$id_qecb, "_", qecbNew.spirorbis$Site.Year.Month.Day, "_", qecbNew.spirorbis$Type.Bloc, "_", qecbNew.spirorbis$Numéro.Bloc.échantillon, "_", qecbNew.spirorbis$Face))[, "mean.x.200"])

for (i in c(1:nrow(qecbNew))) {
  qecbNew$mean.x.200[[i]] <- ifelse(qecbNew$mean.x.200[[i]] == "NaN", NA, qecbNew$mean.x.200[[i]])
}
nrow(subset(qecbNew, is.na(qecbNew$mean.x.200)))
qecbNew$mean.x.200 <- as.integer(qecbNew$mean.x.200)

qecbNew$Nb.spirorbis.total <- as.integer(qecbNew$Nb.spirorbis.total)
unique(qecbNew$Nb.spirorbis.total - qecbNew$mean.x.200) 
table(qecbNew$Nb.spirorbis.total - qecbNew$mean.x.200)
qecbNew$Nb.spirorbis.total.diff <- abs((qecbNew$Nb.spirorbis.total - qecbNew$mean.x.200))
spirorbis.diff <- qecbNew[, c("Nb.spirorbis.1A", "Nb.spirorbis.2A", "Nb.spirorbis.3A", "Nb.spirorbis.4A", "Nb.spirorbis.5A", "Nb.spirorbis.total", "Nb.spirorbis.total.ini", "mean.x.200", "Nb.spirorbis.total.diff")]
spirorbis.diff <- dplyr::arrange(spirorbis.diff, desc(Nb.spirorbis.total.diff), mean.x.200)
spirorbis.diff <- dplyr::arrange(dplyr::filter(spirorbis.diff, Nb.spirorbis.total.diff != 0 & mean.x.200 != 0), desc(Nb.spirorbis.total.diff))

qecbNew$Nb.spirorbis.total <- ifelse(qecbNew$Nb.spirorbis.total.diff != 0 & qecbNew$mean.x.200 != 0, qecbNew$mean.x.200, qecbNew$Nb.spirorbis.total)
spirorbis.diff <- qecbNew[, c("Nb.spirorbis.1A", "Nb.spirorbis.2A", "Nb.spirorbis.3A", "Nb.spirorbis.4A", "Nb.spirorbis.5A", "Nb.spirorbis.total", "Nb.spirorbis.total.ini", "mean.x.200", "Nb.spirorbis.total.diff")]
spirorbis.diff$Nb.spirorbis.total.diff <- abs(as.integer(spirorbis.diff$Nb.spirorbis.total.diff))
spirorbis.diff <- dplyr::arrange(spirorbis.diff, desc(Nb.spirorbis.total.diff), mean.x.200)
spirorbis.diff <- dplyr::arrange(dplyr::filter(spirorbis.diff, Nb.spirorbis.total.diff != 0 & mean.x.200 != 0), desc(Nb.spirorbis.total.diff))
# ok, change made when data x 200 was not correct.

# finally, change NA by mean.x200 for spirorbis total
qecbNew$Nb.spirorbis.total <- as.character(qecbNew$Nb.spirorbis.total)
for (i in c(1:nrow(qecbNew))) { 
  qecbNew$Nb.spirorbis.total[[i]] <- ifelse(qecbNew$Nb.spirorbis.total[[i]] %in% c(NA, "NaN", "Inf", "-Inf"), sum(qecbNew[i, c("Nb.spirorbis.1A", "Nb.spirorbis.2A", "Nb.spirorbis.3A", "Nb.spirorbis.4A", "Nb.spirorbis.5A")], na.rm = T )/sum(!is.na(qecbNew[i, c("Nb.spirorbis.1A", "Nb.spirorbis.2A", "Nb.spirorbis.3A", "Nb.spirorbis.4A", "Nb.spirorbis.5A")]))*200, qecbNew$Nb.spirorbis.total[[i]]) 
} # sum of only NAs/0 = NaN; so replace NaN by Na

sort(dplyr::filter(qecbNew, paste0(qecbNew$id_qecb, "_", qecbNew$Site.Year.Month.Day, "_", qecbNew$Type.Bloc, "_", qecbNew$Numéro.Bloc.échantillon, "_", qecbNew$Face) %in% paste0(qecbNew.spirorbis$id_qecb, "_", qecbNew.spirorbis$Site.Year.Month.Day, "_", qecbNew.spirorbis$Type.Bloc, "_", qecbNew.spirorbis$Numéro.Bloc.échantillon, "_", qecbNew.spirorbis$Face))[,"Nb.spirorbis.total"])

for (i in c(1:nrow(qecbNew))) {
  qecbNew$Nb.spirorbis.total[[i]] <- ifelse(qecbNew$Nb.spirorbis.total[[i]] == "NaN", NA, qecbNew$Nb.spirorbis.total[[i]])
}
nrow(subset(qecbNew, is.na(qecbNew$Nb.spirorbis.total)))
qecbNew$Nb.spirorbis.total <- as.integer(qecbNew$Nb.spirorbis.total)

unique(qecbNew$Nb.spirorbis.total - qecbNew$Nb.spirorbis.total.ini) 
table(qecbNew$Nb.spirorbis.total - qecbNew$Nb.spirorbis.total.ini)
qecbNew$Nb.spirorbis.total.diff <- abs(qecbNew$Nb.spirorbis.total - qecbNew$Nb.spirorbis.total.ini)
spirorbis.diff <- qecbNew[, c("Nb.spirorbis.1A", "Nb.spirorbis.2A", "Nb.spirorbis.3A", "Nb.spirorbis.4A", "Nb.spirorbis.5A", "Nb.spirorbis.total", "Nb.spirorbis.total.ini", "mean.x.200", "Nb.spirorbis.total.diff")]
spirorbis.diff <- dplyr::arrange(spirorbis.diff, desc(Nb.spirorbis.total.diff), mean.x.200)
spirorbis.diff <- dplyr::arrange(dplyr::filter(spirorbis.diff, Nb.spirorbis.total.diff != 0 & mean.x.200 != 0), desc(Nb.spirorbis.total.diff))
table(qecbNew$Nb.spirorbis.total.diff)
length(na.omit(qecbNew$Nb.spirorbis.total))
sum(is.na(qecbNew$Nb.spirorbis.total))
length(na.omit(qecbNew$Nb.spirorbis.total))+sum(is.na(qecbNew$Nb.spirorbis.total))
      
qecbNew <- subset(qecbNew, select = -c(Nb.spirorbis.total.ini, mean.x.200, Nb.spirorbis.total.diff))

rm(qecbNew.spirorbis, spirorbis, spirorbis.data, spirorbis.diff)


# dplyr::filter for abnormal data

qecbNewhist. <- qecbNew
ylab. = "fréquence"

hist. <- qecbNewhist.[, c(
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
  "X..Recouvrement.Sediment"               ,           
  #"Type.Sediment"                          ,           
  "X..Roche.Nue"                           , 
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
  "X..Surface.Accolement"                  , 
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

dplyr::filter(qecbNew, X..algues.brunes > 100)[, c("Site", "date_fiche", "Type.Bloc", "Numéro.Bloc.échantillon", "Face", "X..algues.brunes")]
qecbNew$X..algues.brunes <- ifelse(qecbNew$X..algues.brunes > 100, 100, qecbNew$X..algues.brunes)
dplyr::filter(qecbNew, X..algues.rouges > 100)[, c("Site", "date_fiche", "Type.Bloc", "Numéro.Bloc.échantillon", "Face", "X..algues.rouges")]
qecbNew$X..algues.rouges <- ifelse(qecbNew$X..algues.rouges > 100, 100, qecbNew$X..algues.rouges)
dplyr::filter(qecbNew, Nb.Phallusia.mamillata > 10)[, c("Site", "date_fiche", "Type.Bloc", "Numéro.Bloc.échantillon", "Face", "Nb.Phallusia.mamillata")]
dplyr::filter(qecbNew, Nb.Tethya.aurantium > 2)[, c("Site", "date_fiche", "Type.Bloc", "Numéro.Bloc.échantillon", "Face", "Nb.Tethya.aurantium")]
dplyr::filter(qecbNew, Nb.spirorbis.total > 15000)[, c("Site", "date_fiche", "Type.Bloc", "Numéro.Bloc.échantillon", "Face", "Nb.spirorbis.total")]
ARMO_IlePlate <- dplyr::filter(qecbNew, Site == "ARMO_IlePlate" & date_fiche == "2015-10-29")
rm(ARMO_IlePlate)
dplyr::filter(qecbNew, Nb.Nucella.lapilus..Pourpre. > 20)[, c("Site", "date_fiche", "Type.Bloc", "Numéro.Bloc.échantillon", "Face", "Nb.Nucella.lapilus..Pourpre.")]

rm(qecbNewhist., ylab., hist.)


# see if any supplementary information in "Commentaires" variables

# commentaires avt qecb
unique(qecb$Commentaires.Avant)
table(qecb$Commentaires.Avant)
length(na.omit(qecb$Commentaires.Avant))

# commentaires qecb
unique(qecb$Commentaires)
table(qecb$Commentaires)
length(na.omit(qecb$Commentaires))

# commentaires spp mobiles pêche
unique(qecb$Commentaires.1)
table(qecb$Commentaires.1)
length(na.omit(qecb$Commentaires.1))

rm(df.test, qecb, i)


#######################################

# SCRIPT I - NAs <- 0

# replace NAs by "0" for variables used in qecb determination
qecbNew[,c("X..algues.brunes", 
          "X..algues.rouges", 
          "X..Lithophyllum",
          "X..Cladophora",
          "Nb.Littorina.obtusata",
          "Nb.Gibbula.cineraria",
          "Nb.Gibbula.pennanti",
          "Nb.Gibbula.umbilicalis",
          "X..Eponges",
          "X..Ascidies.Coloniales",
          "X..Ascidies.Solitaires",
          "X..Bryozoaires.Dresses",
          "X..algues.vertes",
          "X..Roche.Nue",
          "Nb.spirorbis.total",
          "X..Balanes.Vivantes",
          "Nb.Spirobranchus.lamarckii.total",
          "X..Surface.Accolement")] <- lapply(qecbNew[, 
               c("X..algues.brunes", 
                 "X..algues.rouges", 
                 "X..Lithophyllum",
                 "X..Cladophora",
                 "Nb.Littorina.obtusata",
                 "Nb.Gibbula.cineraria",
                 "Nb.Gibbula.pennanti",
                 "Nb.Gibbula.umbilicalis",
                 "X..Eponges",
                 "X..Ascidies.Coloniales",
                 "X..Ascidies.Solitaires",
                 "X..Bryozoaires.Dresses",
                 "X..algues.vertes",
                 "X..Roche.Nue",
                 "Nb.spirorbis.total",
                 "X..Balanes.Vivantes",
                 "Nb.Spirobranchus.lamarckii.total",
                 "X..Surface.Accolement")], 
  function(x) replace(x, is.na(x), 0))

# and also replace NA for bivalve by 0 for EGMP and BASQ surveys cfr for accollement correction later on.

qecbNew$X..Mytilus.sp. <- ifelse((substr(qecbNew$Site, 1, 4) %in% c("EGMP", "BASQ")) & is.na(qecbNew$X..Mytilus.sp.), 0, qecbNew$X..Mytilus.sp.) 
qecbNew$Nb.Crassostrea.gigas <- ifelse((substr(qecbNew$Site, 1, 4) %in% c("EGMP", "BASQ")) & is.na(qecbNew$Nb.Crassostrea.gigas), 0, qecbNew$Nb.Crassostrea.gigas) 
qecbNew$Nb.Ostrea.edulis <- ifelse((substr(qecbNew$Site, 1, 4) %in% c("EGMP", "BASQ")) & is.na(qecbNew$Nb.Ostrea.edulis), 0, qecbNew$Nb.Ostrea.edulis) 


# QEBM2 NaN ??
# For field survey "FINS_StNicolasGlenan.2016.04.11", no "Bloc fixé" or "Roche en place"; therefore not possible to calculate QEBM2, and consequently not possible to determine qecb
#qecbNew  %>% dplyr::filter(Site.Year.Month.Day == "FINS_StNicolasGlenan.2016.04.11") -> qecb.i

#######################################

#######################################

# SCRIPT 2

# VERY IMPORTANT here, I removed for below variables any rows with NAs !! Unactivate this line if I replace NAs by 0 next.
# na.omit is nicer for just removing all NA's. complete.cases allows partial selection by including only certain columns of the dataframe.

qecbNew <- qecbNew[complete.cases(qecbNew[ , c("X..algues.brunes", 
                                               "X..algues.rouges", 
                                               "X..Lithophyllum",
                                               "X..Cladophora",
                                               "Nb.Littorina.obtusata",
                                               "Nb.Gibbula.cineraria",
                                               "Nb.Gibbula.pennanti",
                                               "Nb.Gibbula.umbilicalis",
                                               "X..Eponges",
                                               "X..Ascidies.Coloniales",
                                               "X..Ascidies.Solitaires",
                                               "X..Bryozoaires.Dresses",
                                               "X..algues.vertes",
                                               "X..Roche.Nue",
                                               "Nb.spirorbis.total",
                                               "X..Balanes.Vivantes",
                                               "Nb.Spirobranchus.lamarckii.total",
                                               "X..Surface.Accolement")]),]

#######################################


# do not run code below anymore, cfr next loop more complete with detailed list of QECB components saved etc.
#####################################################################

for (i in c(1:length(unique(qecbNew$Site.Year.Month.Day)))) {
    
qecbNew  %>% dplyr::filter(Site.Year.Month.Day == unique(qecbNew$Site.Year.Month.Day)[[i]]) -> qecb.i

{
  
## QEBM.1


# VFS.BM Bloc mobile

qecb.i %>% dplyr::filter(qecb.i$Type.Bloc == "Bloc mobile" & qecb.i$Face == "face supérieure") -> df.
df. <- dplyr::arrange(df., Numéro.Bloc.échantillon)

(  (df.$X..algues.brunes + df.$X..algues.rouges)
  +  df.$X..Lithophyllum
  + (df.$Nb.Littorina.obtusata + df.$Nb.Gibbula.cineraria + df.$Nb.Gibbula.pennanti + df.$Nb.Gibbula.umbilicalis)
  + (df.$X..Eponges + df.$X..Ascidies.Coloniales + df.$X..Ascidies.Solitaires)
) - 
  (   df.$X..algues.vertes
      +  df.$X..Roche.Nue
  ) -> VFS.BM
VFS.BM  


# VFI.BM Bloc mobile         

qecb.i %>% dplyr::filter(Type.Bloc == "Bloc mobile" & Face == "face inférieure") -> df.
df. <- dplyr::arrange(df., Numéro.Bloc.échantillon)
(  (df.$X..Eponges + df.$X..Ascidies.Coloniales + df.$X..Ascidies.Solitaires)
  +  df.$X..Lithophyllum 
) - 
  (  (df.$X..algues.brunes + df.$X..algues.rouges)
     + (df.$Nb.Littorina.obtusata + df.$Nb.Gibbula.cineraria + df.$Nb.Gibbula.pennanti + df.$Nb.Gibbula.umbilicalis)
     +  df.$X..algues.vertes
     +  df.$X..Roche.Nue
  ) -> VFI.BM            
VFI.BM


# VFSI.BM Bloc mobile

qecb.i %>% dplyr::filter(Type.Bloc == "Bloc mobile") -> df.
num.bloc <- as.vector(sort(unique(df.$Numéro.Bloc.échantillon)))
VFSI.BM <- NA

for (k in c(1:length(na.omit(num.bloc)))) {
  
  j. <- num.bloc[k]
  
  VFSIn. <- unname(unlist(
    ( (dplyr::filter(df., Numéro.Bloc.échantillon == j. & Face == "face supérieure")["Nb.spirorbis.total"]
       + dplyr::filter(df., Numéro.Bloc.échantillon == j. & Face == "face inférieure")["Nb.spirorbis.total"]
    ) / 1000 )
    -
      ( 
        ( (dplyr::filter(df., Numéro.Bloc.échantillon == j. & Face == "face supérieure")["X..Balanes.Vivantes"]
           + dplyr::filter(df., Numéro.Bloc.échantillon == j. & Face == "face inférieure")["X..Balanes.Vivantes"]
        ) / 100 )
        + ( (dplyr::filter(df., Numéro.Bloc.échantillon == j. & Face == "face supérieure")["Nb.Spirobranchus.lamarckii.total"]
             + dplyr::filter(df., Numéro.Bloc.échantillon == j. & Face == "face inférieure")["Nb.Spirobranchus.lamarckii.total"]
        ) / 100 )
      )
  )) 
  
  VFSI.BM <<- c(VFSI.BM, VFSIn.)
  
}

VFSI.BM <- VFSI.BM[2:length(VFSI.BM)]
VFSI.BM


# QEBM.1

(QEBM.1 <- VFS.BM + VFI.BM + VFSI.BM)



## QEBM.2 


# VrFS.BF moyenne Bloc fixé ; = VDRmoyenne in excel file

qecb.i %>% dplyr::filter(Type.Bloc %in% c("Bloc fixé", "Roche en place")) -> df.
df. <- dplyr::arrange(df., Numéro.Bloc.échantillon)
(  (df.$X..algues.brunes + df.$X..algues.rouges)
  +  df.$X..Lithophyllum
  + (df.$Nb.Littorina.obtusata + df.$Nb.Gibbula.cineraria + df.$Nb.Gibbula.pennanti + df.$Nb.Gibbula.umbilicalis)
  + (df.$X..Eponges + df.$X..Ascidies.Coloniales + df.$X..Ascidies.Solitaires)
) - 
  (   df.$X..algues.vertes
      +  df.$X..Roche.Nue
  ) -> VrFS.BF # different from Pauline, check with her
VrFS.BF
VrFS.BF <- c(VrFS.BF, rep(NA, length(unique(dplyr::filter(qecb.i, Type.Bloc == "Bloc mobile")["Numéro.Bloc.échantillon"])[,1]) - length(VrFS.BF)))


# (G-(H+I)) Bloc fixé & Roche en place

( df.$Nb.spirorbis.total/1000
  - (df.$X..Balanes.Vivantes/100 + df.$Nb.Spirobranchus.lamarckii.total/100)
) -> `(G-(H+I))BF`
`(G-(H+I))BF`
`(G-(H+I))BF` <- c(`(G-(H+I))BF`, rep(NA, length(unique(dplyr::filter(qecb.i, Type.Bloc == "Bloc mobile")["Numéro.Bloc.échantillon"])[,1]) - length(`(G-(H+I))BF`)))


# VrFS.BF.moy

(mean(VrFS.BF + `(G-(H+I))BF`, na.rm = T) -> VrFS.BF.moy)


# (G-(H+I)S.BM) Bloc mobile face supérieure

qecb.i %>% dplyr::filter(Type.Bloc == "Bloc mobile" & Face == "face supérieure") -> df.
df. <- dplyr::arrange(df., Numéro.Bloc.échantillon)
( df.$Nb.spirorbis.total/1000
  - (df.$X..Balanes.Vivantes/100 + df.$Nb.Spirobranchus.lamarckii.total/100)
) -> `(G-(H+I))S.BM`
`(G-(H+I))S.BM`


# VrFS.BM

(VFS.BM + `(G-(H+I))S.BM` -> VrFS.BM)


# VrFS.BM.moy

(mean(VrFS.BM) -> VrFS.BM.moy)


# |VrFS.BM.moy/VrFS.BF.moy|

(abs(mean(VrFS.BM, na.rm = T)/VrFS.BF.moy) -> `|VrFS.BM.moy/VrFS.BF.moy|`)


# QEBM.2

(QEBM.1 * `|VrFS.BM.moy/VrFS.BF.moy|` -> QEBM.2)


## QECB

mean(QEBM.2, na.rm = T) -> QECB

}

  
qecb.val.qu.list[[i]] <- data.frame(id_qecb = rep(unique(qecb.i$id_qecb), length(QEBM.2)), Site.Year.Month.Day = rep(unique(qecb.i$Site.Year.Month.Day), length(QEBM.2)),
Numéro.Bloc = unique(dplyr::filter(qecb.i, Type.Bloc == "Bloc mobile")["Numéro.Bloc.échantillon"])[,1],
VFS.BM,
VFI.BM,
VFSI.BM,
QEBM.1,
VrFS.BF,
`(G-(H+I))S.BM`,
`(G-(H+I))BF`,
VrFS.BM,
QEBM.2)

qecb.val.list[[i]] <- data.frame(id_qecb = unique(qecb.i$id_qecb), Site.Year.Month.Day = unique(qecb.i$Site.Year.Month.Day),                                     
VrFS.BM.moy,
VrFS.BF.moy,
`|VrFS.BM.moy/VrFS.BF.moy|`,
QECB)


}

rm(list = ls()[!ls() %in% c("fiche", "qecb", "qecbNew", "df.list.loop", "qecb.val.qu.", "qecb.val.list", "qecb.val.qu.list")])

#####################################################################


# do not run code below, because bryozoaire was added in the formula, and face inférieure data were normalized according to % contact surface.
#####################################################################

# same as above, but now we also develop the for loop to include all A, B, C, D ... values in the final table

qecb.val.qu.list <- vector("list", length(unique(qecbNew$Site.Year.Month.Day)))
qecb.val.list <- vector("list", length(unique(qecbNew$Site.Year.Month.Day)))

for (i in c(1:length(unique(qecbNew$Site.Year.Month.Day)))) {
  
  qecbNew  %>% dplyr::filter(Site.Year.Month.Day == unique(qecbNew$Site.Year.Month.Day)[[i]]) -> qecb.i
  
  nb. <- as.vector(unlist(intersect(dplyr::filter(qecb.i, Type.Bloc == "Bloc mobile" & Face == "face supérieure")["Numéro.Bloc.échantillon"], dplyr::filter(qecb.i, Type.Bloc == "Bloc mobile" & Face == "face inférieure")["Numéro.Bloc.échantillon"])))
  
  Bloc.nb <- c(
    paste0("Bloc mobile", " - ", "face supérieure", " - ", nb.), 
    paste0("Bloc mobile", " - ", "face inférieure", " - ", nb.),
    paste0(as.vector(unlist(dplyr::filter(qecb.i, Type.Bloc != "Bloc mobile")["Type.Bloc"])), " - ", as.vector(unlist(dplyr::filter(qecb.i, Type.Bloc != "Bloc mobile")["Face"])), " - ", as.vector(unlist(dplyr::filter(qecb.i, Type.Bloc != "Bloc mobile")["Numéro.Bloc.échantillon"]))) 
  )
  
  qecb.i$Bloc <- paste0(qecb.i$Type.Bloc, " - ", qecb.i$Face, " - ", qecb.i$Numéro.Bloc.échantillon)
  
  qecb.i %>% subset(Bloc %in% Bloc.nb) -> qecb.i
  
  {
    
    ## QEBM.1
    
    
    # VFS.BM Bloc mobile
    
    qecb.i %>% dplyr::filter(qecb.i$Type.Bloc == "Bloc mobile" & qecb.i$Face == "face supérieure") -> df.
    df. <- dplyr::arrange(df., Numéro.Bloc.échantillon)
    
    A.bmS <- df.$X..algues.brunes + df.$X..algues.rouges
    B.bmS <-  df.$X..Lithophyllum
    C.bmS <- df.$Nb.Littorina.obtusata + df.$Nb.Gibbula.cineraria + df.$Nb.Gibbula.pennanti + df.$Nb.Gibbula.umbilicalis
    D.bmS <- df.$X..Eponges + df.$X..Ascidies.Coloniales + df.$X..Ascidies.Solitaires
    E.bmS <- df.$X..algues.vertes
    F.bmS <- df.$X..Roche.Nue
    
    (  (df.$X..algues.brunes + df.$X..algues.rouges)
      +  df.$X..Lithophyllum
      + (df.$Nb.Littorina.obtusata + df.$Nb.Gibbula.cineraria + df.$Nb.Gibbula.pennanti + df.$Nb.Gibbula.umbilicalis)
      + (df.$X..Eponges + df.$X..Ascidies.Coloniales + df.$X..Ascidies.Solitaires)
    ) - 
      (   df.$X..algues.vertes
          +  df.$X..Roche.Nue
      ) -> VFS.BM
    VFS.BM  
    
    
    # VFI.BM Bloc mobile         
    
    qecb.i %>% dplyr::filter(Type.Bloc == "Bloc mobile" & Face == "face inférieure") -> df.
    df. <- dplyr::arrange(df., Numéro.Bloc.échantillon)
    
    D.bmI <- df.$X..Eponges + df.$X..Ascidies.Coloniales + df.$X..Ascidies.Solitaires
    B.bmI <- df.$X..Lithophyllum 
    A.bmI <- df.$X..algues.brunes + df.$X..algues.rouges
    C.bmI <- df.$Nb.Littorina.obtusata + df.$Nb.Gibbula.cineraria + df.$Nb.Gibbula.pennanti + df.$Nb.Gibbula.umbilicalis
    E.bmI <- df.$X..algues.vertes
    F.bmI <- df.$X..Roche.Nue
    
    (  (df.$X..Eponges + df.$X..Ascidies.Coloniales + df.$X..Ascidies.Solitaires)
      +  df.$X..Lithophyllum 
    ) - 
      (  (df.$X..algues.brunes + df.$X..algues.rouges)
         + (df.$Nb.Littorina.obtusata + df.$Nb.Gibbula.cineraria + df.$Nb.Gibbula.pennanti + df.$Nb.Gibbula.umbilicalis)
         +  df.$X..algues.vertes
         +  df.$X..Roche.Nue
      ) -> VFI.BM            
    VFI.BM
    
    
    # VFSI.BM Bloc mobile
    
    qecb.i %>% dplyr::filter(Type.Bloc == "Bloc mobile") -> df.
    num.bloc <- as.vector(sort(unique(df.$Numéro.Bloc.échantillon)))
    
    G.bmSI <- NA
    H.bmSI <- NA
    L.bmSI <- NA
    
    for (k in c(1:length(na.omit(num.bloc)))) {
      
      j. <- num.bloc[k]
      
      GIn. <- unname(unlist(
        (dplyr::filter(df., Numéro.Bloc.échantillon == j. & Face == "face supérieure")["Nb.spirorbis.total"]
           + dplyr::filter(df., Numéro.Bloc.échantillon == j. & Face == "face inférieure")["Nb.spirorbis.total"]
        ) / 1000 ))
      
      G.bmSI <<- c(G.bmSI, GIn.)
      
      HIn. <- unname(unlist(  
        (dplyr::filter(df., Numéro.Bloc.échantillon == j. & Face == "face supérieure")["X..Balanes.Vivantes"]
               + dplyr::filter(df., Numéro.Bloc.échantillon == j. & Face == "face inférieure")["X..Balanes.Vivantes"]
            ) / 100 ))
      
      H.bmSI <<- c(H.bmSI, HIn.)
      
      LIn. <- unname(unlist(    
        (dplyr::filter(df., Numéro.Bloc.échantillon == j. & Face == "face supérieure")["Nb.Spirobranchus.lamarckii.total"]
                 + dplyr::filter(df., Numéro.Bloc.échantillon == j. & Face == "face inférieure")["Nb.Spirobranchus.lamarckii.total"]
            ) / 100 ))
    
      L.bmSI <<- c(L.bmSI, LIn.) # To avoid error message "Error in I <<- c(I, IIn.) : cannot change value of locked binding for 'I'"
  
    }
    
    G.bmSI <- G.bmSI[2:length(G.bmSI)]
    G.bmSI
    H.bmSI <- H.bmSI[2:length(H.bmSI)]
    H.bmSI
    I.bmSI <- L.bmSI[2:length(L.bmSI)]
    I.bmSI
    
    VFSI.BM <- NA  
    
    for (k in c(1:length(na.omit(num.bloc)))) {
      
      j. <- num.bloc[k]
      
      VFSIn. <- unname(unlist(
        ( (dplyr::filter(df., Numéro.Bloc.échantillon == j. & Face == "face supérieure")["Nb.spirorbis.total"]
           + dplyr::filter(df., Numéro.Bloc.échantillon == j. & Face == "face inférieure")["Nb.spirorbis.total"]
        ) / 1000 )
        -
          ( 
            ( (dplyr::filter(df., Numéro.Bloc.échantillon == j. & Face == "face supérieure")["X..Balanes.Vivantes"]
               + dplyr::filter(df., Numéro.Bloc.échantillon == j. & Face == "face inférieure")["X..Balanes.Vivantes"]
            ) / 100 )
            + ( (dplyr::filter(df., Numéro.Bloc.échantillon == j. & Face == "face supérieure")["Nb.Spirobranchus.lamarckii.total"]
                 + dplyr::filter(df., Numéro.Bloc.échantillon == j. & Face == "face inférieure")["Nb.Spirobranchus.lamarckii.total"]
            ) / 100 )
          )
      )) 
      
      VFSI.BM <<- c(VFSI.BM, VFSIn.)
      
    }
    
    VFSI.BM <- VFSI.BM[2:length(VFSI.BM)]
    VFSI.BM
    
    
    # QEBM.1
    
    (QEBM.1 <- VFS.BM + VFI.BM + VFSI.BM)
    
    
    
    ## QEBM.2 
    
    
    # VrFS.BF moyenne Bloc fixé ; = VDRmoyenne in excel file
    
    qecb.i %>% dplyr::filter(Type.Bloc %in% c("Bloc fixé", "Roche en place")) -> df.
    df. <- dplyr::arrange(df., Numéro.Bloc.échantillon)
    
    A.bf <- df.$X..algues.brunes + df.$X..algues.rouges
    B.bf <- df.$X..Lithophyllum
    C.bf <- df.$Nb.Littorina.obtusata + df.$Nb.Gibbula.cineraria + df.$Nb.Gibbula.pennanti + df.$Nb.Gibbula.umbilicalis
    D.bf <- df.$X..Eponges + df.$X..Ascidies.Coloniales + df.$X..Ascidies.Solitaires
    E.bf <- df.$X..algues.vertes
    F.bf <- df.$X..Roche.Nue
    
    A.bf <- c(A.bf, rep(NA, length(unique(dplyr::filter(qecb.i, Type.Bloc == "Bloc mobile")["Numéro.Bloc.échantillon"])[,1]) - length(A.bf)))
    B.bf <- c(B.bf, rep(NA, length(unique(dplyr::filter(qecb.i, Type.Bloc == "Bloc mobile")["Numéro.Bloc.échantillon"])[,1]) - length(B.bf)))
    C.bf <- c(C.bf, rep(NA, length(unique(dplyr::filter(qecb.i, Type.Bloc == "Bloc mobile")["Numéro.Bloc.échantillon"])[,1]) - length(C.bf)))
    D.bf <- c(D.bf, rep(NA, length(unique(dplyr::filter(qecb.i, Type.Bloc == "Bloc mobile")["Numéro.Bloc.échantillon"])[,1]) - length(D.bf)))
    E.bf <- c(E.bf, rep(NA, length(unique(dplyr::filter(qecb.i, Type.Bloc == "Bloc mobile")["Numéro.Bloc.échantillon"])[,1]) - length(E.bf)))
    F.bf <- c(F.bf, rep(NA, length(unique(dplyr::filter(qecb.i, Type.Bloc == "Bloc mobile")["Numéro.Bloc.échantillon"])[,1]) - length(F.bf)))
    
    (  (df.$X..algues.brunes + df.$X..algues.rouges)
      +  df.$X..Lithophyllum
      + (df.$Nb.Littorina.obtusata + df.$Nb.Gibbula.cineraria + df.$Nb.Gibbula.pennanti + df.$Nb.Gibbula.umbilicalis)
      + (df.$X..Eponges + df.$X..Ascidies.Coloniales + df.$X..Ascidies.Solitaires)
    ) - 
      (   df.$X..algues.vertes
          +  df.$X..Roche.Nue
      ) -> VrFS.BF # different from Pauline, check with her
    VrFS.BF
    VrFS.BF <- c(VrFS.BF, rep(NA, length(unique(dplyr::filter(qecb.i, Type.Bloc == "Bloc mobile")["Numéro.Bloc.échantillon"])[,1]) - length(VrFS.BF)))
    
    
    # (G-(H+I)) Bloc fixé & Roche en place
    
    G.bf <- df.$Nb.spirorbis.total/1000
    H.bf <- df.$X..Balanes.Vivantes/100
    I.bf <- df.$Nb.Spirobranchus.lamarckii.total/100
    
    G.bf<- c(G.bf, rep(NA, length(unique(dplyr::filter(qecb.i, Type.Bloc == "Bloc mobile")["Numéro.Bloc.échantillon"])[,1]) - length(G.bf)))
    H.bf<- c(H.bf, rep(NA, length(unique(dplyr::filter(qecb.i, Type.Bloc == "Bloc mobile")["Numéro.Bloc.échantillon"])[,1]) - length(H.bf)))
    I.bf<- c(I.bf, rep(NA, length(unique(dplyr::filter(qecb.i, Type.Bloc == "Bloc mobile")["Numéro.Bloc.échantillon"])[,1]) - length(I.bf)))
    
    ( df.$Nb.spirorbis.total/1000
      - (df.$X..Balanes.Vivantes/100 + df.$Nb.Spirobranchus.lamarckii.total/100)
    ) -> `(G-(H+I))BF`
    `(G-(H+I))BF`
    `(G-(H+I))BF` <- c(`(G-(H+I))BF`, rep(NA, length(unique(dplyr::filter(qecb.i, Type.Bloc == "Bloc mobile")["Numéro.Bloc.échantillon"])[,1]) - length(`(G-(H+I))BF`)))
    
    
    # VrFS.BF.moy
    
    (mean(VrFS.BF + `(G-(H+I))BF`, na.rm = T) -> VrFS.BF.moy)
    
    
    # (G-(H+I)S.BM) Bloc mobile face supérieure
    
    qecb.i %>% dplyr::filter(Type.Bloc == "Bloc mobile" & Face == "face supérieure") -> df.
    df. <- dplyr::arrange(df., Numéro.Bloc.échantillon)
    
    G.bmS <- df.$Nb.spirorbis.total/1000
    H.bmS <- df.$X..Balanes.Vivantes/100 
    I.bmS <- df.$Nb.Spirobranchus.lamarckii.total/100
    
    ( df.$Nb.spirorbis.total/1000
      - (df.$X..Balanes.Vivantes/100 + df.$Nb.Spirobranchus.lamarckii.total/100)
    ) -> `(G-(H+I))S.BM`
    `(G-(H+I))S.BM`
    
    
    # VrFS.BM
    
    (VFS.BM + `(G-(H+I))S.BM` -> VrFS.BM)
    
    
    # VrFS.BM.moy
    
    (mean(VrFS.BM) -> VrFS.BM.moy)
    
    
    # |VrFS.BM.moy/VrFS.BF.moy|
    
    (abs(mean(VrFS.BM, na.rm = T)/VrFS.BF.moy) -> `|VrFS.BM.moy/VrFS.BF.moy|`)
    
    
    # QEBM.2
    
    (QEBM.1 * `|VrFS.BM.moy/VrFS.BF.moy|` -> QEBM.2)
    
    
    ## QECB
    
    mean(QEBM.2, na.rm = T) -> QECB
    
  }
  
  
  qecb.val.qu.list[[i]] <- data.frame(id_qecb = rep(unique(qecb.i$id_qecb), length(QEBM.2)), Site.Year.Month.Day = rep(unique(qecb.i$Site.Year.Month.Day), length(QEBM.2)),
                                      Boulder.nb.bmS = sort(unique(dplyr::filter(qecb.i, Type.Bloc == "Bloc mobile" & Face == "face supérieure")["Numéro.Bloc.échantillon"])[,1]),
                                      Boulder.nb.bmI = sort(unique(dplyr::filter(qecb.i, Type.Bloc == "Bloc mobile" & Face == "face inférieure")["Numéro.Bloc.échantillon"])[,1]),
                                      Boulder.nb.bf = c(sort(unique(dplyr::filter(qecb.i, Type.Bloc %in% c("Bloc fixé", "Roche en place"))["Numéro.Bloc.échantillon"])[,1]), rep(NA, length(unique(dplyr::filter(qecb.i, Type.Bloc == "Bloc mobile" & Face == "face supérieure")["Numéro.Bloc.échantillon"])[,1]) - length(unique(dplyr::filter(qecb.i, Type.Bloc %in% c("Bloc fixé", "Roche en place"))["Numéro.Bloc.échantillon"])[,1]))),
                                      Quadrat.bmS = sort(dplyr::filter(qecb.i, Type.Bloc == "Bloc mobile" & Face == "face supérieure")["Quadrat.bis"][,1]),
                                      Quadrat.bmI = sort(dplyr::filter(qecb.i, Type.Bloc == "Bloc mobile" & Face == "face inférieure")["Quadrat.bis"][,1]),
                                      Quadrat.bf = c(sort(dplyr::filter(qecb.i, Type.Bloc %in% c("Bloc fixé", "Roche en place"))["Quadrat.bis"][,1]), rep(NA, length(unique(dplyr::filter(qecb.i, Type.Bloc == "Bloc mobile")["Numéro.Bloc.échantillon"])[,1]) - length(dplyr::filter(qecb.i, Type.Bloc %in% c("Bloc fixé", "Roche en place"))["Quadrat.bis"][,1]))),
                                      A.bmS,
                                      B.bmS,
                                      C.bmS,
                                      D.bmS,
                                      E.bmS,
                                      F.bmS,
                                      A.bmI,
                                      B.bmI,
                                      C.bmI,
                                      D.bmI,
                                      E.bmI,
                                      F.bmI,
                                      G.bmSI,
                                      H.bmSI,
                                      I.bmSI,
                                      A.bf,
                                      B.bf,
                                      C.bf,
                                      D.bf,
                                      E.bf,
                                      F.bf,
                                      G.bf,
                                      H.bf,
                                      I.bf,
                                      G.bmS,
                                      H.bmS,
                                      I.bmS,
                                      VFS.BM,
                                      VFI.BM,
                                      VFSI.BM,
                                      QEBM.1,
                                      VrFS.BF,
                                      `(G-(H+I))S.BM`,
                                      `(G-(H+I))BF`,
                                      VrFS.BM,
                                      QEBM.2)
  
  qecb.val.list[[i]] <- data.frame(id_qecb = unique(qecb.i$id_qecb), Site.Year.Month.Day = unique(qecb.i$Site.Year.Month.Day),                                     
                                   VrFS.BM.moy,
                                   VrFS.BF.moy,
                                   `|VrFS.BM.moy/VrFS.BF.moy|`,
                                   QECB)
  
  
}

#####################################################################


# last and good (for now) formula, from "qecb_data_coma.R" file

# First of all, lets check the transformation of BM.FI data for accollement
# below portion of script will be introduced in the function as well. Here it is just to check.

library(tibble)
Region <- rep(NA, nrow(qecbNew))
qecbNew <- tibble::add_column(qecbNew, Region, .after = "Site_bis")
qecbNew$Region <- ifelse(qecbNew$Site %in% c("EGMP_GroinCou", "EGMP_PasEmsembert",    "EGMP_BreeBains", "EGMP_PerreAntiochat", "EGMP_Chassiron", "BASQ_FlotsBleusZP", "BASQ_FlotsBleusZF"), "EGMP.BASQ", "Bretagne")
rm(Region)
qecbNew <- dplyr::arrange(qecbNew, Region, Site.Year.Month.Day, Type.Bloc, Numéro.Bloc.échantillon, Face)

qecbNew.EGMP.BASQ <- dplyr::filter(qecbNew, (substr(qecbNew$Site, 1, 4) %in% c("EGMP", "BASQ")) == TRUE & Type.Bloc == "Bloc mobile" & Face == "face supérieure")
`%notin%` <- Negate(`%in%`)
qecbNew.Bretagne <- dplyr::filter(qecbNew, (substr(qecbNew$Site, 1, 4) %notin% c("EGMP", "BASQ")) == TRUE & Type.Bloc == "Bloc mobile" & Face == "face supérieure")

# I have to repeat below loop because only 100 windows plot at a time ...
for (i in c(1:length(unique(qecbNew$Site.Year.Month.Day)))) {
  
  #i <- 5
  #i <- 116
  
  # i %in% 17:53 => EGMP & BASQ
  #i <- 18
  #i <- 19
  
  qecbNew  %>% dplyr::filter(Site.Year.Month.Day == unique(qecbNew$Site.Year.Month.Day)[[i]]) -> qecb.i

  qecb.i$X..Surface.Accolement <- ifelse(is.na(qecb.i$X..Surface.Accolement) == TRUE & qecb.i$Type.Bloc == "Bloc mobile", 0, qecb.i$X..Surface.Accolement)
  
qecb.i %>% dplyr::filter(qecb.i$Type.Bloc == "Bloc mobile" & qecb.i$Face == "face supérieure") -> df.BM.FS # to keep a version of it for later on correction for accollement for BM FI
#df. <- df.BM.FS

qecb.i %>% dplyr::filter(Type.Bloc == "Bloc mobile" & Face == "face inférieure") -> df.
df. <- dplyr::arrange(df., Numéro.Bloc.échantillon)

unique(qecbNew$Couleur.dominante)
unique(substr(qecbNew$Site, 1, 4))

if (unique(substr(qecb.i$Site, 1, 4)) %in% c("EGMP", "BASQ")) {
  
  par(mfrow = c(3,2))
  
  hist(df.BM.FS$X..Mytilus.sp., breaks = ifelse(max(df.BM.FS$X..Mytilus.sp., na.rm = T) >= 1, max(df.BM.FS$X..Mytilus.sp., na.rm = T), 1), main = unique(qecb.i$Site.Year.Month.Day)); 
  hist(qecbNew.EGMP.BASQ$X..Mytilus.sp., breaks = max(qecbNew.EGMP.BASQ$X..Mytilus.sp., na.rm = T), main = "EGMP - BASQ"); 
  
  hist(df.BM.FS$Nb.Crassostrea.gigas, breaks = ifelse(max(df.BM.FS$Nb.Crassostrea.gigas, na.rm = T) >= 1, max(df.BM.FS$Nb.Crassostrea.gigas, na.rm = T), 1), main = unique(qecb.i$Site.Year.Month.Day));
  hist(qecbNew.EGMP.BASQ$Nb.Crassostrea.gigas, breaks = max(qecbNew.EGMP.BASQ$Nb.Crassostrea.gigas, na.rm = T), main = "EGMP - BASQ"); 
  
  hist(df.BM.FS$Nb.Ostrea.edulis, breaks = ifelse(max(df.BM.FS$Nb.Ostrea.edulis, na.rm = T) >= 1, max(df.BM.FS$Nb.Ostrea.edulis, na.rm = T), 1), main = unique(qecb.i$Site.Year.Month.Day));
  hist(qecbNew.EGMP.BASQ$Nb.Ostrea.edulis, breaks = max(qecbNew.EGMP.BASQ$Nb.Ostrea.edulis, na.rm = T), main = "EGMP - BASQ"); 
  
  hist(df.BM.FS$X..algues.rouges, breaks = ifelse(max(df.BM.FS$X..algues.rouges, na.rm = T) >= 1, max(df.BM.FS$X..algues.rouges, na.rm = T), 1), main = unique(qecb.i$Site.Year.Month.Day)); 
  hist(qecbNew.EGMP.BASQ$X..algues.rouges, breaks = max(qecbNew.EGMP.BASQ$X..algues.rouges, na.rm = T), main = "EGMP - BASQ"); 
  
  hist(df.BM.FS$X..algues.brunes, breaks = ifelse(max(df.BM.FS$X..algues.brunes, na.rm = T) >= 1, max(df.BM.FS$X..algues.brunes, na.rm = T), 1), main = unique(qecb.i$Site.Year.Month.Day)); 
  hist(qecbNew.EGMP.BASQ$X..algues.brunes, breaks = max(qecbNew.EGMP.BASQ$X..algues.brunes, na.rm = T), main = "EGMP - BASQ"); 
  
  par(mar = c(7, 4, 4, 2) + 0.1)
  (pos <- plot(as.factor(df.BM.FS$Couleur.dominante), xaxt = "n", ylab = "Frequency", main = unique(qecb.i$Site.Year.Month.Day))); axis(1, las = 2, labels = c(unique(as.character(as.factor(df.BM.FS$Couleur.dominante)))), at = pos); 
  (pos <-  plot(as.factor(qecbNew.EGMP.BASQ$Couleur.dominante), xaxt = 'n', ylab = "Frequency", main = "EGMP - BASQ")); axis(1, las = 2, colMeans(pos), labels = c(unique(as.character(as.factor(qecbNew.EGMP.BASQ$Couleur.dominante)))), at = pos)
  par(mar = c(5, 4, 4, 2) + 0.1)
  
  hist(df.BM.FS$X..Surface.Accolement, breaks = ifelse(max(df.BM.FS$X..Surface.Accolement, na.rm = T) >= 1, max(df.BM.FS$X..Surface.Accolement, na.rm = T), 1), main = unique(qecb.i$Site.Year.Month.Day)); 
  hist(qecbNew.EGMP.BASQ$X..Surface.Accolement, breaks = max(qecbNew.EGMP.BASQ$X..Surface.Accolement, na.rm = T), main = "EGMP - BASQ");
  # surface accollement info dupliquée FI et FS
  
  hist(df.BM.FS$X..Roche.Nue, breaks = ifelse(max(df.BM.FS$X..Roche.Nue, na.rm = T) >= 1, max(df.BM.FS$X..Roche.Nue, na.rm = T), 1), main = unique(qecb.i$Site.Year.Month.Day)); 
  hist(qecbNew.EGMP.BASQ$X..Roche.Nue, breaks = max(qecbNew.EGMP.BASQ$X..Roche.Nue, na.rm = T), main = "EGMP - BASQ");
  
  par(mfrow = c(1,1))

} else {
  
  par(mfrow = c(3,2))
  
  hist(df.BM.FS$X..algues.rouges, breaks = ifelse(max(df.BM.FS$X..algues.rouges, na.rm = T) >= 1, max(df.BM.FS$X..algues.rouges, na.rm = T), 1), main = unique(qecb.i$Site.Year.Month.Day)); 
  hist(qecbNew.Bretagne$X..algues.rouges, breaks = max(qecbNew.Bretagne$X..algues.rouges, na.rm = T), main = "Bretagne"); 
  
  hist(df.BM.FS$X..algues.brunes, breaks = ifelse(max(df.BM.FS$X..algues.brunes, na.rm = T) >= 1, max(df.BM.FS$X..algues.brunes, na.rm = T), 1), main = unique(qecb.i$Site.Year.Month.Day)); 
  hist(qecbNew.Bretagne$X..algues.brunes, breaks = max(qecbNew.Bretagne$X..algues.brunes, na.rm = T), main = "Bretagne"); 
  
  par(mar = c(7, 4, 4, 2) + 0.1)
  (pos <- plot(as.factor(df.BM.FS$Couleur.dominante), xaxt = "n", ylab = "Frequency", main = unique(qecb.i$Site.Year.Month.Day))); axis(1, las = 2, labels = c(unique(as.character(as.factor(df.BM.FS$Couleur.dominante)))), at = pos); 
  (pos <-  plot(as.factor(qecbNew.Bretagne$Couleur.dominante), xaxt = 'n', ylab = "Frequency", main = "Bretagne")); axis(1, las = 2, colMeans(pos), labels = c(unique(as.character(as.factor(qecbNew.Bretagne$Couleur.dominante)))), at = pos)
  par(mar = c(5, 4, 4, 2) + 0.1)
  
  hist(df.BM.FS$X..Surface.Accolement, breaks = ifelse(max(df.BM.FS$X..Surface.Accolement, na.rm = T) >= 1, max(df.BM.FS$X..Surface.Accolement, na.rm = T), 1), main = unique(qecb.i$Site.Year.Month.Day)); 
  hist(qecbNew.Bretagne$X..Surface.Accolement, breaks = max(qecbNew.Bretagne$X..Surface.Accolement, na.rm = T), main = "Bretagne");
  # surface accollement info dupliquée FI et FS
  
  hist(df.BM.FS$X..Roche.Nue, breaks = ifelse(max(df.BM.FS$X..Roche.Nue, na.rm = T) >= 1, max(df.BM.FS$X..Roche.Nue, na.rm = T), 1), main = unique(qecb.i$Site.Year.Month.Day)); 
  hist(qecbNew.Bretagne$X..Roche.Nue, breaks = max(qecbNew.Bretagne$X..Roche.Nue, na.rm = T), main = "Bretagne");
  
  par(mfrow = c(1,1))

 }

}


# accolement function according to recent 'retournement'

qecbNew %>% dplyr::filter(Type.Bloc == "Bloc mobile" & Face == "face supérieure") -> df.BM.FS
qecbNew %>% dplyr::filter(Type.Bloc == "Bloc mobile" & Face == "face inférieure") -> df.BM.FI
head(df.BM.FS[, c("Region", "Site.Year.Month.Day", "Type.Bloc", "Numéro.Bloc.échantillon", "Face")])
head(df.BM.FI[, c("Region", "Site.Year.Month.Day", "Type.Bloc", "Numéro.Bloc.échantillon", "Face")])
tail(df.BM.FS[, c("Region", "Site.Year.Month.Day", "Type.Bloc", "Numéro.Bloc.échantillon", "Face")])
tail(df.BM.FI[, c("Region", "Site.Year.Month.Day", "Type.Bloc", "Numéro.Bloc.échantillon", "Face")])
#setdiff(paste0(df.BM.FI$Site.Year.Month.Day, "-", df.BM.FI$Type.Bloc, "-", df.BM.FI$Numéro.Bloc.échantillon), paste0(df.BM.FS$Site.Year.Month.Day, "-", df.BM.FS$Type.Bloc, "-", df.BM.FS$Numéro.Bloc.échantillon)) # setdiff function not usefull here because it doesn't take into account the order of character string; only all elements considered together for comparison purpose, so the order can differ !! that's why I had an issues previsouly when comparing car.var.acco. function results between this qecb scipt and ecology script !!!
#df.BM.FI$X..Surface.Accolement <- ifelse(is.na(df.BM.FI$X..Surface.Accolement) == TRUE & df.BM.FI$Type.Bloc == "Bloc mobile", 0, df.BM.FI$X..Surface.Accolement) # not needed anymore because surface accolement Nato0 
df.BM.FI$terri. <- substr(df.BM.FI$Site, 1, 4)

acco.fct <- function(var.) {

  col.accol.$var.cor.acco. <<- NA
  
  for (i in c(1:nrow(df.BM.FI))) {
    
    col.accol.$var.cor.acco.[[i]] <<- if (df.BM.FI$terri.[[i]] %notin% c("EGMP", "BASQ")) {
      ifelse(#df.$Couleur.dominante %in% c("Rouge", "Brune", "Brune-Rouge") | 
        df.BM.FS$Couleur.dominante[[i]] %in% c("Blanche", "Verte", "Blanche-Verte", "Colorée"), df.BM.FI[i, var.] / (100 - df.BM.FI$X..Surface.Accolement[[i]]) * 100, df.BM.FI[i, var.])
    } else {
      ifelse(df.BM.FS$Couleur.dominante[[i]] %in% c("Blanche", "Verte", "Blanche-Verte", "Colorée") 
             & df.BM.FI$X..Surface.Accolement[[i]] != 0 # I have to use it in dplyr::filter this time as well for EGMP- BASQ (but not for Bretagne, although could be added, same result); identical/repeated measure for BM.FI and BM.FS
             & df.BM.FS$X..Mytilus.sp.[[i]] == 0, df.BM.FI[i, var.] / (100 - df.BM.FI$X..Surface.Accolement[[i]]) * 100, df.BM.FI[i, var.])
    }
    
  }
 
}

# I would only consider colors in c("Rouge", "Brune", "Brune-Rouge") for BM.FI correction [ and not the series c("Blanche-Brune", "Rouge", "Brune", "Blanche-Rouge", "Brune-Rouge", "Rouge-Verte", "Brune-Verte") ] ; and for BM.FS, the list c("Blanche", "Verte", "Colorée") => we do the correction for BM.FI accollement based on BM.FS color !!!

options(scipen = 0, digits = 2)

col.accol. <- data.frame(Site = df.BM.FI$Site, Site.Year.Month.Day = df.BM.FI$Site.Year.Month.Day, Accol. = df.BM.FI$X..Surface.Accolement, Mytilus.BM.FS = df.BM.FS[, "X..Mytilus.sp."], Col.domi.BM.FS = df.BM.FS[, "Couleur.dominante"], Col.domi.BM.FI = df.BM.FI[, "Couleur.dominante"])

sort(df.BM.FI[, "X..Eponges"], decreasing = TRUE, index.return = FALSE)[1:50]
col.accol.$X..Eponges.BM.FI.pre. <- df.BM.FI[, "X..Eponges"]
acco.fct("X..Eponges")
col.accol. <- dplyr::rename(col.accol., X..Eponges.BM.FI.post = var.cor.acco.)
sort(col.accol.[, "X..Eponges.BM.FI.post"], decreasing = TRUE, index.return = FALSE)[1:50]
col.accol.[, "X..Eponges.diff."] <- col.accol.[, "X..Eponges.BM.FI.post"]/col.accol.[, "X..Eponges.BM.FI.pre."]*100-100
col.accol.[, "X..Eponges.diff."] <- ifelse(is.nan(col.accol.[, "X..Eponges.diff."]), NA, col.accol.[, "X..Eponges.diff."])
par(mfrow=c(1,3))
hist(col.accol.$X..Eponges.BM.FI.pre., main = "X..Eponges", xlab = "pre-corection")
hist(col.accol.[, "X..Eponges.BM.FI.post"], main = "X..Eponges", xlab = "post-corection")
hist(df.BM.FI[as.vector(which(abs(col.accol.$X..Eponges.BM.FI.pre. - col.accol.[, "X..Eponges.BM.FI.post"]) != 0)), "X..Eponges"], main = "X..Eponges", xlab = "diff. post-pre != 0")
par(mfrow=c(1,1))
check <- dplyr::arrange(col.accol.[, c("Site", "Site.Year.Month.Day", "Accol.", "Mytilus.BM.FS", "Col.domi.BM.FS", "Col.domi.BM.FI", "X..Eponges.BM.FI.pre.", "X..Eponges.BM.FI.post", "X..Eponges.diff.")], desc(X..Eponges.BM.FI.post))
nrow(dplyr::filter(col.accol., X..Eponges.diff. > 0))/nrow(col.accol.)*100
par(mfrow=c(1,2))
hist(as.vector(unlist(dplyr::filter(col.accol., X..Eponges.diff. > 0)["X..Eponges.diff."])), breaks = 100, main = "X..Eponges", xlab = "diff. post-pre != 0 (%)") ; hist(as.vector(unlist(dplyr::filter(col.accol., X..Eponges.diff. > 0)["X..Eponges.BM.FI.post"])), breaks = 100, xlim = c(0,ifelse(max(dplyr::filter(col.accol., X..Eponges.BM.FI.post %notin% c(NA, "NaN", "-Inf", "Inf"))["X..Eponges.BM.FI.post"]) <= 100, 100, max(dplyr::filter(col.accol., X..Eponges.BM.FI.post %notin% c(NA, "NaN", "-Inf", "Inf"))["X..Eponges.BM.FI.post"]))), main = "X..Eponges", xlab = "data corrected") 

sort(df.BM.FI[, "X..Ascidies.Coloniales"], decreasing = TRUE, index.return = FALSE)[1:50]
col.accol.$X..Ascidies.Coloniales.BM.FI.pre. <- df.BM.FI[, "X..Ascidies.Coloniales"]
acco.fct("X..Ascidies.Coloniales")
col.accol. <- dplyr::rename(col.accol., X..Ascidies.Coloniales.BM.FI.post = var.cor.acco.)
sort(col.accol.[, "X..Ascidies.Coloniales.BM.FI.post"], decreasing = TRUE, index.return = FALSE)[1:50]
col.accol.[, "X..Ascidies.Coloniales.diff."] <- col.accol.[, "X..Ascidies.Coloniales.BM.FI.post"]/col.accol.[, "X..Ascidies.Coloniales.BM.FI.pre."]*100-100
col.accol.[, "X..Ascidies.Coloniales.diff."] <- ifelse(is.nan(col.accol.[, "X..Ascidies.Coloniales.diff."]), NA, col.accol.[, "X..Ascidies.Coloniales.diff."])
par(mfrow=c(1,3))
hist(col.accol.$X..Ascidies.Coloniales.BM.FI.pre., main = "X..Ascidies.Coloniales", xlab = "pre-corection")
hist(col.accol.[, "X..Ascidies.Coloniales.BM.FI.post"], main = "X..Ascidies.Coloniales", xlab = "post-corection")
hist(df.BM.FI[as.vector(which(abs(col.accol.$X..Ascidies.Coloniales.BM.FI.pre. - col.accol.[, "X..Ascidies.Coloniales.BM.FI.post"]) != 0)), "X..Ascidies.Coloniales"], main = "X..Ascidies.Coloniales", xlab = "diff. post-pre != 0")
par(mfrow=c(1,1))
check <- dplyr::arrange(col.accol.[, c("Site", "Site.Year.Month.Day", "Accol.", "Mytilus.BM.FS", "Col.domi.BM.FS", "Col.domi.BM.FI", "X..Ascidies.Coloniales.BM.FI.pre.", "X..Ascidies.Coloniales.BM.FI.post", "X..Ascidies.Coloniales.diff.")], desc(X..Ascidies.Coloniales.BM.FI.post))
nrow(dplyr::filter(col.accol., X..Ascidies.Coloniales.diff. > 0))/nrow(col.accol.)*100
par(mfrow=c(1,2))
hist(as.vector(unlist(dplyr::filter(col.accol., X..Ascidies.Coloniales.diff. > 0)["X..Ascidies.Coloniales.diff."])), breaks = 100, main = "X..Ascidies.Coloniales", xlab = "diff. post-pre != 0 (%)") ; hist(as.vector(unlist(dplyr::filter(col.accol., X..Ascidies.Coloniales.diff. > 0)["X..Ascidies.Coloniales.BM.FI.post"])), breaks = 100, xlim = c(0,ifelse(max(dplyr::filter(col.accol., X..Ascidies.Coloniales.BM.FI.post %notin% c(NA, "NaN", "-Inf", "Inf"))["X..Ascidies.Coloniales.BM.FI.post"]) <= 100, 100, max(dplyr::filter(col.accol., X..Ascidies.Coloniales.BM.FI.post %notin% c(NA, "NaN", "-Inf", "Inf"))["X..Ascidies.Coloniales.BM.FI.post"]))), main = "X..Ascidies.Coloniales", xlab = "data corrected") 

sort(df.BM.FI[, "X..Ascidies.Solitaires"], decreasing = TRUE, index.return = FALSE)[1:50]
col.accol.$X..Ascidies.Solitaires.BM.FI.pre. <- df.BM.FI[, "X..Ascidies.Solitaires"]
acco.fct("X..Ascidies.Solitaires")
col.accol. <- dplyr::rename(col.accol., X..Ascidies.Solitaires.BM.FI.post = var.cor.acco.)
sort(col.accol.[, "X..Ascidies.Solitaires.BM.FI.post"], decreasing = TRUE, index.return = FALSE)[1:50]
col.accol.[, "X..Ascidies.Solitaires.diff."] <- col.accol.[, "X..Ascidies.Solitaires.BM.FI.post"]/col.accol.[, "X..Ascidies.Solitaires.BM.FI.pre."]*100-100
col.accol.[, "X..Ascidies.Solitaires.diff."] <- ifelse(is.nan(col.accol.[, "X..Ascidies.Solitaires.diff."]), NA, col.accol.[, "X..Ascidies.Solitaires.diff."])
par(mfrow=c(1,3))
hist(col.accol.$X..Ascidies.Solitaires.BM.FI.pre., main = "X..Ascidies.Solitaires", xlab = "pre-corection")
hist(col.accol.[, "X..Ascidies.Solitaires.BM.FI.post"], main = "X..Ascidies.Solitaires", xlab = "post-corection")
hist(df.BM.FI[as.vector(which(abs(col.accol.$X..Ascidies.Solitaires.BM.FI.pre. - col.accol.[, "X..Ascidies.Solitaires.BM.FI.post"]) != 0)), "X..Ascidies.Solitaires"], main = "X..Ascidies.Solitaires", xlab = "diff. post-pre != 0")
par(mfrow=c(1,1))
check <- dplyr::arrange(col.accol.[, c("Site", "Site.Year.Month.Day", "Accol.", "Mytilus.BM.FS", "Col.domi.BM.FS", "Col.domi.BM.FI", "X..Ascidies.Solitaires.BM.FI.pre.", "X..Ascidies.Solitaires.BM.FI.post", "X..Ascidies.Solitaires.diff.")], desc(X..Ascidies.Solitaires.BM.FI.post))
nrow(dplyr::filter(col.accol., X..Ascidies.Solitaires.diff. > 0))/nrow(col.accol.)*100
par(mfrow=c(1,2))
hist(as.vector(unlist(dplyr::filter(col.accol., X..Ascidies.Solitaires.diff. > 0)["X..Ascidies.Solitaires.diff."])), breaks = 100, main = "X..Ascidies.Solitaires", xlab = "diff. post-pre != 0 (%)") ; hist(as.vector(unlist(dplyr::filter(col.accol., X..Ascidies.Solitaires.diff. > 0)["X..Ascidies.Solitaires.BM.FI.post"])), breaks = 100, xlim = c(0,ifelse(max(dplyr::filter(col.accol., X..Ascidies.Solitaires.BM.FI.post %notin% c(NA, "NaN", "-Inf", "Inf"))["X..Ascidies.Solitaires.BM.FI.post"]) <= 100, 100, max(dplyr::filter(col.accol., X..Ascidies.Solitaires.BM.FI.post %notin% c(NA, "NaN", "-Inf", "Inf"))["X..Ascidies.Solitaires.BM.FI.post"]))), main = "X..Ascidies.Solitaires", xlab = "data corrected") 

sort(df.BM.FI[, "X..Bryozoaires.Dresses"], decreasing = TRUE, index.return = FALSE)[1:50]
col.accol.$X..Bryozoaires.Dresses.BM.FI.pre. <- df.BM.FI[, "X..Bryozoaires.Dresses"]
acco.fct("X..Bryozoaires.Dresses")
col.accol. <- dplyr::rename(col.accol., X..Bryozoaires.Dresses.BM.FI.post = var.cor.acco.)
sort(col.accol.[, "X..Bryozoaires.Dresses.BM.FI.post"], decreasing = TRUE, index.return = FALSE)[1:50]
col.accol.[, "X..Bryozoaires.Dresses.diff."] <- col.accol.[, "X..Bryozoaires.Dresses.BM.FI.post"]/col.accol.[, "X..Bryozoaires.Dresses.BM.FI.pre."]*100-100
col.accol.[, "X..Bryozoaires.Dresses.diff."] <- ifelse(is.nan(col.accol.[, "X..Bryozoaires.Dresses.diff."]), NA, col.accol.[, "X..Bryozoaires.Dresses.diff."])
par(mfrow=c(1,3))
hist(col.accol.$X..Bryozoaires.Dresses.BM.FI.pre., main = "X..Bryozoaires.Dresses", xlab = "pre-corection")
hist(col.accol.[, "X..Bryozoaires.Dresses.BM.FI.post"], main = "X..Bryozoaires.Dresses", xlab = "post-corection")
hist(df.BM.FI[as.vector(which(abs(col.accol.$X..Bryozoaires.Dresses.BM.FI.pre. - col.accol.[, "X..Bryozoaires.Dresses.BM.FI.post"]) != 0)), "X..Bryozoaires.Dresses"], main = "X..Bryozoaires.Dresses", xlab = "diff. post-pre != 0")
par(mfrow=c(1,1))
check <- dplyr::arrange(col.accol.[, c("Site", "Site.Year.Month.Day", "Accol.", "Mytilus.BM.FS", "Col.domi.BM.FS", "Col.domi.BM.FI", "X..Bryozoaires.Dresses.BM.FI.pre.", "X..Bryozoaires.Dresses.BM.FI.post", "X..Bryozoaires.Dresses.diff.")], desc(X..Bryozoaires.Dresses.BM.FI.post))
nrow(dplyr::filter(col.accol., X..Bryozoaires.Dresses.diff. > 0))/nrow(col.accol.)*100
par(mfrow=c(1,2))
hist(as.vector(unlist(dplyr::filter(col.accol., X..Bryozoaires.Dresses.diff. > 0)["X..Bryozoaires.Dresses.diff."])), breaks = 100, main = "X..Bryozoaires.Dresses", xlab = "diff. post-pre != 0 (%)") ; hist(as.vector(unlist(dplyr::filter(col.accol., X..Bryozoaires.Dresses.diff. > 0)["X..Bryozoaires.Dresses.BM.FI.post"])), breaks = 100, xlim = c(0,ifelse(max(dplyr::filter(col.accol., X..Bryozoaires.Dresses.BM.FI.post %notin% c(NA, "NaN", "-Inf", "Inf"))["X..Bryozoaires.Dresses.BM.FI.post"]) <= 100, 100, max(dplyr::filter(col.accol., X..Bryozoaires.Dresses.BM.FI.post %notin% c(NA, "NaN", "-Inf", "Inf"))["X..Bryozoaires.Dresses.BM.FI.post"]))), main = "X..Bryozoaires.Dresses", xlab = "data corrected") 

sort(df.BM.FI[, "X..Lithophyllum"], decreasing = TRUE, index.return = FALSE)[1:50]
col.accol.$X..Lithophyllum.BM.FI.pre. <- df.BM.FI[, "X..Lithophyllum"]
acco.fct("X..Lithophyllum")
col.accol. <- dplyr::rename(col.accol., X..Lithophyllum.BM.FI.post = var.cor.acco.)
sort(col.accol.[, "X..Lithophyllum.BM.FI.post"], decreasing = TRUE, index.return = FALSE)[1:50]
col.accol.[, "X..Lithophyllum.diff."] <- col.accol.[, "X..Lithophyllum.BM.FI.post"]/col.accol.[, "X..Lithophyllum.BM.FI.pre."]*100-100
col.accol.[, "X..Lithophyllum.diff."] <- ifelse(is.nan(col.accol.[, "X..Lithophyllum.diff."]), NA, col.accol.[, "X..Lithophyllum.diff."])
par(mfrow=c(1,3))
hist(col.accol.$X..Lithophyllum.BM.FI.pre., main = "X..Lithophyllum", xlab = "pre-corection")
hist(col.accol.[, "X..Lithophyllum.BM.FI.post"], main = "X..Lithophyllum", xlab = "post-corection")
hist(df.BM.FI[as.vector(which(abs(col.accol.$X..Lithophyllum.BM.FI.pre. - col.accol.[, "X..Lithophyllum.BM.FI.post"]) != 0)), "X..Lithophyllum"], main = "X..Lithophyllum", xlab = "diff. post-pre != 0")
par(mfrow=c(1,1))
check <- dplyr::arrange(col.accol.[, c("Site", "Site.Year.Month.Day", "Accol.", "Mytilus.BM.FS", "Col.domi.BM.FS", "Col.domi.BM.FI", "X..Lithophyllum.BM.FI.pre.", "X..Lithophyllum.BM.FI.post", "X..Lithophyllum.diff.")], desc(X..Lithophyllum.BM.FI.post))
nrow(dplyr::filter(col.accol., X..Lithophyllum.diff. > 0))/nrow(col.accol.)*100
par(mfrow=c(1,2))
hist(as.vector(unlist(dplyr::filter(col.accol., X..Lithophyllum.diff. > 0)["X..Lithophyllum.diff."])), breaks = 100, main = "X..Lithophyllum", xlab = "diff. post-pre != 0 (%)") ; hist(as.vector(unlist(dplyr::filter(col.accol., X..Lithophyllum.diff. > 0)["X..Lithophyllum.BM.FI.post"])), breaks = 100, xlim = c(0,ifelse(max(dplyr::filter(col.accol., X..Lithophyllum.BM.FI.post %notin% c(NA, "NaN", "-Inf", "Inf"))["X..Lithophyllum.BM.FI.post"]) <= 100, 100, max(dplyr::filter(col.accol., X..Lithophyllum.BM.FI.post %notin% c(NA, "NaN", "-Inf", "Inf"))["X..Lithophyllum.BM.FI.post"]))), main = "X..Lithophyllum", xlab = "data corrected") 

sort(df.BM.FI[, "X..Balanes.Vivantes"], decreasing = TRUE, index.return = FALSE)[1:50]
col.accol.$X..Balanes.Vivantes.BM.FI.pre. <- df.BM.FI[, "X..Balanes.Vivantes"]
acco.fct("X..Balanes.Vivantes")
col.accol. <- dplyr::rename(col.accol., X..Balanes.Vivantes.BM.FI.post = var.cor.acco.)
sort(col.accol.[, "X..Balanes.Vivantes.BM.FI.post"], decreasing = TRUE, index.return = FALSE)[1:50]
col.accol.[, "X..Balanes.Vivantes.diff."] <- col.accol.[, "X..Balanes.Vivantes.BM.FI.post"]/col.accol.[, "X..Balanes.Vivantes.BM.FI.pre."]*100-100
col.accol.[, "X..Balanes.Vivantes.diff."] <- ifelse(is.nan(col.accol.[, "X..Balanes.Vivantes.diff."]), NA, col.accol.[, "X..Balanes.Vivantes.diff."])
par(mfrow=c(1,3))
hist(col.accol.$X..Balanes.Vivantes.BM.FI.pre., main = "X..Balanes.Vivantes", xlab = "pre-corection")
hist(col.accol.[, "X..Balanes.Vivantes.BM.FI.post"], main = "X..Balanes.Vivantes", xlab = "post-corection")
hist(df.BM.FI[as.vector(which(abs(col.accol.$X..Balanes.Vivantes.BM.FI.pre. - col.accol.[, "X..Balanes.Vivantes.BM.FI.post"]) != 0)), "X..Balanes.Vivantes"], main = "X..Balanes.Vivantes", xlab = "diff. post-pre != 0")
par(mfrow=c(1,1))
check <- dplyr::arrange(col.accol.[, c("Site", "Site.Year.Month.Day", "Accol.", "Mytilus.BM.FS", "Col.domi.BM.FS", "Col.domi.BM.FI", "X..Balanes.Vivantes.BM.FI.pre.", "X..Balanes.Vivantes.BM.FI.post", "X..Balanes.Vivantes.diff.")], desc(X..Balanes.Vivantes.BM.FI.post))
nrow(dplyr::filter(col.accol., X..Balanes.Vivantes.diff. > 0))/nrow(col.accol.)*100
par(mfrow=c(1,2))
hist(as.vector(unlist(dplyr::filter(col.accol., X..Balanes.Vivantes.diff. > 0)["X..Balanes.Vivantes.diff."])), breaks = 100, main = "X..Balanes.Vivantes", xlab = "diff. post-pre != 0 (%)") ; hist(as.vector(unlist(dplyr::filter(col.accol., X..Balanes.Vivantes.diff. > 0)["X..Balanes.Vivantes.BM.FI.post"])), breaks = 100, xlim = c(0,ifelse(max(dplyr::filter(col.accol., X..Balanes.Vivantes.BM.FI.post %notin% c(NA, "NaN", "-Inf", "Inf"))["X..Balanes.Vivantes.BM.FI.post"]) <= 100, 100, max(dplyr::filter(col.accol., X..Balanes.Vivantes.BM.FI.post %notin% c(NA, "NaN", "-Inf", "Inf"))["X..Balanes.Vivantes.BM.FI.post"]))), main = "X..Balanes.Vivantes", xlab = "data corrected") 

options(scipen = 0, digits = 7) # default

#rm(check, col.accol.)


# save the final qecbNew df.

saveRDS(qecbNew, "qecbNew.RDS")
#qecbNew <- readRDS("results/QECB/qecbNew.RDS")


# do calculate QECB values now

qecb.val.qu.list <- vector("list", length(unique(qecbNew$Site.Year.Month.Day)))
qecb.val.list <- vector("list", length(unique(qecbNew$Site.Year.Month.Day)))

# check for missing/NA data? 
qecbNew[, c("X..algues.brunes", "X..algues.rouges", "X..Cladophora", "X..Lithophyllum", "Nb.Littorina.obtusata", "Nb.Gibbula.cineraria", "Nb.Gibbula.pennanti", "Nb.Gibbula.umbilicalis", "X..Eponges", "X..Ascidies.Coloniales", "X..Ascidies.Solitaires", "X..Bryozoaires.Dresses", "X..algues.vertes", "X..Roche.Nue", "Nb.spirorbis.total", "X..Balanes.Vivantes", "Nb.Spirobranchus.lamarckii.total", "X..Surface.Accolement")] -> check.
check. %>% dplyr::filter_all(any_vars(is.na(.))) -> check.
names(check.[colSums(!is.na(check.)) == 0]) # they might be NAs to change to 0 for some face inférieure blocs mobiles in qecb formula; added at the appropriate place in below function.
check.[, names(check.[colSums(!is.na(check.)) == 0])]  
names(check.[colSums(is.na(check.)) != 0]) 
# normal remains suface accollement cfr not corrected for that var Nato0 ; we replace for BM FI NA by O in quecb determination equation below.
rm(check.)

unique(qecbNew$Site)
# Issues with Île de Sein data, so investigate this subdataset first
dplyr::filter(qecbNew, Site %in% c("EGMP_GroinCou")) -> issue.
dplyr::arrange(issue., Site, date_fiche, Type.Bloc, Numéro.Bloc.échantillon, Face, Quadrat.bis) -> issue.
rm(issue.)

unique(qecbNew$Site.Year.Month.Day)

#some more sites to remove, but that could be solved later on so always check the loop below before deleting them; therefore, I unactivated these lines and I have to run them manually by selecting them. It is obviously the only site with that issue.

# "FINS_Quemenes.2020.10.16", bad encoding, I let know Anna Capietto to make changes => was corrected normally, so unactivate next time I download ESTAMP data
dplyr::filter(qecbNew, Site.Year.Month.Day != "FINS_Quemenes.2020.10.16") -> qecbNew

# already changed earlier in the script
#qecbNew$Type.Bloc <- as.character(qecbNew$Type.Bloc)
#qecbNew$Type.Bloc <- ifelse(qecbNew$ID.Fiche == "2018-09-10-GDMO-CDB-001" & qecbNew$Numero.Photo == "2018-09-10_GDMO_01_CDB-5_sup_392578.jpg", "Roche en place", qecbNew$Type.Bloc)
#qecbNew$Type.Bloc <- as.factor(qecbNew$Type.Bloc)
#qecbNew %>% dplyr::filter(!(ID.Fiche == "2018-09-10-GDMO-CDB-001" & Numero.Photo == "")) -> qecbNew

saveRDS(qecbNew, "qecbNew.RDS")
#qecbNew <- readRDS("results/QECB/qecbNew.RDS")


for (i in c(1:length(unique(qecbNew$Site.Year.Month.Day)))) {
  
  #i <- 40
  
  #i <- 5
  #i <- 116
  
  # i %in% 17:53 => EGMP & BASQ
  #i <- 18
  #i <- 19
  #i <- 28
  
  qecbNew  %>% dplyr::filter(Site.Year.Month.Day == unique(qecbNew$Site.Year.Month.Day)[[i]]) -> qecb.i
  
  (terri. <- unique(substr(qecb.i$Site, 1, 4)))
  
  # we do not dplyr::filter for specific boulder after validation process; we keep them all (although it might add some noise)
  #val.VrFS.BM.moy[[i]] -> val.VrFS.BM.moy.i
  #val.QECB[[i]] -> val.QECB.i
  
  nb. <- as.vector(unlist(intersect(dplyr::filter(qecb.i, Type.Bloc == "Bloc mobile" & Face == "face supérieure")["Numéro.Bloc.échantillon"], dplyr::filter(qecb.i, Type.Bloc == "Bloc mobile" & Face == "face inférieure")["Numéro.Bloc.échantillon"])))
  
  Bloc.nb <- c(
    paste0("Bloc mobile", " - ", "face supérieure", " - ", nb.), 
    paste0("Bloc mobile", " - ", "face inférieure", " - ", nb.),
    paste0(as.vector(unlist(dplyr::filter(qecb.i, Type.Bloc != "Bloc mobile")["Type.Bloc"])), " - ", as.vector(unlist(dplyr::filter(qecb.i, Type.Bloc != "Bloc mobile")["Face"])), " - ", as.vector(unlist(dplyr::filter(qecb.i, Type.Bloc != "Bloc mobile")["Numéro.Bloc.échantillon"]))) 
  )
  
  qecb.i$Bloc <- paste0(qecb.i$Type.Bloc, " - ", qecb.i$Face, " - ", qecb.i$Numéro.Bloc.échantillon)
  
  qecb.i %>% subset(Bloc %in% Bloc.nb) -> qecb.i
  
  {
    
    ## QEBM.1
    
    
    # VFS.BM Bloc mobile
    
    qecb.i %>% dplyr::filter(qecb.i$Type.Bloc == "Bloc mobile" & qecb.i$Face == "face supérieure") -> df.BM.FS # to keep a version of it for later on correction for accollement for BM FI
    df. <- df.BM.FS
    df. <- dplyr::arrange(df., Numéro.Bloc.échantillon)
    
    A.bmS <- df.$X..algues.brunes + df.$X..algues.rouges + df.$X..Cladophora
    B.bmS <-  df.$X..Lithophyllum
    C.bmS <- df.$Nb.Littorina.obtusata + df.$Nb.Gibbula.cineraria + df.$Nb.Gibbula.pennanti + df.$Nb.Gibbula.umbilicalis
    D.bmS <- df.$X..Eponges + df.$X..Ascidies.Coloniales + df.$X..Ascidies.Solitaires + df.$X..Bryozoaires.Dresses
    E.bmS <- df.$X..algues.vertes
    F.bmS <- df.$X..Roche.Nue
    
    (  (df.$X..algues.brunes + df.$X..algues.rouges + df.$X..Cladophora)
      +  df.$X..Lithophyllum
      + (df.$Nb.Littorina.obtusata + df.$Nb.Gibbula.cineraria + df.$Nb.Gibbula.pennanti + df.$Nb.Gibbula.umbilicalis)
      + (df.$X..Eponges + df.$X..Ascidies.Coloniales + df.$X..Ascidies.Solitaires + df.$X..Bryozoaires.Dresses)
    ) - 
      (   df.$X..algues.vertes
          +  df.$X..Roche.Nue
      ) -> VFS.BM
    VFS.BM  
    
    
    # VFI.BM Bloc mobile         
    
    qecb.i %>% dplyr::filter(Type.Bloc == "Bloc mobile" & Face == "face inférieure") -> df.
    df. <- dplyr::arrange(df., Numéro.Bloc.échantillon)
    
    df.BM.FI <- df.
    
    # accolement function according to recent 'retournement'
    
    `%notin%` <- Negate(`%in%`)
    
    acco.fct <- function(var.) {
      
      if (terri. %notin% c("EGMP", "BASQ")) {
        ifelse(#df.$Couleur.dominante %in% c("Rouge", "Brune", "Brune-Rouge") | 
          df.BM.FS$Couleur.dominante %in% c("Blanche", "Verte", "Blanche-Verte", "Colorée") , df.BM.FI[, var.] / (100 - df.BM.FI$X..Surface.Accolement) * 100, df.BM.FI[, var.])
      } else {
        ifelse(df.BM.FS$Couleur.dominante %in% c("Blanche", "Verte", "Blanche-Verte", "Colorée") 
               & df.BM.FI$X..Surface.Accolement != 0 # I have to use it in dplyr::filter this time as well for EGMP- BASQ (but not for Bretagne, altough could be added, same result); identical/repeated measure for BM.FI and BM.FS
               & df.BM.FS$X..Mytilus.sp. == 0, df.BM.FI[, var.] / (100 - df.BM.FI$X..Surface.Accolement) * 100, df.BM.FI[, var.])
      }
      
    }
      
    # I would only consider colors in c("Rouge", "Brune", "Brune-Rouge") for BM.FI correction [ and not the series c("Blanche-Brune", "Rouge", "Brune", "Blanche-Rouge", "Brune-Rouge", "Rouge-Verte", "Brune-Verte") ] ; and for BM.FS, the list c("Blanche", "Verte", "Colorée") => we do the correction for BM.FI accollement based on BM.FS color !!!
    
    df.BM.FS$Couleur.dominante 
    df.BM.FS$X..Mytilus.sp.
    
    df.[, "X..Eponges"] ; df.[, "X..Surface.Accolement"]
    df.[, "X..Eponges"] <- acco.fct("X..Eponges")
    df.[, "X..Eponges"] <- as.numeric(ifelse(as.character(df.[, "X..Eponges"]) %in% c(NA, "NaN", "-Inf", "Inf"), "0", as.character(df.[, "X..Eponges"])))
    df.[, "X..Eponges"] <- ifelse(df.[, "X..Eponges"] > 100, 100, df.[, "X..Eponges"])
    df.[, "X..Eponges"]
    
    df.[, "X..Ascidies.Coloniales"] ; df.[, "X..Surface.Accolement"]
    df.[, "X..Ascidies.Coloniales"] <- acco.fct("X..Ascidies.Coloniales")
    df.[, "X..Ascidies.Coloniales"] <- as.numeric(ifelse(as.character(df.[, "X..Ascidies.Coloniales"]) %in% c(NA, "NaN", "-Inf", "Inf"), "0", as.character(df.[, "X..Ascidies.Coloniales"])))
    df.[, "X..Ascidies.Coloniales"] <- ifelse(df.[, "X..Ascidies.Coloniales"] > 100, 100, df.[, "X..Ascidies.Coloniales"])
    df.[, "X..Ascidies.Coloniales"]
    
    df.[, "X..Ascidies.Solitaires"] ; df.[, "X..Surface.Accolement"]
    df.[, "X..Ascidies.Solitaires"] <- acco.fct("X..Ascidies.Solitaires")
    df.[, "X..Ascidies.Solitaires"] <- as.numeric(ifelse(as.character(df.[, "X..Ascidies.Solitaires"]) %in% c(NA, "NaN", "-Inf", "Inf"), "0", as.character(df.[, "X..Ascidies.Solitaires"])))
    df.[, "X..Ascidies.Solitaires"] <- ifelse(df.[, "X..Ascidies.Solitaires"] > 100, 100, df.[, "X..Ascidies.Solitaires"])
    df.[, "X..Ascidies.Solitaires"] 
    
    df.[, "X..Bryozoaires.Dresses"] ; df.[, "X..Surface.Accolement"]
    df.[, "X..Bryozoaires.Dresses"] <- acco.fct("X..Bryozoaires.Dresses")
    df.[, "X..Bryozoaires.Dresses"] <- as.numeric(ifelse(as.character(df.[, "X..Bryozoaires.Dresses"]) %in% c(NA, "NaN", "-Inf", "Inf"), "0", as.character(df.[, "X..Bryozoaires.Dresses"])))
    df.[, "X..Bryozoaires.Dresses"] <- ifelse(df.[, "X..Bryozoaires.Dresses"] > 100, 100, df.[, "X..Bryozoaires.Dresses"])
    df.[, "X..Bryozoaires.Dresses"]
    
    df.[, "X..Lithophyllum"] ; df.[, "X..Surface.Accolement"]
    df.[, "X..Lithophyllum"] <- acco.fct("X..Lithophyllum")
    df.[, "X..Lithophyllum"] <- as.numeric(ifelse(as.character(df.[, "X..Lithophyllum"]) %in% c(NA, "NaN", "-Inf", "Inf"), "0", as.character(df.[, "X..Lithophyllum"])))
    df.[, "X..Lithophyllum"] <- ifelse(df.[, "X..Lithophyllum"] > 100, 100, df.[, "X..Lithophyllum"])
    df.[, "X..Lithophyllum"]
    
    D.bmI <- df.$X..Eponges + df.$X..Ascidies.Coloniales + df.$X..Ascidies.Solitaires + df.$X..Bryozoaires.Dresses 
    B.bmI <- df.$X..Lithophyllum
    A.bmI <- df.$X..algues.brunes + df.$X..algues.rouges + df.$X..Cladophora
    C.bmI <- df.$Nb.Littorina.obtusata + df.$Nb.Gibbula.cineraria + df.$Nb.Gibbula.pennanti + df.$Nb.Gibbula.umbilicalis
    E.bmI <- df.$X..algues.vertes
    F.bmI <- df.$X..Roche.Nue
    
    (  ( df.$X..Eponges + df.$X..Ascidies.Coloniales + df.$X..Ascidies.Solitaires + df.$X..Bryozoaires.Dresses )
      +  df.$X..Lithophyllum 
    ) - 
      (  (df.$X..algues.brunes + df.$X..algues.rouges + df.$X..Cladophora)
         + (df.$Nb.Littorina.obtusata + df.$Nb.Gibbula.cineraria + df.$Nb.Gibbula.pennanti + df.$Nb.Gibbula.umbilicalis)
         +  df.$X..algues.vertes
         +  df.$X..Roche.Nue
      ) -> VFI.BM            
    VFI.BM
    
    # previous, when systematically corrected for surface accolement, without considering recent 'retournement'
    
    #D.bmI <- (df.$X..Eponges  / (100 - df.$X..Surface.Accolement) * 100) + (df.$X..Ascidies.Coloniales  / (100 - df.$X..Surface.Accolement) * 100) + (df.$X..Ascidies.Solitaires / (100 - df.$X..Surface.Accolement) * 100) + (df.$X..Bryozoaires.Dresses / (100 - df.$X..Surface.Accolement) * 100) 
    #B.bmI <- df.$X..Lithophyllum / (100 - df.$X..Surface.Accolement) * 100 
    #A.bmI <- df.$X..algues.brunes + df.$X..algues.rouges + df.$X..Cladophora
    #C.bmI <- df.$Nb.Littorina.obtusata + df.$Nb.Gibbula.cineraria + df.$Nb.Gibbula.pennanti + df.$Nb.Gibbula.umbilicalis
    #E.bmI <- df.$X..algues.vertes
    #F.bmI <- df.$X..Roche.Nue
    
    #(  ( (df.$X..Eponges  / (100 - df.$X..Surface.Accolement) * 100) + (df.$X..Ascidies.Coloniales  / (100 - df.$X..Surface.Accolement) * 100) + (df.$X..Ascidies.Solitaires / (100 - df.$X..Surface.Accolement) * 100) + (df.$X..Bryozoaires.Dresses / (100 - df.$X..Surface.Accolement) * 100) )
    #  +  ( df.$X..Lithophyllum / (100 - df.$X..Surface.Accolement) * 100 ) 
    #) - 
    #  (  (df.$X..algues.brunes + df.$X..algues.rouges + df.$X..Cladophora)
    #     + (df.$Nb.Littorina.obtusata + df.$Nb.Gibbula.cineraria + df.$Nb.Gibbula.pennanti + df.$Nb.Gibbula.umbilicalis)
    #     +  df.$X..algues.vertes
    #     +  df.$X..Roche.Nue
    #  ) -> VFI.BM            
    #VFI.BM
    
    
    # VFSI.BM Bloc mobile
    
    qecb.i %>% dplyr::filter(Type.Bloc == "Bloc mobile") -> df.
    df. <- dplyr::arrange(df., desc(Face), Numéro.Bloc.échantillon)
    num.bloc <- as.vector(sort(unique(df.$Numéro.Bloc.échantillon)))
    
    G.bmSI <- NA
    H.bmSI <- NA
    L.bmSI <- NA
    
    df.BM.FS <- dplyr::filter(df., Face == "face inférieure")
    df.BM.FI <- dplyr::filter(df., Face == "face inférieure") 
    
    df.BM.FS$Couleur.dominante 
    df.BM.FS$X..Mytilus.sp.
    
    df.[, "X..Balanes.Vivantes"] ; df.[, "X..Surface.Accolement"]
    df. <- mutate(df., row.nb = row_number())
    dplyr::filter(df., Face == "face inférieure")["row.nb"]
    df.[c(dplyr::filter(df., Face == "face inférieure")[1, "row.nb"]:unlist(tail(dplyr::filter(df., Face == "face inférieure"), n=1)["row.nb"])), "X..Balanes.Vivantes"] <- acco.fct("X..Balanes.Vivantes")
    df.[, "X..Balanes.Vivantes"] <- as.numeric(ifelse(as.character(df.[, "X..Balanes.Vivantes"]) %in% c(NA, "NaN", "-Inf", "Inf"), "0", as.character(df.[, "X..Balanes.Vivantes"])))
    df.[, "X..Balanes.Vivantes"] <- ifelse(df.[, "X..Balanes.Vivantes"] > 100, 100, df.[, "X..Balanes.Vivantes"])
    df.[, "X..Balanes.Vivantes"]
    
    for (k in c(1:length(na.omit(num.bloc)))) {
      
      j. <- num.bloc[k]
      
      GIn. <- unname(unlist(
        (dplyr::filter(df., Numéro.Bloc.échantillon == j. & Face == "face supérieure")["Nb.spirorbis.total"]
         + dplyr::filter(df., Numéro.Bloc.échantillon == j. & Face == "face inférieure")["Nb.spirorbis.total"]
        ) / 1000 ))
      
      G.bmSI <<- c(G.bmSI, GIn.)
      
      HIn. <- unname(unlist(  
        (dplyr::filter(df., Numéro.Bloc.échantillon == j. & Face == "face supérieure")["X..Balanes.Vivantes"]
         + dplyr::filter(df., Numéro.Bloc.échantillon == j. & Face == "face inférieure")["X..Balanes.Vivantes"] 
        ) / 100 ))
      
      #HIn. <- unname(unlist(  
      #  (dplyr::filter(df., Numéro.Bloc.échantillon == j. & Face == "face supérieure")["X..Balanes.Vivantes"]
      #   + ( dplyr::filter(df., Numéro.Bloc.échantillon == j. & Face == "face inférieure")["X..Balanes.Vivantes"] / (100 - dplyr::filter(df., Numéro.Bloc.échantillon == j. & Face == "face inférieure")["X..Surface.Accolement"]) * 100 ) 
      #  ) / 100 ))
      
      H.bmSI <<- c(H.bmSI, HIn.)
      
      LIn. <- unname(unlist(    
        (dplyr::filter(df., Numéro.Bloc.échantillon == j. & Face == "face supérieure")["Nb.Spirobranchus.lamarckii.total"]
         + dplyr::filter(df., Numéro.Bloc.échantillon == j. & Face == "face inférieure")["Nb.Spirobranchus.lamarckii.total"]
        ) / 100 ))
      
      L.bmSI <<- c(L.bmSI, LIn.) # To avoid error message "Error in I <<- c(I, IIn.) : cannot change value of locked binding for 'I'"
      
    }
    
    G.bmSI <- G.bmSI[2:length(G.bmSI)]
    G.bmSI
    H.bmSI <- H.bmSI[2:length(H.bmSI)]
    H.bmSI
    I.bmSI <- L.bmSI[2:length(L.bmSI)]
    I.bmSI
    
    VFSI.BM <- NA  
    
    for (k in c(1:length(na.omit(num.bloc)))) {
      
      j. <- num.bloc[k]
      
      VFSIn. <- unname(unlist(
        ( (dplyr::filter(df., Numéro.Bloc.échantillon == j. & Face == "face supérieure")["Nb.spirorbis.total"]
           + dplyr::filter(df., Numéro.Bloc.échantillon == j. & Face == "face inférieure")["Nb.spirorbis.total"]
        ) / 1000 )
        -
          ( 
            ( (dplyr::filter(df., Numéro.Bloc.échantillon == j. & Face == "face supérieure")["X..Balanes.Vivantes"]
               + dplyr::filter(df., Numéro.Bloc.échantillon == j. & Face == "face inférieure")["X..Balanes.Vivantes"]
            ) / 100 )
            + ( (dplyr::filter(df., Numéro.Bloc.échantillon == j. & Face == "face supérieure")["Nb.Spirobranchus.lamarckii.total"]
                 + dplyr::filter(df., Numéro.Bloc.échantillon == j. & Face == "face inférieure")["Nb.Spirobranchus.lamarckii.total"]
            ) / 100 )
          )
      ))
      
      #VFSIn. <- unname(unlist(
      #  ( (dplyr::filter(df., Numéro.Bloc.échantillon == j. & Face == "face supérieure")["Nb.spirorbis.total"]
      #     + dplyr::filter(df., Numéro.Bloc.échantillon == j. & Face == "face inférieure")["Nb.spirorbis.total"]
      #  ) / 1000 )
      #  -
      #    ( 
      #      ( (dplyr::filter(df., Numéro.Bloc.échantillon == j. & Face == "face supérieure")["X..Balanes.Vivantes"]
      #         + ( dplyr::filter(df., Numéro.Bloc.échantillon == j. & Face == "face inférieure")["X..Balanes.Vivantes"] / (100 - dplyr::filter(df., Numéro.Bloc.échantillon == j. & Face == "face inférieure")["X..Surface.Accolement"]) * 100 )
      #      ) / 100 )
      #      + ( (dplyr::filter(df., Numéro.Bloc.échantillon == j. & Face == "face supérieure")["Nb.Spirobranchus.lamarckii.total"]
      #           + dplyr::filter(df., Numéro.Bloc.échantillon == j. & Face == "face inférieure")["Nb.Spirobranchus.lamarckii.total"]
      #      ) / 100 )
      #    )
      #)) 
      
      VFSI.BM <<- c(VFSI.BM, VFSIn.)
      
    }
    
    VFSI.BM <- VFSI.BM[2:length(VFSI.BM)]
    VFSI.BM
    
    
    # QEBM.1
    
    (QEBM.1 <- VFS.BM + VFI.BM + VFSI.BM)
    
    
    
    ## QEBM.2 
    
    
    # VrFS.BF moyenne Bloc fixé ; = VDRmoyenne in excel file
    
    qecb.i %>% dplyr::filter(Type.Bloc %in% c("Bloc fixé", "Roche en place")) -> df.
    df. <- dplyr::arrange(df., Numéro.Bloc.échantillon)
    
    A.bf <- df.$X..algues.brunes + df.$X..algues.rouges + df.$X..Cladophora
    B.bf <- df.$X..Lithophyllum
    C.bf <- df.$Nb.Littorina.obtusata + df.$Nb.Gibbula.cineraria + df.$Nb.Gibbula.pennanti + df.$Nb.Gibbula.umbilicalis
    D.bf <- df.$X..Eponges + df.$X..Ascidies.Coloniales + df.$X..Ascidies.Solitaires + df.$X..Bryozoaires.Dresses
    E.bf <- df.$X..algues.vertes
    F.bf <- df.$X..Roche.Nue
    
    A.bf <- c(A.bf, rep(NA, length(unique(dplyr::filter(qecb.i, Type.Bloc == "Bloc mobile")["Numéro.Bloc.échantillon"])[,1]) - length(A.bf)))
    B.bf <- c(B.bf, rep(NA, length(unique(dplyr::filter(qecb.i, Type.Bloc == "Bloc mobile")["Numéro.Bloc.échantillon"])[,1]) - length(B.bf)))
    C.bf <- c(C.bf, rep(NA, length(unique(dplyr::filter(qecb.i, Type.Bloc == "Bloc mobile")["Numéro.Bloc.échantillon"])[,1]) - length(C.bf)))
    D.bf <- c(D.bf, rep(NA, length(unique(dplyr::filter(qecb.i, Type.Bloc == "Bloc mobile")["Numéro.Bloc.échantillon"])[,1]) - length(D.bf)))
    E.bf <- c(E.bf, rep(NA, length(unique(dplyr::filter(qecb.i, Type.Bloc == "Bloc mobile")["Numéro.Bloc.échantillon"])[,1]) - length(E.bf)))
    F.bf <- c(F.bf, rep(NA, length(unique(dplyr::filter(qecb.i, Type.Bloc == "Bloc mobile")["Numéro.Bloc.échantillon"])[,1]) - length(F.bf)))
    
    (  (df.$X..algues.brunes + df.$X..algues.rouges + df.$X..Cladophora)
      +  df.$X..Lithophyllum
      + (df.$Nb.Littorina.obtusata + df.$Nb.Gibbula.cineraria + df.$Nb.Gibbula.pennanti + df.$Nb.Gibbula.umbilicalis)
      + (df.$X..Eponges + df.$X..Ascidies.Coloniales + df.$X..Ascidies.Solitaires + df.$X..Bryozoaires.Dresses)
    ) - 
      (   df.$X..algues.vertes
          +  df.$X..Roche.Nue
      ) -> VrFS.BF # different from Pauline, check with her
    VrFS.BF
    VrFS.BF <- c(VrFS.BF, rep(NA, length(unique(dplyr::filter(qecb.i, Type.Bloc == "Bloc mobile")["Numéro.Bloc.échantillon"])[,1]) - length(VrFS.BF)))
    
    
    # (G-(H+I)) Bloc fixé & Roche en place
    
    G.bf <- df.$Nb.spirorbis.total/1000
    H.bf <- df.$X..Balanes.Vivantes/100
    I.bf <- df.$Nb.Spirobranchus.lamarckii.total/100
    
    G.bf<- c(G.bf, rep(NA, length(unique(dplyr::filter(qecb.i, Type.Bloc == "Bloc mobile")["Numéro.Bloc.échantillon"])[,1]) - length(G.bf)))
    H.bf<- c(H.bf, rep(NA, length(unique(dplyr::filter(qecb.i, Type.Bloc == "Bloc mobile")["Numéro.Bloc.échantillon"])[,1]) - length(H.bf)))
    I.bf<- c(I.bf, rep(NA, length(unique(dplyr::filter(qecb.i, Type.Bloc == "Bloc mobile")["Numéro.Bloc.échantillon"])[,1]) - length(I.bf)))
    
    ( df.$Nb.spirorbis.total/1000
      - (df.$X..Balanes.Vivantes/100 + df.$Nb.Spirobranchus.lamarckii.total/100)
    ) -> `(G-(H+I))BF`
    `(G-(H+I))BF`
    `(G-(H+I))BF` <- c(`(G-(H+I))BF`, rep(NA, length(unique(dplyr::filter(qecb.i, Type.Bloc == "Bloc mobile")["Numéro.Bloc.échantillon"])[,1]) - length(`(G-(H+I))BF`)))
    
    
    # VrFS.BF.moy
    
    (mean(VrFS.BF + `(G-(H+I))BF`, na.rm = T) -> VrFS.BF.moy)
    
    
    # (G-(H+I)S.BM) Bloc mobile face supérieure
    
    qecb.i %>% dplyr::filter(Type.Bloc == "Bloc mobile" & Face == "face supérieure") -> df.
    df. <- dplyr::arrange(df., Numéro.Bloc.échantillon)
    
    G.bmS <- df.$Nb.spirorbis.total/1000
    H.bmS <- df.$X..Balanes.Vivantes/100 
    I.bmS <- df.$Nb.Spirobranchus.lamarckii.total/100
    
    ( df.$Nb.spirorbis.total/1000
      - (df.$X..Balanes.Vivantes/100 + df.$Nb.Spirobranchus.lamarckii.total/100)
    ) -> `(G-(H+I))S.BM`
    `(G-(H+I))S.BM`
    
    
    # VrFS.BM
    
    (VFS.BM + `(G-(H+I))S.BM` -> VrFS.BM)
    
    
    # VrFS.BM.moy
    
    (mean(VrFS.BM#[val.VrFS.BM.moy.i]
          , na.rm = T) -> VrFS.BM.moy)
    
    
    # |VrFS.BM.moy/VrFS.BF.moy|
    
    (abs(mean(VrFS.BM#[val.VrFS.BM.moy.i]
              , na.rm = T)/VrFS.BF.moy) -> `|VrFS.BM.moy/VrFS.BF.moy|`)
    
    
    # QEBM.2
    
    (QEBM.1 * `|VrFS.BM.moy/VrFS.BF.moy|` -> QEBM.2)
    
    
    ## QECB
    
    (mean(QEBM.2#[val.QECB.i]
          , na.rm = T) -> QECB)
    
  }
  
  
  qecb.val.qu.list[[i]] <- data.frame(id_qecb = rep(unique(qecb.i$id_qecb), length(QEBM.2)),
    Site = rep(unique(qecb.i$Site), length(QEBM.2)), 
    Site_bis = rep(unique(qecb.i$Site_bis), length(QEBM.2)),     
    Site.Year.Month.Day = rep(unique(qecb.i$Site.Year.Month.Day), length(QEBM.2)),
    Boulder.nb.bmS = sort(unique(dplyr::filter(qecb.i, Type.Bloc == "Bloc mobile" & Face == "face supérieure")["Numéro.Bloc.échantillon"])[,1]),
    Boulder.nb.bmI = sort(unique(dplyr::filter(qecb.i, Type.Bloc == "Bloc mobile" & Face == "face inférieure")["Numéro.Bloc.échantillon"])[,1]),
    Boulder.nb.bf = c(sort(unique(dplyr::filter(qecb.i, Type.Bloc %in% c("Bloc fixé", "Roche en place"))["Numéro.Bloc.échantillon"])[,1]), rep(NA, length(unique(dplyr::filter(qecb.i, Type.Bloc == "Bloc mobile" & Face == "face supérieure")["Numéro.Bloc.échantillon"])[,1]) - length(unique(dplyr::filter(qecb.i, Type.Bloc %in% c("Bloc fixé", "Roche en place"))["Numéro.Bloc.échantillon"])[,1]))),
    Quadrat.bmS = sort(dplyr::filter(qecb.i, Type.Bloc == "Bloc mobile" & Face == "face supérieure")["Quadrat.bis"][,1]),
    Quadrat.bmI = sort(dplyr::filter(qecb.i, Type.Bloc == "Bloc mobile" & Face == "face inférieure")["Quadrat.bis"][,1]),
    Quadrat.bf = c(sort(dplyr::filter(qecb.i, Type.Bloc %in% c("Bloc fixé", "Roche en place"))["Quadrat.bis"][,1]), rep(NA, length(unique(dplyr::filter(qecb.i, Type.Bloc == "Bloc mobile")["Numéro.Bloc.échantillon"])[,1]) - length(dplyr::filter(qecb.i, Type.Bloc %in% c("Bloc fixé", "Roche en place"))["Quadrat.bis"][,1]))),
    A.bmS,
    B.bmS,
    C.bmS,
    D.bmS,
    E.bmS,
    F.bmS,
    A.bmI,
    B.bmI,
    C.bmI,
    D.bmI,
    E.bmI,
    F.bmI,
    G.bmSI,
    H.bmSI,
    I.bmSI,
    A.bf,
    B.bf,
    C.bf,
    D.bf,
    E.bf,
    F.bf,
    G.bf,
    H.bf,
    I.bf,
    G.bmS,
    H.bmS,
    I.bmS,
    VFS.BM,
    VFI.BM,
    VFSI.BM,
    QEBM.1,
    VrFS.BF,
    `(G-(H+I))S.BM`,
    `(G-(H+I))BF`,
    VrFS.BM,
    QEBM.2)
  
  qecb.val.list[[i]] <- data.frame(id_qecb = unique(qecb.i$id_qecb), 
    Site = unique(qecb.i$Site), 
    Site_bis = unique(qecb.i$Site_bis),     
    Site.Year.Month.Day = unique(qecb.i$Site.Year.Month.Day),                 
    VrFS.BM.moy,
    VrFS.BF.moy,
    `|VrFS.BM.moy/VrFS.BF.moy|`,
    QECB)
  
  rm(qecb.i)
  rm(df.BM.FS, df.BM.FI)
  rm("(G-(H+I))BF", "(G-(H+I))S.BM", "|VrFS.BM.moy/VrFS.BF.moy|", "A.bf", "A.bmI", "A.bmS", "B.bf", "B.bmI", "B.bmS", "Bloc.nb", "C.bf", "C.bmI", "C.bmS", "D.bf", "D.bmI", "D.bmS", "E.bf", "E.bmI", "E.bmS", "F.bf", "F.bmI", "F.bmS", "G.bf", "G.bmS", "G.bmSI", "GIn.", "H.bf", "H.bmS", "H.bmSI", "HIn.", "i", "I.bf", "I.bmS", "I.bmSI", "j.", "k", "L.bmSI", "LIn.", "nb.", "num.bloc", "QEBM.1", "QEBM.2", "QECB", "VFI.BM", "VFS.BM", "VFSI.BM", "VFSIn.", "VrFS.BF", "VrFS.BF.moy", "VrFS.BM", "VrFS.BM.moy")     
  
}


# to be sourced from another file
#rs(filename = "R & reports/QECB/CB_qecb.r", from = 993, to = 1287)

qecb.val.qu. <- do.call("rbind", qecb.val.qu.list)
qecb.val.qu. <- dplyr::arrange(qecb.val.qu., Site.Year.Month.Day, Boulder.nb.bmS)
unique(qecb.val.qu.$Site.Year.Month.Day)
#library(stringr)
#Site <- str_sub(qecb.val.qu.$Site.Year.Month.Day,end = -12)
#library(tibble)
#qecb.val.qu. <- tibble::add_column(qecb.val.qu., Site, .after = "Site.Year.Month.Day")
Date <- as.Date(str_sub(qecb.val.qu.$Site.Year.Month.Day,-10,-1), format = "%Y.%m.%d", origin = "1970-01-01")
qecb.val.qu. <- tibble::add_column(qecb.val.qu., Date, .after = "Site_bis")
rm(#Site,
  Date)

# check for diff QEBM.1 components
setdiff(qecb.val.qu.$VFS.BM, ((qecb.val.qu.$A.bmS + qecb.val.qu.$B.bmS + qecb.val.qu.$C.bmS + qecb.val.qu.$D.bmS) - (qecb.val.qu.$E.bmS + qecb.val.qu.$F.bmS)))
setdiff(qecb.val.qu.$VFI.BM, ((qecb.val.qu.$D.bmI + qecb.val.qu.$B.bmI) - (qecb.val.qu.$A.bmI + qecb.val.qu.$C.bmI + qecb.val.qu.$E.bmI + qecb.val.qu.$F.bmI)))
setdiff(qecb.val.qu.$VFSI.BM, (qecb.val.qu.$G.bmSI - (qecb.val.qu.$H.bmSI + qecb.val.qu.$I.bmSI)))


# Calculate QEBMs.1.2 via A, B, C, ... and add them as variables.bis

qecb.val.qu.$QEBM.1.bis <- ( 
  (
    (qecb.val.qu.$A.bmS 
    + qecb.val.qu.$B.bmS 
    + qecb.val.qu.$C.bmS 
    + qecb.val.qu.$D.bmS) 
    - 
    (qecb.val.qu.$E.bmS 
    + qecb.val.qu.$F.bmS)
  )
+ (
    (qecb.val.qu.$D.bmI 
    + qecb.val.qu.$B.bmI) 
    - 
    (qecb.val.qu.$A.bmI 
     + qecb.val.qu.$C.bmI 
     + qecb.val.qu.$E.bmI 
     + qecb.val.qu.$F.bmI)
  )
+ (
  qecb.val.qu.$G.bmSI 
  - 
  (qecb.val.qu.$H.bmSI 
  + qecb.val.qu.$I.bmSI)
  ) 
)


QEBM.2.list <- vector("list", length(unique(qecb.val.qu.$Site.Year.Month.Day)))

for (i in c(1:length(unique(qecb.val.qu.$Site.Year.Month.Day)))) {

qecb.val.qu. %>% dplyr::filter(Site.Year.Month.Day == unique(qecb.val.qu.$Site.Year.Month.Day)[[i]]) -> QEBM.2.i
  
  QEBM.2.i$QEBM.1 * 
  abs(
  (mean(
    (
      (QEBM.2.i$A.bmS 
      + QEBM.2.i$B.bmS 
      + QEBM.2.i$C.bmS 
      + QEBM.2.i$D.bmS) 
    - 
      (QEBM.2.i$E.bmS 
      + QEBM.2.i$F.bmS)
    )
    + 
    (QEBM.2.i$G.bmS 
     - (QEBM.2.i$H.bmS 
        + QEBM.2.i$I.bmS)
    )
    , na.rm = T)
  )
  /
  (mean(
    (
    (QEBM.2.i$A.bf 
     + QEBM.2.i$B.bf 
     + QEBM.2.i$C.bf 
     + QEBM.2.i$D.bf) 
    - 
    (QEBM.2.i$E.bf 
     + QEBM.2.i$F.bf)
    )
    +
    (QEBM.2.i$G.bf 
     - 
    (QEBM.2.i$H.bf 
     + QEBM.2.i$I.bf)
    )
    , na.rm = T)
  )
  ) -> QEBM.2.bis

QEBM.2.list[[i]] <- data.frame(Site.Year.Month.Day = unique(QEBM.2.i$Site.Year.Month.Day), QEBM.2.bis)

rm(i, QEBM.2.i, QEBM.2.bis)  

}

qecb.val.qu.[,ncol(qecb.val.qu.)+1] <- do.call("rbind", QEBM.2.list)[2]  


qecb.val. <- do.call("rbind", qecb.val.list)
qecb.val. <- dplyr::arrange(qecb.val., Site.Year.Month.Day)
#Site <- str_sub(qecb.val.$Site.Year.Month.Day,end = -12)
#qecb.val. <- tibble::add_column(qecb.val., Site, .after = "Site.Year.Month.Day")
Date <- as.Date(str_sub(qecb.val.$Site.Year.Month.Day,-10,-1), format = "%Y.%m.%d", origin = "1970-01-01")
qecb.val. <- tibble::add_column(qecb.val., Date, .after = "Site_bis")
rm(#Site, 
   Date)


rm(list = ls()[!ls() %in% c("fiche", "qecb", "qecbNew", "qecb.val.", "qecb.val.qu.")])


separate(qecb.val.qu., Date, c("Annee", "Mois", "Jour"), remove = F) -> qecb.val.qu.
qecb.val.qu.$Annee <- as.integer(qecb.val.qu.$Annee)
qecb.val.qu.$Mois <- as.integer(qecb.val.qu.$Mois)
qecb.val.qu.$Jour <- as.integer(qecb.val.qu.$Jour)


#dplyr::filter(qecb.val.qu., is.nan(QEBM.2))
#dplyr::filter(qecb.val.qu., !is.nan(QEBM.2)) -> qecb.val.qu.NaN
dplyr::filter(qecb.val.qu., QEBM.2 %in% c("Inf", "NaN"))
qecb.val.qu.NaN <- qecb.val.qu.
qecb.val.qu.NaN$QEBM.2 <- ifelse(qecb.val.qu.NaN$QEBM.2 %in% c("-Inf", "NaN"), NA, qecb.val.qu.NaN$QEBM.2)

qecb.val.qu.NaN %>% dplyr::group_by(id_qecb, Site, Site_bis, Annee, Mois, Jour) %>% summarize(qecb.moy = mean(QEBM.2, na.rm = T), qecb.et = sd(QEBM.2, na.rm = T), qecb.med = median(QEBM.2, na.rm = T), qecb.min = min(QEBM.2, na.rm = T), qecb.max = max(QEBM.2, na.rm = T), nb. = n(), nb.notNa = sum(!is.na(QEBM.2))) -> qecb.val.qu.stat.

Date <- as.Date(paste0(qecb.val.qu.stat.$Annee, "-", qecb.val.qu.stat.$Mois, "-", qecb.val.qu.stat.$Jour), origin = "1970-01-01")
qecb.val.qu.stat. <- tibble::add_column(qecb.val.qu.stat., Date, .after = "Site_bis")
rm(Date)

qecb.val.qu.stat. <- as.data.frame(qecb.val.qu.stat.)

saveRDS(qecb.val.qu.stat., "results/qecb/qecb.val.qu.stat.RDS")

min(qecb.val.qu.stat.$nb.notNa)


Survey.list <- vector("list", length(unique(qecb.val.qu.$Site.Year.Month.Day)))

for (i in c(1:length(unique(qecb.val.qu.$Site.Year.Month.Day)))) {
  
  qecb.val.qu.  %>% dplyr::filter(Site.Year.Month.Day == unique(qecb.val.qu.$Site.Year.Month.Day)[[i]]) -> qecb.i
  
  Survey.list[[i]] <- data.frame(
    Site.Year.Month.Day = rep(unique(qecb.i$Site.Year.Month.Day), nrow(qecb.i)), 
    Survey.nb = rep(i, nrow(qecb.i))
  )
  
}
  
Survey <- do.call("rbind", Survey.list)
 
setdiff(qecb.val.qu.$Site.Year.Month.Day, Survey$Site.Year.Month.Day)
qecb.val.qu. <- tibble::add_column(qecb.val.qu., Survey.nb = Survey$Survey.nb, .after = "Site.Year.Month.Day")

rm(i, Survey.list, Survey, qecb.i) 

Survey.nb <- c(1:nrow(qecb.val.))
qecb.val. <- tibble::add_column(qecb.val., Survey.nb, .after = "Site.Year.Month.Day")

rm(Survey.nb)  

saveRDS(qecb.val., "qecb.val.RDS")
saveRDS(qecb.val.qu., "qecb.val.qu.RDS")


## Plots

#qecb.val. <- readRDS("results/QECB/qecb.val.RDS")
#qecb.val.qu. <- readRDS("results/QECB/qecb.val.qu.RDS")


## diagnostic plots
# NB. max about 100 plots in the plots window

# qecb.val.qu.

for (i in c(1:100)) {
  
  #i <- 75
  
  library(dplyr)
  dplyr::filter(qecb.val.qu., Site.Year.Month.Day == unique(qecb.val.qu.$Site.Year.Month.Day)[i]) -> df.  
  
  max.bm = length(na.omit(c(df.$Boulder.nb.bmS, df.$Boulder.nb.bmI)))/2
  max.bf = length(na.omit(df.$Boulder.nb.bf))
  
  par(mar=c(4,4,1.5,5))
  par(mfrow=c(4,2))
  
  # to avoid plot error, replace below values by 0.000000001 and plot red points
  df.$VFS.BM <- ifelse(is.na(df.$VFS.BM), 0.000000001, df.$VFS.BM)
  df.$VFI.BM <- ifelse(is.na(df.$VFI.BM), 0.000000001, df.$VFI.BM)
  df.$VFSI.BM <- ifelse(is.na(df.$VFSI.BM), 0.000000001, df.$VFSI.BM)
  df.$QEBM.1 <- ifelse(is.na(df.$QEBM.2), 0.000000001, df.$QEBM.1)
  df.$X.G..H.I..S.BM <- ifelse(is.na(df.$X.G..H.I..S.BM), 0.000000001, df.$X.G..H.I..S.BM)
  df.$X.G..H.I..BF[1:max.bf] <- ifelse(is.na(df.$X.G..H.I..BF[1:max.bf]), 0.000000001, df.$X.G..H.I..BF[1:max.bf])
  df.$X.G..H.I..BF[(max.bf+1):max.bm] <- ifelse(is.na(df.$X.G..H.I..BF[(max.bf+1):max.bm]), 0.000000002, df.$X.G..H.I..BF[6:max.bm])
  df.$VrFS.BF[1:max.bf] <- ifelse(is.na(df.$VrFS.BF[1:max.bf]), 0.000000001, df.$VrFS.BF[1:max.bf])
  df.$VrFS.BF[(max.bf+1):max.bm] <- ifelse(is.na(df.$VrFS.BF[(max.bf+1):max.bm]), 0.000000002, df.$VrFS.BF[(max.bf+1):max.bm])
  df.$VrFS.BM <- ifelse(is.na(df.$VrFS.BM), 0.000000001, df.$VrFS.BM)
  #df.$QEBM.2 <- ifelse(df.$QEBM.2 %in% c("Inf", "NaN"), 0.000000001, df.$QEBM.2)
  df.$QEBM.2 <- ifelse(is.na(df.$QEBM.2), 0.000000001, df.$QEBM.2)
  
  # For blocs mobiles/fixes, only use variable Boulder.nb.bmS; does refer here to a bloc, but not to its particular nb (globally for blocs fixé / Roche en place)
  
  plot(as.numeric(df.$Boulder.nb.bmS), df.$VFS.BM, xlab = "", ylab = "VFS.BM", xlim = c(1,10), pch = 19, xaxt = "n")
  bk.red <- dplyr::filter(df., VFS.BM == 0.000000001)
  points(bk.red$Boulder.nb.bmS, bk.red$VFS.BM, pch = 19, col = "red")
  axis(1, at = c(1,2,3,4,5,6,7,8,9,10), labels = c(1,2,3,4,5,6,7,8,9,10))
  mtext("Bloc mobile /  ", side = 1, line = 3, cex = 0.7)
  mtext("                          fixe", side = 1, line = 3, col = "blue", cex = 0.7)
  par(new = T)
  with(df., plot(Boulder.nb.bmS, VrFS.BF, axes = FALSE, xlab = NA, ylab = NA, yaxt = "n", cex = 0))
  axis(side = 4)
  mtext("VrFS.BF", side = 4, line = 3, col = "blue", cex = 0.7)
  bk. <- dplyr::filter(df., VrFS.BF != 0.000000002)
  points(bk.$Boulder.nb.bmS, bk.$VrFS.BF, col = "blue")
  bk.red <- dplyr::filter(df., VrFS.BF == 0.000000001)
  points(bk.red$Boulder.nb.bmS, bk.red$VrFS.BF, col = "red")
  
  plot(as.numeric(df.$Boulder.nb.bmS), df.$VFI.BM, xlab = "Bloc mobile", ylab = "VFI.BM", xlim = c(1,10), pch = 19, xaxt = "n")
  bk.red <- dplyr::filter(df., VFI.BM == 0.000000001)
  points(bk.red$Boulder.nb.bmS, bk.red$VFI.BM, pch = 19, col = "red")
  axis(1, at = c(1,2,3,4,5,6,7,8,9,10), labels = c(1,2,3,4,5,6,7,8,9,10))
  
  plot(as.numeric(df.$Boulder.nb.bmS), df.$VFSI.BM, xlab = "Bloc mobile", ylab = "VFSI.BM", xlim = c(1,10), pch = 19, xaxt = "n")
  bk.red <- dplyr::filter(df., VFSI.BM == 0.000000001)
  points(bk.red$Boulder.nb.bmS, bk.red$VFSI.BM, pch = 19, col = "red")
  axis(1, at = c(1,2,3,4,5,6,7,8,9,10), labels = c(1,2,3,4,5,6,7,8,9,10))
  
  plot(as.numeric(df.$Boulder.nb.bmS), df.$QEBM.1, xlab = "Bloc mobile", ylab = "QEBM.1", xlim = c(1,10), pch = 19, xaxt = "n")
  bk.red <- dplyr::filter(df., QEBM.1 == 0.000000001)
  points(bk.red$Boulder.nb.bmS, bk.red$QEBM.1, pch = 19, col = "red")
  axis(1, at = c(1,2,3,4,5,6,7,8,9,10), labels = c(1,2,3,4,5,6,7,8,9,10))
  
  plot(as.numeric(df.$Boulder.nb.bmS), df.$X.G..H.I..S.BM, xlab = "", ylab = "X.G..H.I..S.BM", xlim = c(1,10), pch = 19, xaxt = "n")
  bk.red <- dplyr::filter(df., X.G..H.I..S.BM == 0.000000001)
  points(bk.red$Boulder.nb.bmS, bk.red$X.G..H.I..S.BM, pch = 19, col = "red")
  axis(1, at = c(1,2,3,4,5,6,7,8,9,10), labels = c(1,2,3,4,5,6,7,8,9,10))
  mtext("Bloc mobile /  ", side = 1, line = 3, cex = 0.7)
  mtext("                          fixe", side = 1, line = 3, col = "blue", cex = 0.7)
  par(new = T)
  with(df., plot(Boulder.nb.bmS, X.G..H.I..BF, axes = FALSE, xlab = NA, ylab = NA, yaxt = "n", cex = 0))
  axis(side = 4)
  mtext("X.G..H.I..BF", side = 4, line = 3, col = "blue", cex = 0.7)
  bk. <- dplyr::filter(df., X.G..H.I..BF != 0.000000002)
  points(bk.$Boulder.nb.bmS, bk.$X.G..H.I..BF, col = "blue")
  bk.red <- dplyr::filter(df., X.G..H.I..BF == 0.000000001)
  points(bk.red$Boulder.nb.bmS, bk.red$X.G..H.I..BF, col = "red")
  
  plot(as.numeric(df.$Boulder.nb.bmS), df.$VrFS.BM, xlab = "Bloc mobile", ylab = "VrFS.BM", xlim = c(1,10), pch = 19, xaxt = "n")
  bk.red <- dplyr::filter(df., VrFS.BM == 0.000000001)
  points(bk.red$Boulder.nb.bmS, bk.red$VrFS.BM, pch = 19, col = "red")
  axis(1, at = c(1,2,3,4,5,6,7,8,9,10), labels = c(1,2,3,4,5,6,7,8,9,10))
  
  plot(as.numeric(df.$Boulder.nb.bmS), df.$QEBM.2, xlab = "Bloc mobile", ylab = "QEBM.2", xlim = c(1,10), pch = 19, xaxt = "n")
  axis(1, at = c(1,2,3,4,5,6,7,8,9,10), labels = c(1,2,3,4,5,6,7,8,9,10))
  bk. <- dplyr::filter(df., QEBM.2 != 0.000000001)
  points(bk.$Boulder.nb.bmS, bk.$QEBM.2, col = "black", pch = 19)
  bk.red. <- dplyr::filter(df., QEBM.2 == 0.000000001)
  points(bk.red.$Boulder.nb.bmS, bk.red.$QEBM.2, col = "red", pch = 19)
  
  plot(-1:1, -1:1, axes = FALSE, xlab = NA, ylab = NA, xaxt = "n", yaxt = "n", cex = 0)
  text(x = 0, y = 0, paste0(df.$Site.Year.Month.Day, " - ", df.$Survey.nb), cex = 1)
  
  par(mfrow=c(1,1))
  
  rm(df., bk., bk.red.)
  
}

for (i in c(101:length(unique(qecb.val.qu.$Site.Year.Month.Day)))) {
  
  library(dplyr)
  dplyr::filter(qecb.val.qu., Site.Year.Month.Day == unique(qecb.val.qu.$Site.Year.Month.Day)[i]) -> df.  
  
  max.bm = length(na.omit(c(df.$Boulder.nb.bmS, df.$Boulder.nb.bmI)))/2
  max.bf = length(na.omit(df.$Boulder.nb.bf))
  
  par(mar=c(4,4,1.5,5))
  par(mfrow=c(4,2))
  
  # to avoid plot error, replace below values by 0.000000001 and plot red points
  df.$VFS.BM <- ifelse(is.na(df.$VFS.BM), 0.000000001, df.$VFS.BM)
  df.$VFI.BM <- ifelse(is.na(df.$VFI.BM), 0.000000001, df.$VFI.BM)
  df.$VFSI.BM <- ifelse(is.na(df.$VFSI.BM), 0.000000001, df.$VFSI.BM)
  df.$QEBM.1 <- ifelse(is.na(df.$QEBM.2), 0.000000001, df.$QEBM.1)
  df.$X.G..H.I..S.BM <- ifelse(is.na(df.$X.G..H.I..S.BM), 0.000000001, df.$X.G..H.I..S.BM)
  df.$X.G..H.I..BF[1:max.bf] <- ifelse(is.na(df.$X.G..H.I..BF[1:max.bf]), 0.000000001, df.$X.G..H.I..BF[1:max.bf])
  df.$X.G..H.I..BF[(max.bf+1):max.bm] <- ifelse(is.na(df.$X.G..H.I..BF[(max.bf+1):max.bm]), 0.000000002, df.$X.G..H.I..BF[6:max.bm])
  df.$VrFS.BF[1:max.bf] <- ifelse(is.na(df.$VrFS.BF[1:max.bf]), 0.000000001, df.$VrFS.BF[1:max.bf])
  df.$VrFS.BF[(max.bf+1):max.bm] <- ifelse(is.na(df.$VrFS.BF[(max.bf+1):max.bm]), 0.000000002, df.$VrFS.BF[(max.bf+1):max.bm])
  df.$VrFS.BM <- ifelse(is.na(df.$VrFS.BM), 0.000000001, df.$VrFS.BM)
  #df.$QEBM.2 <- ifelse(df.$QEBM.2 %in% c("Inf", "NaN"), 0.000000001, df.$QEBM.2)
  df.$QEBM.2 <- ifelse(is.na(df.$QEBM.2), 0.000000001, df.$QEBM.2)
  
  # For blocs mobiles/fixes, only use variable Boulder.nb.bmS; does refer here to a bloc, but not to its particular nb (globally for blocs fixé / Roche en place)
  
  plot(as.numeric(df.$Boulder.nb.bmS), df.$VFS.BM, xlab = "", ylab = "VFS.BM", xlim = c(1,10), pch = 19, xaxt = "n")
  bk.red <- dplyr::filter(df., VFS.BM == 0.000000001)
  points(bk.red$Boulder.nb.bmS, bk.red$VFS.BM, pch = 19, col = "red")
  axis(1, at = c(1,2,3,4,5,6,7,8,9,10), labels = c(1,2,3,4,5,6,7,8,9,10))
  mtext("Bloc mobile /  ", side = 1, line = 3, cex = 0.7)
  mtext("                          fixe", side = 1, line = 3, col = "blue", cex = 0.7)
  par(new = T)
  with(df., plot(Boulder.nb.bmS, VrFS.BF, axes = FALSE, xlab = NA, ylab = NA, yaxt = "n", cex = 0))
  axis(side = 4)
  mtext("VrFS.BF", side = 4, line = 3, col = "blue", cex = 0.7)
  bk. <- dplyr::filter(df., VrFS.BF != 0.000000002)
  points(bk.$Boulder.nb.bmS, bk.$VrFS.BF, col = "blue")
  bk.red <- dplyr::filter(df., VrFS.BF == 0.000000001)
  points(bk.red$Boulder.nb.bmS, bk.red$VrFS.BF, col = "red")
  
  plot(as.numeric(df.$Boulder.nb.bmS), df.$VFI.BM, xlab = "Bloc mobile", ylab = "VFI.BM", xlim = c(1,10), pch = 19, xaxt = "n")
  bk.red <- dplyr::filter(df., VFI.BM == 0.000000001)
  points(bk.red$Boulder.nb.bmS, bk.red$VFI.BM, pch = 19, col = "red")
  axis(1, at = c(1,2,3,4,5,6,7,8,9,10), labels = c(1,2,3,4,5,6,7,8,9,10))
  
  plot(as.numeric(df.$Boulder.nb.bmS), df.$VFSI.BM, xlab = "Bloc mobile", ylab = "VFSI.BM", xlim = c(1,10), pch = 19, xaxt = "n")
  bk.red <- dplyr::filter(df., VFSI.BM == 0.000000001)
  points(bk.red$Boulder.nb.bmS, bk.red$VFSI.BM, pch = 19, col = "red")
  axis(1, at = c(1,2,3,4,5,6,7,8,9,10), labels = c(1,2,3,4,5,6,7,8,9,10))
  
  plot(as.numeric(df.$Boulder.nb.bmS), df.$QEBM.1, xlab = "Bloc mobile", ylab = "QEBM.1", xlim = c(1,10), pch = 19, xaxt = "n")
  bk.red <- dplyr::filter(df., QEBM.1 == 0.000000001)
  points(bk.red$Boulder.nb.bmS, bk.red$QEBM.1, pch = 19, col = "red")
  axis(1, at = c(1,2,3,4,5,6,7,8,9,10), labels = c(1,2,3,4,5,6,7,8,9,10))
  
  plot(as.numeric(df.$Boulder.nb.bmS), df.$X.G..H.I..S.BM, xlab = "", ylab = "X.G..H.I..S.BM", xlim = c(1,10), pch = 19, xaxt = "n")
  bk.red <- dplyr::filter(df., X.G..H.I..S.BM == 0.000000001)
  points(bk.red$Boulder.nb.bmS, bk.red$X.G..H.I..S.BM, pch = 19, col = "red")
  axis(1, at = c(1,2,3,4,5,6,7,8,9,10), labels = c(1,2,3,4,5,6,7,8,9,10))
  mtext("Bloc mobile /  ", side = 1, line = 3, cex = 0.7)
  mtext("                          fixe", side = 1, line = 3, col = "blue", cex = 0.7)
  par(new = T)
  with(df., plot(Boulder.nb.bmS, X.G..H.I..BF, axes = FALSE, xlab = NA, ylab = NA, yaxt = "n", cex = 0))
  axis(side = 4)
  mtext("X.G..H.I..BF", side = 4, line = 3, col = "blue", cex = 0.7)
  bk. <- dplyr::filter(df., X.G..H.I..BF != 0.000000002)
  points(bk.$Boulder.nb.bmS, bk.$X.G..H.I..BF, col = "blue")
  bk.red <- dplyr::filter(df., X.G..H.I..BF == 0.000000001)
  points(bk.red$Boulder.nb.bmS, bk.red$X.G..H.I..BF, col = "red")
  
  plot(as.numeric(df.$Boulder.nb.bmS), df.$VrFS.BM, xlab = "Bloc mobile", ylab = "VrFS.BM", xlim = c(1,10), pch = 19, xaxt = "n")
  bk.red <- dplyr::filter(df., VrFS.BM == 0.000000001)
  points(bk.red$Boulder.nb.bmS, bk.red$VrFS.BM, pch = 19, col = "red")
  axis(1, at = c(1,2,3,4,5,6,7,8,9,10), labels = c(1,2,3,4,5,6,7,8,9,10))
  
  plot(as.numeric(df.$Boulder.nb.bmS), df.$QEBM.2, xlab = "Bloc mobile", ylab = "QEBM.2", xlim = c(1,10), pch = 19, xaxt = "n")
  axis(1, at = c(1,2,3,4,5,6,7,8,9,10), labels = c(1,2,3,4,5,6,7,8,9,10))
  bk. <- dplyr::filter(df., QEBM.2 != 0.000000001)
  points(bk.$Boulder.nb.bmS, bk.$QEBM.2, col = "black", pch = 19)
  bk.red. <- dplyr::filter(df., QEBM.2 == 0.000000001)
  points(bk.red.$Boulder.nb.bmS, bk.red.$QEBM.2, col = "red", pch = 19)
  
  plot(-1:1, -1:1, axes = FALSE, xlab = NA, ylab = NA, xaxt = "n", yaxt = "n", cex = 0)
  text(x = 0, y = 0, paste0(df.$Site.Year.Month.Day, " - ", df.$Survey.nb), cex = 1)
  
  par(mfrow=c(1,1))
  
  rm(df., bk., bk.red.)
  
}

#dplyr::filter(df., Site.Year.Month.Day == unique(df.$Site.Year.Month.Day)) -> check
#unique(check$Site.Year.Month.Day)
#rm(check)


# qecb.val.

min(qecb.val.$Date) ; max(qecb.val.$Date) 
#xlim. <- c(as.Date("2014-01-01", origin = "1970-01-01"), as.Date("2018-01-01", origin = "1970-01-01"))

for (i in c(1:length(unique(qecb.val.$Site)))) {
  
library(dplyr)
dplyr::filter(qecb.val., Site == unique(qecb.val.$Site)[i]) -> df. 

library(stringr)
xmin. <- as.Date(ifelse(min(as.numeric(substring(df.$Date, 1, 4))) >= 2014, "2014-01-01", paste0(min(as.numeric(substring(df.$Date, 1, 4))), "-01-01")), origin = "1970-01-01")
xmax. <- as.Date(ifelse(max(as.numeric(substring(df.$Date, 1, 4))) <= 2017, "2018-01-01", #paste0(max(as.numeric(substring(df.$Date, 1, 4)))+1, 
                        "2022-01-01")
                 #)
                 , origin = "1970-01-01")

par(mar=c(4,4,4,3))
par(mfrow=c(2,2))

# to avoid plot error, replace below values by 0.000000001 and plot red points
df.$VrFS.BF.moy <- ifelse(is.na(df.$VrFS.BF.moy), 0.000000001, df.$VrFS.BF.moy)
df.$VrFS.BF.moy <- ifelse(df.$VrFS.BF.moy %in% c("Inf", "NaN"), 0.000000001, df.$VrFS.BF.moy)
df.$X.VrFS.BM.moy.VrFS.BF.moy. <- ifelse(df.$X.VrFS.BM.moy.VrFS.BF.moy. %in% c("Inf", "NaN"), 0.000000001, df.$X.VrFS.BM.moy.VrFS.BF.moy.)
df.$QECB <- ifelse(df.$QECB %in% c("Inf", "NaN"), 0.000000001, df.$QECB)

plot(df.$Date, df.$VrFS.BM.moy, xlab = "Date", ylab = "VrFS.BM.moy", pch = 19, xlim = c(xmin., xmax.), main = unique(df.$Site)
       )

plot(df.$Date, df.$VrFS.BF.moy, xlab = "Date", ylab = "VrFS.BF.moy", pch = 19, xlim = c(xmin., xmax.), main = unique(df.$Site)
)

bk. <- dplyr::filter(df., VrFS.BF.moy == 0.000000001)
points(bk.$Date, bk.$VrFS.BF.moy, pch = 19, col = "red")

plot(df.$Date, df.$X.VrFS.BM.moy.VrFS.BF.moy., xlab = "Date", ylab = "X.VrFS.BM.moy.VrFS.BF.moy.", pch = 19, xlim = c(xmin., xmax.), main = unique(df.$Site)
)

bk. <- dplyr::filter(df., X.VrFS.BM.moy.VrFS.BF.moy. == 0.000000001)
points(bk.$Date, bk.$X.VrFS.BM.moy.VrFS.BF.moy., pch = 19, col = "red")

plot(df.$Date, df.$QECB, xlab = "Date", ylab = "QECB", pch = 19, xlim = c(xmin., xmax.), main = unique(df.$Site)
)

bk. <- dplyr::filter(df., QECB == 0.000000001 | QECB > 420 | QECB < -420)
points(bk.$Date, bk.$QECB, pch = 19, col = "red")

rm(bk., xmax., xmin.)

}


# QECB Site plots 

par(mfrow = c(3,2))

par(mar=c(5, 4, 4, 2) + 0.1)

for (i in c(1:length(unique(qecb.val.$Site)))) {
  
  dplyr::filter(qecb.val., Site == unique(qecb.val.$Site)[i]) -> qecb.val.eg
  
  library(stringr)
  xmin. <- as.Date(ifelse(min(as.numeric(substring(qecb.val.eg$Date, 1, 4))) >= 2014, "2014-01-01", paste0(min(as.numeric(substring(qecb.val.eg$Date, 1, 4))), "-01-01")), origin = "1970-01-01")
  xmax. <- as.Date(ifelse(max(as.numeric(substring(qecb.val.eg$Date, 1, 4))) <= 2017, "2018-01-01", #paste0(max(as.numeric(substring(qecb.val.eg$Date, 1, 4)))+1, 
                          "2022-01-01")
                   #)
                   , origin = "1970-01-01")
  
  qecb.val.eg$QECB <- ifelse(qecb.val.eg$QECB %in% c("Inf", "NaN"), 0.000000001, qecb.val.eg$QECB)
  qecb.val.eg$QECB <- ifelse(is.na(qecb.val.eg$QECB), 0.000000001, qecb.val.eg$QECB)
  
  plot(qecb.val.eg$Date, qecb.val.eg$QECB, xlim = c(xmin., xmax.), pch = 19, main = unique(qecb.val.eg$Site), xlab = "Date", ylab = "QECB", type = "b")
  
  bk. <- dplyr::filter(qecb.val.eg, QECB == 0.000000001 | QECB > 360 | QECB < -360)
  points(bk.$Date, bk.$QECB, pch = 19, col = "red")
  
  rm(bk., xmax., xmin.)
  
}


par(mfrow=c(1,1))
hist(qecb.val.$QECB, xlim = c(-360,360), breaks = 1000, xlab = "QECB", main = "")
quantile(qecb.val.$QECB, c(.25,.5,.75), na.rm = T)


# QECB Site plots, with theoretical limits of -360 to 360 

par(mfrow = c(3,2))

par(mar=c(5, 4, 4, 2) + 0.1)

for (i in c(1:length(unique(qecb.val.$Site)))) {
  
  dplyr::filter(qecb.val., Site == unique(qecb.val.$Site)[i]) -> qecb.val.eg
  
  library(stringr)
  xmin. <- as.Date(ifelse(min(as.numeric(substring(qecb.val.eg$Date, 1, 4))) >= 2014, "2014-01-01", paste0(min(as.numeric(substring(qecb.val.eg$Date, 1, 4))), "-01-01")), origin = "1970-01-01")
  xmax. <- as.Date(ifelse(max(as.numeric(substring(qecb.val.eg$Date, 1, 4))) <= 2017, "2018-01-01", #paste0(max(as.numeric(substring(qecb.val.eg$Date, 1, 4)))+1, 
                          "2022-01-01")
                   #)
                   , origin = "1970-01-01")
  
  qecb.val.eg$QECB <- ifelse(qecb.val.eg$QECB %in% c("Inf", "NaN"), 0.000000001, qecb.val.eg$QECB)
  qecb.val.eg$QECB <- ifelse(is.na(qecb.val.eg$QECB), 0.000000001, qecb.val.eg$QECB)
  
  plot(qecb.val.eg$Date, qecb.val.eg$QECB, xlim = c(xmin., xmax.), ylim = c(-360,360), pch = 19, main = unique(qecb.val.eg$Site), xlab = "Date", ylab = "QECB", type = "b")
  
  bk. <- dplyr::filter(qecb.val.eg, QECB == 0.000000001 | QECB > 360 | QECB < -360)
  points(bk.$Date, bk.$QECB, pch = 19, col = "red")
  
  rm(bk., xmax., xmin.)
  
}


# QECB Site plots, with and without theoretical limits, and with abnormal qecb values in red 

par(mfrow = c(3,2))

for (i in c(1:length(unique(qecb.val.$Site)))) {
  
  dplyr::filter(qecb.val., Site == unique(qecb.val.$Site)[i]) -> qecb.val.eg
  
  library(stringr)
  xmin. <- as.Date(ifelse(min(as.numeric(substring(qecb.val.eg$Date, 1, 4))) >= 2014, "2014-01-01", paste0(min(as.numeric(substring(qecb.val.eg$Date, 1, 4))), "-01-01")), origin = "1970-01-01")
  xmax. <- as.Date(ifelse(max(as.numeric(substring(qecb.val.eg$Date, 1, 4))) <= 2017, "2018-01-01", #paste0(max(as.numeric(substring(qecb.val.eg$Date, 1, 4)))+1, 
                          "2022-01-01")
                   #)
                   , origin = "1970-01-01")
  
  qecb.val.eg$QECB <- ifelse(qecb.val.eg$QECB %in% c("Inf", "NaN"), 0.000000001, qecb.val.eg$QECB)
  qecb.val.eg$QECB <- ifelse(is.na(qecb.val.eg$QECB), 0.000000001, qecb.val.eg$QECB)
  
  plot(qecb.val.eg$Date, qecb.val.eg$QECB, xlim = c(xmin., xmax.), pch = 19, main = unique(qecb.val.eg$Site), xlab = "Date", ylab = "QECB")
  
  bk. <- dplyr::filter(qecb.val.eg, QECB == 0.000000001 | QECB > 360 | QECB < -360)
  points(bk.$Date, bk.$QECB, pch = 19, col = "red")
  
  plot(qecb.val.eg$Date, qecb.val.eg$QECB, xlim = c(xmin., xmax.), ylim = c(-420,420), pch = 19, main = unique(qecb.val.eg$Site), xlab = "Date", ylab = "QECB")
  
  bk. <- dplyr::filter(qecb.val.eg, QECB == 0.000000001 | QECB > 360 | QECB < -360)
  points(bk.$Date, bk.$QECB, pch = 19, col = "red")
  
  show(unique(qecb.val.eg$Site))
  show(dplyr::filter(qecb.val.eg, QECB < -360)["QECB"])
  show(dplyr::filter(qecb.val.eg, QECB > 360)["QECB"])

  rm(bk., xmax., xmin.)
  
}


# data check based on above diagnostic plots

unique(qecbNew$Site)
x. <- grep("GroinCou", qecbNew$Site.Year.Month.Day)
x. <- qecbNew[x.,]
x. <- dplyr::arrange(x., date_fiche, Type.Bloc, Face)


# summary plots

xlim. <- c(as.Date("2014-01-01", origin = "1970-01-01"), as.Date("2021-01-01", origin = "1970-01-01"))

par(mfrow=c(2,1))
plot(qecb.val.$Date, qecb.val.$QECB, xlim = xlim., pch = 19, xlab = "Date", ylab = "QECB")

qecb.NA<- qecb.val.
qecb.NA$QECB <- ifelse(qecb.NA$QECB %in% c("Inf", "NaN"), 0.000000001, qecb.NA$QECB)
qecb.NA$QECB <- ifelse(is.na(qecb.NA$QECB), 0.000000001, qecb.NA$QECB)
bk. <- dplyr::filter(qecb.NA, QECB == 0.000000001 | QECB > 360 | QECB < -360)
points(bk.$Date, bk.$QECB, pch = 19, col = "red")

plot(qecb.val.$Date, qecb.val.$QECB, xlim = xlim., ylim = c(-360,360), pch = 19, xlab = "Date", ylab = "QECB")
points(bk.$Date, bk.$QECB, pch = 19, col = "red")

rm(bk.)

par(mfrow=c(2,2))
plot(qecb.val.qu.$Date, qecb.val.qu.$QEBM.1, xlim = xlim., ylim = c(-300,300), pch = 19, xlab = "Date", ylab = "QEBM.1")
plot(qecb.val.qu.$Date, qecb.val.qu.$QEBM.2, xlim = xlim., ylim = c(-700,600), pch = 19, xlab = "Date", ylab = "QEBM.2")
plot(qecb.val.$Date, qecb.val.$QECB, xlim = xlim., ylim = c(-360,360), pch = 19, xlab = "Date", ylab = "QECB")

par(mfrow=c(1,1))

hist(qecb.val.$QECB, breaks = 400, xlim = c(-360,360))
hist(qecb.val.qu.$QEBM.1, breaks = 400, xlim = c(-300,300))
summary(qecb.val.qu.$QEBM.2)
hist(qecb.val.qu.$QEBM.2, breaks = 10000, xlim = c(-300,300))

rm(df., i, qecb.val.eg, x., xlim.)


# correlation

#cor(qecb.val.qu.$QEBM.1, qecb.val.qu.$QEBM.2, use = "pairwise.complete.obs")
#x. <- na.omit(qecb.val.qu.[, c("QEBM.1", "QEBM.2")])
#x. <- data.frame(x.)
#summary(x.)
#x. <- dplyr::filter(x., QEBM.2 < 1000)
#(xp. <- cor(x.$QEBM.1, x.$QEBM.2))
#corrplot::corrplot()


# plot qecb stats

#qecb.val.qu.stat. <- readRDS("results/qecb/qecb.val.qu.stat.RDS")

`%notin%` <- Negate(`%in%`)
#qecb.val.qu.stat. <- dplyr::filter(qecb.val.qu.stat., Site %notin% c("FINS_Quemenes", "FINS_SeinGoulenez"))
qecb.val.qu.stat.NaN <- qecb.val.qu.stat.
qecb.val.qu.stat.NaN[, c("qecb.moy", "qecb.et", "qecb.med", "qecb.min", "qecb.max")] <- ifelse(qecb.val.qu.stat.NaN[, c("qecb.moy", "qecb.et", "qecb.med", "qecb.min", "qecb.max")] %in% c("-Inf", "NaN"), NA, qecb.val.qu.stat.NaN[, c("qecb.moy", "qecb.et", "qecb.med", "qecb.min", "qecb.max")])

min(qecb.val.qu.stat.NaN$Date)
max(qecb.val.qu.stat.NaN$Date)

par(mfrow = c(3,2))

for (i in c(1:length(unique(qecb.val.qu.stat.NaN$Site)))) {
  
  library(hablar) # to remove (-)Inf for min and max fct
  
  dplyr::filter(qecb.val.qu.stat.NaN, Site == unique(qecb.val.qu.stat.NaN$Site)[i]) -> qecb.val.eg
  
  xmin. <- as.Date(ifelse(min(qecb.val.eg$Annee) >= 2014, "2014-01-01", paste0(min(qecb.val.eg$Annee), "-01-01")), origin = "1970-01-01")
  xmax. <- as.Date(ifelse(max(qecb.val.eg$Annee) <= 2017, "2018-01-01", #paste0(max(qecb.val.eg$Annee)+1, 
                          "2022-01-01")
                   #)
                   , origin = "1970-01-01")
  
  qecb.val.eg$qecb.moy <- ifelse(is.na(qecb.val.eg$qecb.moy), 0.000000001, qecb.val.eg$qecb.moy)
  qecb.val.eg$qecb.et <- ifelse(is.na(qecb.val.eg$qecb.et), 0.000000001, qecb.val.eg$qecb.et)
  
  ymin. <- min(s(qecb.val.eg$qecb.moy - qecb.val.eg$qecb.et))
  ymax. <- max(s(qecb.val.eg$qecb.moy + qecb.val.eg$qecb.et))
  
  dplyr::filter(qecb.val.eg, nb.notNa > 2) -> sd.
  
  plot(qecb.val.qu.stat.NaN$Date, qecb.val.qu.stat.NaN$qecb.moy, xlim = c(xmin., xmax.),
       ylim = c(ymin., ymax.), 
       pch = 19, main = unique(qecb.val.eg$Site), xlab = "Date", ylab = "QECB (moy. et ET)", col = "grey")

  bk. <- dplyr::filter(qecb.val.eg, qecb.moy != 0.000000001)
  points(bk.$Date, bk.$qecb.moy, pch = 19, cex = 1.5)
  arrows(sd.$Date, sd.$qecb.moy + sd.$qecb.et, sd.$Date, sd.$qecb.moy - sd.$qecb.et, code = 3, angle = 90, length = 0.00)
  bk. <- dplyr::filter(qecb.val.eg, qecb.moy == 0.000000001)
  points(bk.$Date, bk.$qecb.moy, col = "red", cex = 1.5)
  
  rm(xmin., xmax., qecb.val.eg, sd., bk.)
  
}

par(mfrow = c(1,1))


# QECB limits = c(-360, 360)

par(mfrow = c(3,2))

for (i in c(1:length(unique(qecb.val.qu.stat.NaN$Site)))) {
  
  dplyr::filter(qecb.val.qu.stat.NaN, Site == unique(qecb.val.qu.stat.NaN$Site)[i]) -> qecb.val.eg
  
  xmin. <- as.Date(ifelse(min(qecb.val.eg$Annee) >= 2014, "2014-01-01", paste0(min(qecb.val.eg$Annee), "-01-01")), origin = "1970-01-01")
  xmax. <- as.Date(ifelse(max(qecb.val.eg$Annee) <= 2017, "2018-01-01", #paste0(max(qecb.val.eg$Annee)+1, 
                          "2022-01-01")
                   #)
                   , origin = "1970-01-01")
  
  qecb.val.eg$qecb.moy <- ifelse(is.na(qecb.val.eg$qecb.moy), 0.000000001, qecb.val.eg$qecb.moy)
  qecb.val.eg$qecb.et <- ifelse(is.na(qecb.val.eg$qecb.et), 0.000000001, qecb.val.eg$qecb.et)
  
  ymin. <- min(qecb.val.eg$qecb.moy - qecb.val.eg$qecb.et, na.rm = T)
  ymax. <- max(qecb.val.eg$qecb.moy + qecb.val.eg$qecb.et, na.rm =T)
  
  dplyr::filter(qecb.val.eg, nb.notNa > 2) -> sd.
  
  plot(qecb.val.qu.stat.NaN$Date, qecb.val.qu.stat.NaN$qecb.moy, xlim = c(xmin., xmax.),
       ylim = c(-360, 360), 
       pch = 19, main = unique(qecb.val.eg$Site), xlab = "Date", ylab = "QECB (moy. et ET)", col = "grey")
  
  bk. <- dplyr::filter(qecb.val.eg, qecb.moy != 0.000000001)
  points(bk.$Date, bk.$qecb.moy, pch = 19, cex = 1.5)
  arrows(sd.$Date, sd.$qecb.moy + sd.$qecb.et, sd.$Date, sd.$qecb.moy - sd.$qecb.et, code = 3, angle = 90, length = 0.00)
  bk. <- dplyr::filter(qecb.val.eg, qecb.moy == 0.000000001)
  points(bk.$Date, bk.$qecb.moy, col = "red", cex = 1.5)
  
  rm(xmin., xmax., qecb.val.eg, sd., bk.)
  
}

par(mfrow = c(1,1))


# Maud quality scale with fixed limits

par(mfrow = c(3,2))

for (i in c(1:length(unique(qecb.val.qu.stat.NaN$Site)))) {
  
  dplyr::filter(qecb.val.qu.stat.NaN, Site == unique(qecb.val.qu.stat.NaN$Site)[i]) -> qecb.val.eg
  
  xmin. <- as.Date(ifelse(min(qecb.val.eg$Annee) >= 2014, "2014-01-01", paste0(min(qecb.val.eg$Annee), "-01-01")), origin = "1970-01-01")
  xmax. <- as.Date(ifelse(max(qecb.val.eg$Annee) <= 2017, "2018-01-01", #paste0(max(qecb.val.eg$Annee)+1, 
                          "2022-01-01")
                   #)
                   , origin = "1970-01-01")
  
  qecb.val.eg$qecb.moy <- ifelse(is.na(qecb.val.eg$qecb.moy), 0.000000001, qecb.val.eg$qecb.moy)
  qecb.val.eg$qecb.et <- ifelse(is.na(qecb.val.eg$qecb.et), 0.000000001, qecb.val.eg$qecb.et)
  
  ymin. <- min(qecb.val.eg$qecb.moy - qecb.val.eg$qecb.et, na.rm = T)
  ymax. <- max(qecb.val.eg$qecb.moy + qecb.val.eg$qecb.et, na.rm =T)
  
  dplyr::filter(qecb.val.eg, nb.notNa > 2) -> sd.
  
  plot(qecb.val.eg$Date, qecb.val.eg$qecb.moy, xlim = c(xmin., xmax.), ylim = c(-360, 360), pch = 19, main = unique(qecb.val.eg$Site), xlab = "", ylab = "", type = 'n', axes = F
       #, yaxs = 'i'
  )
  
  rect(as.Date("2013-01-01", origin = "1970-01-01"), -400, as.Date("2023-01-01", origin = "1970-01-01"), -216, col = "red", border = NA)
  rect(as.Date("2013-01-01", origin = "1970-01-01"), -216, as.Date("2023-01-01", origin = "1970-01-01"), -72, col = "orange", border = NA)
  rect(as.Date("2013-01-01", origin = "1970-01-01"), -72, as.Date("2023-01-01", origin = "1970-01-01"), 72, col = "yellow", border = NA)
  rect(as.Date("2013-01-01", origin = "1970-01-01"), 72, as.Date("2023-01-01", origin = "1970-01-01"), 216, col = "olivedrab", border = NA)
  rect(as.Date("2013-01-01", origin = "1970-01-01"), 216, as.Date("2023-01-01", origin = "1970-01-01"), 400, col = "blue", border = NA)
  
  par(new = T)
  plot(qecb.val.qu.stat.NaN$Date, qecb.val.qu.stat.NaN$qecb.moy, xlim = c(xmin., xmax.),
       ylim = c(-360, 360), 
       pch = 19, main = unique(qecb.val.eg$Site), xlab = "Date", ylab = "QECB (moy. et ET)", col = "grey")
  
  bk. <- dplyr::filter(qecb.val.eg, qecb.moy != 0.000000001)
  points(bk.$Date, bk.$qecb.moy, pch = 19, cex = 1.5)
  arrows(sd.$Date, sd.$qecb.moy + sd.$qecb.et, sd.$Date, sd.$qecb.moy - sd.$qecb.et, code = 3, angle = 90, length = 0.00)
  bk. <- dplyr::filter(qecb.val.eg, qecb.moy == 0.000000001)
  points(bk.$Date, bk.$qecb.moy, col = "red", cex = 1.5)
  
  rm(xmin., xmax., qecb.val.eg, sd., bk.)
  
}

par(mfrow = c(1,1))


# New quality scale based on quartiles

dplyr::filter(qecb.val.qu.NaN, QEBM.2 >= -360) -> dt.
dplyr::filter(dt., QEBM.2 <= 360) -> dt. 
quantile(dt.$QEBM.2, c(0.06, 0.94), na.rm=T)
quantile(dt.$QEBM.2, c(0.05, 0.95), na.rm=T)
min(dt.$QEBM.2) ; max(dt.$QEBM.2)
hist(dt.$QEBM.2, xlim = c(-150, 150), breaks = 100, main = "", xlab = "QEBM.2", ylab = "Fréquence")

dplyr::filter(dt., QEBM.2 >= quantile(dt.$QEBM.2, c(0.05), na.rm=T)) -> dt.bis
dplyr::filter(dt.bis, QEBM.2 <= quantile(dt.bis$QEBM.2, c(0.95), na.rm=T)) -> dt.bis

par(mfrow = c(3,2))
# I have unactivated the model line drawing because aberant for some sites with bad qecb values
for (i in c(1:length(unique(qecb.val.qu.stat.NaN$Site)))) {
  
  dplyr::filter(qecb.val.qu.stat.NaN, Site == unique(qecb.val.qu.stat.NaN$Site)[i]) -> qecb.val.eg
  
  xmin. <- as.Date(ifelse(min(qecb.val.eg$Annee) >= 2014, "2014-01-01", paste0(min(qecb.val.eg$Annee), "-01-01")), origin = "1970-01-01")
  xmax. <- as.Date(ifelse(max(qecb.val.eg$Annee) <= 2017, "2018-01-01", #paste0(max(qecb.val.eg$Annee)+1, 
                          "2022-01-01")
                   #)
                   , origin = "1970-01-01")
  
  qecb.val.eg$qecb.moy <- ifelse(is.na(qecb.val.eg$qecb.moy), 0.000000001, qecb.val.eg$qecb.moy)
  qecb.val.eg$qecb.et <- ifelse(is.na(qecb.val.eg$qecb.et), 0.000000001, qecb.val.eg$qecb.et)
  
  ymin. <- min(qecb.val.eg$qecb.moy - qecb.val.eg$qecb.et, na.rm = T)
  ymax. <- max(qecb.val.eg$qecb.moy + qecb.val.eg$qecb.et, na.rm =T)
  
  dplyr::filter(qecb.val.eg, nb.notNa > 2) -> sd.
  
  plot(qecb.val.eg$Date, qecb.val.eg$qecb.moy, xlim = c(xmin., xmax.), ylim = c(-80, 120), pch = 19, main = unique(qecb.val.eg$Site), xlab = "", ylab = "", type = 'n', axes = F
       #, yaxs = 'i'
  )
  
  one <- mean(unlist(dplyr::filter(dt.bis, QEBM.2 <= quantile(dt.bis$QEBM.2, 0.25, na.rm = T))["QEBM.2"]))
  two <- mean(unlist(dplyr::filter(dt.bis, QEBM.2 > quantile(dt.bis$QEBM.2, 0.25, na.rm = T) & QEBM.2 <= quantile(dt.bis$QEBM.2, 0.5, na.rm = T))["QEBM.2"]))
  three <- mean(unlist(dplyr::filter(dt.bis, QEBM.2 > quantile(dt.bis$QEBM.2, 0.5, na.rm = T) & QEBM.2 <= quantile(dt.bis$QEBM.2, 0.75, na.rm = T))["QEBM.2"]))
  four <- mean(unlist(dplyr::filter(dt.bis, QEBM.2 > quantile(dt.bis$QEBM.2, 0.75, na.rm = T))["QEBM.2"]))
  
  rect(as.Date("2013-01-01", origin = "1970-01-01"), -400, as.Date("2023-01-01", origin = "1970-01-01"), one, col = "red", border = NA)
  rect(as.Date("2013-01-01", origin = "1970-01-01"), one, as.Date("2023-01-01", origin = "1970-01-01"), two, col = "orange", border = NA)
  rect(as.Date("2013-01-01", origin = "1970-01-01"), two, as.Date("2023-01-01", origin = "1970-01-01"), three, col = "yellow", border = NA)
  rect(as.Date("2013-01-01", origin = "1970-01-01"), three, as.Date("2023-01-01", origin = "1970-01-01"), four, col = "olivedrab", border = NA)
  rect(as.Date("2013-01-01", origin = "1970-01-01"), four, as.Date("2023-01-01", origin = "1970-01-01"), 400, col = "blue", border = NA)
  
  par(new = T)
  plot(qecb.val.qu.stat.NaN$Date, qecb.val.qu.stat.NaN$qecb.moy, xlim = c(xmin., xmax.),
       #ylim = c(-80, 120), 
       ylim = c(-360, 360), 
       pch = 19, main = unique(qecb.val.eg$Site), xlab = "Date", ylab = "QECB (moy. et ET)", col = "grey")
  
  bk. <- dplyr::filter(qecb.val.eg, qecb.moy != 0.000000001)
  points(bk.$Date, bk.$qecb.moy, pch = 19, cex = 1.5)
  arrows(sd.$Date, sd.$qecb.moy + sd.$qecb.et, sd.$Date, sd.$qecb.moy - sd.$qecb.et, code = 3, angle = 90, length = 0.00)
  bk. <- dplyr::filter(qecb.val.eg, qecb.moy == 0.000000001)
  points(bk.$Date, bk.$qecb.moy, col = "red", cex = 1.5)
 
  dplyr::filter(qecb.val.qu.NaN, Site == unique(qecb.val.eg$Site)) -> qecb.val.mod.eg
  dplyr::filter(qecb.val.mod.eg, !is.na(qecb.val.mod.eg$QEBM.2)) -> qecb.val.mod.eg
  dplyr::filter(qecb.val.mod.eg, is.finite(qecb.val.mod.eg$QEBM.2)) -> qecb.val.mod.eg 
  
  nrow(qecb.val.mod.eg)
  if (nrow(qecb.val.mod.eg) == 0) {
    dplyr::filter(qecb.val.qu.NaN, Site == unique(qecb.val.eg$Site)) -> qecb.val.mod.eg.Na.only
    qecb.val.mod.eg.Na.only[, "QEBM.2"] <- 1
    lw. <- loess(is.na(qecb.val.mod.eg.Na.only$QEBM.2) ~ as.numeric(qecb.val.mod.eg.Na.only$Date), span = 3)
  } else {
    lw. <- loess(qecb.val.mod.eg$QEBM.2 ~ as.numeric(qecb.val.mod.eg$Date), span = 3)
  }
  
  #lw. <- loess(qecb.val.mod.eg$QEBM.2 ~ as.numeric(qecb.val.mod.eg$Date), span = 3)
  j <- order(as.numeric(qecb.val.mod.eg$Date))
  #lines(as.numeric(qecb.val.mod.eg$Date)[j],lw.$fitted[j], col = "black",lwd = 3, lty = "dashed")
  
  rm(xmin., xmax., qecb.val.eg, sd., bk., qecb.val.mod.eg, lw., j)
  
}

par(mfrow = c(1,1))


library(plotrix)

#par(mar=c(5, 4, 4, 2) + 0.1)
par(mar=c(5, 4, 2, 2) + 0.1)

par(mfrow = c(3,2))
# idem, model removes
for (i in c(1:length(unique(qecb.val.qu.stat.NaN$Site)))) {
     #c(1,3,5,7,15,16))
  
  #i <- 14
     
  dplyr::filter(qecb.val.qu.stat.NaN, Site == unique(qecb.val.qu.stat.NaN$Site)[i]) -> qecb.val.eg
  
  xmin. <- as.Date(ifelse(min(qecb.val.eg$Annee) >= 2014, "2014-01-01", paste0(min(qecb.val.eg$Annee), "-01-01")), origin = "1970-01-01")
  xmax. <- as.Date(ifelse(max(qecb.val.eg$Annee) <= 2017, "2018-01-01", #paste0(max(qecb.val.eg$Annee)+1, 
                          "2023-01-01")
                   #)
                   , origin = "1970-01-01")
  
  qecb.val.eg$qecb.moy <- ifelse(is.na(qecb.val.eg$qecb.moy), 0.000000001, qecb.val.eg$qecb.moy)
  qecb.val.eg$qecb.et <- ifelse(is.na(qecb.val.eg$qecb.et), 0.000000001, qecb.val.eg$qecb.et)
  
  ymin. <- min(qecb.val.eg$qecb.moy - qecb.val.eg$qecb.et, na.rm = T)
  ymax. <- max(qecb.val.eg$qecb.moy + qecb.val.eg$qecb.et, na.rm =T)
  
  dplyr::filter(qecb.val.eg, nb.notNa > 2) -> sd.
  
  plot(qecb.val.eg$Date, qecb.val.eg$qecb.moy, xlim = c(xmin., xmax.), ylim = c(-80, 120), pch = 19, main = unique(qecb.val.eg$Site), xlab = "", ylab = "", type = 'n', axes = F
       #, yaxs = 'i'
  )
  
  one <- mean(unlist(dplyr::filter(dt.bis, QEBM.2 <= quantile(dt.bis$QEBM.2, 0.25, na.rm = T))["QEBM.2"]))
  two <- mean(unlist(dplyr::filter(dt.bis, QEBM.2 > quantile(dt.bis$QEBM.2, 0.25, na.rm = T) & QEBM.2 <= quantile(dt.bis$QEBM.2, 0.5, na.rm = T))["QEBM.2"]))
  three <- mean(unlist(dplyr::filter(dt.bis, QEBM.2 > quantile(dt.bis$QEBM.2, 0.5, na.rm = T) & QEBM.2 <= quantile(dt.bis$QEBM.2, 0.75, na.rm = T))["QEBM.2"]))
  four <- mean(unlist(dplyr::filter(dt.bis, QEBM.2 > quantile(dt.bis$QEBM.2, 0.75, na.rm = T))["QEBM.2"]))
  
  rect(as.Date("2013-01-01", origin = "1970-01-01"), -400, as.Date("2023-01-01", origin = "1970-01-01"), one, col = "red", border = NA)
  rect(as.Date("2013-01-01", origin = "1970-01-01"), one, as.Date("2023-01-01", origin = "1970-01-01"), two, col = "orange", border = NA)
  rect(as.Date("2013-01-01", origin = "1970-01-01"), two, as.Date("2023-01-01", origin = "1970-01-01"), three, col = "yellow", border = NA)
  rect(as.Date("2013-01-01", origin = "1970-01-01"), three, as.Date("2023-01-01", origin = "1970-01-01"), four, col = "olivedrab", border = NA)
  rect(as.Date("2013-01-01", origin = "1970-01-01"), four, as.Date("2023-01-01", origin = "1970-01-01"), 400, col = "blue", border = NA)
  
  par(new = T)
  plot(qecb.val.qu.stat.NaN$Date, qecb.val.qu.stat.NaN$qecb.moy, xlim = c(xmin., xmax.),
       ylim = c(-80, 120), 
       #ylim = c(-360, 360), 
       pch = 19, main = unique(qecb.val.eg$Site), xlab = "Date", ylab = "QECB (moy. et ET)", col = "grey")
  
  bk. <- dplyr::filter(qecb.val.eg, qecb.moy != 0.000000001)
  points(bk.$Date, bk.$qecb.moy, pch = 19, cex = 1.5)
  arrows(sd.$Date, sd.$qecb.moy + sd.$qecb.et, sd.$Date, sd.$qecb.moy - sd.$qecb.et, code = 3, angle = 90, length = 0.00)
  bk. <- dplyr::filter(qecb.val.eg, qecb.moy == 0.000000001)
  points(bk.$Date, bk.$qecb.moy, col = "red", cex = 1.5)
  
  dplyr::filter(qecb.val.qu.NaN, Site == unique(qecb.val.eg$Site)) -> qecb.val.mod.eg
  dplyr::filter(qecb.val.mod.eg, !is.na(qecb.val.mod.eg$QEBM.2)) -> qecb.val.mod.eg
  dplyr::filter(qecb.val.mod.eg, is.finite(qecb.val.mod.eg$QEBM.2)) -> qecb.val.mod.eg 
  
  nrow(qecb.val.mod.eg)
  if (nrow(qecb.val.mod.eg) == 0) {
    dplyr::filter(qecb.val.qu.NaN, Site == unique(qecb.val.eg$Site)) -> qecb.val.mod.eg.Na.only
    qecb.val.mod.eg.Na.only[, "QEBM.2"] <- 1
    lw. <- loess(is.na(qecb.val.mod.eg.Na.only$QEBM.2) ~ as.numeric(qecb.val.mod.eg.Na.only$Date), span = 3)
  } else {
    lw. <- loess(qecb.val.mod.eg$QEBM.2 ~ as.numeric(qecb.val.mod.eg$Date), span = 3)
  }
  
  #lw. <- loess(qecb.val.mod.eg$QEBM.2 ~ as.numeric(qecb.val.mod.eg$Date), span = 3)
  j <- order(as.numeric(qecb.val.mod.eg$Date))
  #lines(as.numeric(qecb.val.mod.eg$Date)[j],lw.$fitted[j], col = "black",lwd = 3, lty = "dashed")
  
  rm(xmin., xmax., qecb.val.eg, sd., bk., qecb.val.mod.eg, lw., j)
  
}

par(mfrow = c(1,1))
