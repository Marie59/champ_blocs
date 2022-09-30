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

qecb <- read.csv2(input_data, header = TRUE, fileEncoding = "Latin1") # fileEncoding = "Latin1",  cfr é in variable names

qecb_next <- read.csv2(input_data2, header = TRUE, fileEncoding = "Latin1") # fileEncoding = "Latin1",  cfr é in variable names

# bind qecb dfs.

qecb <- dplyr::bind_rows(qecb, qecb_next)
rm(qecb_next)

# import csv files ficheterrain

fiche <- read.csv2(fiche_val, header = TRUE, fileEncoding = "Latin1") # fileEncoding = "Latin1",  cfr é in variable names

fiche_next <- read.csv2(fiche_term, header = TRUE, fileEncoding = "Latin1") # fileEncoding = "Latin1",  cfr é in variable names

# bind ficheterrain
fiche <- dplyr::bind_rows(fiche, fiche_next)
rm(fiche_next)

## work on "Fiche terrain"

date_fiche <- as.Date(stringr::str_sub(fiche$date.sortie, end = 10), origin = "1970-01-01")
fiche <- tibble::add_column(fiche, date_fiche, .after = "date.sortie")
rm(date_fiche)

## qecb vs fiche terrain

#sort(unique(qecb[, c("id")]))
#sort(unique(fiche[, c("ID.Fiche")]))
fiche_red <- dplyr::filter(fiche, fiche$ID.Fiche %in% unique(qecb[, c("id")]))

id_count <- qecb %>% dplyr::group_by(id) %>% dplyr::count()
id_count <- dplyr::rename(id_count, "ID.Fiche" ="id")
id_count <- data.frame(id_count)
fiche_red <- dplyr::left_join(fiche_red, id_count)

# rep fiche terrain information
fiche_expanded <- fiche_red[rep(row.names(fiche_red), fiche_red$n), 1:ncol(fiche_red)]
fiche_expanded <- dplyr::rename(fiche_expanded, "id" = "ID.Fiche")

## merge qecb data and ficheterrain information

qecb <- dplyr::bind_cols(qecb, fiche_expanded)
qecb <- dplyr::rename(qecb, "id_qecb" = "id...1")
qecb <- dplyr::rename(qecb, "id_fiche" = "id...68")

rm(fiche_expanded, fiche_red, id_count)

qecb <- qecb %>% tidyr::separate(date_fiche, c("Year", "Month", "Day"), sep = "-", remove = FALSE)


## Quadrat nb : in contrast to ivr df, quadrat number is missing for many observations in qecb df ; but from the Numero.Photo variable (boulder photo associated to field data collection), I could get back most missing quadrat numbers 

Quadrat <- stringr::str_extract(qecb$Numero.Photo, "Q[12345]")
qecb <- tibble::add_column(qecb, Quadrat, .after = "Numero.Photo")
rm(Quadrat)

# check
Quadrat_bis <- rep(NA, length = nrow(qecb))
qecb <- tibble::add_column(qecb, Quadrat_bis, .after = "Quadrat")
rm(Quadrat_bis)

qecb$Quadrat_bis <- ifelse(qecb$Numéro.Bloc.échantillon %in% c(1,2) && qecb$Type.Bloc == "Bloc mobile", "Q1", qecb$Quadrat_bis)
qecb$Quadrat_bis <- ifelse(qecb$Numéro.Bloc.échantillon %in% c(3,4) && qecb$Type.Bloc == "Bloc mobile", "Q2", qecb$Quadrat_bis)
qecb$Quadrat_bis <- ifelse(qecb$Numéro.Bloc.échantillon %in% c(5,6) && qecb$Type.Bloc == "Bloc mobile", "Q3", qecb$Quadrat_bis)
qecb$Quadrat_bis <- ifelse(qecb$Numéro.Bloc.échantillon %in% c(7,8) && qecb$Type.Bloc == "Bloc mobile", "Q4", qecb$Quadrat_bis)
qecb$Quadrat_bis <- ifelse(qecb$Numéro.Bloc.échantillon %in% c(9,10) && qecb$Type.Bloc == "Bloc mobile", "Q5", qecb$Quadrat_bis)


qecb$Quadrat_bis <- ifelse(qecb$Numéro.Bloc.échantillon == 1 && qecb$Type.Bloc %in% c("Bloc fixé", "Roche en place"), "Q1", qecb$Quadrat_bis)
qecb$Quadrat_bis <- ifelse(qecb$Numéro.Bloc.échantillon == 2 && qecb$Type.Bloc %in% c("Bloc fixé", "Roche en place"), "Q2", qecb$Quadrat_bis)
qecb$Quadrat_bis <- ifelse(qecb$Numéro.Bloc.échantillon == 3 && qecb$Type.Bloc %in% c("Bloc fixé", "Roche en place"), "Q3", qecb$Quadrat_bis)
qecb$Quadrat_bis <- ifelse(qecb$Numéro.Bloc.échantillon == 4 && qecb$Type.Bloc %in% c("Bloc fixé", "Roche en place"), "Q4", qecb$Quadrat_bis)
qecb$Quadrat_bis <- ifelse(qecb$Numéro.Bloc.échantillon == 5 && qecb$Type.Bloc %in% c("Bloc fixé", "Roche en place"), "Q5", qecb$Quadrat_bis)

## I create two new variables for Site names, one for data analysis and one for data reporting. Only works for actual ivr df with 22 sites !

# Name for data analysis

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
# Anne Boulet forgot to specify zone.habitat in 2020. I asked her to correct it in ESTAMP
qecb$Site <- ifelse(qecb$zone.habitat == unique(qecb$zone.habitat)[23], "GDMO_Locmariaquer", qecb$Site)
unique(qecb$Site)
unique(qecb[, c("Site", "zone.habitat")])

# Name for report/plot

qecb <- tibble::add_column(qecb, Site_bis = NA, .after = "Site")

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


## change some variables to factor

# change 'X..' variables that are indeed % to numeric; https://stackoverflow.com/questions/59410939/apply-function-to-all-variables-with-string-in-name
ix <- grep("^X..", names(qecb))
qecb[ix] <- lapply(qecb[ix], as.numeric)
rm(ix)


## save the final, complete qecb df.

qecb <- qecb[, c(72:107, 1:71)]

saveRDS(qecb, "qecb.RDS")

## qecb df preparation prior qecb calculation

# Several issues to solve in the df first

qecb$Type.Bloc <- factor(qecb$Type.Bloc, levels = c("Bloc mobile", "Bloc fixé", "Roche en place"))
#unique(qecb$Face)
qecb$Face <- factor(qecb$Face, levels = c("face supérieure", "face inférieure"))
#unique(qecb$Numéro.Bloc.échantillon)

qecb <- dplyr::arrange(qecb, Type.Bloc, Face, Numéro.Bloc.échantillon)

qecb <- tibble::add_column(qecb, Site.Year.Month.Day = paste0(qecb$Site, ".", qecb$Year, ".", qecb$Month, ".", qecb$Day), .after = "Site_bis")

# save qecb as a new df. for analysis purpose => several changes to operate to run the code and functions

qecbnew <- qecb

# df with list object nb and corresponding Site.Year.Month.Day value to solve for loop issues

df_list_loop <- data.frame("Site.Year.Month.Day" = unique(qecbnew$Site.Year.Month.Day),
     "loop nb" = c(1:length(unique(qecbnew$Site.Year.Month.Day))))

# dplyr::filter for df that makes problem, then eventually correct in the dataframe for wrong coding; brackets (xx) for nb because will change when qecb df. enlarged.
# these listed boulder field survey error when highlighted when running the loop, that ran into an error ; it was a step by step procedure with solving one listed observation after another when issues appeared. Surely not the best way to proceed, maybe better just to skip these surveys (site + date), but in the present case I wanted to keep most of the observations, therefore I corrected them manually whenever needed.

# list nb (28) - EGMP_BreeBains.2016.04.06
qecbnew$Face <- as.character(qecbnew$Face)
qecbnew$Face <- ifelse(qecbnew$ID.Fiche == "BDD_IVR&QECB_La Bree_20160406_VImport.xlsx" && qecbnew$Référence.bloc == "avr16-LaBreeB9sup", "face supérieure", qecbnew$Face)
qecbnew$Face <- ifelse(qecbnew$ID.Fiche == "BDD_IVR&QECB_La Bree_20160406_VImport.xlsx" && qecbnew$Référence.bloc == "avr16-LaBreeB10sup", "face supérieure", qecbnew$Face)
qecbnew$Face <- as.factor(qecbnew$Face)
unique(qecbnew$Face)

# list nb 33 - EGMP_PerreAntiochat.2016.04.07
qecbnew$Face <- as.character(qecbnew$Face)
qecbnew$Face <- ifelse(qecbnew$ID.Fiche == "BDD_IVR&QECB_PerAnt_20160407_VImport.xlsx" && qecbnew$Référence.bloc == "avr16-PerAntB9sup", "face supérieure", qecbnew$Face)
qecbnew$Face <- ifelse(qecbnew$ID.Fiche == "BDD_IVR&QECB_PerAnt_20160407_VImport.xlsx" && qecbnew$Référence.bloc == "avr16-PerAntB10sup", "face supérieure", qecbnew$Face)
qecbnew$Face <- as.factor(qecbnew$Face)
unique(qecbnew$Face)

# list nb 37 - EGMP_Chassiron.2016.03.09
qecbnew$Face <- as.character(qecbnew$Face)
qecbnew$Face <- ifelse(qecbnew$ID.Fiche == "BDD_IVR&QECB_Chassiron_20160309&&10_VImport.xlsx" && qecbnew$Référence.bloc == "mars16-ChassB9sup", "face supérieure", qecbnew$Face)
qecbnew$Face <- ifelse(qecbnew$ID.Fiche == "BDD_IVR&QECB_Chassiron_20160309&&10_VImport.xlsx" && qecbnew$Référence.bloc == "mars16-ChasB10sup", "face supérieure", qecbnew$Face)
qecbnew$Face <- as.factor(qecbnew$Face)
unique(qecbnew$Face)

# list nb 76 - ARMO_Verdelet.2015.03.23
qecbnew$Face <- as.character(qecbnew$Face)
qecbnew$Face <- ifelse(qecbnew$ID.Fiche == "BDD_IVR&QECB_Verdelet_20150323_VImport.xlsx" && qecbnew$Référence.bloc == "mar15-VerB10inf", "face inférieure", qecbnew$Face)
qecbnew$Face <- as.factor(qecbnew$Face)
unique(qecbnew$Face)

# list nb 116 - "GDMO_Locmariaquer.2018.09.10"
qecbnew$Type.Bloc <- as.character(qecbnew$Type.Bloc)
qecbnew$Type.Bloc <- ifelse(qecbnew$ID.Fiche == "2018-09-10-GDMO-CDB-001" && qecbnew$Numero.Photo == "2018-09-10_GDMO_01_CDB-5_sup_392578.jpg", "Roche en place", qecbnew$Type.Bloc)
qecbnew$Type.Bloc <- as.factor(qecbnew$Type.Bloc)
qecbnew$Quadrat_bis <- ifelse(qecbnew$ID.Fiche == "2018-09-10-GDMO-CDB-001" && qecbnew$Numero.Photo == "2018-09-10_GDMO_01_CDB-5_sup_392578.jpg", "Q5", qecbnew$Quadrat_bis)
qecbnew %>% dplyr::filter(!(ID.Fiche == "2018-09-10-GDMO-CDB-001" && Numero.Photo == "")) -> qecbnew

# Few sites to remove prior running the for loop because it was not just a encoding mistake for one data, but a globally wroing coding for the site + date survey.

qecb.i <- qecbnew  %>% dplyr::filter(Site.Year.Month.Day == "FINS_StNicolasGlenan.2016.04.08") # no bloc fixe !
qecbnew <- qecbnew  %>% dplyr::filter(Site.Year.Month.Day != "FINS_StNicolasGlenan.2016.04.08")
qecb.i <- qecbnew  %>% dplyr::filter(Site.Year.Month.Day == "GDMO_Locmariaquer.2019.09.30")# most faces of blocs mobiles do not correspond to each other; only 3 over 10 boulder have data for both face supérieure and face inférieure
qecbnew <- qecbnew  %>% dplyr::filter(Site.Year.Month.Day != "GDMO_Locmariaquer.2019.09.30")

rm(df_list_loop, qecb.i)


# check for species with count within sub-0.1m^2-quadrat (i.e. reduced size quadrat compare to most organisms on boulder to count them, because abundant ; then some extrapolation)

# first for Spirobranchus 

qecbnew$Nb.Spirobranchus.lamarckii.total.ini <- qecbnew$Nb.Spirobranchus.lamarckii.total
qecbnew$Nb.Spirobranchus.lamarckii.total <- as.character(qecbnew$Nb.Spirobranchus.lamarckii.total)

table(qecbnew$Nb.Spirobranchus.lamarckii.total)
subset(qecbnew, is.na(qecbnew$Nb.Spirobranchus.lamarckii.total))
nrow(subset(qecbnew, is.na(qecbnew$Nb.Spirobranchus.lamarckii.total)))
subset(qecbnew, is.nan(qecbnew$Nb.Spirobranchus.lamarckii.total))
subset(qecbnew, is.finite(qecbnew$Nb.Spirobranchus.lamarckii.total))
nrow(dplyr::filter(qecbnew, qecbnew$Nb.Spirobranchus.lamarckii.total %in% c(NA, "NaN", "Inf", "-Inf")))

qecbnew.Spirobranchus <- (dplyr::filter(qecbnew, Nb.Spirobranchus.lamarckii.total %in% c(NA, "NaN", "Inf", "-Inf")))
qecbnew.Spirobranchus[, c("Nb.Spirobranchus.lamarckii.1B", "Nb.Spirobranchus.lamarckii.2B", "Nb.Spirobranchus.lamarckii.3B", "Nb.Spirobranchus.lamarckii.4B", "Nb.Spirobranchus.lamarckii.5B")] <- sapply(qecbnew.Spirobranchus[, c("Nb.Spirobranchus.lamarckii.1B", "Nb.Spirobranchus.lamarckii.2B", "Nb.Spirobranchus.lamarckii.3B", "Nb.Spirobranchus.lamarckii.4B", "Nb.Spirobranchus.lamarckii.5B")],as.character)
(Spirobranchus.data <- subset(qecbnew.Spirobranchus, !is.na(qecbnew.Spirobranchus$Nb.Spirobranchus.lamarckii.1B) | !is.na(qecbnew.Spirobranchus$Nb.Spirobranchus.lamarckii.2B) | !is.na(qecbnew.Spirobranchus$Nb.Spirobranchus.lamarckii.3B) | !is.na(qecbnew.Spirobranchus$Nb.Spirobranchus.lamarckii.4B) | !is.na(qecbnew.Spirobranchus$Nb.Spirobranchus.lamarckii.5B))[, c("Site.Year.Month.Day", "Nb.Spirobranchus.lamarckii.1B", "Nb.Spirobranchus.lamarckii.2B", "Nb.Spirobranchus.lamarckii.3B", "Nb.Spirobranchus.lamarckii.4B", "Nb.Spirobranchus.lamarckii.5B", "Nb.Spirobranchus.lamarckii.total")])
unique(Spirobranchus.data$Site.Year.Month.Day)

Quemenes <- dplyr::filter(qecbnew, Site == "FINS_Quemenes")
Quemenes <- dplyr::arrange(Quemenes, date_fiche)
# for Quemenes, issue because for sampling date "FINS_Quemenes.2015.09.30" the 5 counts of Spirobranchus were encoded in 1B instead of total !!! I noticed this issue when mining data (see below), therefore I corrected before running below script for Spirobranchus. 
qecbnew$Nb.Spirobranchus.lamarckii.total <- ifelse(qecbnew$Site.Year.Month.Day == "FINS_Quemenes.2015.09.30" && is.na(qecbnew$Nb.Spirobranchus.lamarckii.total), qecbnew$Nb.Spirobranchus.lamarckii.1B, qecbnew$Nb.Spirobranchus.lamarckii.total)
(Quemenes <- dplyr::filter(qecbnew, Site.Year.Month.Day == "FINS_Quemenes.2015.09.30")[, c("Site.Year.Month.Day", "Nb.Spirobranchus.lamarckii.1B", "Nb.Spirobranchus.lamarckii.2B", "Nb.Spirobranchus.lamarckii.3B", "Nb.Spirobranchus.lamarckii.4B", "Nb.Spirobranchus.lamarckii.5B", "Nb.Spirobranchus.lamarckii.total")])
rm(Quemenes)

SeinKilaourou <- dplyr::filter(qecbnew, Site == "FINS_SeinKilaourou")
SeinKilaourou <- dplyr::arrange(SeinKilaourou, date_fiche)
# same issue with SeinKilaourou
qecbnew$Nb.Spirobranchus.lamarckii.total <- ifelse(qecbnew$Site.Year.Month.Day == "FINS_SeinKilaourou.2015.04.21" && is.na(qecbnew$Nb.Spirobranchus.lamarckii.total), qecbnew$Nb.Spirobranchus.lamarckii.1B, qecbnew$Nb.Spirobranchus.lamarckii.total)
(SeinKilaourou <- dplyr::filter(qecbnew, Site.Year.Month.Day == "FINS_SeinKilaourou.2015.04.21")[, c("Site.Year.Month.Day", "Nb.Spirobranchus.lamarckii.1B", "Nb.Spirobranchus.lamarckii.2B", "Nb.Spirobranchus.lamarckii.3B", "Nb.Spirobranchus.lamarckii.4B", "Nb.Spirobranchus.lamarckii.5B", "Nb.Spirobranchus.lamarckii.total")])
rm(SeinKilaourou)

# some more issues however with "x100"count data

Spirobranchus <- subset(qecbnew, !is.na(qecbnew$Nb.Spirobranchus.lamarckii.1B) && !is.na(qecbnew$Nb.Spirobranchus.lamarckii.2B) && !is.na(qecbnew$Nb.Spirobranchus.lamarckii.3B) && !is.na(qecbnew$Nb.Spirobranchus.lamarckii.4B) && !is.na(qecbnew$Nb.Spirobranchus.lamarckii.5B) && !is.na(qecbnew$Nb.Spirobranchus.lamarckii.total))[, c("Site.Year.Month.Day", "Nb.Spirobranchus.lamarckii.1B", "Nb.Spirobranchus.lamarckii.2B", "Nb.Spirobranchus.lamarckii.3B", "Nb.Spirobranchus.lamarckii.4B", "Nb.Spirobranchus.lamarckii.5B", "Nb.Spirobranchus.lamarckii.total")]
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
Spirobranchus <- dplyr::arrange(dplyr::filter(Spirobranchus, diff != 0 && mean.x.100 != 0), desc(diff))

# check it all in the qecbnew df

for (i in c(1:nrow(qecbnew))) { 
  qecbnew$mean.x.100[[i]] <- 
    #ifelse(qecbnew$Nb.Spirobranchus.lamarckii.total[[i]] %in% c(NA, "NaN", "Inf", "-Inf"), 
    sum(qecbnew[i, c("Nb.Spirobranchus.lamarckii.1B", "Nb.Spirobranchus.lamarckii.2B", "Nb.Spirobranchus.lamarckii.3B", "Nb.Spirobranchus.lamarckii.4B", "Nb.Spirobranchus.lamarckii.5B")], na.rm = TRUE )/sum(!is.na(qecbnew[i, c("Nb.Spirobranchus.lamarckii.1B", "Nb.Spirobranchus.lamarckii.2B", "Nb.Spirobranchus.lamarckii.3B", "Nb.Spirobranchus.lamarckii.4B", "Nb.Spirobranchus.lamarckii.5B")]))*100
  #, qecbnew$Nb.Spirobranchus.lamarckii.total[[i]]) 
} # sum of only NAs/0 = NaN; so replace NaN by Na
qecbnew$mean.x.100 <- as.character(qecbnew$mean.x.100)

sort(dplyr::filter(qecbnew, paste0(qecbnew$id_qecb, "_", qecbnew$Site.Year.Month.Day, "_", qecbnew$Type.Bloc, "_", qecbnew$Numéro.Bloc.échantillon, "_", qecbnew$Face) %in% paste0(qecbnew.Spirobranchus$id_qecb, "_", qecbnew.Spirobranchus$Site.Year.Month.Day, "_", qecbnew.Spirobranchus$Type.Bloc, "_", qecbnew.Spirobranchus$Numéro.Bloc.échantillon, "_", qecbnew.Spirobranchus$Face))[, "mean.x.100"])

for (i in c(1:nrow(qecbnew))) {
  qecbnew$mean.x.100[[i]] <- ifelse(qecbnew$mean.x.100[[i]] == "NaN", NA, qecbnew$mean.x.100[[i]])
}
nrow(subset(qecbnew, is.na(qecbnew$mean.x.100)))
qecbnew$mean.x.100 <- as.integer(qecbnew$mean.x.100)

qecbnew$Nb.Spirobranchus.lamarckii.total <- as.integer(qecbnew$Nb.Spirobranchus.lamarckii.total)
unique(qecbnew$Nb.Spirobranchus.lamarckii.total - qecbnew$mean.x.100) 
table(qecbnew$Nb.Spirobranchus.lamarckii.total - qecbnew$mean.x.100)
qecbnew$Nb.Spirobranchus.lamarckii.total.diff <- abs((qecbnew$Nb.Spirobranchus.lamarckii.total - qecbnew$mean.x.100))
Spirobranchus.diff <- qecbnew[, c("Nb.Spirobranchus.lamarckii.1B", "Nb.Spirobranchus.lamarckii.2B", "Nb.Spirobranchus.lamarckii.3B", "Nb.Spirobranchus.lamarckii.4B", "Nb.Spirobranchus.lamarckii.5B", "Nb.Spirobranchus.lamarckii.total", "Nb.Spirobranchus.lamarckii.total.ini", "mean.x.100", "Nb.Spirobranchus.lamarckii.total.diff")]
Spirobranchus.diff <- dplyr::arrange(Spirobranchus.diff, desc(Nb.Spirobranchus.lamarckii.total.diff), mean.x.100)
Spirobranchus.diff <- dplyr::arrange(dplyr::filter(Spirobranchus.diff, Nb.Spirobranchus.lamarckii.total.diff != 0 && mean.x.100 != 0), desc(Nb.Spirobranchus.lamarckii.total.diff))

qecbnew$Nb.Spirobranchus.lamarckii.total <- ifelse(qecbnew$Nb.Spirobranchus.lamarckii.total.diff != 0 && qecbnew$mean.x.100 != 0, qecbnew$mean.x.100, qecbnew$Nb.Spirobranchus.lamarckii.total)
Spirobranchus.diff <- qecbnew[, c("Nb.Spirobranchus.lamarckii.1B", "Nb.Spirobranchus.lamarckii.2B", "Nb.Spirobranchus.lamarckii.3B", "Nb.Spirobranchus.lamarckii.4B", "Nb.Spirobranchus.lamarckii.5B", "Nb.Spirobranchus.lamarckii.total", "Nb.Spirobranchus.lamarckii.total.ini", "mean.x.100", "Nb.Spirobranchus.lamarckii.total.diff")]
Spirobranchus.diff$Nb.Spirobranchus.lamarckii.total.diff <- abs(as.integer(Spirobranchus.diff$Nb.Spirobranchus.lamarckii.total.diff))
Spirobranchus.diff <- dplyr::arrange(Spirobranchus.diff, desc(Nb.Spirobranchus.lamarckii.total.diff), mean.x.100)
Spirobranchus.diff <- dplyr::arrange(dplyr::filter(Spirobranchus.diff, Nb.Spirobranchus.lamarckii.total.diff != 0 && mean.x.100 != 0), desc(Nb.Spirobranchus.lamarckii.total.diff))
# ok, change made when data x 100 was not correct.

# finally, change NA by mean.x100 for Spirobranchus total
qecbnew$Nb.Spirobranchus.lamarckii.total <- as.character(qecbnew$Nb.Spirobranchus.lamarckii.total)
for (i in c(1:nrow(qecbnew))) { 
  qecbnew$Nb.Spirobranchus.lamarckii.total[[i]] <- ifelse(qecbnew$Nb.Spirobranchus.lamarckii.total[[i]] %in% c(NA, "NaN", "Inf", "-Inf"), sum(qecbnew[i, c("Nb.Spirobranchus.lamarckii.1B", "Nb.Spirobranchus.lamarckii.2B", "Nb.Spirobranchus.lamarckii.3B", "Nb.Spirobranchus.lamarckii.4B", "Nb.Spirobranchus.lamarckii.5B")], na.rm = T )/sum(!is.na(qecbnew[i, c("Nb.Spirobranchus.lamarckii.1B", "Nb.Spirobranchus.lamarckii.2B", "Nb.Spirobranchus.lamarckii.3B", "Nb.Spirobranchus.lamarckii.4B", "Nb.Spirobranchus.lamarckii.5B")]))*100, qecbnew$Nb.Spirobranchus.lamarckii.total[[i]]) 
} # sum of only NAs/0 = NaN; so replace NaN by Na

sort(dplyr::filter(qecbnew, paste0(qecbnew$id_qecb, "_", qecbnew$Site.Year.Month.Day, "_", qecbnew$Type.Bloc, "_", qecbnew$Numéro.Bloc.échantillon, "_", qecbnew$Face) %in% paste0(qecbnew.Spirobranchus$id_qecb, "_", qecbnew.Spirobranchus$Site.Year.Month.Day, "_", qecbnew.Spirobranchus$Type.Bloc, "_", qecbnew.Spirobranchus$Numéro.Bloc.échantillon, "_", qecbnew.Spirobranchus$Face))[,"Nb.Spirobranchus.lamarckii.total"])

for (i in c(1:nrow(qecbnew))) {
  qecbnew$Nb.Spirobranchus.lamarckii.total[[i]] <- ifelse(qecbnew$Nb.Spirobranchus.lamarckii.total[[i]] == "NaN", NA, qecbnew$Nb.Spirobranchus.lamarckii.total[[i]])
}
nrow(subset(qecbnew, is.na(qecbnew$Nb.Spirobranchus.lamarckii.total)))
qecbnew$Nb.Spirobranchus.lamarckii.total <- as.integer(qecbnew$Nb.Spirobranchus.lamarckii.total)

unique(qecbnew$Nb.Spirobranchus.lamarckii.total - qecbnew$Nb.Spirobranchus.lamarckii.total.ini) 
table(qecbnew$Nb.Spirobranchus.lamarckii.total - qecbnew$Nb.Spirobranchus.lamarckii.total.ini)
qecbnew$Nb.Spirobranchus.lamarckii.total.diff <- abs(qecbnew$Nb.Spirobranchus.lamarckii.total - qecbnew$Nb.Spirobranchus.lamarckii.total.ini)
Spirobranchus.diff <- qecbnew[, c("Nb.Spirobranchus.lamarckii.1B", "Nb.Spirobranchus.lamarckii.2B", "Nb.Spirobranchus.lamarckii.3B", "Nb.Spirobranchus.lamarckii.4B", "Nb.Spirobranchus.lamarckii.5B", "Nb.Spirobranchus.lamarckii.total", "Nb.Spirobranchus.lamarckii.total.ini", "mean.x.100", "Nb.Spirobranchus.lamarckii.total.diff")]
Spirobranchus.diff <- dplyr::arrange(Spirobranchus.diff, desc(Nb.Spirobranchus.lamarckii.total.diff), mean.x.100)
Spirobranchus.diff <- dplyr::arrange(dplyr::filter(Spirobranchus.diff, Nb.Spirobranchus.lamarckii.total.diff != 0 && mean.x.100 != 0), desc(Nb.Spirobranchus.lamarckii.total.diff))
table(qecbnew$Nb.Spirobranchus.lamarckii.total.diff)
length(na.omit(qecbnew$Nb.Spirobranchus.lamarckii.total))
sum(is.na(qecbnew$Nb.Spirobranchus.lamarckii.total))
length(na.omit(qecbnew$Nb.Spirobranchus.lamarckii.total))+sum(is.na(qecbnew$Nb.Spirobranchus.lamarckii.total))

qecbnew <- subset(qecbnew, select = -c(Nb.Spirobranchus.lamarckii.total.ini, mean.x.100, Nb.Spirobranchus.lamarckii.total.diff))

rm(qecbnew.Spirobranchus, Spirobranchus, Spirobranchus.data, Spirobranchus.diff)

# do the same for spirorbis

qecbnew$Nb.spirorbis.total.ini <- qecbnew$Nb.spirorbis.total
qecbnew$Nb.spirorbis.total <- as.character(qecbnew$Nb.spirorbis.total)

table(qecbnew$Nb.spirorbis.total)
subset(qecbnew, is.na(qecbnew$Nb.spirorbis.total))
nrow(subset(qecbnew, is.na(qecbnew$Nb.spirorbis.total)))
subset(qecbnew, is.nan(qecbnew$Nb.spirorbis.total))
subset(qecbnew, is.finite(qecbnew$Nb.spirorbis.total))

nrow(dplyr::filter(qecbnew, qecbnew$Nb.spirorbis.total %in% c(NA, "NaN", "Inf", "-Inf")))

qecbnew.spirorbis <- (dplyr::filter(qecbnew, Nb.spirorbis.total %in% c(NA, "NaN", "Inf", "-Inf")))
qecbnew.spirorbis[, c("Nb.spirorbis.1A", "Nb.spirorbis.2A", "Nb.spirorbis.3A", "Nb.spirorbis.4A", "Nb.spirorbis.5A")] <- sapply(qecbnew.spirorbis[, c("Nb.spirorbis.1A", "Nb.spirorbis.2A", "Nb.spirorbis.3A", "Nb.spirorbis.4A", "Nb.spirorbis.5A")],as.character)
(spirorbis.data <- subset(qecbnew.spirorbis, !is.na(qecbnew.spirorbis$Nb.spirorbis.1A) | !is.na(qecbnew.spirorbis$Nb.spirorbis.2A) | !is.na(qecbnew.spirorbis$Nb.spirorbis.3A) | !is.na(qecbnew.spirorbis$Nb.spirorbis.4A) | !is.na(qecbnew.spirorbis$Nb.spirorbis.5A))[, c("Site.Year.Month.Day", "Nb.spirorbis.1A", "Nb.spirorbis.2A", "Nb.spirorbis.3A", "Nb.spirorbis.4A", "Nb.spirorbis.5A", "Nb.spirorbis.total")])
unique(spirorbis.data$Site.Year.Month.Day)

# In contrast to Spirobranchus data, no encoding issues for spirorbis data, cfr when sub-quadrat 1A-5A are ALL encoded, NA for total. 

# some more issues however with "x200"count data

spirorbis <- subset(qecbnew, !is.na(qecbnew$Nb.spirorbis.1A) && !is.na(qecbnew$Nb.spirorbis.2A) && !is.na(qecbnew$Nb.spirorbis.3A) && !is.na(qecbnew$Nb.spirorbis.4A) && !is.na(qecbnew$Nb.spirorbis.5A) && !is.na(qecbnew$Nb.spirorbis.total))[, c("Site.Year.Month.Day", "Nb.spirorbis.1A", "Nb.spirorbis.2A", "Nb.spirorbis.3A", "Nb.spirorbis.4A", "Nb.spirorbis.5A", "Nb.spirorbis.total")]
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
spirorbis <- dplyr::arrange(dplyr::filter(spirorbis, diff != 0 && mean.x.200 != 0), desc(diff))

# check it all in the qecbnew df

for (i in c(1:nrow(qecbnew))) { 
    qecbnew$mean.x.200[[i]] <- 
      #ifelse(qecbnew$Nb.spirorbis.total[[i]] %in% c(NA, "NaN", "Inf", "-Inf"), 
      sum(qecbnew[i, c("Nb.spirorbis.1A", "Nb.spirorbis.2A", "Nb.spirorbis.3A", "Nb.spirorbis.4A", "Nb.spirorbis.5A")], na.rm = T )/sum(!is.na(qecbnew[i, c("Nb.spirorbis.1A", "Nb.spirorbis.2A", "Nb.spirorbis.3A", "Nb.spirorbis.4A", "Nb.spirorbis.5A")]))*200
    #, qecbnew$Nb.spirorbis.total[[i]]) 
} # sum of only NAs/0 = NaN; so replace NaN by Na
qecbnew$mean.x.200 <- as.character(qecbnew$mean.x.200)

sort(dplyr::filter(qecbnew, paste0(qecbnew$id_qecb, "_", qecbnew$Site.Year.Month.Day, "_", qecbnew$Type.Bloc, "_", qecbnew$Numéro.Bloc.échantillon, "_", qecbnew$Face) %in% paste0(qecbnew.spirorbis$id_qecb, "_", qecbnew.spirorbis$Site.Year.Month.Day, "_", qecbnew.spirorbis$Type.Bloc, "_", qecbnew.spirorbis$Numéro.Bloc.échantillon, "_", qecbnew.spirorbis$Face))[, "mean.x.200"])

for (i in c(1:nrow(qecbnew))) {
  qecbnew$mean.x.200[[i]] <- ifelse(qecbnew$mean.x.200[[i]] == "NaN", NA, qecbnew$mean.x.200[[i]])
}
nrow(subset(qecbnew, is.na(qecbnew$mean.x.200)))
qecbnew$mean.x.200 <- as.integer(qecbnew$mean.x.200)

qecbnew$Nb.spirorbis.total <- as.integer(qecbnew$Nb.spirorbis.total)
unique(qecbnew$Nb.spirorbis.total - qecbnew$mean.x.200) 
table(qecbnew$Nb.spirorbis.total - qecbnew$mean.x.200)
qecbnew$Nb.spirorbis.total.diff <- abs((qecbnew$Nb.spirorbis.total - qecbnew$mean.x.200))
spirorbis.diff <- qecbnew[, c("Nb.spirorbis.1A", "Nb.spirorbis.2A", "Nb.spirorbis.3A", "Nb.spirorbis.4A", "Nb.spirorbis.5A", "Nb.spirorbis.total", "Nb.spirorbis.total.ini", "mean.x.200", "Nb.spirorbis.total.diff")]
spirorbis.diff <- dplyr::arrange(spirorbis.diff, desc(Nb.spirorbis.total.diff), mean.x.200)
spirorbis.diff <- dplyr::arrange(dplyr::filter(spirorbis.diff, Nb.spirorbis.total.diff != 0 && mean.x.200 != 0), desc(Nb.spirorbis.total.diff))

qecbnew$Nb.spirorbis.total <- ifelse(qecbnew$Nb.spirorbis.total.diff != 0 && qecbnew$mean.x.200 != 0, qecbnew$mean.x.200, qecbnew$Nb.spirorbis.total)
spirorbis.diff <- qecbnew[, c("Nb.spirorbis.1A", "Nb.spirorbis.2A", "Nb.spirorbis.3A", "Nb.spirorbis.4A", "Nb.spirorbis.5A", "Nb.spirorbis.total", "Nb.spirorbis.total.ini", "mean.x.200", "Nb.spirorbis.total.diff")]
spirorbis.diff$Nb.spirorbis.total.diff <- abs(as.integer(spirorbis.diff$Nb.spirorbis.total.diff))
spirorbis.diff <- dplyr::arrange(spirorbis.diff, desc(Nb.spirorbis.total.diff), mean.x.200)
spirorbis.diff <- dplyr::arrange(dplyr::filter(spirorbis.diff, Nb.spirorbis.total.diff != 0 && mean.x.200 != 0), desc(Nb.spirorbis.total.diff))
# ok, change made when data x 200 was not correct.

# finally, change NA by mean.x200 for spirorbis total
qecbnew$Nb.spirorbis.total <- as.character(qecbnew$Nb.spirorbis.total)
for (i in c(1:nrow(qecbnew))) { 
  qecbnew$Nb.spirorbis.total[[i]] <- ifelse(qecbnew$Nb.spirorbis.total[[i]] %in% c(NA, "NaN", "Inf", "-Inf"), sum(qecbnew[i, c("Nb.spirorbis.1A", "Nb.spirorbis.2A", "Nb.spirorbis.3A", "Nb.spirorbis.4A", "Nb.spirorbis.5A")], na.rm = T )/sum(!is.na(qecbnew[i, c("Nb.spirorbis.1A", "Nb.spirorbis.2A", "Nb.spirorbis.3A", "Nb.spirorbis.4A", "Nb.spirorbis.5A")]))*200, qecbnew$Nb.spirorbis.total[[i]]) 
} # sum of only NAs/0 = NaN; so replace NaN by Na

sort(dplyr::filter(qecbnew, paste0(qecbnew$id_qecb, "_", qecbnew$Site.Year.Month.Day, "_", qecbnew$Type.Bloc, "_", qecbnew$Numéro.Bloc.échantillon, "_", qecbnew$Face) %in% paste0(qecbnew.spirorbis$id_qecb, "_", qecbnew.spirorbis$Site.Year.Month.Day, "_", qecbnew.spirorbis$Type.Bloc, "_", qecbnew.spirorbis$Numéro.Bloc.échantillon, "_", qecbnew.spirorbis$Face))[,"Nb.spirorbis.total"])

for (i in c(1:nrow(qecbnew))) {
  qecbnew$Nb.spirorbis.total[[i]] <- ifelse(qecbnew$Nb.spirorbis.total[[i]] == "NaN", NA, qecbnew$Nb.spirorbis.total[[i]])
}
nrow(subset(qecbnew, is.na(qecbnew$Nb.spirorbis.total)))
qecbnew$Nb.spirorbis.total <- as.integer(qecbnew$Nb.spirorbis.total)

unique(qecbnew$Nb.spirorbis.total - qecbnew$Nb.spirorbis.total.ini) 
table(qecbnew$Nb.spirorbis.total - qecbnew$Nb.spirorbis.total.ini)
qecbnew$Nb.spirorbis.total.diff <- abs(qecbnew$Nb.spirorbis.total - qecbnew$Nb.spirorbis.total.ini)
spirorbis.diff <- qecbnew[, c("Nb.spirorbis.1A", "Nb.spirorbis.2A", "Nb.spirorbis.3A", "Nb.spirorbis.4A", "Nb.spirorbis.5A", "Nb.spirorbis.total", "Nb.spirorbis.total.ini", "mean.x.200", "Nb.spirorbis.total.diff")]
spirorbis.diff <- dplyr::arrange(spirorbis.diff, desc(Nb.spirorbis.total.diff), mean.x.200)
spirorbis.diff <- dplyr::arrange(dplyr::filter(spirorbis.diff, Nb.spirorbis.total.diff != 0 && mean.x.200 != 0), desc(Nb.spirorbis.total.diff))
table(qecbnew$Nb.spirorbis.total.diff)
length(na.omit(qecbnew$Nb.spirorbis.total))
sum(is.na(qecbnew$Nb.spirorbis.total))
length(na.omit(qecbnew$Nb.spirorbis.total))+sum(is.na(qecbnew$Nb.spirorbis.total))
      
qecbnew <- subset(qecbnew, select = -c(Nb.spirorbis.total.ini, mean.x.200, Nb.spirorbis.total.diff))

rm(qecbnew.spirorbis, spirorbis, spirorbis.data, spirorbis.diff, i)


# dplyr::filter for abnormal data, based on histogram distribution of data

qecbnewhist. <- qecbnew
ylab. = "fréquence"

hist. <- qecbnewhist.[, c(
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

dplyr::filter(qecbnew, X..algues.brunes > 100)[, c("Site", "date_fiche", "Type.Bloc", "Numéro.Bloc.échantillon", "Face", "X..algues.brunes")]
qecbnew$X..algues.brunes <- ifelse(qecbnew$X..algues.brunes > 100, 100, qecbnew$X..algues.brunes)
dplyr::filter(qecbnew, X..algues.rouges > 100)[, c("Site", "date_fiche", "Type.Bloc", "Numéro.Bloc.échantillon", "Face", "X..algues.rouges")]
qecbnew$X..algues.rouges <- ifelse(qecbnew$X..algues.rouges > 100, 100, qecbnew$X..algues.rouges)
dplyr::filter(qecbnew, Nb.Phallusia.mamillata > 10)[, c("Site", "date_fiche", "Type.Bloc", "Numéro.Bloc.échantillon", "Face", "Nb.Phallusia.mamillata")]
dplyr::filter(qecbnew, Nb.Tethya.aurantium > 2)[, c("Site", "date_fiche", "Type.Bloc", "Numéro.Bloc.échantillon", "Face", "Nb.Tethya.aurantium")]
dplyr::filter(qecbnew, Nb.spirorbis.total > 15000)[, c("Site", "date_fiche", "Type.Bloc", "Numéro.Bloc.échantillon", "Face", "Nb.spirorbis.total")]
ARMO_IlePlate <- dplyr::filter(qecbnew, Site == "ARMO_IlePlate" && date_fiche == "2015-10-29")
rm(ARMO_IlePlate)
dplyr::filter(qecbnew, Nb.Nucella.lapilus..Pourpre. > 20)[, c("Site", "date_fiche", "Type.Bloc", "Numéro.Bloc.échantillon", "Face", "Nb.Nucella.lapilus..Pourpre.")]

rm(qecbnewhist., ylab., hist.)


## SCRIPT I - NAs <- 0 ; cfr previous comment makes no sense to have NA encoded when the presence of an organism is in reality = 0

# We are facing an issues with NA observations, because either they were not measured/counted, then they are effectively NAs; or these NAs = indeed "0"; but I cannot have NAs for variables that are included in the index determination, cfr if 5+0 = 5, 5+NA = NA; see for example Site.Year.Month.Day == "ARMO_Bilfot.2014.04.28", Nb.Spirobranchus.lamarckii.total is NA ...
# I theregore change these NAs by 0

# replace NAs by "0" for variables used in qecb determination
qecbnew[,c("X..algues.brunes", 
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
          "X..Surface.Accolement")] <- lapply(qecbnew[, 
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

qecbnew$X..Mytilus.sp. <- ifelse((substr(qecbnew$Site, 1, 4) %in% c("EGMP", "BASQ")) && is.na(qecbnew$X..Mytilus.sp.), 0, qecbnew$X..Mytilus.sp.) 
qecbnew$Nb.Crassostrea.gigas <- ifelse((substr(qecbnew$Site, 1, 4) %in% c("EGMP", "BASQ")) && is.na(qecbnew$Nb.Crassostrea.gigas), 0, qecbnew$Nb.Crassostrea.gigas) 
qecbnew$Nb.Ostrea.edulis <- ifelse((substr(qecbnew$Site, 1, 4) %in% c("EGMP", "BASQ")) && is.na(qecbnew$Nb.Ostrea.edulis), 0, qecbnew$Nb.Ostrea.edulis) 


# add a Region variable

library(tibble)
Region <- rep(NA, nrow(qecbnew))
qecbnew <- tibble::add_column(qecbnew, Region, .after = "Site_bis")
qecbnew$Region <- ifelse(qecbnew$Site %in% c("EGMP_GroinCou", "EGMP_PasEmsembert",    "EGMP_BreeBains", "EGMP_PerreAntiochat", "EGMP_Chassiron", "BASQ_FlotsBleusZP", "BASQ_FlotsBleusZF"), "EGMP.BASQ", "Bretagne")
rm(Region)
qecbnew <- dplyr::arrange(qecbnew, Region, Site.Year.Month.Day, Type.Bloc, Numéro.Bloc.échantillon, Face)

# accolement function according to recent 'retournement'

qecbnew %>% dplyr::filter(Type.Bloc == "Bloc mobile" && Face == "face supérieure") -> df.BM.FS
qecbnew %>% dplyr::filter(Type.Bloc == "Bloc mobile" && Face == "face inférieure") -> df.BM.FI

df.BM.FI$terri. <- substr(df.BM.FI$Site, 1, 4)

`%notin%` <- Negate(`%in%`)

acco.fct <- function(var.) {

  col.accol.$var.cor.acco. <<- NA
  
  for (i in c(1:nrow(df.BM.FI))) {
    
    col.accol.$var.cor.acco.[[i]] <<- if (df.BM.FI$terri.[[i]] %notin% c("EGMP", "BASQ")) {
      ifelse(#df.$Couleur.dominante %in% c("Rouge", "Brune", "Brune-Rouge") | 
        df.BM.FS$Couleur.dominante[[i]] %in% c("Blanche", "Verte", "Blanche-Verte", "Colorée"), df.BM.FI[i, var.] / (100 - df.BM.FI$X..Surface.Accolement[[i]]) * 100, df.BM.FI[i, var.])
    } else {
      ifelse(df.BM.FS$Couleur.dominante[[i]] %in% c("Blanche", "Verte", "Blanche-Verte", "Colorée") 
             && df.BM.FI$X..Surface.Accolement[[i]] != 0 # I have to use it in dplyr::filter this time as well for EGMP- BASQ (but not for Bretagne, although could be added, same result); identical/repeated measure for BM.FI and BM.FS
             && df.BM.FS$X..Mytilus.sp.[[i]] == 0, df.BM.FI[i, var.] / (100 - df.BM.FI$X..Surface.Accolement[[i]]) * 100, df.BM.FI[i, var.])
    }
    
  }
 
}

# I would only consider colors in c("Rouge", "Brune", "Brune-Rouge") for BM.FI correction [ and not the series c("Blanche-Brune", "Rouge", "Brune", "Blanche-Rouge", "Brune-Rouge", "Rouge-Verte", "Brune-Verte") ] ; and for BM.FS, the list c("Blanche", "Verte", "Colorée") => we do the correction for BM.FI accollement based on BM.FS color !!!

options(scipen = 0, digits = 2)

col.accol. <- data.frame(Site = df.BM.FI$Site, Site.Year.Month.Day = df.BM.FI$Site.Year.Month.Day, Accol. = df.BM.FI$X..Surface.Accolement, Mytilus.BM.FS = df.BM.FS[, "X..Mytilus.sp."], Col.domi.BM.FS = df.BM.FS[, "Couleur.dominante"], Col.domi.BM.FI = df.BM.FI[, "Couleur.dominante"])

col.accol.$X..Eponges.BM.FI.pre. <- df.BM.FI[, "X..Eponges"]
acco.fct("X..Eponges")
col.accol. <- dplyr::rename(col.accol., X..Eponges.BM.FI.post = var.cor.acco.)
col.accol.[, "X..Eponges.diff."] <- col.accol.[, "X..Eponges.BM.FI.post"]/col.accol.[, "X..Eponges.BM.FI.pre."]*100-100
col.accol.[, "X..Eponges.diff."] <- ifelse(is.nan(col.accol.[, "X..Eponges.diff."]), NA, col.accol.[, "X..Eponges.diff."])

col.accol.$X..Ascidies.Coloniales.BM.FI.pre. <- df.BM.FI[, "X..Ascidies.Coloniales"]
acco.fct("X..Ascidies.Coloniales")
col.accol. <- dplyr::rename(col.accol., X..Ascidies.Coloniales.BM.FI.post = var.cor.acco.)
col.accol.[, "X..Ascidies.Coloniales.diff."] <- col.accol.[, "X..Ascidies.Coloniales.BM.FI.post"]/col.accol.[, "X..Ascidies.Coloniales.BM.FI.pre."]*100-100
col.accol.[, "X..Ascidies.Coloniales.diff."] <- ifelse(is.nan(col.accol.[, "X..Ascidies.Coloniales.diff."]), NA, col.accol.[, "X..Ascidies.Coloniales.diff."])

col.accol.$X..Ascidies.Solitaires.BM.FI.pre. <- df.BM.FI[, "X..Ascidies.Solitaires"]
acco.fct("X..Ascidies.Solitaires")
col.accol. <- dplyr::rename(col.accol., X..Ascidies.Solitaires.BM.FI.post = var.cor.acco.)
col.accol.[, "X..Ascidies.Solitaires.diff."] <- col.accol.[, "X..Ascidies.Solitaires.BM.FI.post"]/col.accol.[, "X..Ascidies.Solitaires.BM.FI.pre."]*100-100
col.accol.[, "X..Ascidies.Solitaires.diff."] <- ifelse(is.nan(col.accol.[, "X..Ascidies.Solitaires.diff."]), NA, col.accol.[, "X..Ascidies.Solitaires.diff."])

col.accol.$X..Bryozoaires.Dresses.BM.FI.pre. <- df.BM.FI[, "X..Bryozoaires.Dresses"]
acco.fct("X..Bryozoaires.Dresses")
col.accol. <- dplyr::rename(col.accol., X..Bryozoaires.Dresses.BM.FI.post = var.cor.acco.)
col.accol.[, "X..Bryozoaires.Dresses.diff."] <- col.accol.[, "X..Bryozoaires.Dresses.BM.FI.post"]/col.accol.[, "X..Bryozoaires.Dresses.BM.FI.pre."]*100-100
col.accol.[, "X..Bryozoaires.Dresses.diff."] <- ifelse(is.nan(col.accol.[, "X..Bryozoaires.Dresses.diff."]), NA, col.accol.[, "X..Bryozoaires.Dresses.diff."])

col.accol.$X..Lithophyllum.BM.FI.pre. <- df.BM.FI[, "X..Lithophyllum"]
acco.fct("X..Lithophyllum")
col.accol. <- dplyr::rename(col.accol., X..Lithophyllum.BM.FI.post = var.cor.acco.)
col.accol.[, "X..Lithophyllum.diff."] <- col.accol.[, "X..Lithophyllum.BM.FI.post"]/col.accol.[, "X..Lithophyllum.BM.FI.pre."]*100-100
col.accol.[, "X..Lithophyllum.diff."] <- ifelse(is.nan(col.accol.[, "X..Lithophyllum.diff."]), NA, col.accol.[, "X..Lithophyllum.diff."])

col.accol.$X..Balanes.Vivantes.BM.FI.pre. <- df.BM.FI[, "X..Balanes.Vivantes"]
acco.fct("X..Balanes.Vivantes")
col.accol. <- dplyr::rename(col.accol., X..Balanes.Vivantes.BM.FI.post = var.cor.acco.)
col.accol.[, "X..Balanes.Vivantes.diff."] <- col.accol.[, "X..Balanes.Vivantes.BM.FI.post"]/col.accol.[, "X..Balanes.Vivantes.BM.FI.pre."]*100-100
col.accol.[, "X..Balanes.Vivantes.diff."] <- ifelse(is.nan(col.accol.[, "X..Balanes.Vivantes.diff."]), NA, col.accol.[, "X..Balanes.Vivantes.diff."])

options(scipen = 0, digits = 7) # default

rm(col.accol., df.BM.FI, df.BM.FS)

# do remove some more data ...
# "FINS_Quemenes.2020.10.16", bad encoding, I let know Anna Capietto to make changes => was corrected normally, so unactivate next time I download ESTAMP data
dplyr::filter(qecbnew, Site.Year.Month.Day != "FINS_Quemenes.2020.10.16") -> qecbnew

# save the final qecbnew df.

saveRDS(qecbnew, "qecbnew.RDS")
#qecbnew <- readRDS("QECB/qecbnew.RDS")


## do calculate QECB values now

# create lists

qecb.val.qu.list <- vector("list", length(unique(qecbnew$Site.Year.Month.Day)))
qecb.val.list <- vector("list", length(unique(qecbnew$Site.Year.Month.Day)))

for (i in c(1:length(unique(qecbnew$Site.Year.Month.Day)))) {
  
  qecbnew  %>% dplyr::filter(Site.Year.Month.Day == unique(qecbnew$Site.Year.Month.Day)[[i]]) -> qecb.i
  
  (terri. <- unique(substr(qecb.i$Site, 1, 4)))
  
  nb. <- as.vector(unlist(intersect(dplyr::filter(qecb.i, Type.Bloc == "Bloc mobile" && Face == "face supérieure")["Numéro.Bloc.échantillon"], dplyr::filter(qecb.i, Type.Bloc == "Bloc mobile" && Face == "face inférieure")["Numéro.Bloc.échantillon"])))
  
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
    
    qecb.i %>% dplyr::filter(qecb.i$Type.Bloc == "Bloc mobile" && qecb.i$Face == "face supérieure") -> df.BM.FS # to keep a version of it for later on correction for accollement for BM FI
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
    
    qecb.i %>% dplyr::filter(Type.Bloc == "Bloc mobile" && Face == "face inférieure") -> df.
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
               && df.BM.FI$X..Surface.Accolement != 0 # I have to use it in dplyr::filter this time as well for EGMP- BASQ (but not for Bretagne, altough could be added, same result); identical/repeated measure for BM.FI and BM.FS
               && df.BM.FS$X..Mytilus.sp. == 0, df.BM.FI[, var.] / (100 - df.BM.FI$X..Surface.Accolement) * 100, df.BM.FI[, var.])
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
    df. <- dplyr::mutate(df., row.nb = dplyr::row_number())
    dplyr::filter(df., Face == "face inférieure")["row.nb"]
    df.[c(dplyr::filter(df., Face == "face inférieure")[1, "row.nb"]:unlist(tail(dplyr::filter(df., Face == "face inférieure"), n=1)["row.nb"])), "X..Balanes.Vivantes"] <- acco.fct("X..Balanes.Vivantes")
    df.[, "X..Balanes.Vivantes"] <- as.numeric(ifelse(as.character(df.[, "X..Balanes.Vivantes"]) %in% c(NA, "NaN", "-Inf", "Inf"), "0", as.character(df.[, "X..Balanes.Vivantes"])))
    df.[, "X..Balanes.Vivantes"] <- ifelse(df.[, "X..Balanes.Vivantes"] > 100, 100, df.[, "X..Balanes.Vivantes"])
    df.[, "X..Balanes.Vivantes"]
    
    for (k in c(1:length(na.omit(num.bloc)))) {
      
      j. <- num.bloc[k]
      
      GIn. <- unname(unlist(
        (dplyr::filter(df., Numéro.Bloc.échantillon == j. && Face == "face supérieure")["Nb.spirorbis.total"]
         + dplyr::filter(df., Numéro.Bloc.échantillon == j. && Face == "face inférieure")["Nb.spirorbis.total"]
        ) / 1000 ))
      
      G.bmSI <<- c(G.bmSI, GIn.)
      
      HIn. <- unname(unlist(  
        (dplyr::filter(df., Numéro.Bloc.échantillon == j. && Face == "face supérieure")["X..Balanes.Vivantes"]
         + dplyr::filter(df., Numéro.Bloc.échantillon == j. && Face == "face inférieure")["X..Balanes.Vivantes"] 
        ) / 100 ))
      
      H.bmSI <<- c(H.bmSI, HIn.)
      
      LIn. <- unname(unlist(    
        (dplyr::filter(df., Numéro.Bloc.échantillon == j. && Face == "face supérieure")["Nb.Spirobranchus.lamarckii.total"]
         + dplyr::filter(df., Numéro.Bloc.échantillon == j. && Face == "face inférieure")["Nb.Spirobranchus.lamarckii.total"]
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
        ( (dplyr::filter(df., Numéro.Bloc.échantillon == j. && Face == "face supérieure")["Nb.spirorbis.total"]
           + dplyr::filter(df., Numéro.Bloc.échantillon == j. && Face == "face inférieure")["Nb.spirorbis.total"]
        ) / 1000 )
        -
          ( 
            ( (dplyr::filter(df., Numéro.Bloc.échantillon == j. && Face == "face supérieure")["X..Balanes.Vivantes"]
               + dplyr::filter(df., Numéro.Bloc.échantillon == j. && Face == "face inférieure")["X..Balanes.Vivantes"]
            ) / 100 )
            + ( (dplyr::filter(df., Numéro.Bloc.échantillon == j. && Face == "face supérieure")["Nb.Spirobranchus.lamarckii.total"]
                 + dplyr::filter(df., Numéro.Bloc.échantillon == j. && Face == "face inférieure")["Nb.Spirobranchus.lamarckii.total"]
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
    
    
    # (G-(H+I)) Bloc fixé && Roche en place
    
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
    
    qecb.i %>% dplyr::filter(Type.Bloc == "Bloc mobile" && Face == "face supérieure") -> df.
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
    Boulder.nb.bmS = sort(unique(dplyr::filter(qecb.i, Type.Bloc == "Bloc mobile" && Face == "face supérieure")["Numéro.Bloc.échantillon"])[,1]),
    Boulder.nb.bmI = sort(unique(dplyr::filter(qecb.i, Type.Bloc == "Bloc mobile" && Face == "face inférieure")["Numéro.Bloc.échantillon"])[,1]),
    Boulder.nb.bf = c(sort(unique(dplyr::filter(qecb.i, Type.Bloc %in% c("Bloc fixé", "Roche en place"))["Numéro.Bloc.échantillon"])[,1]), rep(NA, length(unique(dplyr::filter(qecb.i, Type.Bloc == "Bloc mobile" && Face == "face supérieure")["Numéro.Bloc.échantillon"])[,1]) - length(unique(dplyr::filter(qecb.i, Type.Bloc %in% c("Bloc fixé", "Roche en place"))["Numéro.Bloc.échantillon"])[,1]))),
    Quadrat.bmS = sort(dplyr::filter(qecb.i, Type.Bloc == "Bloc mobile" && Face == "face supérieure")["Quadrat_bis"][,1]),
    Quadrat.bmI = sort(dplyr::filter(qecb.i, Type.Bloc == "Bloc mobile" && Face == "face inférieure")["Quadrat_bis"][,1]),
    Quadrat.bf = c(sort(dplyr::filter(qecb.i, Type.Bloc %in% c("Bloc fixé", "Roche en place"))["Quadrat_bis"][,1]), rep(NA, length(unique(dplyr::filter(qecb.i, Type.Bloc == "Bloc mobile")["Numéro.Bloc.échantillon"])[,1]) - length(dplyr::filter(qecb.i, Type.Bloc %in% c("Bloc fixé", "Roche en place"))["Quadrat_bis"][,1]))),
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
  rm("(G-(H+I))BF", "(G-(H+I))S.BM", "|VrFS.BM.moy/VrFS.BF.moy|", "A.bf", "A.bmI", "A.bmS", "B.bf", "B.bmI", "B.bmS", "Bloc.nb", "C.bf", "C.bmI", "C.bmS", "D.bf", "D.bmI", "D.bmS", "E.bf", "E.bmI", "E.bmS", "F.bf", "F.bmI", "F.bmS", "G.bf", "G.bmS", "G.bmSI", "GIn.", "H.bf", "H.bmS", "H.bmSI", "HIn.", "i", "I.bf", "I.bmS", "I.bmSI", "j.", "k", "L.bmSI", "LIn.", "nb.", "num.bloc", "QEBM.1", "QEBM.2", "QECB", "VFI.BM", "VFS.BM", "VFSI.BM", "VFSIn.", "VrFS.BF", "VrFS.BF.moy", "VrFS.BM", "VrFS.BM.moy", "terri.")     
  
}

qecb.val.qu. <- do.call("rbind", qecb.val.qu.list)
qecb.val.qu. <- dplyr::arrange(qecb.val.qu., Site.Year.Month.Day, Boulder.nb.bmS)
Date <- as.Date(stringr::str_sub(qecb.val.qu.$Site.Year.Month.Day,-10,-1), format = "%Y.%m.%d", origin = "1970-01-01")
qecb.val.qu. <- tibble::add_column(qecb.val.qu., Date, .after = "Site_bis")
rm(Date)

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
Date <- as.Date(stringr::str_sub(qecb.val.$Site.Year.Month.Day,-10,-1), format = "%Y.%m.%d", origin = "1970-01-01")
qecb.val. <- tibble::add_column(qecb.val., Date, .after = "Site_bis")
rm(Date)

rm(list = ls()[!ls() %in% c("fiche", "qecb", "qecbnew", "qecb.val.qu.")])

qecb.val.qu. <- tidyr::separate(qecb.val.qu., Date, c("Annee", "Mois", "Jour"), remove = FALSE)
qecb.val.qu.$Annee <- as.integer(qecb.val.qu.$Annee)
qecb.val.qu.$Mois <- as.integer(qecb.val.qu.$Mois)
qecb.val.qu.$Jour <- as.integer(qecb.val.qu.$Jour)

dplyr::filter(qecb.val.qu., QEBM.2 %in% c("Inf", "NaN"))

qecb.val.qu.NaN <- qecb.val.qu. 
qecb.val.qu.NaN$QEBM.2 <- ifelse(qecb.val.qu.NaN$QEBM.2 %in% c("-Inf", "NaN"), NA, qecb.val.qu.NaN$QEBM.2)

qecb.val.qu.stat. <- qecb.val.qu.NaN %>% dplyr::group_by(id_qecb, Site, Site_bis, Annee, Mois, Jour) %>% dplyr::summarize(qecb.moy = mean(QEBM.2, na.rm = TRUE), qecb.et = sd(QEBM.2, na.rm = TRUE), qecb.med = median(QEBM.2, na.rm = TRUE), qecb.min = min(QEBM.2, na.rm = TRUE), qecb.max = max(QEBM.2, na.rm = TRUE), nb. = dplyr::n(), nb.notNa = sum(!is.na(QEBM.2)))

Date <- as.Date(paste0(qecb.val.qu.stat.$Annee, "-", qecb.val.qu.stat.$Mois, "-", qecb.val.qu.stat.$Jour), origin = "1970-01-01")
qecb.val.qu.stat. <- tibble::add_column(qecb.val.qu.stat., Date, .after = "Site_bis")
rm(Date)

qecb.val.qu.stat. <- as.data.frame(qecb.val.qu.stat.)
indic <- qecb.val.qu.stat.
saveRDS(qecb.val.qu.stat., "qecb.val.qu.stat.RDS")


Survey.list <- vector("list", length(unique(qecb.val.qu.$Site.Year.Month.Day)))

for (i in c(1:length(unique(qecb.val.qu.$Site.Year.Month.Day)))) {
  
  qecb.i <- qecb.val.qu.  %>% dplyr::filter(Site.Year.Month.Day == unique(qecb.val.qu.$Site.Year.Month.Day)[[i]])
  
  Survey.list[[i]] <- data.frame(
    Site.Year.Month.Day = rep(unique(qecb.i$Site.Year.Month.Day), nrow(qecb.i)), 
    Survey.nb = rep(i, nrow(qecb.i))
  )
  
}
  
Survey <- do.call("rbind", Survey.list)
 
#setdiff(qecb.val.qu.$Site.Year.Month.Day, Survey$Site.Year.Month.Day)
qecb.val.qu. <- tibble::add_column(qecb.val.qu., Survey.nb = Survey$Survey.nb, .after = "Site.Year.Month.Day")
indic_full <- qecb.val.qu.
rm(i, Survey.list, Survey, qecb.i) 

Survey.nb <- c(1:nrow(qecb.val.qu.))
qecb.val. <- tibble::add_column(qecb.val.qu., Survey.nb, .after = "Site.Year.Month.Day")

rm(Survey.nb, qecb.val.qu.NaN)  

saveRDS(qecb.val., "qecb.val.RDS")
saveRDS(qecb.val.qu., "qecb.val.qu.RDS")


## Plots qecb

qecb.val.qu.NaN <- qecb.val.qu.

`%notin%` <- Negate(`%in%`)
qecb.val.qu.stat.NaN <- qecb.val.qu.stat.
qecb.val.qu.stat.NaN[, c("qecb.moy", "qecb.et", "qecb.med", "qecb.min", "qecb.max")] <- ifelse(qecb.val.qu.stat.NaN[, c("qecb.moy", "qecb.et", "qecb.med", "qecb.min", "qecb.max")] %in% c("-Inf", "NaN"), NA, qecb.val.qu.stat.NaN[, c("qecb.moy", "qecb.et", "qecb.med", "qecb.min", "qecb.max")])

write.table(qecb.val.qu.stat.NaN, "Valeurs_stat.tabular", row.names = FALSE, quote = FALSE, sep = "\t", dec = ".", fileEncoding = "UTF-8")

par(mfrow = c(1, 1))

for (i in c(1:length(unique(qecb.val.qu.stat.NaN$Site)))) {
  
  dplyr::filter(qecb.val.qu.stat.NaN, Site == unique(qecb.val.qu.stat.NaN$Site)[i]) -> df1
  
  
  xmin. <- as.Date(ifelse(min(df1$Annee) >= 2014, "2014-01-01", paste0(min(df$Annee), "-01-01")), origin = "1970-01-01")
  xmax. <- as.Date(ifelse(max(df1$Annee) <= 2017, "2018-01-01", #paste0(max(qecb.val.eg$Annee)+1, 
                          "2023-01-01")
                   #)
                   , origin = "1970-01-01")

  png(paste0("old_qecb_", unique(qecb.val.qu.stat.NaN$Site), ".png"))
  plot(qecb.val.qu.stat.NaN$Date, qecb.val.qu.stat.NaN$qecb.med, xlim = c(xmin., xmax.), ylim = c(-360, 360), pch = 19, cex = 1, main = unique(df1$Site_bis), xlab = "Année", #ylab = expression(paste("", QEBM^{2},"")), 
       ylab = "QECB", col = "grey")
  points(df1$Date, df1$qecb.med, pch = 19, cex = 1.5)
  arrows(df1$Date, df1$qecb.med, df1$Date, df1$qecb.max, code = 3, angle = 90, length = 0.00)
  arrows(df1$Date, df1$qecb.med, df1$Date, df1$qecb.min, code = 3, angle = 90, length = 0.00)
  
  abline(h = c(-216, -72, 72, 216), lty = "dashed")
  text(xmax., -288, "1")
  text(xmax., -146, "2")
  text(xmax., 0, "3")
  text(xmax., 146, "4")
  text(xmax., 288, "5")
  
}

# New quality scale based on quartiles

dplyr::filter(qecb.val.qu.NaN, QEBM.2 >= -360) -> dt.
dplyr::filter(dt., QEBM.2 <= 360) -> dt. 
dplyr::filter(dt., QEBM.2 >= quantile(dt.$QEBM.2, c(0.05), na.rm = TRUE)) -> dt.bis
dplyr::filter(dt.bis, QEBM.2 <= quantile(dt.bis$QEBM.2, c(0.95), na.rm = TRUE)) -> dt.bis

one <- round(mean(unlist(dplyr::filter(dt.bis, QEBM.2 <= quantile(dt.bis$QEBM.2, 0.25, na.rm = TRUE))["QEBM.2"])), digits = 0)
two <- round(mean(unlist(dplyr::filter(dt.bis, QEBM.2 > quantile(dt.bis$QEBM.2, 0.25, na.rm = TRUE) && QEBM.2 <= quantile(dt.bis$QEBM.2, 0.5, na.rm = TRUE))["QEBM.2"])), digits = 0)
three <- round(mean(unlist(dplyr::filter(dt.bis, QEBM.2 > quantile(dt.bis$QEBM.2, 0.5, na.rm = TRUE) && QEBM.2 <= quantile(dt.bis$QEBM.2, 0.75, na.rm = TRUE))["QEBM.2"])), digits = 0)
four <- round(mean(unlist(dplyr::filter(dt.bis, QEBM.2 > quantile(dt.bis$QEBM.2, 0.75, na.rm = TRUE))["QEBM.2"])), digits = 0)

par(mfrow = c(1, 1))
# I have unactivated the model line drawing because aberant for some sites with bad qecb values
for (i in c(1:length(unique(qecb.val.qu.stat.NaN$Site)))) {
  
  dplyr::filter(qecb.val.qu.stat.NaN, Site == unique(qecb.val.qu.stat.NaN$Site)[i]) -> df1
  
  xmin. <- as.Date(ifelse(min(df1$Annee) >= 2014, "2014-01-01", paste0(min(df$Annee), "-01-01")), origin = "1970-01-01")
  xmax. <- as.Date(ifelse(max(df1$Annee) <= 2017, "2018-01-01", #paste0(max(qecb.val.eg$Annee)+1, 
                          "2022-01-01")
                   #)
                   , origin = "1970-01-01")
  
  ymin. = ifelse(min(df1$qecb.med, na.rm = T) < -70, -360, -70)
  ymax. = ifelse(max(df1$qecb.med, na.rm = T) > 200, 360, 200)
  
  png(paste0("new_qecb_", unique(qecb.val.qu.stat.NaN$Site), ".png"))
  plot(qecb.val.qu.stat.NaN$Date, qecb.val.qu.stat.NaN$qecb.med, xlim = c(xmin., xmax.), ylim = c(ymin., ymax.), pch = 19, main = "", xlab = "", ylab = "", type = 'n', axes = FALSE)
  
  rect(as.Date("2013-01-01", origin = "1970-01-01"), -400, as.Date("2023-01-01", origin = "1970-01-01"), one, col = "red", border = NA)
  rect(as.Date("2013-01-01", origin = "1970-01-01"), one, as.Date("2023-01-01", origin = "1970-01-01"), two, col = "orange", border = NA)
  rect(as.Date("2013-01-01", origin = "1970-01-01"), two, as.Date("2023-01-01", origin = "1970-01-01"), three, col = "yellow", border = NA)
  rect(as.Date("2013-01-01", origin = "1970-01-01"), three, as.Date("2023-01-01", origin = "1970-01-01"), four, col = "olivedrab", border = NA)
  rect(as.Date("2013-01-01", origin = "1970-01-01"), four, as.Date("2023-01-01", origin = "1970-01-01"), 400, col = "blue", border = NA)
  
  par(new = TRUE)
  plot(qecb.val.qu.stat.NaN$Date, qecb.val.qu.stat.NaN$qecb.med, xlim = c(xmin., xmax.), ylim = c(ymin., ymax.), pch = 19, cex = 1, main = unique(df1$Site_bis), xlab = "Année", #ylab = expression(paste("", QEBM^{2},"")), 
       ylab = "QECB", col = "grey")
  points(df1$Date, df1$qecb.med, pch = 19, cex = 1.5)
  arrows(df1$Date, df1$qecb.med, df1$Date, df1$qecb.max, code = 3, angle = 90, length = 0.00)
  arrows(df1$Date, df1$qecb.med, df1$Date, df1$qecb.min, code = 3, angle = 90, length = 0.00)
  
}

rm(df1, dt., dt.bis, four, i, one, three, two, xmax., xmin., ymax., ymin.)

#report <- args[5]
#loop_file <- source(args[6])
