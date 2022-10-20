

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
library(magrittr)
#####Load arguments

args <- commandArgs(trailingOnly = TRUE)

#####Import data

if (length(args) < 1) {
    stop("This tool needs at least 1 argument")
}else {
    input_data <- args[1]
    fiche_val <- args[2]

}

#############################################################
#                                                           #
#               Loading and cleaning data                   #
#                                                           #
#############################################################
# load qecb data

qecb <- read.csv2(input_data, header = TRUE, fileEncoding = "Latin1") # fileEncoding = "Latin1",  cfr é in variable names

# import csv files ficheterrain

fiche <- read.csv2(fiche_val, header = TRUE, fileEncoding = "Latin1") # fileEncoding = "Latin1",  cfr é in variable names

## work on "Fiche terrain"

date_fiche <- as.Date(stringr::str_sub(fiche$date.sortie, end = 10), origin = "1970-01-01")
fiche <- tibble::add_column(fiche, date_fiche, .after = "date.sortie")
rm(date_fiche)

## qecb vs fiche terrain

fiche_red <- dplyr::filter(fiche, fiche$ID.Fiche %in% unique(qecb[, c("id")]))

id_count <- qecb %>% dplyr::group_by(id) %>% dplyr::count()
id_count <- dplyr::rename(id_count, "ID.Fiche" = "id")
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


## quadrat nb : in contrast to ivr df, quadrat number is missing for many observations in qecb df ; but from the Numero.Photo variable (boulder photo associated to field data collection), I could get back most missing quadrat numbers

quadrat <- stringr::str_extract(qecb$Numero.Photo, "Q[12345]")
qecb <- tibble::add_column(qecb, quadrat, .after = "Numero.Photo")
rm(quadrat)

# check
quadrat_bis <- rep(NA, length = nrow(qecb))
qecb <- tibble::add_column(qecb, quadrat_bis, .after = "quadrat")
rm(quadrat_bis)

qecb$quadrat_bis <- ifelse(qecb$Numéro.Bloc.échantillon %in% c(1, 2) & qecb$Type.Bloc == "Bloc mobile", "Q1", qecb$quadrat_bis)
qecb$quadrat_bis <- ifelse(qecb$Numéro.Bloc.échantillon %in% c(3, 4) & qecb$Type.Bloc == "Bloc mobile", "Q2", qecb$quadrat_bis)
qecb$quadrat_bis <- ifelse(qecb$Numéro.Bloc.échantillon %in% c(5, 6) & qecb$Type.Bloc == "Bloc mobile", "Q3", qecb$quadrat_bis)
qecb$quadrat_bis <- ifelse(qecb$Numéro.Bloc.échantillon %in% c(7, 8) & qecb$Type.Bloc == "Bloc mobile", "Q4", qecb$quadrat_bis)
qecb$quadrat_bis <- ifelse(qecb$Numéro.Bloc.échantillon %in% c(9, 10) & qecb$Type.Bloc == "Bloc mobile", "Q5", qecb$quadrat_bis)


qecb$quadrat_bis <- ifelse(qecb$Numéro.Bloc.échantillon == 1 & qecb$Type.Bloc %in% c("Bloc fixé", "Roche en place"), "Q1", qecb$quadrat_bis)
qecb$quadrat_bis <- ifelse(qecb$Numéro.Bloc.échantillon == 2 & qecb$Type.Bloc %in% c("Bloc fixé", "Roche en place"), "Q2", qecb$quadrat_bis)
qecb$quadrat_bis <- ifelse(qecb$Numéro.Bloc.échantillon == 3 & qecb$Type.Bloc %in% c("Bloc fixé", "Roche en place"), "Q3", qecb$quadrat_bis)
qecb$quadrat_bis <- ifelse(qecb$Numéro.Bloc.échantillon == 4 & qecb$Type.Bloc %in% c("Bloc fixé", "Roche en place"), "Q4", qecb$quadrat_bis)
qecb$quadrat_bis <- ifelse(qecb$Numéro.Bloc.échantillon == 5 & qecb$Type.Bloc %in% c("Bloc fixé", "Roche en place"), "Q5", qecb$quadrat_bis)

## I create two new variables for Site names, one for data analysis and one for data reporting. Only works for actual ivr df with 22 sites !

# Name for data analysis

qecb <- tibble::add_column(qecb, Site = qecb$zone.habitat, .after = "ID.Fiche")

for (x in seq_along(qecb$Site)) {
  if (grepl(pattern = "Locmariaquer", qecb$Site[x]) == TRUE) {
    qecb$Site[x] <- "GDMO_Locmariaquer"
 } else if (grepl(pattern = "Beg Lann", qecb$Site[x]) == TRUE) {
    qecb$Site[x] <- "GDMO_BegLann"
 } else if (grepl(pattern = "Plateau du Four", qecb$Site[x]) == TRUE) {
    qecb$Site[x] <- "FOUR_PlateauFour"
 } else if (grepl(pattern = "Grouin", qecb$Site[x]) == TRUE) {
    qecb$Site[x] <- "EGMP_GroinCou"
 } else if (grepl(pattern = "Ensembert", qecb$Site[x]) == TRUE) {
    qecb$Site[x] <- "EGMP_PasEmsembert"
 } else if (grepl(pattern = "Brée-les-Bains", qecb$Site[x]) == TRUE) {
    qecb$Site[x] <- "EGMP_BreeBains"
 } else if (grepl(pattern = "Antiochat", qecb$Site[x]) == TRUE) {
    qecb$Site[x] <- "EGMP_PerreAntiochat"
 } else if (grepl(pattern = "Chassiron", qecb$Site[x]) == TRUE) {
    qecb$Site[x] <- "EGMP_Chassiron"
 } else if (grepl(pattern = "zone p", qecb$Site[x]) == TRUE) {
    qecb$Site[x] <- "BASQ_FlotsBleusZP"
 } else if (grepl(pattern = "zone f", qecb$Site[x]) == TRUE) {
    qecb$Site[x] <- "BASQ_FlotsBleusZF"
 } else if (grepl(pattern = "Saint-Michel", qecb$Site[x]) == TRUE) {
    qecb$Site[x] <- "GONB_IlotStMichel"
 } else if (grepl(pattern = "Quéménès", qecb$Site[x]) == TRUE) {
    qecb$Site[x] <- "FINS_Quemenes"
 } else if (grepl(pattern = "Goulenez", qecb$Site[x]) == TRUE) {
    qecb$Site[x] <- "FINS_SeinGoulenez"
 } else if (grepl(pattern = "Kilaourou", qecb$Site[x]) == TRUE) {
    qecb$Site[x] <- "FINS_SeinKilaourou"
 } else if (grepl(pattern = "Verdelet", qecb$Site[x]) == TRUE) {
    qecb$Site[x] <- "ARMO_Verdelet"
 } else if (grepl(pattern = "Piégu", qecb$Site[x]) == TRUE) {
    qecb$Site[x] <- "ARMO_Piegu"
 } else if (grepl(pattern = "Bilfot", qecb$Site[x]) == TRUE) {
    qecb$Site[x] <- "ARMO_Bilfot"
 } else if (grepl(pattern = "Plate", qecb$Site[x]) == TRUE) {
    qecb$Site[x] <- "ARMO_IlePlate"
 } else if (grepl(pattern = "Perharidy", qecb$Site[x]) == TRUE) {
    qecb$Site[x] <- "PDMO_Perharidy"
 } else if (grepl(pattern = "Keraliou", qecb$Site[x]) == TRUE) {
    qecb$Site[x] <- "BRES_Keraliou"
 } else if (grepl(pattern = "Mousterlin", qecb$Site[x]) == TRUE) {
    qecb$Site[x] <- "FINS_Mousterlin"
 } else if (grepl(pattern = "Nicolas", qecb$Site[x]) == TRUE) {
    qecb$Site[x] <- "FINS_StNicolasGlenan"
 } 
  if (grepl(pattern = "Roz", qecb$site[x]) == TRUE) {
    qecb$Site[x] <- "FINS_AnseRoz"
}
}

# Name for report/plot

qecb <- tibble::add_column(qecb, Site_bis = qecb$Site, .after = "Site")

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
qecb$Site_bis <- ifelse(qecb$Site == "FINS_AnseRoz", "Pointe de l'Anse du Roz", qecb$Site_bis)

## change some variables to factor

# change 'X..' variables that are indeed % to numeric; https://stackoverflow.com/questions/59410939/apply-function-to-all-variables-with-string-in-name
ix <- grep("^X..", names(qecb))
qecb[ix] <- lapply(qecb[ix], as.numeric)
rm(ix)


## save the final, complete qecb df_

qecb <- qecb[, c(72:107, 1:71)]

#saveRDS(qecb, "qecb.RDS")

## qecb df preparation prior qecb calculation

# Several issues to solve in the df first


qecb$Type.Bloc <- factor(qecb$Type.Bloc, levels = c("Bloc mobile", "Bloc fixé", "Roche en place"))

qecb$Face <- factor(qecb$Face, levels = c("face supérieure", "face inférieure"))

qecb <- dplyr::arrange(qecb, Type.Bloc, Face, Numéro.Bloc.échantillon)

qecb <- tibble::add_column(qecb, site_year_month_day = paste0(qecb$Site, ".", qecb$Year, ".", qecb$Month, ".", qecb$Day), .after = "Site_bis")

# save qecb as a new df_ for analysis purpose => several changes to operate to run the code and functions

qecbnew <- qecb

# df with list object nb and corresponding site_year_month_day value to solve for loop issues

df_list_loop <- data.frame("site_year_month_day" = unique(qecbnew$site_year_month_day),
     "loop nb" = c(1:length(unique(qecbnew$site_year_month_day))))

# dplyr::filter for df that makes problem, then eventually correct in the dataframe for wrong coding; brackets (xx) for nb because will change when qecb df_ enlarged.
# these listed boulder field survey error when highlighted when running the loop, that ran into an error ; it was a step by step procedure with solving one listed observation after another when issues appeared. Surely not the best way to proceed, maybe better just to skip these surveys (site + date), but in the present case I wanted to keep most of the observations, therefore I corrected them manually whenever needed.

# list nb (28) - EGMP_BreeBains.2016.04.06
qecbnew$Face <- as.character(qecbnew$Face)
qecbnew$Face <- ifelse(qecbnew$ID.Fiche == "BDD_IVR&QECB_La Bree_20160406_VImport.xlsx" & qecbnew$Référence.bloc == "avr16-LaBreeB9sup", "face supérieure", qecbnew$Face)
qecbnew$Face <- ifelse(qecbnew$ID.Fiche == "BDD_IVR&QECB_La Bree_20160406_VImport.xlsx" & qecbnew$Référence.bloc == "avr16-LaBreeB10sup", "face supérieure", qecbnew$Face)
qecbnew$Face <- as.factor(qecbnew$Face)

# list nb 33 - EGMP_PerreAntiochat.2016.04.07
qecbnew$Face <- as.character(qecbnew$Face)
qecbnew$Face <- ifelse(qecbnew$ID.Fiche == "BDD_IVR&QECB_PerAnt_20160407_VImport.xlsx" & qecbnew$Référence.bloc == "avr16-PerAntB9sup", "face supérieure", qecbnew$Face)
qecbnew$Face <- ifelse(qecbnew$ID.Fiche == "BDD_IVR&QECB_PerAnt_20160407_VImport.xlsx" & qecbnew$Référence.bloc == "avr16-PerAntB10sup", "face supérieure", qecbnew$Face)
qecbnew$Face <- as.factor(qecbnew$Face)

# list nb 37 - EGMP_Chassiron.2016.03.09
qecbnew$Face <- as.character(qecbnew$Face)
qecbnew$Face <- ifelse(qecbnew$ID.Fiche == "BDD_IVR&QECB_Chassiron_20160309&10_VImport.xlsx" & qecbnew$Référence.bloc == "mars16-ChassB9sup", "face supérieure", qecbnew$Face)
qecbnew$Face <- ifelse(qecbnew$ID.Fiche == "BDD_IVR&QECB_Chassiron_20160309&10_VImport.xlsx" & qecbnew$Référence.bloc == "mars16-ChasB10sup", "face supérieure", qecbnew$Face)
qecbnew$Face <- as.factor(qecbnew$Face)

# list nb 76 - ARMO_Verdelet.2015.03.23
qecbnew$Face <- as.character(qecbnew$Face)
qecbnew$Face <- ifelse(qecbnew$ID.Fiche == "BDD_IVR&QECB_Verdelet_20150323_VImport.xlsx" & qecbnew$Référence.bloc == "mar15-VerB10inf", "face inférieure", qecbnew$Face)
qecbnew$Face <- as.factor(qecbnew$Face)

# list nb 116 - "GDMO_Locmariaquer.2018.09.10"
qecbnew$Type.Bloc <- as.character(qecbnew$Type.Bloc)
qecbnew$Type.Bloc <- ifelse(qecbnew$ID.Fiche == "2018-09-10-GDMO-CDB-001" & qecbnew$Numero.Photo == "2018-09-10_GDMO_01_CDB-5_sup_392578.jpg", "Roche en place", qecbnew$Type.Bloc)
qecbnew$Type.Bloc <- as.factor(qecbnew$Type.Bloc)
qecbnew$quadrat_bis <- ifelse(qecbnew$ID.Fiche == "2018-09-10-GDMO-CDB-001" & qecbnew$Numero.Photo == "2018-09-10_GDMO_01_CDB-5_sup_392578.jpg", "Q5", qecbnew$quadrat_bis)
qecbnew <- qecbnew %>% dplyr::filter(!(ID.Fiche == "2018-09-10-GDMO-CDB-001" & Numero.Photo == ""))

# Few sites to remove prior running the for loop because it was not just a encoding mistake for one data, but a globally wroing coding for the site + date survey.

qecb_i <- qecbnew  %>% dplyr::filter(site_year_month_day == "FINS_StNicolasGlenan.2016.04.08") # no bloc fixe !
qecbnew <- qecbnew  %>% dplyr::filter(site_year_month_day != "FINS_StNicolasGlenan.2016.04.08")
qecb_i <- qecbnew  %>% dplyr::filter(site_year_month_day == "GDMO_Locmariaquer.2019.09.30") # most faces of blocs mobiles do not correspond to each other; only 3 over 10 boulder have data for both face supérieure and face inférieure
qecbnew <- qecbnew  %>% dplyr::filter(site_year_month_day != "GDMO_Locmariaquer.2019.09.30")
rm(df_list_loop, qecb_i)


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

qecbnew_spirobranchus <- (dplyr::filter(qecbnew, Nb.Spirobranchus.lamarckii.total %in% c(NA, "NaN", "Inf", "-Inf")))
qecbnew_spirobranchus[, c("Nb.Spirobranchus.lamarckii.1B", "Nb.Spirobranchus.lamarckii.2B", "Nb.Spirobranchus.lamarckii.3B", "Nb.Spirobranchus.lamarckii.4B", "Nb.Spirobranchus.lamarckii.5B")] <- sapply(qecbnew_spirobranchus[, c("Nb.Spirobranchus.lamarckii.1B", "Nb.Spirobranchus.lamarckii.2B", "Nb.Spirobranchus.lamarckii.3B", "Nb.Spirobranchus.lamarckii.4B", "Nb.Spirobranchus.lamarckii.5B")], as.character)
(spirobranchus_data <- subset(qecbnew_spirobranchus, !is.na(qecbnew_spirobranchus$Nb.Spirobranchus.lamarckii.1B) || !is.na(qecbnew_spirobranchus$Nb.Spirobranchus.lamarckii.2B) || !is.na(qecbnew_spirobranchus$Nb.Spirobranchus.lamarckii.3B) || !is.na(qecbnew_spirobranchus$Nb.Spirobranchus.lamarckii.4B) || !is.na(qecbnew_spirobranchus$Nb.Spirobranchus.lamarckii.5B))[, c("site_year_month_day", "Nb.Spirobranchus.lamarckii.1B", "Nb.Spirobranchus.lamarckii.2B", "Nb.Spirobranchus.lamarckii.3B", "Nb.Spirobranchus.lamarckii.4B", "Nb.Spirobranchus.lamarckii.5B", "Nb.Spirobranchus.lamarckii.total")])
unique(spirobranchus_data$site_year_month_day)

quemenes <- dplyr::filter(qecbnew, Site == "FINS_Quemenes")
quemenes <- dplyr::arrange(quemenes, date_fiche)
# for Quemenes, issue because for sampling date "FINS_Quemenes.2015.09.30" the 5 counts of Spirobranchus were encoded in 1B instead of total !!! I noticed this issue when mining data (see below), therefore I corrected before running below script for Spirobranchus.
qecbnew$Nb.Spirobranchus.lamarckii.total <- ifelse(qecbnew$site_year_month_day == "FINS_Quemenes.2015.09.30" & is.na(qecbnew$Nb.Spirobranchus.lamarckii.total), qecbnew$Nb.Spirobranchus.lamarckii.1B, qecbnew$Nb.Spirobranchus.lamarckii.total)
(quemenes <- dplyr::filter(qecbnew, site_year_month_day == "FINS_Quemenes.2015.09.30")[, c("site_year_month_day", "Nb.Spirobranchus.lamarckii.1B", "Nb.Spirobranchus.lamarckii.2B", "Nb.Spirobranchus.lamarckii.3B", "Nb.Spirobranchus.lamarckii.4B", "Nb.Spirobranchus.lamarckii.5B", "Nb.Spirobranchus.lamarckii.total")])
rm(quemenes)

seinkilaourou <- dplyr::filter(qecbnew, Site == "FINS_SeinKilaourou")
seinkilaourou <- dplyr::arrange(seinkilaourou, date_fiche)
# same issue with SeinKilaourou
qecbnew$Nb.Spirobranchus.lamarckii.total <- ifelse(qecbnew$site_year_month_day == "FINS_SeinKilaourou.2015.04.21" & is.na(qecbnew$Nb.Spirobranchus.lamarckii.total), qecbnew$Nb.Spirobranchus.lamarckii.1B, qecbnew$Nb.Spirobranchus.lamarckii.total)
(seinkilaourou <- dplyr::filter(qecbnew, site_year_month_day == "FINS_SeinKilaourou.2015.04.21")[, c("site_year_month_day", "Nb.Spirobranchus.lamarckii.1B", "Nb.Spirobranchus.lamarckii.2B", "Nb.Spirobranchus.lamarckii.3B", "Nb.Spirobranchus.lamarckii.4B", "Nb.Spirobranchus.lamarckii.5B", "Nb.Spirobranchus.lamarckii.total")])
rm(seinkilaourou)

# some more issues however with "x100"count data

Spirobranchus <- subset(qecbnew, !is.na(qecbnew$Nb.Spirobranchus.lamarckii.1B) & !is.na(qecbnew$Nb.Spirobranchus.lamarckii.2B) & !is.na(qecbnew$Nb.Spirobranchus.lamarckii.3B) & !is.na(qecbnew$Nb.Spirobranchus.lamarckii.4B) & !is.na(qecbnew$Nb.Spirobranchus.lamarckii.5B) & !is.na(qecbnew$Nb.Spirobranchus.lamarckii.total))[, c("site_year_month_day", "Nb.Spirobranchus.lamarckii.1B", "Nb.Spirobranchus.lamarckii.2B", "Nb.Spirobranchus.lamarckii.3B", "Nb.Spirobranchus.lamarckii.4B", "Nb.Spirobranchus.lamarckii.5B", "Nb.Spirobranchus.lamarckii.total")]
for (i in c(1:nrow(Spirobranchus))) {
  Spirobranchus$mean.x.100[[i]] <- sum(Spirobranchus[i, c(2:6)], na.rm = TRUE) / sum(!is.na(Spirobranchus[i, c(2:6)])) * 100
}
Spirobranchus$mean.x.100 <- unlist(Spirobranchus$mean.x.100)
Spirobranchus$Nb.Spirobranchus.lamarckii.total <- as.numeric(Spirobranchus$Nb.Spirobranchus.lamarckii.total)
for (i in c(1:nrow(Spirobranchus))) {
  Spirobranchus$diff[[i]] <- Spirobranchus[i, "Nb.Spirobranchus.lamarckii.total"] - Spirobranchus[i, "mean.x.100"]
}
Spirobranchus$diff <- abs(as.integer(Spirobranchus$diff))
Spirobranchus <- dplyr::arrange(Spirobranchus, desc(diff), mean.x.100)
Spirobranchus <- dplyr::arrange(dplyr::filter(Spirobranchus, diff != 0 & mean.x.100 != 0), desc(diff))

# check it all in the qecbnew df

for (i in c(1:nrow(qecbnew))) {
  qecbnew$mean.x.100[[i]] <-
    #ifelse(qecbnew$Nb.Spirobranchus.lamarckii.total[[i]] %in% c(NA, "NaN", "Inf", "-Inf"),
    sum(qecbnew[i, c("Nb.Spirobranchus.lamarckii.1B", "Nb.Spirobranchus.lamarckii.2B", "Nb.Spirobranchus.lamarckii.3B", "Nb.Spirobranchus.lamarckii.4B", "Nb.Spirobranchus.lamarckii.5B")], na.rm = TRUE) / sum(!is.na(qecbnew[i, c("Nb.Spirobranchus.lamarckii.1B", "Nb.Spirobranchus.lamarckii.2B", "Nb.Spirobranchus.lamarckii.3B", "Nb.Spirobranchus.lamarckii.4B", "Nb.Spirobranchus.lamarckii.5B")])) * 100
  #, qecbnew$Nb.Spirobranchus.lamarckii.total[[i]])
} # sum of only NAs/0 = NaN; so replace NaN by Na
qecbnew$mean.x.100 <- as.character(qecbnew$mean.x.100)

sort(dplyr::filter(qecbnew, paste0(qecbnew$id_qecb, "_", qecbnew$site_year_month_day, "_", qecbnew$Type.Bloc, "_", qecbnew$Numéro.Bloc.échantillon, "_", qecbnew$Face) %in% paste0(qecbnew_spirobranchus$id_qecb, "_", qecbnew_spirobranchus$site_year_month_day, "_", qecbnew_spirobranchus$Type.Bloc, "_", qecbnew_spirobranchus$Numéro.Bloc.échantillon, "_", qecbnew_spirobranchus$Face))[, "mean.x.100"])

for (i in c(1:nrow(qecbnew))) {
  qecbnew$mean.x.100[[i]] <- ifelse(qecbnew$mean.x.100[[i]] == "NaN", NA, qecbnew$mean.x.100[[i]])
}
nrow(subset(qecbnew, is.na(qecbnew$mean.x.100)))
qecbnew$mean.x.100 <- as.integer(qecbnew$mean.x.100)

qecbnew$Nb.Spirobranchus.lamarckii.total <- as.integer(qecbnew$Nb.Spirobranchus.lamarckii.total)
unique(qecbnew$Nb.Spirobranchus.lamarckii.total - qecbnew$mean.x.100)
table(qecbnew$Nb.Spirobranchus.lamarckii.total - qecbnew$mean.x.100)
qecbnew$Nb.Spirobranchus.lamarckii.total.diff <- abs((qecbnew$Nb.Spirobranchus.lamarckii.total - qecbnew$mean.x.100))
spirobranchus_diff <- qecbnew[, c("Nb.Spirobranchus.lamarckii.1B", "Nb.Spirobranchus.lamarckii.2B", "Nb.Spirobranchus.lamarckii.3B", "Nb.Spirobranchus.lamarckii.4B", "Nb.Spirobranchus.lamarckii.5B", "Nb.Spirobranchus.lamarckii.total", "Nb.Spirobranchus.lamarckii.total.ini", "mean.x.100", "Nb.Spirobranchus.lamarckii.total.diff")]
spirobranchus_diff <- dplyr::arrange(spirobranchus_diff, desc(Nb.Spirobranchus.lamarckii.total.diff), mean.x.100)
spirobranchus_diff <- dplyr::arrange(dplyr::filter(spirobranchus_diff, Nb.Spirobranchus.lamarckii.total.diff != 0 & mean.x.100 != 0), desc(Nb.Spirobranchus.lamarckii.total.diff))

qecbnew$Nb.Spirobranchus.lamarckii.total <- ifelse(qecbnew$Nb.Spirobranchus.lamarckii.total.diff != 0 & qecbnew$mean.x.100 != 0, qecbnew$mean.x.100, qecbnew$Nb.Spirobranchus.lamarckii.total)
spirobranchus_diff <- qecbnew[, c("Nb.Spirobranchus.lamarckii.1B", "Nb.Spirobranchus.lamarckii.2B", "Nb.Spirobranchus.lamarckii.3B", "Nb.Spirobranchus.lamarckii.4B", "Nb.Spirobranchus.lamarckii.5B", "Nb.Spirobranchus.lamarckii.total", "Nb.Spirobranchus.lamarckii.total.ini", "mean.x.100", "Nb.Spirobranchus.lamarckii.total.diff")]
spirobranchus_diff$Nb.Spirobranchus.lamarckii.total.diff <- abs(as.integer(spirobranchus_diff$Nb.Spirobranchus.lamarckii.total.diff))
spirobranchus_diff <- dplyr::arrange(spirobranchus_diff, desc(Nb.Spirobranchus.lamarckii.total.diff), mean.x.100)
spirobranchus_diff <- dplyr::arrange(dplyr::filter(spirobranchus_diff, Nb.Spirobranchus.lamarckii.total.diff != 0 & mean.x.100 != 0), desc(Nb.Spirobranchus.lamarckii.total.diff))
# ok, change made when data x 100 was not correct.

# finally, change NA by mean.x100 for Spirobranchus total
qecbnew$Nb.Spirobranchus.lamarckii.total <- as.character(qecbnew$Nb.Spirobranchus.lamarckii.total)
for (i in c(1:nrow(qecbnew))) {
  qecbnew$Nb.Spirobranchus.lamarckii.total[[i]] <- ifelse(qecbnew$Nb.Spirobranchus.lamarckii.total[[i]] %in% c(NA, "NaN", "Inf", "-Inf"), sum(qecbnew[i, c("Nb.Spirobranchus.lamarckii.1B", "Nb.Spirobranchus.lamarckii.2B", "Nb.Spirobranchus.lamarckii.3B", "Nb.Spirobranchus.lamarckii.4B", "Nb.Spirobranchus.lamarckii.5B")], na.rm = TRUE) / sum(!is.na(qecbnew[i, c("Nb.Spirobranchus.lamarckii.1B", "Nb.Spirobranchus.lamarckii.2B", "Nb.Spirobranchus.lamarckii.3B", "Nb.Spirobranchus.lamarckii.4B", "Nb.Spirobranchus.lamarckii.5B")])) * 100, qecbnew$Nb.Spirobranchus.lamarckii.total[[i]])
} # sum of only NAs/0 = NaN; so replace NaN by Na

sort(dplyr::filter(qecbnew, paste0(qecbnew$id_qecb, "_", qecbnew$site_year_month_day, "_", qecbnew$Type.Bloc, "_", qecbnew$Numéro.Bloc.échantillon, "_", qecbnew$Face) %in% paste0(qecbnew_spirobranchus$id_qecb, "_", qecbnew_spirobranchus$site_year_month_day, "_", qecbnew_spirobranchus$Type.Bloc, "_", qecbnew_spirobranchus$Numéro.Bloc.échantillon, "_", qecbnew_spirobranchus$Face))[, "Nb.Spirobranchus.lamarckii.total"])

for (i in c(1:nrow(qecbnew))) {
  qecbnew$Nb.Spirobranchus.lamarckii.total[[i]] <- ifelse(qecbnew$Nb.Spirobranchus.lamarckii.total[[i]] == "NaN", NA, qecbnew$Nb.Spirobranchus.lamarckii.total[[i]])
}
nrow(subset(qecbnew, is.na(qecbnew$Nb.Spirobranchus.lamarckii.total)))
qecbnew$Nb.Spirobranchus.lamarckii.total <- as.integer(qecbnew$Nb.Spirobranchus.lamarckii.total)

unique(qecbnew$Nb.Spirobranchus.lamarckii.total - qecbnew$Nb.Spirobranchus.lamarckii.total.ini)
table(qecbnew$Nb.Spirobranchus.lamarckii.total - qecbnew$Nb.Spirobranchus.lamarckii.total.ini)
qecbnew$Nb.Spirobranchus.lamarckii.total.diff <- abs(qecbnew$Nb.Spirobranchus.lamarckii.total - qecbnew$Nb.Spirobranchus.lamarckii.total.ini)
spirobranchus_diff <- qecbnew[, c("Nb.Spirobranchus.lamarckii.1B", "Nb.Spirobranchus.lamarckii.2B", "Nb.Spirobranchus.lamarckii.3B", "Nb.Spirobranchus.lamarckii.4B", "Nb.Spirobranchus.lamarckii.5B", "Nb.Spirobranchus.lamarckii.total", "Nb.Spirobranchus.lamarckii.total.ini", "mean.x.100", "Nb.Spirobranchus.lamarckii.total.diff")]
spirobranchus_diff <- dplyr::arrange(spirobranchus_diff, desc(Nb.Spirobranchus.lamarckii.total.diff), mean.x.100)
spirobranchus_diff <- dplyr::arrange(dplyr::filter(spirobranchus_diff, Nb.Spirobranchus.lamarckii.total.diff != 0 & mean.x.100 != 0), desc(Nb.Spirobranchus.lamarckii.total.diff))
table(qecbnew$Nb.Spirobranchus.lamarckii.total.diff)
length(na.omit(qecbnew$Nb.Spirobranchus.lamarckii.total))
sum(is.na(qecbnew$Nb.Spirobranchus.lamarckii.total))
length(na.omit(qecbnew$Nb.Spirobranchus.lamarckii.total)) + sum(is.na(qecbnew$Nb.Spirobranchus.lamarckii.total))

qecbnew <- subset(qecbnew, select = -c(Nb.Spirobranchus.lamarckii.total.ini, mean.x.100, Nb.Spirobranchus.lamarckii.total.diff))

rm(qecbnew_spirobranchus, Spirobranchus, spirobranchus_data, spirobranchus_diff)

# do the same for spirorbis

qecbnew$Nb.spirorbis.total.ini <- qecbnew$Nb.spirorbis.total
qecbnew$Nb.spirorbis.total <- as.character(qecbnew$Nb.spirorbis.total)

table(qecbnew$Nb.spirorbis.total)
subset(qecbnew, is.na(qecbnew$Nb.spirorbis.total))
nrow(subset(qecbnew, is.na(qecbnew$Nb.spirorbis.total)))
subset(qecbnew, is.nan(qecbnew$Nb.spirorbis.total))
subset(qecbnew, is.finite(qecbnew$Nb.spirorbis.total))

nrow(dplyr::filter(qecbnew, qecbnew$Nb.spirorbis.total %in% c(NA, "NaN", "Inf", "-Inf")))

qecbnew_spirorbis <- (dplyr::filter(qecbnew, Nb.spirorbis.total %in% c(NA, "NaN", "Inf", "-Inf")))
qecbnew_spirorbis[, c("Nb.spirorbis.1A", "Nb.spirorbis.2A", "Nb.spirorbis.3A", "Nb.spirorbis.4A", "Nb.spirorbis.5A")] <- sapply(qecbnew_spirorbis[, c("Nb.spirorbis.1A", "Nb.spirorbis.2A", "Nb.spirorbis.3A", "Nb.spirorbis.4A", "Nb.spirorbis.5A")], as.character)
(spirobranchus_data <- subset(qecbnew_spirorbis, !is.na(qecbnew_spirorbis$Nb.spirorbis.1A) || !is.na(qecbnew_spirorbis$Nb.spirorbis.2A) || !is.na(qecbnew_spirorbis$Nb.spirorbis.3A) || !is.na(qecbnew_spirorbis$Nb.spirorbis.4A) || !is.na(qecbnew_spirorbis$Nb.spirorbis.5A))[, c("site_year_month_day", "Nb.spirorbis.1A", "Nb.spirorbis.2A", "Nb.spirorbis.3A", "Nb.spirorbis.4A", "Nb.spirorbis.5A", "Nb.spirorbis.total")])
unique(spirobranchus_data$site_year_month_day)

# In contrast to Spirobranchus data, no encoding issues for spirorbis data, cfr when sub-quadrat 1A-5A are ALL encoded, NA for total.

# some more issues however with "x200"count data

spirorbis <- subset(qecbnew, !is.na(qecbnew$Nb.spirorbis.1A) & !is.na(qecbnew$Nb.spirorbis.2A) & !is.na(qecbnew$Nb.spirorbis.3A) & !is.na(qecbnew$Nb.spirorbis.4A) & !is.na(qecbnew$Nb.spirorbis.5A) & !is.na(qecbnew$Nb.spirorbis.total))[, c("site_year_month_day", "Nb.spirorbis.1A", "Nb.spirorbis.2A", "Nb.spirorbis.3A", "Nb.spirorbis.4A", "Nb.spirorbis.5A", "Nb.spirorbis.total")]
for (i in c(1:nrow(spirorbis))) {
    spirorbis$mean.x.200[[i]] <- sum(spirorbis[i, c(2:6)], na.rm = TRUE) / sum(!is.na(spirorbis[i, c(2:6)])) * 200
}
spirorbis$mean.x.200 <- unlist(spirorbis$mean.x.200)
spirorbis$Nb.spirorbis.total <- as.numeric(spirorbis$Nb.spirorbis.total)
for (i in c(1:nrow(spirorbis))) {
  spirorbis$diff[[i]] <- spirorbis[i, "Nb.spirorbis.total"] - spirorbis[i, "mean.x.200"]
}
spirorbis$diff <- abs(as.integer(spirorbis$diff))
spirorbis <- dplyr::arrange(spirorbis, desc(diff), mean.x.200)
(gonb_ilotstmichel_2015_04_18 <- dplyr::filter(spirorbis, site_year_month_day == "GONB_IlotStMichel.2015.04.18"))
rm(gonb_ilotstmichel_2015_04_18)
spirorbis <- dplyr::arrange(dplyr::filter(spirorbis, diff != 0 & mean.x.200 != 0), desc(diff))

# check it all in the qecbnew df

for (i in c(1:nrow(qecbnew))) {
    qecbnew$mean.x.200[[i]] <-
      #ifelse(qecbnew$Nb.spirorbis.total[[i]] %in% c(NA, "NaN", "Inf", "-Inf"),
      sum(qecbnew[i, c("Nb.spirorbis.1A", "Nb.spirorbis.2A", "Nb.spirorbis.3A", "Nb.spirorbis.4A", "Nb.spirorbis.5A")], na.rm = TRUE) / sum(!is.na(qecbnew[i, c("Nb.spirorbis.1A", "Nb.spirorbis.2A", "Nb.spirorbis.3A", "Nb.spirorbis.4A", "Nb.spirorbis.5A")])) * 200
    #, qecbnew$Nb.spirorbis.total[[i]])
} # sum of only NAs/0 = NaN; so replace NaN by Na
qecbnew$mean.x.200 <- as.character(qecbnew$mean.x.200)

sort(dplyr::filter(qecbnew, paste0(qecbnew$id_qecb, "_", qecbnew$site_year_month_day, "_", qecbnew$Type.Bloc, "_", qecbnew$Numéro.Bloc.échantillon, "_", qecbnew$Face) %in% paste0(qecbnew_spirorbis$id_qecb, "_", qecbnew_spirorbis$site_year_month_day, "_", qecbnew_spirorbis$Type.Bloc, "_", qecbnew_spirorbis$Numéro.Bloc.échantillon, "_", qecbnew_spirorbis$Face))[, "mean.x.200"])

for (i in c(1:nrow(qecbnew))) {
  qecbnew$mean.x.200[[i]] <- ifelse(qecbnew$mean.x.200[[i]] == "NaN", NA, qecbnew$mean.x.200[[i]])
}
nrow(subset(qecbnew, is.na(qecbnew$mean.x.200)))
qecbnew$mean.x.200 <- as.integer(qecbnew$mean.x.200)

qecbnew$Nb.spirorbis.total <- as.integer(qecbnew$Nb.spirorbis.total)
unique(qecbnew$Nb.spirorbis.total - qecbnew$mean.x.200)
table(qecbnew$Nb.spirorbis.total - qecbnew$mean.x.200)
qecbnew$Nb.spirorbis.total.diff <- abs((qecbnew$Nb.spirorbis.total - qecbnew$mean.x.200))
spirorbis_diff <- qecbnew[, c("Nb.spirorbis.1A", "Nb.spirorbis.2A", "Nb.spirorbis.3A", "Nb.spirorbis.4A", "Nb.spirorbis.5A", "Nb.spirorbis.total", "Nb.spirorbis.total.ini", "mean.x.200", "Nb.spirorbis.total.diff")]
spirorbis_diff <- dplyr::arrange(spirorbis_diff, desc(Nb.spirorbis.total.diff), mean.x.200)
spirorbis_diff <- dplyr::arrange(dplyr::filter(spirorbis_diff, Nb.spirorbis.total.diff != 0 & mean.x.200 != 0), desc(Nb.spirorbis.total.diff))

qecbnew$Nb.spirorbis.total <- ifelse(qecbnew$Nb.spirorbis.total.diff != 0 & qecbnew$mean.x.200 != 0, qecbnew$mean.x.200, qecbnew$Nb.spirorbis.total)
spirorbis_diff <- qecbnew[, c("Nb.spirorbis.1A", "Nb.spirorbis.2A", "Nb.spirorbis.3A", "Nb.spirorbis.4A", "Nb.spirorbis.5A", "Nb.spirorbis.total", "Nb.spirorbis.total.ini", "mean.x.200", "Nb.spirorbis.total.diff")]
spirorbis_diff$Nb.spirorbis.total.diff <- abs(as.integer(spirorbis_diff$Nb.spirorbis.total.diff))
spirorbis_diff <- dplyr::arrange(spirorbis_diff, desc(Nb.spirorbis.total.diff), mean.x.200)
spirorbis_diff <- dplyr::arrange(dplyr::filter(spirorbis_diff, Nb.spirorbis.total.diff != 0 & mean.x.200 != 0), desc(Nb.spirorbis.total.diff))
# ok, change made when data x 200 was not correct.

# finally, change NA by mean.x200 for spirorbis total
qecbnew$Nb.spirorbis.total <- as.character(qecbnew$Nb.spirorbis.total)
for (i in c(1:nrow(qecbnew))) {
  qecbnew$Nb.spirorbis.total[[i]] <- ifelse(qecbnew$Nb.spirorbis.total[[i]] %in% c(NA, "NaN", "Inf", "-Inf"), sum(qecbnew[i, c("Nb.spirorbis.1A", "Nb.spirorbis.2A", "Nb.spirorbis.3A", "Nb.spirorbis.4A", "Nb.spirorbis.5A")], na.rm = TRUE) / sum(!is.na(qecbnew[i, c("Nb.spirorbis.1A", "Nb.spirorbis.2A", "Nb.spirorbis.3A", "Nb.spirorbis.4A", "Nb.spirorbis.5A")])) * 200, qecbnew$Nb.spirorbis.total[[i]])
} # sum of only NAs/0 = NaN; so replace NaN by Na

sort(dplyr::filter(qecbnew, paste0(qecbnew$id_qecb, "_", qecbnew$site_year_month_day, "_", qecbnew$Type.Bloc, "_", qecbnew$Numéro.Bloc.échantillon, "_", qecbnew$Face) %in% paste0(qecbnew_spirorbis$id_qecb, "_", qecbnew_spirorbis$site_year_month_day, "_", qecbnew_spirorbis$Type.Bloc, "_", qecbnew_spirorbis$Numéro.Bloc.échantillon, "_", qecbnew_spirorbis$Face))[, "Nb.spirorbis.total"])

for (i in c(1:nrow(qecbnew))) {
  qecbnew$Nb.spirorbis.total[[i]] <- ifelse(qecbnew$Nb.spirorbis.total[[i]] == "NaN", NA, qecbnew$Nb.spirorbis.total[[i]])
}
nrow(subset(qecbnew, is.na(qecbnew$Nb.spirorbis.total)))
qecbnew$Nb.spirorbis.total <- as.integer(qecbnew$Nb.spirorbis.total)

unique(qecbnew$Nb.spirorbis.total - qecbnew$Nb.spirorbis.total.ini)
table(qecbnew$Nb.spirorbis.total - qecbnew$Nb.spirorbis.total.ini)
qecbnew$Nb.spirorbis.total.diff <- abs(qecbnew$Nb.spirorbis.total - qecbnew$Nb.spirorbis.total.ini)
spirorbis_diff <- qecbnew[, c("Nb.spirorbis.1A", "Nb.spirorbis.2A", "Nb.spirorbis.3A", "Nb.spirorbis.4A", "Nb.spirorbis.5A", "Nb.spirorbis.total", "Nb.spirorbis.total.ini", "mean.x.200", "Nb.spirorbis.total.diff")]
spirorbis_diff <- dplyr::arrange(spirorbis_diff, desc(Nb.spirorbis.total.diff), mean.x.200)
spirorbis_diff <- dplyr::arrange(dplyr::filter(spirorbis_diff, Nb.spirorbis.total.diff != 0 & mean.x.200 != 0), desc(Nb.spirorbis.total.diff))
table(qecbnew$Nb.spirorbis.total.diff)
length(na.omit(qecbnew$Nb.spirorbis.total))
sum(is.na(qecbnew$Nb.spirorbis.total))
length(na.omit(qecbnew$Nb.spirorbis.total)) + sum(is.na(qecbnew$Nb.spirorbis.total))

qecbnew <- subset(qecbnew, select = -c(Nb.spirorbis.total.ini, mean.x.200, Nb.spirorbis.total.diff))

rm(qecbnew_spirorbis, spirorbis, spirobranchus_data, spirorbis_diff, i)


# dplyr::filter for abnormal data, based on histogram distribution of data

dplyr::filter(qecbnew, X..algues.brunes > 100)[, c("Site", "date_fiche", "Type.Bloc", "Numéro.Bloc.échantillon", "Face", "X..algues.brunes")]
qecbnew$X..algues.brunes <- ifelse(qecbnew$X..algues.brunes > 100, 100, qecbnew$X..algues.brunes)
dplyr::filter(qecbnew, X..algues.rouges > 100)[, c("Site", "date_fiche", "Type.Bloc", "Numéro.Bloc.échantillon", "Face", "X..algues.rouges")]
qecbnew$X..algues.rouges <- ifelse(qecbnew$X..algues.rouges > 100, 100, qecbnew$X..algues.rouges)


## SCRIPT I - NAs <- 0 ; cfr previous comment makes no sense to have NA encoded when the presence of an organism is in reality = 0

# We are facing an issues with NA observations, because either they were not measured/counted, then they are effectively NAs; or these NAs = indeed "0"; but I cannot have NAs for variables that are included in the index determination, cfr if 5+0 = 5, 5+NA = NA; see for example site_year_month_day == "ARMO_Bilfot.2014.04.28", Nb.Spirobranchus.lamarckii.total is NA ...
# I theregore change these NAs by 0

# replace NAs by "0" for variables used in qecb determination
qecbnew[, c("X..algues.brunes",
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

qecbnew$X..Mytilus.sp. <- ifelse((substr(qecbnew$Site, 1, 4) %in% c("EGMP", "BASQ")) & is.na(qecbnew$X..Mytilus.sp.), 0, qecbnew$X..Mytilus.sp.)
qecbnew$Nb.Crassostrea.gigas <- ifelse((substr(qecbnew$Site, 1, 4) %in% c("EGMP", "BASQ")) & is.na(qecbnew$Nb.Crassostrea.gigas), 0, qecbnew$Nb.Crassostrea.gigas)
qecbnew$Nb.Ostrea.edulis <- ifelse((substr(qecbnew$Site, 1, 4) %in% c("EGMP", "BASQ")) & is.na(qecbnew$Nb.Ostrea.edulis), 0, qecbnew$Nb.Ostrea.edulis)


# add a region variable
region <- rep(NA, nrow(qecbnew))
qecbnew <- tibble::add_column(qecbnew, region, .after = "Site_bis")
qecbnew$region <- ifelse(qecbnew$Site %in% c("EGMP_GroinCou", "EGMP_PasEmsembert",    "EGMP_BreeBains", "EGMP_PerreAntiochat", "EGMP_Chassiron", "BASQ_FlotsBleusZP", "BASQ_FlotsBleusZF"), "EGMP.BASQ", "Bretagne")
rm(region)
qecbnew <- dplyr::arrange(qecbnew, region, site_year_month_day, Type.Bloc, Numéro.Bloc.échantillon, Face)

# accolement function according to recent 'retournement'

## before I go further ahead, I have to correct for surface d'accollement for several variable for BM.FI !!

# not the same file name between script qecb script (qecbNew) and this script (qecbNew); doesn't matter, only appears here in the first dplyr::filter lines.

qecbnew <- tibble::add_column(qecbnew, terri_ = substr(qecbnew$Site, 1, 4), .after = "Site_bis")

qecbnew$X..Eponges_ini <- qecbnew$X..Eponges
qecbnew$X..Ascidies.Coloniales_ini <- qecbnew$X..Ascidies.Coloniales
qecbnew$X..Ascidies.Solitaires_ini <- qecbnew$X..Ascidies.Solitaires
qecbnew$X..Bryozoaires.Dresses_ini <- qecbnew$X..Bryozoaires.Dresses
qecbnew$X..Lithophyllum_ini <- qecbnew$X..Lithophyllum
qecbnew$X..Balanes.Vivantes_ini <- qecbnew$X..Balanes.Vivantes

df_bm_fs <- qecbnew %>% dplyr::filter(Type.Bloc == "Bloc mobile" & Face == "face supérieure")
df_bm_fi <- qecbnew %>% dplyr::filter(Type.Bloc == "Bloc mobile" & Face == "face inférieure")
df_bf <- qecbnew %>% dplyr::filter(Type.Bloc != "Bloc mobile")

`%notin%` <- Negate(`%in%`)

acco_fct <- function(var_) {

  df_bm_fi$var_cor.acco. <<- NA

  for (i in c(1:nrow(df_bm_fi))) {

    df_bm_fi$var_cor.acco.[[i]] <<- if (df_bm_fi$terri_[[i]] %notin% c("EGMP", "BASQ")) {
      ifelse(#df_$Couleur.dominante %in% c("Rouge", "Brune", "Brune-Rouge") ||
        df_bm_fs$Couleur.dominante[[i]] %in% c("Blanche", "Verte", "Blanche-Verte", "Colorée"), df_bm_fi[i, var_] / (100 - df_bm_fi$X..Surface.Accolement[[i]]) * 100, df_bm_fi[i, var_])
    } else {
      ifelse(df_bm_fs$Couleur.dominante[[i]] %in% c("Blanche", "Verte", "Blanche-Verte", "Colorée")
             & df_bm_fi$X..Surface.Accolement[[i]] != 0 # I have to use it in dplyr::filter this time as well for EGMP- BASQ (but not for Bretagne, although could be added, same result); identical/repeated measure for BM.FI and BM.FS
             & df_bm_fs$X..Mytilus.sp.[[i]] == 0, df_bm_fi[i, var_] / (100 - df_bm_fi$X..Surface.Accolement[[i]]) * 100, df_bm_fi[i, var_])
    }

  }

}

# I would only consider colors in c("Rouge", "Brune", "Brune-Rouge") for BM.FI correction [ and not the series c("Blanche-Brune", "Rouge", "Brune", "Blanche-Rouge", "Brune-Rouge", "Rouge-Verte", "Brune-Verte") ] ; and for BM.FS, the list c("Blanche", "Verte", "Colorée") => we do the correction for BM.FI accollement based on BM.FS color !!!


# apply acco_fct to BM.FI variables

apply_acco_fct <- function(var_) {

  show(sort(df_bm_fi[, var_], decreasing = TRUE, index.return = FALSE)[1:50])
  pre_ <- as.vector(df_bm_fi[, var_])
  acco_fct(var_)
  df_bm_fi <<- tibble::add_column(df_bm_fi, var_cor. = df_bm_fi$var_cor.acco., .after = var_)
  show(sort(df_bm_fi$var_cor., decreasing = TRUE, index.return = FALSE)[1:50])
  df_bm_fi$var_cor. <<- as.numeric(ifelse(as.character(df_bm_fi$var_cor.) %in% c(NA, "NaN", "-Inf", "Inf"), "0", as.character(df_bm_fi$var_cor.)))
  df_bm_fi$var_cor. <<- ifelse(df_bm_fi$var_cor. > 100, 100, df_bm_fi$var_cor.)
  show(sort(df_bm_fi$var_cor., decreasing = TRUE, index.return = FALSE)[1:50])
  show(length(na.omit(which(abs(as.vector(df_bm_fi$var_cor.) - pre_) != 0))) / na.omit(length(df_bm_fi$var_cor.)) * 100)
  par(mfrow = c(1, 3))
  hist(pre_, main = var_, xlab = "pre-corection")
  hist(df_bm_fi$var_cor., main = var_, xlab = "post-corection")
  hist(df_bm_fi[as.vector(which(abs(as.vector(df_bm_fi$var_cor.) - pre_) != 0)), var_], main = var_, xlab = "diff. post-pre != 0")
  par(mfrow = c(1, 1))
  df_bm_fi <<- df_bm_fi[, -which(names(df_bm_fi) %in% c(var_, "var_cor.acco."))]
  colnames(df_bm_fi)[colnames(df_bm_fi) == "var_cor."] <<- var_

  rm(pre_)

}

apply_acco_fct("X..Eponges")
apply_acco_fct("X..Ascidies.Coloniales")
apply_acco_fct("X..Ascidies.Solitaires")
apply_acco_fct("X..Bryozoaires.Dresses")
apply_acco_fct("X..Lithophyllum")
apply_acco_fct("X..Balanes.Vivantes")

qecbnew <- dplyr::bind_rows(df_bm_fs, df_bm_fi)
qecbnew <- dplyr::bind_rows(qecbnew, df_bf)

qecbnew <- dplyr::arrange(qecbnew, region, site_year_month_day, Type.Bloc, Numéro.Bloc.échantillon, Face)

# do remove some more data ...
# "FINS_Quemenes.2020.10.16", bad encoding, I let know Anna Capietto to make changes => was corrected normally, so unactivate next time I download ESTAMP data
qecbnew <- dplyr::filter(qecbnew, site_year_month_day != "FINS_Quemenes.2020.10.16")

# save the final qecbnew df_

#saveRDS(qecbnew, "qecbnew.RDS")

qecb <- qecbnew

write.table(qecb, "Valeurs_stat.tabular", row.names = FALSE, quote = FALSE, sep = "\t", dec = ".", fileEncoding = "UTF-8")


`%notin%` <- Negate(`%in%`)

## reorder and/or create new variables

# variable site_year_month_day moved for clarity purpose, not needed necessarily
qecb <- tibble::add_column(qecb, qecb$site_year_month_day, .after = "Site_bis")
qecb <- qecb[, -which(names(qecb) %in% c("site_year_month_day"))]
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

bretagne_bm <- dplyr::filter(qecb, region == "Bretagne" & Type.Bloc == "Bloc mobile")
bretagne_bf <- dplyr::filter(qecb, region == "Bretagne" & Type.Bloc %in% c("Bloc fixé", "Roche en place"))
egmp_basq_bm <- dplyr::filter(qecb, region == "EGMP.BASQ" & Type.Bloc == "Bloc mobile")
egmp_basq_bf <- dplyr::filter(qecb, region == "EGMP.BASQ" & Type.Bloc %in% c("Bloc fixé", "Roche en place"))

# replace NAs by "0" for variables used in qecb determination

{
  # bretagne_bm
  bretagne_bm[, c(
    "X..algues.brunes",
    "Strate.algues.brunes",
    "X..algues.rouges",
    "Strate.algues.rouges",
    "X..algues.vertes",
    "Strate.algues.vertes",
    "X..Cladophora",
    "X..Lithophyllum",
    "X..Recouvrement.Sediment", # is NA, then replace by 0 as well because no sense to have a NA value for "% recouvrement sédiment" as well.
    #"Type.Sediment",
    "X..Roche.Nue", # is NA, then replace by 0 as well because no sense to have a NA value for "% roche nue" as well.
    "Nb.Littorina.obtusata",
    "Nb.Gibbula.cineraria",
    "Nb.Gibbula.pennanti",
    "Nb.Gibbula.umbilicalis",
    "Nb.Phallusia.mamillata",
    "Nb.Tethya.aurantium",
    #"Nb.Spirobranchus.lamarckii.1B",
    #"Nb.Spirobranchus.lamarckii.2B",
    #"Nb.Spirobranchus.lamarckii.3B",
    #"Nb.Spirobranchus.lamarckii.4B",
    #"Nb.Spirobranchus.lamarckii.5B",
    "Nb.Spirobranchus.lamarckii.total",
    #"Nb.spirorbis.1A",
    #"Nb.spirorbis.2A",
    #"Nb.spirorbis.3A",
    #"Nb.spirorbis.4A",
    #"Nb.spirorbis.5A",
    "Nb.spirorbis.total",
    #.."Nb.Crassostrea.gigas",
    #.."Nb.Ostrea.edulis",
    #.."X..Mytilus.sp.",
    #.."X..Hermelles",
    #.."X..Hydraires",
    "X..Eponges",
    "X..Ascidies.Coloniales",
    "X..Ascidies.Solitaires",
    "X..Bryozoaires.Dresses",
    "X..Balanes.Vivantes",
    #"Commentaires.Avant",
    "X..Surface.Accolement", # is NA, then replace by 0 as well because no sense to have a NA value for "% surface accolement" as well.
    #"Type.sustrat.observé",
    #"Commentaires",
    "Nb.Cancer.pagurus..Tourteau.",
    "Nb.Necora.puber..Etrille.",
    "Nb.Carcinus.maenas..Crabe.vert.",
    "Nb.Nucella.lapilus..Pourpre.",
    #.."Nb.Eriphia.verrucosa..Crabe.verruqueux.",
    #.."Nb.Octopus.vulgaris..Poulpe.",
    "Nb.Galathea..Galathées.",
    #.."Nb.Paracentrotus.lividus..Oursin.",
    "Nb.Lophozozymus.incisus..ancien.Xantho.incisus.",
    "Nb.Palaemon.sp..Crevette.bouquet.ou.crevette.rose.",
    "Nb.Haliotis.tuberculata..Ormeau.",
    #"Nb.Stramonita.haemastoma..Pourpre.bouche.de.sang.",
    "Nb.Littorina.littorea..Bigorneau.",
    "Nb.Xantho.pilipes..Xanthe.poilu.",
    "Nb.Mimachlamys.varia..Pétoncle.noir."
  )
  ] <- lapply(bretagne_bm[, c(
    "X..algues.brunes",
    "Strate.algues.brunes",
    "X..algues.rouges",
    "Strate.algues.rouges",
    "X..algues.vertes",
    "Strate.algues.vertes",
    "X..Cladophora",
    "X..Lithophyllum",
    "X..Recouvrement.Sediment", # is NA, then replace by 0 as well because no sense to have a NA value for "% recouvrement sédiment" as well.
    #"Type.Sediment",
    "X..Roche.Nue", # is NA, then replace by 0 as well because no sense to have a NA value for "% roche nue" as well.
    "Nb.Littorina.obtusata",
    "Nb.Gibbula.cineraria",
    "Nb.Gibbula.pennanti",
    "Nb.Gibbula.umbilicalis",
    "Nb.Phallusia.mamillata",
    "Nb.Tethya.aurantium",
    #"Nb.Spirobranchus.lamarckii.1B",
    #"Nb.Spirobranchus.lamarckii.2B",
    #"Nb.Spirobranchus.lamarckii.3B",
    #"Nb.Spirobranchus.lamarckii.4B",
    #"Nb.Spirobranchus.lamarckii.5B",
    "Nb.Spirobranchus.lamarckii.total",
    #"Nb.spirorbis.1A",
    #"Nb.spirorbis.2A",
    #"Nb.spirorbis.3A",
    #"Nb.spirorbis.4A",
    #"Nb.spirorbis.5A",
    "Nb.spirorbis.total",
    #.."Nb.Crassostrea.gigas",
    #.."Nb.Ostrea.edulis",
    #.."X..Mytilus.sp.",
    #.."X..Hermelles",
    #.."X..Hydraires",
    "X..Eponges",
    "X..Ascidies.Coloniales",
    "X..Ascidies.Solitaires",
    "X..Bryozoaires.Dresses",
    "X..Balanes.Vivantes",
    #"Commentaires.Avant",
    "X..Surface.Accolement", # is NA, then replace by 0 as well because no sense to have a NA value for "% surface accolement" as well.
    #"Type.sustrat.observé",
    #"Commentaires",
    "Nb.Cancer.pagurus..Tourteau.",
    "Nb.Necora.puber..Etrille.",
    "Nb.Carcinus.maenas..Crabe.vert.",
    "Nb.Nucella.lapilus..Pourpre.",
    #.."Nb.Eriphia.verrucosa..Crabe.verruqueux.",
    #.."Nb.Octopus.vulgaris..Poulpe.",
    "Nb.Galathea..Galathées.",
    #.."Nb.Paracentrotus.lividus..Oursin.",
    "Nb.Lophozozymus.incisus..ancien.Xantho.incisus.",
    "Nb.Palaemon.sp..Crevette.bouquet.ou.crevette.rose.",
    "Nb.Haliotis.tuberculata..Ormeau.",
    #"Nb.Stramonita.haemastoma..Pourpre.bouche.de.sang.",
    "Nb.Littorina.littorea..Bigorneau.",
    "Nb.Xantho.pilipes..Xanthe.poilu.",
    "Nb.Mimachlamys.varia..Pétoncle.noir."
  )
  ], function(x) replace(x, is.na(x), 0))


  # bretagne_bf
  bretagne_bf[, c(
    "X..algues.brunes",
    "Strate.algues.brunes",
    "X..algues.rouges",
    "Strate.algues.rouges",
    "X..algues.vertes",
    "Strate.algues.vertes",
    "X..Cladophora",
    "X..Lithophyllum",
    "X..Recouvrement.Sediment", # is NA, then replace by 0 as well because no sense to have a NA value for "% recouvrement sédiment" as well.
    #"Type.Sediment",
    "X..Roche.Nue", # is NA, then replace by 0 as well because no sense to have a NA value for "% roche nue" as well.
    "Nb.Littorina.obtusata",
    "Nb.Gibbula.cineraria",
    "Nb.Gibbula.pennanti",
    "Nb.Gibbula.umbilicalis",
    "Nb.Phallusia.mamillata",
    "Nb.Tethya.aurantium",
    #"Nb.Spirobranchus.lamarckii.1B",
    #"Nb.Spirobranchus.lamarckii.2B",
    #"Nb.Spirobranchus.lamarckii.3B",
    #"Nb.Spirobranchus.lamarckii.4B",
    #"Nb.Spirobranchus.lamarckii.5B",
    "Nb.Spirobranchus.lamarckii.total",
    #"Nb.spirorbis.1A",
    #"Nb.spirorbis.2A",
    #"Nb.spirorbis.3A",
    #"Nb.spirorbis.4A",
    #"Nb.spirorbis.5A",
    "Nb.spirorbis.total",
    #.."Nb.Crassostrea.gigas",
    #.."Nb.Ostrea.edulis",
    #.."X..Mytilus.sp.",
    #.."X..Hermelles",
    #.."X..Hydraires",
    "X..Eponges",
    "X..Ascidies.Coloniales",
    "X..Ascidies.Solitaires",
    "X..Bryozoaires.Dresses",
    "X..Balanes.Vivantes",
    #"Commentaires.Avant",
    "X..Surface.Accolement"#, # is NA, then replace by 0 as well because no sense to have a NA value for "% surface accolement" as well.
    #"Type.sustrat.observé",
    #"Commentaires",
    #."Nb.Cancer.pagurus..Tourteau.",
    #.."Nb.Necora.puber..Etrille.",
    #."Nb.Carcinus.maenas..Crabe.vert.",
    #."Nb.Nucella.lapilus..Pourpre.",
    #.."Nb.Eriphia.verrucosa..Crabe.verruqueux.",
    #.."Nb.Octopus.vulgaris..Poulpe.",
    #."Nb.Galathea..Galathées.",
    #.."Nb.Paracentrotus.lividus..Oursin.",
    #."Nb.Lophozozymus.incisus..ancien.Xantho.incisus.",
    #."Nb.Palaemon.sp..Crevette.bouquet.ou.crevette.rose.",
    #."Nb.Haliotis.tuberculata..Ormeau.",
    #."Nb.Stramonita.haemastoma..Pourpre.bouche.de.sang.",
    #."Nb.Littorina.littorea..Bigorneau.",
    #."Nb.Xantho.pilipes..Xanthe.poilu.",
    #."Nb.Mimachlamys.varia..Pétoncle.noir."
  )
  ] <- lapply(bretagne_bf[, c(
    "X..algues.brunes",
    "Strate.algues.brunes",
    "X..algues.rouges",
    "Strate.algues.rouges",
    "X..algues.vertes",
    "Strate.algues.vertes",
    "X..Cladophora",
    "X..Lithophyllum",
    "X..Recouvrement.Sediment", # is NA, then replace by 0 as well because no sense to have a NA value for "% recouvrement sédiment" as well.
    #"Type.Sediment",
    "X..Roche.Nue", # is NA, then replace by 0 as well because no sense to have a NA value for "% roche nue" as well.
    "Nb.Littorina.obtusata",
    "Nb.Gibbula.cineraria",
    "Nb.Gibbula.pennanti",
    "Nb.Gibbula.umbilicalis",
    "Nb.Phallusia.mamillata",
    "Nb.Tethya.aurantium",
    #"Nb.Spirobranchus.lamarckii.1B",
    #"Nb.Spirobranchus.lamarckii.2B",
    #"Nb.Spirobranchus.lamarckii.3B",
    #"Nb.Spirobranchus.lamarckii.4B",
    #"Nb.Spirobranchus.lamarckii.5B",
    "Nb.Spirobranchus.lamarckii.total",
    #"Nb.spirorbis.1A",
    #"Nb.spirorbis.2A",
    #"Nb.spirorbis.3A",
    #"Nb.spirorbis.4A",
    #"Nb.spirorbis.5A",
    "Nb.spirorbis.total",
    #.."Nb.Crassostrea.gigas",
    #.."Nb.Ostrea.edulis",
    #.."X..Mytilus.sp.",
    #.."X..Hermelles",
    #.."X..Hydraires",
    "X..Eponges",
    "X..Ascidies.Coloniales",
    "X..Ascidies.Solitaires",
    "X..Bryozoaires.Dresses",
    "X..Balanes.Vivantes",
    #"Commentaires.Avant",
    "X..Surface.Accolement"#, # is NA, then replace by 0 as well because no sense to have a NA value for "% surface accolement" as well.
    #"Type.sustrat.observé",
    #"Commentaires",
    #."Nb.Cancer.pagurus..Tourteau.",
    #.."Nb.Necora.puber..Etrille.",
    #."Nb.Carcinus.maenas..Crabe.vert.",
    #."Nb.Nucella.lapilus..Pourpre.",
    #.."Nb.Eriphia.verrucosa..Crabe.verruqueux.",
    #.."Nb.Octopus.vulgaris..Poulpe.",
    #."Nb.Galathea..Galathées.",
    #.."Nb.Paracentrotus.lividus..Oursin.",
    #."Nb.Lophozozymus.incisus..ancien.Xantho.incisus.",
    #."Nb.Palaemon.sp..Crevette.bouquet.ou.crevette.rose.",
    #."Nb.Haliotis.tuberculata..Ormeau.",
    #."Nb.Stramonita.haemastoma..Pourpre.bouche.de.sang.",
    #."Nb.Littorina.littorea..Bigorneau.",
    #."Nb.Xantho.pilipes..Xanthe.poilu.",
    #."Nb.Mimachlamys.varia..Pétoncle.noir."
  )
  ], function(x) replace(x, is.na(x), 0))


  # egmp_basq_bm
  egmp_basq_bm[, c(
    "X..algues.brunes",
    "Strate.algues.brunes",
    "X..algues.rouges",
    "Strate.algues.rouges",
    "X..algues.vertes",
    "Strate.algues.vertes",
    "X..Cladophora",
    "X..Lithophyllum",
    "X..Recouvrement.Sediment", # is NA, then replace by 0 as well because no sense to have a NA value for "% recouvrement sédiment" as well.
    #"Type.Sediment",
    "X..Roche.Nue", # is NA, then replace by 0 as well because no sense to have a NA value for "% roche nue" as well.
    "Nb.Littorina.obtusata",
    "Nb.Gibbula.cineraria",
    "Nb.Gibbula.pennanti",
    "Nb.Gibbula.umbilicalis",
    "Nb.Phallusia.mamillata",
    "Nb.Tethya.aurantium",
    #"Nb.Spirobranchus.lamarckii.1B",
    #"Nb.Spirobranchus.lamarckii.2B",
    #"Nb.Spirobranchus.lamarckii.3B",
    #"Nb.Spirobranchus.lamarckii.4B",
    #"Nb.Spirobranchus.lamarckii.5B",
    "Nb.Spirobranchus.lamarckii.total",
    #"Nb.spirorbis.1A",
    #"Nb.spirorbis.2A",
    #"Nb.spirorbis.3A",
    #"Nb.spirorbis.4A",
    #"Nb.spirorbis.5A",
    "Nb.spirorbis.total",
    "Nb.Crassostrea.gigas",
    "Nb.Ostrea.edulis",
    "X..Mytilus.sp.",
    "X..Hermelles",
    "X..Hydraires",
    "X..Eponges",
    "X..Ascidies.Coloniales",
    "X..Ascidies.Solitaires",
    "X..Bryozoaires.Dresses",
    "X..Balanes.Vivantes",
    #"Commentaires.Avant",
    "X..Surface.Accolement", # is NA, then replace by 0 as well because no sense to have a NA value for "% surface accolement" as well.
    #"Type.sustrat.observé",
    #"Commentaires",
    "Nb.Cancer.pagurus..Tourteau.",
    "Nb.Necora.puber..Etrille.",
    "Nb.Carcinus.maenas..Crabe.vert.",
    "Nb.Nucella.lapilus..Pourpre.",
    "Nb.Eriphia.verrucosa..Crabe.verruqueux.",
    "Nb.Octopus.vulgaris..Poulpe.",
    "Nb.Galathea..Galathées.",
    "Nb.Paracentrotus.lividus..Oursin.",
    "Nb.Lophozozymus.incisus..ancien.Xantho.incisus.",
    "Nb.Palaemon.sp..Crevette.bouquet.ou.crevette.rose.",
    "Nb.Haliotis.tuberculata..Ormeau.",
    "Nb.Stramonita.haemastoma..Pourpre.bouche.de.sang.",
    "Nb.Littorina.littorea..Bigorneau.",
    "Nb.Xantho.pilipes..Xanthe.poilu.",
    "Nb.Mimachlamys.varia..Pétoncle.noir."
  )
  ] <- lapply(egmp_basq_bm[, c(
    "X..algues.brunes",
    "Strate.algues.brunes",
    "X..algues.rouges",
    "Strate.algues.rouges",
    "X..algues.vertes",
    "Strate.algues.vertes",
    "X..Cladophora",
    "X..Lithophyllum",
    "X..Recouvrement.Sediment", # is NA, then replace by 0 as well because no sense to have a NA value for "% recouvrement sédiment" as well.
    #"Type.Sediment",
    "X..Roche.Nue", # is NA, then replace by 0 as well because no sense to have a NA value for "% roche nue" as well.
    "Nb.Littorina.obtusata",
    "Nb.Gibbula.cineraria",
    "Nb.Gibbula.pennanti",
    "Nb.Gibbula.umbilicalis",
    "Nb.Phallusia.mamillata",
    "Nb.Tethya.aurantium",
    #"Nb.Spirobranchus.lamarckii.1B",
    #"Nb.Spirobranchus.lamarckii.2B",
    #"Nb.Spirobranchus.lamarckii.3B",
    #"Nb.Spirobranchus.lamarckii.4B",
    #"Nb.Spirobranchus.lamarckii.5B",
    "Nb.Spirobranchus.lamarckii.total",
    #"Nb.spirorbis.1A",
    #"Nb.spirorbis.2A",
    #"Nb.spirorbis.3A",
    #"Nb.spirorbis.4A",
    #"Nb.spirorbis.5A",
    "Nb.spirorbis.total",
    "Nb.Crassostrea.gigas",
    "Nb.Ostrea.edulis",
    "X..Mytilus.sp.",
    "X..Hermelles",
    "X..Hydraires",
    "X..Eponges",
    "X..Ascidies.Coloniales",
    "X..Ascidies.Solitaires",
    "X..Bryozoaires.Dresses",
    "X..Balanes.Vivantes",
    #"Commentaires.Avant",
    "X..Surface.Accolement", # is NA, then replace by 0 as well because no sense to have a NA value for "% surface accolement" as well.
    #"Type.sustrat.observé",
    #"Commentaires",
    "Nb.Cancer.pagurus..Tourteau.",
    "Nb.Necora.puber..Etrille.",
    "Nb.Carcinus.maenas..Crabe.vert.",
    "Nb.Nucella.lapilus..Pourpre.",
    "Nb.Eriphia.verrucosa..Crabe.verruqueux.",
    "Nb.Octopus.vulgaris..Poulpe.",
    "Nb.Galathea..Galathées.",
    "Nb.Paracentrotus.lividus..Oursin.",
    "Nb.Lophozozymus.incisus..ancien.Xantho.incisus.",
    "Nb.Palaemon.sp..Crevette.bouquet.ou.crevette.rose.",
    "Nb.Haliotis.tuberculata..Ormeau.",
    "Nb.Stramonita.haemastoma..Pourpre.bouche.de.sang.",
    "Nb.Littorina.littorea..Bigorneau.",
    "Nb.Xantho.pilipes..Xanthe.poilu.",
    "Nb.Mimachlamys.varia..Pétoncle.noir."
  )
  ], function(x) replace(x, is.na(x), 0))


  # egmp_basq_bf
  egmp_basq_bf[, c(
    "X..algues.brunes",
    "Strate.algues.brunes",
    "X..algues.rouges",
    "Strate.algues.rouges",
    "X..algues.vertes",
    "Strate.algues.vertes",
    "X..Cladophora",
    "X..Lithophyllum",
    "X..Recouvrement.Sediment", # is NA, then replace by 0 as well because no sense to have a NA value for "% recouvrement sédiment" as well.
    #"Type.Sediment",
    "X..Roche.Nue", # is NA, then replace by 0 as well because no sense to have a NA value for "% roche nue" as well.
    "Nb.Littorina.obtusata",
    "Nb.Gibbula.cineraria",
    "Nb.Gibbula.pennanti",
    "Nb.Gibbula.umbilicalis",
    "Nb.Phallusia.mamillata",
    "Nb.Tethya.aurantium",
    #"Nb.Spirobranchus.lamarckii.1B",
    #"Nb.Spirobranchus.lamarckii.2B",
    #"Nb.Spirobranchus.lamarckii.3B",
    #"Nb.Spirobranchus.lamarckii.4B",
    #"Nb.Spirobranchus.lamarckii.5B",
    "Nb.Spirobranchus.lamarckii.total",
    #"Nb.spirorbis.1A",
    #"Nb.spirorbis.2A",
    #"Nb.spirorbis.3A",
    #"Nb.spirorbis.4A",
    #"Nb.spirorbis.5A",
    "Nb.spirorbis.total",
    "Nb.Crassostrea.gigas",
    "Nb.Ostrea.edulis",
    "X..Mytilus.sp.",
    "X..Hermelles",
    "X..Hydraires",
    "X..Eponges",
    "X..Ascidies.Coloniales",
    "X..Ascidies.Solitaires",
    "X..Bryozoaires.Dresses",
    "X..Balanes.Vivantes",
    #"Commentaires.Avant",
    "X..Surface.Accolement"#, # is NA, then replace by 0 as well because no sense to have a NA value for "% surface accolement" as well.
    #"Type.sustrat.observé",
    #"Commentaires",
    #."Nb.Cancer.pagurus..Tourteau.",
    #.."Nb.Necora.puber..Etrille.",
    #."Nb.Carcinus.maenas..Crabe.vert.",
    #."Nb.Nucella.lapilus..Pourpre.",
    #.."Nb.Eriphia.verrucosa..Crabe.verruqueux.",
    #.."Nb.Octopus.vulgaris..Poulpe.",
    #."Nb.Galathea..Galathées.",
    #.."Nb.Paracentrotus.lividus..Oursin.",
    #."Nb.Lophozozymus.incisus..ancien.Xantho.incisus.",
    #."Nb.Palaemon.sp..Crevette.bouquet.ou.crevette.rose.",
    #."Nb.Haliotis.tuberculata..Ormeau.",
    #."Nb.Stramonita.haemastoma..Pourpre.bouche.de.sang.",
    #."Nb.Littorina.littorea..Bigorneau.",
    #."Nb.Xantho.pilipes..Xanthe.poilu.",
    #."Nb.Mimachlamys.varia..Pétoncle.noir."
  )
  ] <- lapply(egmp_basq_bf[, c(
    "X..algues.brunes",
    "Strate.algues.brunes",
    "X..algues.rouges",
    "Strate.algues.rouges",
    "X..algues.vertes",
    "Strate.algues.vertes",
    "X..Cladophora",
    "X..Lithophyllum",
    "X..Recouvrement.Sediment", # is NA, then replace by 0 as well because no sense to have a NA value for "% recouvrement sédiment" as well.
    #"Type.Sediment",
    "X..Roche.Nue", # is NA, then replace by 0 as well because no sense to have a NA value for "% roche nue" as well.
    "Nb.Littorina.obtusata",
    "Nb.Gibbula.cineraria",
    "Nb.Gibbula.pennanti",
    "Nb.Gibbula.umbilicalis",
    "Nb.Phallusia.mamillata",
    "Nb.Tethya.aurantium",
    #"Nb.Spirobranchus.lamarckii.1B",
    #"Nb.Spirobranchus.lamarckii.2B",
    #"Nb.Spirobranchus.lamarckii.3B",
    #"Nb.Spirobranchus.lamarckii.4B",
    #"Nb.Spirobranchus.lamarckii.5B",
    "Nb.Spirobranchus.lamarckii.total",
    #"Nb.spirorbis.1A",
    #"Nb.spirorbis.2A",
    #"Nb.spirorbis.3A",
    #"Nb.spirorbis.4A",
    #"Nb.spirorbis.5A",
    "Nb.spirorbis.total",
    "Nb.Crassostrea.gigas",
    "Nb.Ostrea.edulis",
    "X..Mytilus.sp.",
    "X..Hermelles",
    "X..Hydraires",
    "X..Eponges",
    "X..Ascidies.Coloniales",
    "X..Ascidies.Solitaires",
    "X..Bryozoaires.Dresses",
    "X..Balanes.Vivantes",
    #"Commentaires.Avant",
    "X..Surface.Accolement"#, # is NA, then replace by 0 as well because no sense to have a NA value for "% surface accolement" as well.
    #"Type.sustrat.observé",
    #"Commentaires",
    #."Nb.Cancer.pagurus..Tourteau.",
    #.."Nb.Necora.puber..Etrille.",
    #."Nb.Carcinus.maenas..Crabe.vert.",
    #."Nb.Nucella.lapilus..Pourpre.",
    #.."Nb.Eriphia.verrucosa..Crabe.verruqueux.",
    #.."Nb.Octopus.vulgaris..Poulpe.",
    #."Nb.Galathea..Galathées.",
    #.."Nb.Paracentrotus.lividus..Oursin.",
    #."Nb.Lophozozymus.incisus..ancien.Xantho.incisus.",
    #."Nb.Palaemon.sp..Crevette.bouquet.ou.crevette.rose.",
    #."Nb.Haliotis.tuberculata..Ormeau.",
    #."Nb.Stramonita.haemastoma..Pourpre.bouche.de.sang.",
    #."Nb.Littorina.littorea..Bigorneau.",
    #."Nb.Xantho.pilipes..Xanthe.poilu.",
    #."Nb.Mimachlamys.varia..Pétoncle.noir."
  )
  ], function(x) replace(x, is.na(x), 0))

}

# merge dfs.
qecbnato0 <- dplyr::bind_rows(bretagne_bm, bretagne_bf)
qecbnato0 <- dplyr::bind_rows(qecbnato0, egmp_basq_bm)
qecbnato0 <- dplyr::bind_rows(qecbnato0, egmp_basq_bf)

qecbnato0 <- dplyr::arrange(qecbnato0, region, site_year_month_day, Type.Bloc, Numéro.Bloc.échantillon, Face)

rm(bretagne_bm, bretagne_bf, egmp_basq_bm, egmp_basq_bf)


## analyse matricielle

# NB some variables were dplyr::renamed or created, cfr I originally merged qecb and ivr data in below script to do some correlation analysis. This is not the case anymore, so no more merging anymore.

qecbnato0 <- tibble::add_column(qecbnato0, region.site_year_month_day = paste0(qecbnato0$region, qecbnato0$site_year_month_day), .before = "region")


numero_quadrat <- stringr::str_sub(qecbnato0$quadrat_bis, start = -1)
qecbnato0 <- tibble::add_column(qecbnato0, numero_quadrat, .after = "quadrat_bis")
rm(numero_quadrat)
qecbnato0$numero_quadrat <- as.integer(qecbnato0$numero_quadrat)


qecbnato0$Year <- as.integer(qecbnato0$Year)
qecbnato0$Month <- as.integer(qecbnato0$Month)
qecbnato0$Day <- as.integer(qecbnato0$Day)

############################################################
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# Anna still hasn't corrected for boulder nb in FINS_Quemenes.2020.10.16 data encoding ! removed from the df_
qecbnato0 <- qecbnato0 %>% dplyr::filter(site_year_month_day != "FINS_Quemenes.2020.10.16")
############################################################

# what to do with spirorbes & Nb.Spirobranchus.lamarckii.total? log10 transformation

qecbnato0 <- tibble::add_column(qecbnato0, log10.Nb.spirorbis.total = log10(qecbnato0$Nb.spirorbis.total + 1), .after = "Nb.spirorbis.total")
qecbnato0 <- tibble::add_column(qecbnato0, log10.Nb.Spirobranchus.lamarckii.total = log10(qecbnato0$Nb.Spirobranchus.lamarckii.total + 1), .after = "Nb.Spirobranchus.lamarckii.total")

saveRDS(qecbnato0, "qecbnato0.RDS")

