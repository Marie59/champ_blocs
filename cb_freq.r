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
    input_hab <- args[1]
    input_site <- args[2]
    input_soussite <- args[3]
    fiche_val <- args[4]
    input_obs <- args[5]
    fiche_2 <- args[6]
    input_qecb <- args[7]

}



freq_hab <- read.csv2(input_hab, fileEncoding = "Latin1")
freq_hab$status <- "valide"

freq_site <- read.csv2(input_site, fileEncoding = "Latin1")
freq_site$status <- "valide"

freq_ss_site <- read.csv2(input_soussite, fileEncoding = "Latin1")
freq_ss_site$status <- "valide"

ficheterrain <- read.csv2(fiche_val, fileEncoding = "Latin1")
ficheterrain$date.sortie <- as.Date(ficheterrain$date.sortie)


# only keep frequentation data related to boulder field in freqh.hab. df

freq_hab <- dplyr::filter(freq_hab, grepl(c("champs | Champs | blocs"), Libellé.Habitat))


# make coinciding site names between frequentation df and qecb df

qecbnew <- readRDS(input_qecb)

freq_site <- dplyr::filter(freq_site, Code.Site %in% c(sort(unique(qecbnew$code.site))))

freq_ss_site <- dplyr::filter(freq_ss_site, Code.Site %in% c(sort(unique(qecbnew$code.site))))
`%notin%` <- Negate(`%in%`)
freq_ss_site <- dplyr::filter(freq_ss_site, Code.Sous.Site %notin% c("ARMO_082", "ARMO_153", "EGMP_114_2", "EGMP_015"))


# some other possibilities to merge df based on a common variable?
# obviously Code.Site is the only variable with common observations/values

ficheterrain <- dplyr::filter(ficheterrain, code.site %in% c(sort(unique(qecbnew$code.site))))


# intersect between ficheterrain and data according to ID.Fiche variable, cfr Elodie Gamp discu.

ficheterrain <- dplyr::rename(ficheterrain, Libellé.Sortie = libellé.sortie)

freq_site <- dplyr::left_join(freq_site, ficheterrain, by = c("ID.Fiche"
                                                       , "Libellé.Sortie"
))
freq_ss_site <- dplyr::left_join(freq_ss_site, ficheterrain, by =  c("ID.Fiche"
                                                    , "Libellé.Sortie"
))
freq_hab <- dplyr::left_join(freq_hab, ficheterrain, by = c("ID.Fiche"
                                                       , "Libellé.Sortie"
))

# I noticed an issue with -999 data instead of NAs for freq_hab df I correct it here below.
min(freq_hab$Nb.Total)
freq_hab$Nb.Total <- ifelse(freq_hab$Nb.Total == min(freq_hab$Nb.Total), NA, freq_hab$Nb.Total)

unique(freq_hab[, c("code.site", "Code.Habitat", "Libellé.Habitat")])
# only Flots Bleus is repeated: 1 site, 2 ss-sites


# insert site name based on qecb df

(df <- unique(qecbnew[, c("code.site", "Site", "Site_bis")]))

###########################################################
# Show df_join table throughout the process to check for site name correspondence.
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!
###########################################################

(df_join <- unique(freq_hab[, c("code.site", "site", "Code.Habitat", "Libellé.Habitat")]))
(df_join <- dplyr::left_join(df_join, df, by = "code.site"))
###########################################################
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# check below kine after running code because lines to be removed might need to be updated !! or code another way with character strings
(df_join <- df_join[-c(9,10),]) # current
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!
###########################################################
freq_hab$Site <- NA
freq_hab$Site <- as.character(freq_hab$Site)
library(data.table) # multiple ifelse statement doesn't work, so I used another function from package data.table
for (i in c(1:nrow(df_join))) {
  #i <- 1
  setDT(freq_hab)[code.site == df_join[i, "code.site"] & Code.Habitat == df_join[i, "Code.Habitat"], Site := df_join[i, "Site"]]
}
freq_hab <- data.frame(freq_hab)
# NB: from table to dataframe space " " are replace by a ".", therefore colnames changes, e.g. below for "code.site" and "Code.Habitat".

freq_hab <- tibble::add_column(freq_hab[, c(1:(ncol(freq_hab)-1))], Site = freq_hab$Site, .after = "Libellé.Habitat")
freq_hab$Site_bis <- NA
freq_hab$Site_bis <- as.character(freq_hab$Site_bis)
library(data.table) # multiple ifelse statement doesn't work, so I used another function from package data.table
for (i in c(1:nrow(df_join))) {
  #i <- 1
  setDT(freq_hab)[code.site == df_join[i, "code.site"] & Code.Habitat == df_join[i, "Code.Habitat"], Site_bis := df_join[i, "Site_bis"]]
}
freq_hab <- data.frame(freq_hab)

freq_hab <- tibble::add_column(freq_hab[, c(1:(ncol(freq_hab)-1))], Site_bis = freq_hab$Site_bis, .after = "Site")

rm(df_join)

(df_join <- unique(freq_site[, c("Code.Site", "Libellé.Site"
                                 #, "site"
                                 )]))
df_join <- dplyr::rename(df_join, code.site = Code.Site)
(df_join <- dplyr::left_join(df_join, df, by = "code.site"))
df_join$Site <- ifelse(df_join$code.site == "BASQ_01", "BASQ_FlotsBleus", df_join$Site)
df_join$Site <- ifelse(df_join$code.site == "ARMO_042-043", "ARMO_Piégu / Verdelet", df_join$Site)
df_join$Site_bis <- ifelse(df_join$code.site == "BASQ_01", "Les Flots Bleus", df_join$Site_bis)
df_join$Site_bis <- ifelse(df_join$code.site == "ARMO_042-043", "Îlot du Verdelet / Piégu", df_join$Site_bis)
df_join <- df_join[!duplicated(df_join), ]
freq_site$Site <- NA
freq_site$Site <- as.character(freq_site$Site)
library(data.table)
for (i in c(1:nrow(df_join))) {
  #i <- 1
  setDT(freq_site)[Code.Site == df_join[i, "code.site"], Site := df_join[i,"Site"]]
}
freq_site <- data.frame(freq_site)

freq_site <- tibble::add_column(freq_site[, c(1:(ncol(freq_site)-1))], Site = freq_site$Site, .after = "Libellé.Site")
# check for Flots Bleus and Piégu / Verdelet ?
freq_site$Site_bis <- NA
freq_site$Site_bis <- as.character(freq_site$Site_bis)
library(data.table)
for (i in c(1:nrow(df_join))) {
  #i <- 1
  setDT(freq_site)[Code.Site == df_join[i, "code.site"], Site_bis := df_join[i,"Site_bis"]]
}
freq_site <- data.frame(freq_site)

freq_site <- tibble::add_column(freq_site[, c(1:(ncol(freq_site)-1))], Site_bis = freq_site$Site_bis, .after = "Site")
# check for Flots Bleus and Piégu / Verdelet ?
rm(df_join)


(df_join <- unique(freq_ss_site[, c("Code.Site", "Libellé.Site", "Code.Sous.Site", "Libellé.Sous.Site")]))
df_join <- dplyr::rename(df_join, code.site = Code.Site)
(df_join <- dplyr::left_join(df_join, df, by = "code.site"))
###########################################################
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# check below kine after running code because lines to be removed might need to be updated !! or code another way with character strings
(df_join <- df_join[-c(1,5),]) # current
###########################################################
freq_ss_site$Site <- NA
freq_ss_site$Site <- as.character(freq_ss_site$Site)
library(data.table)
for (i in c(1:nrow(df_join))) {
  #i <- 1
  setDT(freq_ss_site)[Code.Site == df_join[i, "code.site"] & Code.Sous.Site == df_join[i, "Code.Sous.Site"], Site := df_join[i,"Site"]]
}
freq_ss_site <- data.frame(freq_ss_site)

freq_ss_site <- tibble::add_column(freq_ss_site[, c(1:(ncol(freq_ss_site)-1))], Site = freq_ss_site$Site, .after = "Libellé.Site")
# check for Piégu / Verdelet ?

freq_ss_site$Site_bis <- NA
freq_ss_site$Site_bis <- as.character(freq_ss_site$Site_bis)
library(data.table)
for (i in c(1:nrow(df_join))) {
  #i <- 1
  setDT(freq_ss_site)[Code.Site == df_join[i, "code.site"] & Code.Sous.Site == df_join[i, "Code.Sous.Site"], Site_bis := df_join[i,"Site_bis"]]
}
freq_ss_site <- data.frame(freq_ss_site)

freq_ss_site <- tibble::add_column(freq_ss_site[, c(1:(ncol(freq_ss_site)-1))], Site_bis = freq_ss_site$Site_bis, .after = "Site")
# check for Piégu / Verdelet ?

rm(df_join)

rm(df)


# Conclusions from data mining (not shown) regarding frequentation data ; considerations required for better understanding of the script. not needed I guess in an application.

# for freq_hab, consider $Nb.Total var.
# for freq_site, consider $Nb.Total and $Nb.Pecheurs.Site var. (also eventually add $`Nb Pecheurs Arrivé` to $Nb.Pecheurs.Site to have a more exhaustif nb.); NB: $Nb.Pecheurs.Site is major part of $Nb.Total, but not always specified as such, so better to only consider $Nb.Total 
# Ccl.: all count data in freq_ss_site are within freq_site, and freq_hab data are little, and no correlation between both site and habitat count ! 


# see now if df are complementary with regards to frequentation
# NB: Now we consider "Site" according to geographic level, i.e. Site vs Ss.Site vs Habitat (cfr e.g. FlotsBleus, with 2 Ss.Sites).

freq_habred_ <- freq_hab[, c("Code.Habitat", "Libellé.Habitat", "code.site", "Site", "Site_bis", "site", "date.sortie", "Nb.Total", "Nb.Adultes", "Nb.Enfants")]

colnames(freq_habred_)[c(3, 8:10)] <- c(paste0("frha_", names(freq_habred_[, c(3, 8:10)])))
freq_habred_$Site.date.sortie <- paste0(freq_habred_$Site, ".", freq_habred_$date.sortie)

# add replicate number when several counts the same date

freq_habred_ <- freq_habred_ %>% 
  dplyr::group_by(Site.date.sortie) %>%
  dplyr::mutate(repl. = dplyr::row_number(Site.date.sortie))
freq_habred_ <- data.frame(freq_habred_)

freq_ss_site_red_ <- freq_ss_site[, c("Code.Sous.Site", "Libellé.Sous.Site", "Code.Site", "Libellé.Site", "Site", "Site_bis", "site", "date.sortie", "Nb.Total", "Nb.Adultes", "Nb.Enfants")]
colnames(freq_ss_site_red_)[c(3, 4, 9:11)] <- c(paste0("frsssi_", names(freq_ss_site_red_[, c(3, 4, 9:11)])))
freq_ss_site_red_$Site.date.sortie <- paste0(freq_ss_site_red_$Site, ".", freq_ss_site_red_$date.sortie)
freq_ss_site_red_ <- dplyr::arrange(freq_ss_site_red_, Site, date.sortie, frsssi_Nb.Total)
freq_ss_site_red_ <- freq_ss_site_red_ %>% 
  dplyr::group_by(Site.date.sortie) %>%
  dplyr::mutate(repl. = dplyr::row_number(Site.date.sortie))
freq_ss_site_red_ <- data.frame(freq_ss_site_red_)

freq_site_red_ <- freq_site[, c("Code.Site", "Libellé.Site", "Site", "Site_bis", "site", "date.sortie", "Nb.Total", "Nb.Adultes", "Nb.Enfants", "Nb.Pecheurs.Site", "Nb.Pecheurs.Arrivee", "Nb.Pecheurs.Départ", "Nb.Pecheurs.Zone.Interdite")]
colnames(freq_site_red_)[c(1, 2, 7:13)] <- c(paste0("frsi_", names(freq_site_red_[, c(1, 2, 7:13)])))
freq_site_red_$Site.date.sortie <- paste0(freq_site_red_$Site, ".", freq_site_red_$date.sortie)
freq_site_red_ <- dplyr::arrange(freq_site_red_, Site, date.sortie, frsi_Nb.Total)
freq_site_red_ <- freq_site_red_ %>% 
  dplyr::group_by(Site.date.sortie) %>%
  dplyr::mutate(repl. = dplyr::row_number(Site.date.sortie))
freq_site_red_ <- data.frame(freq_site_red_)


df <- dplyr::full_join(freq_site_red_#[, c("Site", "date.sortie", "Site.date.sortie", "repl.", "Nb.Total.freq_site")]
                 , freq_ss_site_red_#[, c("Site", "date.sortie", "Site.date.sortie", "repl.", "Nb.Total.freq_ss_site")]
                 , by = c("Site", "Site_bis", "site", "date.sortie", "Site.date.sortie", "repl."))
df <- dplyr::full_join(df, freq_habred_#[, c("Site", "date.sortie", "Site.date.sortie", "repl.", "Nb.Total.freq_hab")]
                 , by = c("Site", "Site_bis", "site", "date.sortie", "Site.date.sortie", "repl."))


df <- dplyr::arrange(df, Site, date.sortie, repl.)
freq_ <- data.frame(df)
freq_ <- freq_[, c(3:6, 14, 15, 1, 2, 7:13, 16:28)]
rm(df)


freq_ <- tidyr::separate(freq_, date.sortie, into = c("Annee", "Mois", "Jour"), remove = FALSE)
freq_$Annee <- as.numeric(freq_$Annee)
freq_$Mois <- as.numeric(freq_$Mois)
freq_$Jour <- as.numeric(freq_$Jour)


# prior saving, I will create 3 new variables in the freq dataset: one with freq data, one with the corresponding origin variable, one for the diff between frequ data 

freq_$Fr_Nb <- NA

for (i in 1:nrow(freq_)){
if (!is.na(freq_$frsi_Nb.Total[i])) {
  freq_$Fr_Nb[i] = freq_$frsi_Nb.Total[i]
  } else if (is.na(freq_$frsi_Nb.Total[i]) & !is.na(freq_$frsi_Nb.Pecheurs.Site[i])) {
    freq_$Fr_Nb[i] = freq_$frsi_Nb.Pecheurs.Site[i]
    } else if (is.na(freq_$frsi_Nb.Total[i]) & !is.na(freq_$frsssi_Nb.Total[i])) {
      freq_$Fr_Nb[i] = freq_$frsssi_Nb.Total[i] 
      } else {
        freq_$Fr_Nb[i] = freq_$frha_Nb.Total[i]
      }
  }


freq_$Fr_var <- NA

for (i in 1:nrow(freq_)){
  if (!is.na(freq_$frsi_Nb.Total[i])) {
    freq_$Fr_var[i] = "frsi_Nb.Total"
  } else if (is.na(freq_$frsi_Nb.Total[i]) & !is.na(freq_$frsi_Nb.Pecheurs.Site[i])) {
    freq_$Fr_var[i] = "frsi_Nb.Pecheurs.Site"
  } else if (is.na(freq_$frsi_Nb.Total[i]) & !is.na(freq_$frsssi_Nb.Total[i])) {
    freq_$Fr_var[i] = "frsssi_Nb.Total" 
  } else {
    freq_$Fr_var[i] = "frha_Nb.Total"
  }
}

freq_$Fr_diff <- NA
  
for (i in 1:nrow(freq_)){
freq_$Fr_diff[i]  <- abs(max(freq_[i, c("frsi_Nb.Total", "frsi_Nb.Pecheurs.Site", "frsssi_Nb.Total", "frha_Nb.Total")], na.rm = TRUE) - freq_$Fr_Nb[i])
}


# final plot

# almost only unique value !
freq_ %>%
  dplyr::group_by(repl.) %>%
  dplyr::summarise(nb = dplyr::n())   

ymin_ <- min(freq_$Annee)
ymax_ <- max(freq_$Annee)

for (i in c(1:length(unique(freq_[, "Site"])))) {
  
  #i <- 3

  freq_i <- dplyr::filter(freq_, Site == unique(freq_[, "Site"])[i])

  xmin_ <- as.Date(ifelse(min(freq_i$Annee, na.rm = TRUE) >= 2014, as.Date("2014-01-01", origin = "1970-01-01"), as.Date(paste0(ymin_, "-01-01"), origin = "1970-01-01")), origin = "1970-01-01")
  xmax_ <- as.Date(ifelse(max(freq_i$Annee, na.rm = TRUE) <= 2017, as.Date("2018-01-01", origin = "1970-01-01"), as.Date(paste0((ymax_ + 1), "-01-01"), origin = "1970-01-01")), origin = "1970-01-01")
  
  # I have to invert plot(x,y) by plot(y,x) in order to work ... ?
  freq_plot <- ggplot2::ggplot() +
  #ggplot2::geom_point(ggplot2::aes(x = freq_$date.sortie, y = freq_$frsi_Nb.Total), col = "grey") +
  #ggplot2::geom_point(ggplot2::aes(x = freq_$date.sortie, y = freq_$frsssi_Nb.Total), col = "grey") +
  #ggplot2::geom_point(ggplot2::aes(x = freq_$date.sortie, y = freq_$frha_Nb.Total), col = "grey") +
  ggplot2::geom_point(ggplot2::aes(x = freq_i$date.sortie, y = freq_i$frsi_Nb.Total), col = "red") +
  ggplot2::geom_point(ggplot2::aes(x = freq_i$date.sortie, y = freq_i$frsssi_Nb.Total), col = "darkblue") +
  ggplot2::geom_point(ggplot2::aes(x = freq_i$date.sortie, y = freq_i$frha_Nb.Total), col = "forestgreen") +
  ggplot2::xlab("Date") +
  ggplot2::ylab("fréquentation") +
  ggplot2::ggtitle(unique(freq_i$Site)) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1), legend.position = "bottom")

ggplot2::ggsave(paste0("freq_", unique(freq_i$Site), ".png"), freq_plot, height = 3, width = 3.5)

}


# summarize data by date then season

# remove data if any (not the case anymore here) without date.sortie value.

freq_ <- freq_ %>% dplyr::filter(!is.na(freq_$date.sortie))

table((na.omit(freq_[, c("frsi_Code.Site", "Site.date.sortie", "frsi_Nb.Total")]) %>%
  dplyr::group_by(frsi_Code.Site, Site.date.sortie) %>%
  dplyr::summarise(nb = dplyr::n()) )["nb"])  
table((na.omit(freq_[, c("Code.Sous.Site", "Site.date.sortie", "frsssi_Nb.Total")]) %>%
          dplyr::group_by(Code.Sous.Site, Site.date.sortie) %>%
          dplyr::summarise(nb = dplyr::n()) )["nb"])  
table((na.omit(freq_[, c("Code.Habitat", "Site.date.sortie", "frha_Nb.Total")]) %>%
          dplyr::group_by(Code.Habitat, Site.date.sortie) %>%
          dplyr::summarise(nb = dplyr::n()) )["nb"]) 

# median and mean of two values are identical, so dplyr::filter for nb > 2
# and for remaining data, I'll consider the mean value cfr df above

# first add a semester variable

freq_$Mois <- as.numeric(freq_$Mois)
freq_$semester <- ifelse(freq_$Mois %in% c(1:6), "s1", "s2")
freq_$semester <- as.factor(freq_$semester)
freq_$Annee_semester <- paste0(freq_$Annee, "_", freq_$semester)


frsi <- na.omit(freq_[, c("Site", "Site_bis", "frsi_Code.Site", "Site.date.sortie", "Annee_semester", "frsi_Nb.Total")]) %>%
  dplyr::group_by(Site, Site_bis, frsi_Code.Site, Site.date.sortie, Annee_semester) %>%
  dplyr::summarise(frsi_Nb.Total = mean(frsi_Nb.Total), nb = dplyr::n())
frsi <- data.frame(frsi)


frsssi <- na.omit(freq_[, c("Site", "Site_bis", "Code.Sous.Site", "Site.date.sortie", "Annee_semester", "frsssi_Nb.Total")]) %>%
  dplyr::group_by(Site, Site_bis, Code.Sous.Site, Site.date.sortie, Annee_semester) %>%
  dplyr::summarise(frsssi_Nb.Total = mean(frsssi_Nb.Total), nb = dplyr::n())
frsssi <- data.frame(frsssi)


frha <- na.omit(freq_[, c("Site", "Site_bis", "Code.Habitat", "Site.date.sortie", "Annee_semester", "frha_Nb.Total")]) %>%
  dplyr::group_by(Site, Site_bis, Code.Habitat, Site.date.sortie, Annee_semester) %>%
  dplyr::summarise(frha_Nb.Total = mean(frha_Nb.Total), nb = dplyr::n())
frha <- data.frame(frha)



frsi_stat <- na.omit(frsi[, c("Site", "Site_bis", "frsi_Code.Site", "Site.date.sortie", "Annee_semester", "frsi_Nb.Total")]) %>%
  dplyr::group_by(Site, Site_bis, frsi_Code.Site, Annee_semester) %>%
  dplyr::summarise(mean.frsi_Nb.Total = mean(frsi_Nb.Total), median.frsi_Nb.Total = median(frsi_Nb.Total), min.frsi_Nb.Total = min(frsi_Nb.Total), max.frsi_Nb.Total = max(frsi_Nb.Total), nb.frsi_Nb.Total = dplyr::n())
frsi_stat <- data.frame(frsi_stat)


frsssi_stat <- na.omit(frsssi[, c("Site", "Site_bis", "Code.Sous.Site", "Site.date.sortie", "Annee_semester", "frsssi_Nb.Total")]) %>%
  dplyr::group_by(Site, Site_bis, Code.Sous.Site, Annee_semester) %>%
  dplyr::summarise(mean.frsssi_Nb.Total = mean(frsssi_Nb.Total), median.frsssi_Nb.Total = median(frsssi_Nb.Total), min.frsssi_Nb.Total = min(frsssi_Nb.Total), max.frsssi_Nb.Total = max(frsssi_Nb.Total), nb.frsssi_Nb.Total = dplyr::n())
frsssi_stat <- data.frame(frsssi_stat)


frha_stat <- na.omit(frha[, c("Site", "Site_bis", "Code.Habitat", "Site.date.sortie", "Annee_semester", "frha_Nb.Total")]) %>%
  dplyr::group_by(Site, Site_bis, Code.Habitat, Annee_semester) %>%
  dplyr::summarise(mean.frha_Nb.Total = mean(frha_Nb.Total), median.frha_Nb.Total = median(frha_Nb.Total), min.frha_Nb.Total = min(frha_Nb.Total), max.frha_Nb.Total = max(frha_Nb.Total), nb.frha_Nb.Total = dplyr::n())
frha_stat <- data.frame(frha_stat)

freq_nb_total_stat <- dplyr::full_join(frsi_stat, frsssi_stat, by = c("Site", "Site_bis", "Annee_semester"))
freq_nb_total_stat <- dplyr::full_join(freq_nb_total_stat, frha_stat, by = c("Site", "Site_bis", "Annee_semester"))

#missing Annee_semester value and site value; add dummy observations to fill in gaps

freq_nb_total_stat[nrow(freq_nb_total_stat) + 2, ] <- NA
freq_nb_total_stat[nrow(freq_nb_total_stat) - 1, "Annee_semester"]  <- "2012_s1"
freq_nb_total_stat[nrow(freq_nb_total_stat), "Annee_semester"]  <- "2017_s1"

freq_nb_total_stat$Annee_semester_to.nb <- freq_nb_total_stat$Annee_semester
freq_nb_total_stat$Annee_semester_to.nb <- as.factor(freq_nb_total_stat$Annee_semester_to.nb)

levels(freq_nb_total_stat$Annee_semester_to.nb) <- c(1:length(sort(unique(freq_nb_total_stat$Annee_semester_to.nb))))
freq_nb_total_stat$Annee_semester_to.nb <- as.numeric(freq_nb_total_stat$Annee_semester_to.nb)

#saveRDS(freq_nb_total_stat, "freq_ & cptmt/freq_nb_total_stat.RDS")

op <- par(mar = c(8, 4, 4, 2) + 0.1) ## default is c(5,4,4,2) + 0.1



# par(mfrow = c(1,1))
par(op)

# to be able to run the Rmarkdown loop

freq_$Site <- as.factor(freq_$Site)
freq_$Site <- ordered(freq_$Site)
freq_$Site.nb <- freq_$Site
levels(freq_$Site.nb) <- 1:length(unique(freq_$Site))
freq_$Site.nb <- as.integer(freq_$Site.nb)


freq_$Site.nb <- ifelse(freq_$Site %in% c("ARMO_Piegu", "ARMO_Verdelet") == TRUE, 
                     unique(dplyr::filter(freq_, Site == "ARMO_Piégu / Verdelet")[,"Site.nb"]),
                     freq_$Site.nb)
freq_$Site.nb <- ifelse(freq_$Site %in% c("BASQ_FlotsBleusZF", "BASQ_FlotsBleusZP"), 
                     unique(dplyr::filter(freq_, Site == "BASQ_FlotsBleus")[, "Site.nb"]),
                     freq_$Site.nb)


saveRDS(freq_, "freq.RDS")


freq_nb_total_stat$Site <- as.factor(freq_nb_total_stat$Site)
freq_nb_total_stat$Site  <- ordered(freq_nb_total_stat$Site)
freq_nb_total_stat$Site.nb <- freq_nb_total_stat$Site
levels(freq_nb_total_stat$Site.nb) <- 1:length(unique(freq_nb_total_stat$Site))
freq_nb_total_stat$Site.nb <- as.integer(freq_nb_total_stat$Site.nb)

freq_nb_total_stat$Site.nb <- ifelse(freq_nb_total_stat$Site %in% c("ARMO_Piegu", "ARMO_Verdelet") == TRUE, 
                          unique(dplyr::filter(freq_nb_total_stat, Site == "ARMO_Piégu / Verdelet")[,"Site.nb"]),
                          freq_nb_total_stat$Site.nb)
freq_nb_total_stat$Site.nb <- ifelse(freq_nb_total_stat$Site %in% c("BASQ_FlotsBleusZF", "BASQ_FlotsBleusZP"), 
                          unique(dplyr::filter(freq_nb_total_stat, Site == "BASQ_FlotsBleus")[, "Site.nb"]),
                          freq_nb_total_stat$Site.nb)



## Work on comportment data now


obs_ <- read.csv2(input_obs, fileEncoding = "Latin1")

fiche <- read.csv2(fiche_2, fileEncoding = "Latin1")

obs_ <- dplyr::left_join(obs_, fiche)

obs_$date.sortie <- as.Date(obs_$date.sortie, origin = "1970-01-01")


obs_$Heure.Debut <- ifelse(obs_$Heure.Debut == "", NA, obs_$Heure.Debut)
obs_$Heure.Fin <- ifelse(obs_$Heure.Debut == "", NA, obs_$Heure.Fin)
obs_ <- tibble::add_column(obs_, Tps.obs = difftime(strptime(obs_$Heure.Fin, format = "%H:%M"), strptime(obs_$Heure.Debut, format = "%H:%M"), units = "mins"), .after = "Heure.Fin")
table(obs_$Tps.obs)["15"]
obs_$Tps.obs <- as.integer(obs_$Tps.obs)

qecbnew <- readRDS(input_qecb)

# insert site name based on qecb df

(df <- unique(qecbnew[, c("code.site", "Site", "Site_bis")]))

(df_join <- unique(obs_[, c("code.site", "zone.habitat"
)]))
(df_join <- dplyr::left_join(df_join, df, by = "code.site"))
df_join <- df_join[!(df_join$code.site == "ARMO_042-043" & df_join$Site == "ARMO_Piegu"),]
df_join <- df_join[!(df_join$zone.habitat == "Les Flots Bleus (champ de blocs) - zone pècheurs" & df_join$Site == "BASQ_FlotsBleusZF"),]
df_join <- df_join[!(df_join$zone.habitat == "Les Flots Bleus (champ de blocs) - zone famille" & df_join$Site == "BASQ_FlotsBleusZP"),]

library(data.table)
for (i in c(1:nrow(df_join))) {
  #i <- 1
  setDT(obs_)[code.site == df_join[i, "code.site"], Site := df_join[i,"Site"]]
}
obs_ <- data.frame(obs_)

obs_$Site <- ifelse(obs_$code.site == "BASQ_01" & obs_$zone.habitat == "Les Flots Bleus (champ de blocs) - zone pècheurs", "BASQ_FlotsBleusZP", obs_$Site)

rm(df_join)
rm(df)

obs_ <- tibble::add_column(obs_, Site = obs_$Site, .after = "code.site")
obs_ <- obs_[ , !names(obs_) %in% c("Site")]
obs_ <- dplyr::rename(obs_, Site = Site.1)

# There are some missing time data, either because there was no fishers, either because data were not encoded. Has to be completed.
obs_$Tps.obs <- ifelse(!is.na(obs_$Heure.Debut) & is.na(obs_$Tps.obs), 15, obs_$Tps.obs)

obs_ <- tibble::add_column(obs_, Nb.Blocs.Retournes.remis.norm15min = obs_$Nb.Blocs.Retournes.remis/obs_$Tps.obs * 15, .before = "date.sortie")
obs_ <- tibble::add_column(obs_, Nb.Blocs.Deplaces.non.remis.norm15min = obs_$Nb.Blocs.Deplaces.non.remis/obs_$Tps.obs * 15, .before = "date.sortie")
obs_ <- tibble::add_column(obs_, Nb.Blocs.Retournes.non.remis.norm15min = obs_$Nb.Blocs.Retournes.non.remis/obs_$Tps.obs * 15, .before = "date.sortie")

# some issues with encoded data

check <- dplyr::filter(obs_, is.na(obs_$Nb.Blocs.Retournes.remis) | is.na(obs_$Nb.Blocs.Retournes.remis.norm15min) | is.na(obs_$Nb.Blocs.Retournes.non.remis) | is.na(obs_$Nb.Blocs.Retournes.non.remis.norm15min) | is.na(obs_$Nb.Blocs.Deplaces.non.remis) | is.na(obs_$Nb.Blocs.Deplaces.non.remis.norm15min))
obs_$Nb.Blocs.Retournes.non.remis <- ifelse(obs_$ID.Fiche == 373166, 0, obs_$Nb.Blocs.Retournes.non.remis)
obs_$Nb.Blocs.Retournes.non.remis.norm15min <- ifelse(obs_$ID.Fiche == 373166, 0, obs_$Nb.Blocs.Retournes.non.remis.norm15min)
obs_$Nb.Blocs.Deplaces.non.remis <- ifelse(obs_$ID.Fiche == 373166, 0, obs_$Nb.Blocs.Deplaces.non.remis)
obs_$Nb.Blocs.Deplaces.non.remis.norm15min <- ifelse(obs_$ID.Fiche == 373166, 0, obs_$Nb.Blocs.Deplaces.non.remis.norm15min)
check <- dplyr::filter(obs_, is.na(obs_$Nb.Blocs.Retournes.remis) | is.na(obs_$Nb.Blocs.Retournes.remis.norm15min) | is.na(obs_$Nb.Blocs.Retournes.non.remis) | is.na(obs_$Nb.Blocs.Retournes.non.remis.norm15min) | is.na(obs_$Nb.Blocs.Deplaces.non.remis) | is.na(obs_$Nb.Blocs.Deplaces.non.remis.norm15min))
obs_ <- dplyr::filter(obs_, ID.Fiche %notin% c(check$ID.Fiche))

# add replicate numbers for fishers observed during the same survey

obs_ <- tibble::add_column(obs_, Site.date.sortie = paste0(obs_$Site, ".", obs_$date.sortie), .after = "Site")

obs_ <- obs_ %>% 
  dplyr::group_by(Site.date.sortie) %>%
  dplyr::mutate(repl. = dplyr::row_number(Site.date.sortie))
obs_ <- data.frame(obs_)

# add a unique variable for the three obs_ cptmt., with ponderation.

ratio_ <- ((obs_$Nb.Blocs.Retournes.remis * 0) + (obs_$Nb.Blocs.Deplaces.non.remis * 0.5) + (obs_$Nb.Blocs.Retournes.non.remis * 1)) /(obs_$Nb.Blocs.Retournes.remis+obs_$Nb.Blocs.Deplaces.non.remis+obs_$Nb.Blocs.Retournes.non.remis)
obs_ <- tibble::add_column(obs_, ratio_obs = ratio_, .after = "Nb.Blocs.Retournes.non.remis")
obs_$ratio_obs <- ifelse(obs_$Nb.Blocs.Retournes.remis == 0 & obs_$Nb.Blocs.Deplaces.non.remis == 0 & obs_$Nb.Blocs.Retournes.non.remis == 0, 0, obs_$ratio_obs)

ratio_norm15min <- ((obs_$Nb.Blocs.Retournes.remis.norm15min * 0) + (obs_$Nb.Blocs.Deplaces.non.remis.norm15min * 0.5) + (obs_$Nb.Blocs.Retournes.non.remis.norm15min * 1)) /(obs_$Nb.Blocs.Retournes.remis.norm15min+obs_$Nb.Blocs.Deplaces.non.remis.norm15min+obs_$Nb.Blocs.Retournes.non.remis.norm15min)
obs_ <- tibble::add_column(obs_, ratio_norm15min.obs = ratio_norm15min, .after = "Nb.Blocs.Retournes.non.remis.norm15min")
obs_$ratio_norm15min.obs <- ifelse(obs_$Nb.Blocs.Retournes.remis.norm15min == 0 & obs_$Nb.Blocs.Deplaces.non.remis.norm15min == 0 & obs_$Nb.Blocs.Retournes.non.remis.norm15min == 0, 0, obs_$ratio_norm15min.obs)


saveRDS(obs_, "obs.RDS")


xmin_ <- as.Date("2013-01-01", origin = "1970-01-01")
xmax_ <- as.Date("2022-01-01", origin = "1970-01-01")

ymax_ = max(c(obs_$Nb.Blocs.Deplaces.non.remis, obs_$Nb.Blocs.Deplaces.non.remis.norm15min, obs_$Nb.Blocs.Retournes.non.remis, obs_$Nb.Blocs.Retournes.non.remis.norm15min, obs_$Nb.Blocs.Retournes.remis, obs_$Nb.Blocs.Retournes.remis.norm15min), na.rm = TRUE)


par(mfrow = c(1, 1))

for (i in c(1:length(unique(obs_$Site)))) {
  
  #i <- 5
  
  obs_i <- dplyr::filter(obs_, Site == unique(obs_$Site)[i])
  
  ymax_ <- max(c(obs_i$Nb.Blocs.Deplaces.non.remis, obs_i$Nb.Blocs.Deplaces.non.remis.norm15min, obs_i$Nb.Blocs.Retournes.non.remis, obs_i$Nb.Blocs.Retournes.non.remis.norm15min, obs_i$Nb.Blocs.Retournes.remis, obs_i$Nb.Blocs.Retournes.remis.norm15min), na.rm = TRUE)
  

  obs_plot <- ggplot2::ggplot()  +
  #ggplot2::geom_point(ggplot2::aes(x = obs_$date.sortie, y = obs_$$Nb.Blocs.Retournes.remis.norm15min), col = "Other sites good behaviour", alpha = 0.2) +
  ggplot2::geom_point(ggplot2::aes(obs_i$date.sortie, obs_i$Nb.Blocs.Retournes.remis.norm15min, colour = "Good : put the bloc back")) +
  ggplot2::geom_point(ggplot2::aes(obs_i$date.sortie, obs_i$Nb.Blocs.Retournes.non.remis.norm15min, colour = "Bad : turned bloc")) +
  ggplot2::geom_point(ggplot2::aes(obs_i$date.sortie, obs_i$Nb.Blocs.Deplaces.non.remis.norm15min, colour = "Bad : moved bloc")) +
  #ggplot2::geom_point(ggplot2::aes(x = obs_$date.sortie, y = obs_$Nb.Blocs.Retournes.remis.norm15min), col = "grey") +
  #ggplot2::geom_point(ggplot2::aes(x = obs_$date.sortie, y = obs_$Nb.Blocs.Retournes.non.remis.norm15min), col = "grey") +
  #ggplot2::geom_point(ggplot2::aes(x = obs_$date.sortie, y = obs_$Nb.Blocs.Deplaces.non.remis.norm15min), col = "grey") +
  ggplot2::scale_fill_manual(values = c("#35B117", "#E12D07", "#F9A806"), name = "Behaviour", breaks = c("Good : put the bloc back", "Bad : turned bloc", "Bad : moved bloc")) +
  ggplot2::ylab("Nbr de blocs (normalisé pour 15 min d'obs_)") +
  ggplot2::xlab("Date") +
  ggplot2::ggtitle(unique(obs_i$Site)) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1), legend.title = ggplot2::element_text("Behaviour"))

  #plot(obs_i$date.sortie, obs_i$Nb.Blocs.Retournes.remis.norm15min, xlim = c(xmin_,xmax_), ylim = c(0,105)#ymax_*1.05)
#, xlab = "", ylab = "Nbr de blocs (normalisé pour 15 min d'obs_)", main = unique(obs_i$Site), col = "darkgreen")
  #points(obs_i$date.sortie, obs_i$Nb.Blocs.Retournes.non.remis.norm15min, col = "red")
  #points(obs_i$date.sortie, obs_i$Nb.Blocs.Deplaces.non.remis.norm15min, col = "orange")
  
  #legend("bottom", inset = c(0, -0.4), legend = c("ret.re.","ret.n.re.", "dépl.n.re."), pch = c(19, 19, 19), col = c("darkgreen", "red", "orange"), horiz = TRUE, bty = "n", xpd = TRUE)
  ggplot2::ggsave(paste0("obs_", unique(obs_i$Site), ".png"), obs_plot, height = 4, width = 6)
}


