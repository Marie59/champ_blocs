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
    input_qecb <- args[6]

}



freq.hab. <- read.csv2(input_hab, fileEncoding = "Latin1")
freq.hab.$status <- "valide"

freq.site <- read.csv2(input_site, fileEncoding = "Latin1")
freq.site$status <- "valide"

freq.ss.site <- read.csv2(input_soussite, fileEncoding = "Latin1")
freq.ss.site$status <- "valide"

ficheterrain <- read.csv2(fiche_val, fileEncoding = "Latin1")
ficheterrain$date.sortie <- as.Date(ficheterrain$date.sortie)


# only keep frequentation data related to boulder field in freqh.hab. df
#unique(freq.hab.$Libellé.Habitat)
freq.hab. <- dplyr::filter(freq.hab., grepl(c("champs | Champs | blocs"), Libellé.Habitat))
#unique(freq.hab.$Libellé.Habitat)


# make coinciding site names between frequentation df. and qecb df.

qecbNew <- readRDS(input_qecb)

#intersect(sort(unique(qecbNew$code.site)), sort(unique(freq.site$Code.Site)))
#setdiff(sort(unique(qecbNew$code.site)), intersect(sort(unique(qecbNew$code.site)), sort(unique(freq.site$Code.Site)))) # two sites are missing in freq.site compared to qecb data: "PNMI_02" = Quéménès & "PNMI_07" = Île de Sein - Goulenez et Île de Sein - Kilaourou. According to Anna Capietto (email Mercredi 19 Mai 2021 15:43:53): "Nous (...) n’avons pas de données de fréquentation associées à ces suivis." (i.e. qecb & ivr).
#unique(dplyr::filter(qecbNew, code.site %in% setdiff(sort(unique(qecbNew$code.site)), intersect(sort(unique(qecbNew$code.site)), sort(unique(freq.site$Code.Site)))))[c("Site", "code.site")])
freq.site <- dplyr::filter(freq.site, Code.Site %in% c(sort(unique(qecbNew$code.site))))

#intersect(sort(unique(qecbNew$code.site)), sort(unique(freq.ss.site$Code.Site)))
#intersect(sort(unique(qecbNew$code.site)), sort(unique(freq.ss.site$Code.Sous.Site)))
freq.ss.site <- dplyr::filter(freq.ss.site, Code.Site %in% c(sort(unique(qecbNew$code.site))))
`%notin%` <- Negate(`%in%`)
freq.ss.site <- dplyr::filter(freq.ss.site, Code.Sous.Site %notin% c("ARMO_082", "ARMO_153", "EGMP_114_2", "EGMP_015"))


# some other possibilities to merge df. based on a common variable?
# obviously Code.Site is the only variable with common observations/values

#sort(unique(qecbNew$code.site))
#sort(unique(ficheterrain$code.site))
#intersect(sort(unique(qecbNew$code.site)), sort(unique(ficheterrain$code.site)))
#sort(unique(freq.site$Code.Site)) # same sites
ficheterrain <- dplyr::filter(ficheterrain, code.site %in% c(sort(unique(qecbNew$code.site))))


# intersect between ficheterrain and data according to ID.Fiche variable, cfr Elodie Gamp discu.

ficheterrain <- dplyr::rename(ficheterrain, Libellé.Sortie = libellé.sortie)

freq.site <- dplyr::left_join(freq.site, ficheterrain, by = c("ID.Fiche"
                                                       , "Libellé.Sortie"
))
freq.ss.site <- dplyr::left_join(freq.ss.site, ficheterrain, by =  c("ID.Fiche"
                                                    , "Libellé.Sortie"
))
freq.hab. <- dplyr::left_join(freq.hab., ficheterrain, by = c("ID.Fiche"
                                                       , "Libellé.Sortie"
))

# I noticed an issue with -999 data instead of NAs for freq.hab. df. I correct it here below.
min(freq.hab.$Nb.Total)
freq.hab.$Nb.Total <- ifelse(freq.hab.$Nb.Total == min(freq.hab.$Nb.Total), NA, freq.hab.$Nb.Total)

unique(freq.hab.[, c("code.site", "Code.Habitat", "Libellé.Habitat")])
# only Flots Bleus is repeated: 1 site, 2 ss-sites


# insert site name based on qecb df.

(df. <- unique(qecbNew[, c("code.site", "Site", "Site_bis")]))

###########################################################
# Show df.join table throughout the process to check for site name correspondence.
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!
###########################################################

(df.join <- unique(freq.hab.[, c("code.site", "site", "Code.Habitat", "Libellé.Habitat")]))
(df.join <- dplyr::left_join(df.join, df., by = "code.site"))
###########################################################
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# check below kine after running code because lines to be removed might need to be updated !! or code another way with character strings
(df.join <- df.join[-c(9,10),]) # current
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!
###########################################################
freq.hab.$Site <- NA
freq.hab.$Site <- as.character(freq.hab.$Site)
library(data.table) # multiple ifelse statement doesn't work, so I used another function from package data.table
for (i in c(1:nrow(df.join))) {
  #i <- 1
  setDT(freq.hab.)[code.site == df.join[i, "code.site"] & Code.Habitat == df.join[i, "Code.Habitat"], Site := df.join[i,"Site"]]
}
freq.hab. <- data.frame(freq.hab.)
# NB: from table to dataframe space " " are replace by a ".", therefore colnames changes, e.g. below for "code.site" and "Code.Habitat".
#unique(freq.hab.[, c("code.site", "Code.Habitat", "Site")])
freq.hab. <- tibble::add_column(freq.hab.[, c(1:(ncol(freq.hab.)-1))], Site = freq.hab.$Site, .after = "Libellé.Habitat")
freq.hab.$Site_bis <- NA
freq.hab.$Site_bis <- as.character(freq.hab.$Site_bis)
library(data.table) # multiple ifelse statement doesn't work, so I used another function from package data.table
for (i in c(1:nrow(df.join))) {
  #i <- 1
  setDT(freq.hab.)[code.site == df.join[i, "code.site"] & Code.Habitat == df.join[i, "Code.Habitat"], Site_bis := df.join[i,"Site_bis"]]
}
freq.hab. <- data.frame(freq.hab.)
#unique(freq.hab.[, c("code.site", "Code.Habitat", "Site", "Site_bis")])
freq.hab. <- tibble::add_column(freq.hab.[, c(1:(ncol(freq.hab.)-1))], Site_bis = freq.hab.$Site_bis, .after = "Site")

rm(df.join)

#unique(freq.site[, c("sous.site", "zone.habitat")])
(df.join <- unique(freq.site[, c("Code.Site", "Libellé.Site"
                                 #, "site"
                                 )]))
df.join <- dplyr::rename(df.join, code.site = Code.Site)
(df.join <- dplyr::left_join(df.join, df., by = "code.site"))
df.join$Site <- ifelse(df.join$code.site == "BASQ_01", "BASQ_FlotsBleus", df.join$Site)
df.join$Site <- ifelse(df.join$code.site == "ARMO_042-043", "ARMO_Piégu / Verdelet", df.join$Site)
df.join$Site_bis <- ifelse(df.join$code.site == "BASQ_01", "Les Flots Bleus", df.join$Site_bis)
df.join$Site_bis <- ifelse(df.join$code.site == "ARMO_042-043", "Îlot du Verdelet / Piégu", df.join$Site_bis)
df.join <- df.join[!duplicated(df.join), ]
freq.site$Site <- NA
freq.site$Site <- as.character(freq.site$Site)
library(data.table)
for (i in c(1:nrow(df.join))) {
  #i <- 1
  setDT(freq.site)[Code.Site == df.join[i, "code.site"], Site := df.join[i,"Site"]]
}
freq.site <- data.frame(freq.site)
#unique(freq.site[, c("Code.Site", "Libellé.Site", "Site")])
freq.site <- tibble::add_column(freq.site[, c(1:(ncol(freq.site)-1))], Site = freq.site$Site, .after = "Libellé.Site")
# check for Flots Bleus and Piégu / Verdelet ?
freq.site$Site_bis <- NA
freq.site$Site_bis <- as.character(freq.site$Site_bis)
library(data.table)
for (i in c(1:nrow(df.join))) {
  #i <- 1
  setDT(freq.site)[Code.Site == df.join[i, "code.site"], Site_bis := df.join[i,"Site_bis"]]
}
freq.site <- data.frame(freq.site)

freq.site <- tibble::add_column(freq.site[, c(1:(ncol(freq.site)-1))], Site_bis = freq.site$Site_bis, .after = "Site")
# check for Flots Bleus and Piégu / Verdelet ?
#unique(dplyr::filter(freq.site, Code.Site == "BASQ_01")[, c("Code.Site", "Site_bis")])
#unique(dplyr::filter(freq.site, Code.Site == "ARMO_042-043")[, c("Code.Site", "Site_bis")])
rm(df.join)

#unique(freq.ss.site[, c("sous.site", "zone.habitat")])
(df.join <- unique(freq.ss.site[, c("Code.Site", "Libellé.Site", "Code.Sous.Site", "Libellé.Sous.Site")]))
df.join <- dplyr::rename(df.join, code.site = Code.Site)
(df.join <- dplyr::left_join(df.join, df., by = "code.site"))
###########################################################
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# check below kine after running code because lines to be removed might need to be updated !! or code another way with character strings
(df.join <- df.join[-c(1,5),]) # current
###########################################################
freq.ss.site$Site <- NA
freq.ss.site$Site <- as.character(freq.ss.site$Site)
library(data.table)
for (i in c(1:nrow(df.join))) {
  #i <- 1
  setDT(freq.ss.site)[Code.Site == df.join[i, "code.site"] & Code.Sous.Site == df.join[i, "Code.Sous.Site"], Site := df.join[i,"Site"]]
}
freq.ss.site <- data.frame(freq.ss.site)
#unique(freq.ss.site[, c("Code.Site", "Libellé.Site", "Code.Sous.Site", "Libellé.Sous.Site", "Site")])
freq.ss.site <- tibble::add_column(freq.ss.site[, c(1:(ncol(freq.ss.site)-1))], Site = freq.ss.site$Site, .after = "Libellé.Site")
# check for Piégu / Verdelet ?
#unique(dplyr::filter(freq.ss.site, Code.Site == "ARMO_042-043")[, c("Code.Site", "Site")])
freq.ss.site$Site_bis <- NA
freq.ss.site$Site_bis <- as.character(freq.ss.site$Site_bis)
library(data.table)
for (i in c(1:nrow(df.join))) {
  #i <- 1
  setDT(freq.ss.site)[Code.Site == df.join[i, "code.site"] & Code.Sous.Site == df.join[i, "Code.Sous.Site"], Site_bis := df.join[i,"Site_bis"]]
}
freq.ss.site <- data.frame(freq.ss.site)
#unique(freq.ss.site[, c("Code.Site", "Libellé.Site", "Code.Sous.Site", "Libellé.Sous.Site", "Site", "Site_bis")])
freq.ss.site <- tibble::add_column(freq.ss.site[, c(1:(ncol(freq.ss.site)-1))], Site_bis = freq.ss.site$Site_bis, .after = "Site")
# check for Piégu / Verdelet ?
#unique(dplyr::filter(freq.ss.site, Code.Site == "ARMO_042-043")[, c("Code.Site", "Site", "Site_bis")])
rm(df.join)

rm(df.)


# Conclusions from data mining (not shown) regarding frequentation data ; considerations required for better understanding of the script. not needed I guess in an application.

# for freq.hab., consider $Nb.Total var.
# for freq.site, consider $Nb.Total and $Nb.Pecheurs.Site var. (also eventually add $`Nb Pecheurs Arrivé` to $Nb.Pecheurs.Site to have a more exhaustif nb.); NB: $Nb.Pecheurs.Site is major part of $Nb.Total, but not always specified as such, so better to only consider $Nb.Total 
# Ccl.: all count data in freq.ss.site are within freq.site, and freq.hab. data are little, and no correlation between both site and habitat count ! 


# see now if df are complementary with regards to frequentation
# NB: Now we consider "Site" according to geographic level, i.e. Site vs Ss.Site vs Habitat (cfr e.g. FlotsBleus, with 2 Ss.Sites).

freq.hab.red. <- freq.hab.[, c("Code.Habitat", "Libellé.Habitat", "code.site", "Site", "Site_bis", "site", "date.sortie", "Nb.Total", "Nb.Adultes", "Nb.Enfants")]
#freq.hab.red. <- dplyr::rename(freq.hab.red., Nb.Total.freq.hab. = Nb.Total)
colnames(freq.hab.red.)[c(3,8:10)] <- c(paste0("FrHa_", names(freq.hab.red.[, c(3,8:10)])))
freq.hab.red.$Site.date.sortie <- paste0(freq.hab.red.$Site, ".", freq.hab.red.$date.sortie)

# add replicate number when several counts the same date

freq.hab.red. <- freq.hab.red. %>% 
  dplyr::group_by(Site.date.sortie) %>%
  dplyr::mutate(repl. = dplyr::row_number(Site.date.sortie))
freq.hab.red. <- data.frame(freq.hab.red.)

freq.ss.site.red. <- freq.ss.site[, c("Code.Sous.Site", "Libellé.Sous.Site", "Code.Site", "Libellé.Site", "Site", "Site_bis", "site", "date.sortie", "Nb.Total", "Nb.Adultes", "Nb.Enfants")]
colnames(freq.ss.site.red.)[c(3,4,9:11)] <- c(paste0("FrSsSi_", names(freq.ss.site.red.[, c(3,4,9:11)])))
freq.ss.site.red.$Site.date.sortie <- paste0(freq.ss.site.red.$Site, ".", freq.ss.site.red.$date.sortie)
freq.ss.site.red. <- dplyr::arrange(freq.ss.site.red., Site, date.sortie, FrSsSi_Nb.Total)
freq.ss.site.red. <- freq.ss.site.red. %>% 
  dplyr::group_by(Site.date.sortie) %>%
  dplyr::mutate(repl. = dplyr::row_number(Site.date.sortie))
freq.ss.site.red. <- data.frame(freq.ss.site.red.)

freq.site.red. <- freq.site[, c("Code.Site", "Libellé.Site", "Site", "Site_bis", "site", "date.sortie", "Nb.Total", "Nb.Adultes", "Nb.Enfants", "Nb.Pecheurs.Site", "Nb.Pecheurs.Arrivee", "Nb.Pecheurs.Départ", "Nb.Pecheurs.Zone.Interdite")]
colnames(freq.site.red.)[c(1,2,7:13)] <- c(paste0("FrSi_", names(freq.site.red.[, c(1,2,7:13)])))
freq.site.red.$Site.date.sortie <- paste0(freq.site.red.$Site, ".", freq.site.red.$date.sortie)
freq.site.red. <- dplyr::arrange(freq.site.red., Site, date.sortie, FrSi_Nb.Total)
freq.site.red. <- freq.site.red. %>% 
  dplyr::group_by(Site.date.sortie) %>%
  dplyr::mutate(repl. = dplyr::row_number(Site.date.sortie))
freq.site.red. <- data.frame(freq.site.red.)


df. <- dplyr::full_join(freq.site.red.#[, c("Site", "date.sortie", "Site.date.sortie", "repl.", "Nb.Total.freq.site")]
                 , freq.ss.site.red.#[, c("Site", "date.sortie", "Site.date.sortie", "repl.", "Nb.Total.freq.ss.site")]
                 , by = c("Site", "Site_bis", "site", "date.sortie", "Site.date.sortie", "repl."))
df. <- dplyr::full_join(df., freq.hab.red.#[, c("Site", "date.sortie", "Site.date.sortie", "repl.", "Nb.Total.freq.hab.")]
                 , by = c("Site", "Site_bis", "site", "date.sortie", "Site.date.sortie", "repl."))

names(df.)

df. <- dplyr::arrange(df., Site, date.sortie, repl.)
freq. <- data.frame(df.)
freq. <- freq.[, c(3:6, 14, 15, 1, 2, 7:13, 16:28)]
rm(df.)


freq. <- tidyr::separate(freq., date.sortie, into = c("Annee", "Mois", "Jour"), remove = FALSE)
freq.$Annee <- as.numeric(freq.$Annee)
freq.$Mois <- as.numeric(freq.$Mois)
freq.$Jour <- as.numeric(freq.$Jour)


# prior saving, I will create 3 new variables in the freq dataset: one with freq data, one with the corresponding origin variable, one for the diff between frequ data 

freq.$Fr_Nb <- NA

for (i in 1:nrow(freq.)){
if (!is.na(freq.$FrSi_Nb.Total[i])) {
  freq.$Fr_Nb[i] = freq.$FrSi_Nb.Total[i]
  } else if (is.na(freq.$FrSi_Nb.Total[i]) & !is.na(freq.$FrSi_Nb.Pecheurs.Site[i])) {
    freq.$Fr_Nb[i] = freq.$FrSi_Nb.Pecheurs.Site[i]
    } else if (is.na(freq.$FrSi_Nb.Total[i]) & !is.na(freq.$FrSsSi_Nb.Total[i])) {
      freq.$Fr_Nb[i] = freq.$FrSsSi_Nb.Total[i] 
      } else {
        freq.$Fr_Nb[i] = freq.$FrHa_Nb.Total[i]
      }
  }


freq.$Fr_var <- NA

for (i in 1:nrow(freq.)){
  if (!is.na(freq.$FrSi_Nb.Total[i])) {
    freq.$Fr_var[i] = "FrSi_Nb.Total"
  } else if (is.na(freq.$FrSi_Nb.Total[i]) & !is.na(freq.$FrSi_Nb.Pecheurs.Site[i])) {
    freq.$Fr_var[i] = "FrSi_Nb.Pecheurs.Site"
  } else if (is.na(freq.$FrSi_Nb.Total[i]) & !is.na(freq.$FrSsSi_Nb.Total[i])) {
    freq.$Fr_var[i] = "FrSsSi_Nb.Total" 
  } else {
    freq.$Fr_var[i] = "FrHa_Nb.Total"
  }
}

freq.$Fr_diff <- NA
  
for (i in 1:nrow(freq.)){
freq.$Fr_diff[i]  <- abs(max(freq.[i, c("FrSi_Nb.Total", "FrSi_Nb.Pecheurs.Site", "FrSsSi_Nb.Total", "FrHa_Nb.Total")], na.rm = T) - freq.$Fr_Nb[i])
}



#saveRDS(freq., "freq.RDS")


# final plot

# almost only unique value !
freq. %>%
  dplyr::group_by(repl.) %>%
  dplyr::summarise(nb = dplyr::n())   

Ymin. <- min(freq.$Annee)
Ymax. <- max(freq.$Annee)

for (i in c(1:length(unique(freq.[, "Site"])))) {
  
  #i <- 3

  freq.i <- dplyr::filter(freq., Site == unique(freq.[, "Site"])[i])

  xmin. <- as.Date(ifelse(min(freq.i$Annee, na.rm = T) >= 2014, as.Date("2014-01-01", origin = "1970-01-01"), as.Date(paste0(Ymin., "-01-01"), origin = "1970-01-01")), origin = "1970-01-01")
  xmax. <- as.Date(ifelse(max(freq.i$Annee, na.rm = T) <= 2017, as.Date("2018-01-01", origin = "1970-01-01"), as.Date(paste0((Ymax.+1), "-01-01"), origin = "1970-01-01")), origin = "1970-01-01")
  
  # I have to invert plot(x,y) by plot(y,x) in order to work ... ?
  png(paste0("freq_1_", unique(freq.i$Site), ".png"))
  plot(freq.i$date.sortie, rep(0, length(freq.i$date.sortie)), 
       xlim = c(xmin., xmax.), 
       ylim = c(-8, 165)
#round(0-((max(#freq.i[, "FrSi_Nb.Total"],
 #freq.i[, "FrSsSi_Nb.Total"]#, freq.i[, "FrHa_Nb.Total"]
#, na.rm = T)*1.05)-max(#freq.i[, "FrSi_Nb.Total"], 
#freq.i[, "FrSsSi_Nb.Total"]#, freq.i[, "FrHa_Nb.Total"]
#, na.rm = T))), #log10
    #                     round(max(#freq.i[, "FrSi_Nb.Total"], 
#freq.i[, "FrSsSi_Nb.Total"]#, freq.i[, "FrHa_Nb.Total"]
#, na.rm = T)*1.05)),
                , main = unique(freq.i$Site), xlab = "", ylab = "fréquentation", col = "white")
  
  #points(c(freq.$FrSi_Nb.Total, freq.$FrSsSi_Nb.Total, freq.$FrHa_Nb.Total), c(freq.$date.sortie, freq.$date.sortie, freq.$date.sortie), pch = 19, col = "lightgrey", cex = .5)

  points(freq.i$date.sortie, #log10
         (freq.i$FrSi_Nb.Total
          #+1
          ), pch = 19, col = "red")
  points(freq.i$date.sortie, #log10
         (freq.i$FrSsSi_Nb.Total
          #+1
         ), pch = 19, col = "darkblue")
  points(freq.i$date.sortie, #log10
         (freq.i$FrHa_Nb.Total
          #+1
         ), pch = 19, col = "forestgreen")

  legend("bottom", inset = c(0,-0.4), legend = c("Si","SsSi", "Ha"), pch = c(19,19,19), col = c("red", "darkblue", "forestgreen"), horiz = TRUE, bty = "n", xpd = TRUE)
  #inset = c(-0.45, 0) # You will need to fine-tune the first value depending on the windows size
  # xpd = TRUE # You need to specify this graphical parameter to put the legend outside the plot
  
}


# summarize data by date then season

# remove data if any (not the case anymore here) without date.sortie value.

freq. <- freq. %>% dplyr::filter(!is.na(freq.$date.sortie))

table(( na.omit(freq.[, c("FrSi_Code.Site", "Site.date.sortie", "FrSi_Nb.Total")]) %>%
  dplyr::group_by(FrSi_Code.Site, Site.date.sortie) %>%
  dplyr::summarise(nb = dplyr::n()) )["nb"])  
table(( na.omit(freq.[, c("Code.Sous.Site", "Site.date.sortie", "FrSsSi_Nb.Total")]) %>%
          dplyr::group_by(Code.Sous.Site, Site.date.sortie) %>%
          dplyr::summarise(nb = dplyr::n()) )["nb"])  
table(( na.omit(freq.[, c("Code.Habitat", "Site.date.sortie", "FrHa_Nb.Total")]) %>%
          dplyr::group_by(Code.Habitat, Site.date.sortie) %>%
          dplyr::summarise(nb = dplyr::n()) )["nb"]) 

# median and mean of two values are identical, so dplyr::filter for nb > 2
# and for remaining data, I'll consider the mean value cfr df. above

# first add a semester variable

freq.$Mois <- as.numeric(freq.$Mois)
ifelse(freq.$Mois %in% c(1:6), "s1", "s2") -> freq.$semester
freq.$semester <- as.factor(freq.$semester)
freq.$Annee_semester <- paste0(freq.$Annee, "_", freq.$semester)


FrSi <- na.omit(freq.[, c("Site", "Site_bis", "FrSi_Code.Site", "Site.date.sortie", "Annee_semester", "FrSi_Nb.Total")]) %>%
  dplyr::group_by(Site, Site_bis, FrSi_Code.Site, Site.date.sortie, Annee_semester) %>%
  dplyr::summarise(FrSi_Nb.Total = mean(FrSi_Nb.Total), nb = dplyr::n())
FrSi <- data.frame(FrSi)


FrSsSi <- na.omit(freq.[, c("Site", "Site_bis", "Code.Sous.Site", "Site.date.sortie", "Annee_semester", "FrSsSi_Nb.Total")]) %>%
  dplyr::group_by(Site, Site_bis, Code.Sous.Site, Site.date.sortie, Annee_semester) %>%
  dplyr::summarise(FrSsSi_Nb.Total = mean(FrSsSi_Nb.Total), nb = dplyr::n())
FrSsSi <- data.frame(FrSsSi)


FrHa <- na.omit(freq.[, c("Site", "Site_bis", "Code.Habitat", "Site.date.sortie", "Annee_semester", "FrHa_Nb.Total")]) %>%
  dplyr::group_by(Site, Site_bis, Code.Habitat, Site.date.sortie, Annee_semester) %>%
  dplyr::summarise(FrHa_Nb.Total = mean(FrHa_Nb.Total), nb = dplyr::n())
FrHa <- data.frame(FrHa)



FrSi.stat <- na.omit(FrSi[, c("Site", "Site_bis", "FrSi_Code.Site", "Site.date.sortie", "Annee_semester", "FrSi_Nb.Total")]) %>%
  dplyr::group_by(Site, Site_bis, FrSi_Code.Site, Annee_semester) %>%
  dplyr::summarise(mean.FrSi_Nb.Total = mean(FrSi_Nb.Total), median.FrSi_Nb.Total = median(FrSi_Nb.Total), min.FrSi_Nb.Total = min(FrSi_Nb.Total), max.FrSi_Nb.Total = max(FrSi_Nb.Total), nb.FrSi_Nb.Total = dplyr::n())
FrSi.stat <- data.frame(FrSi.stat)


FrSsSi.stat <- na.omit(FrSsSi[, c("Site", "Site_bis", "Code.Sous.Site", "Site.date.sortie", "Annee_semester", "FrSsSi_Nb.Total")]) %>%
  dplyr::group_by(Site, Site_bis, Code.Sous.Site, Annee_semester) %>%
  dplyr::summarise(mean.FrSsSi_Nb.Total = mean(FrSsSi_Nb.Total), median.FrSsSi_Nb.Total = median(FrSsSi_Nb.Total), min.FrSsSi_Nb.Total = min(FrSsSi_Nb.Total), max.FrSsSi_Nb.Total = max(FrSsSi_Nb.Total), nb.FrSsSi_Nb.Total = dplyr::n())
FrSsSi.stat <- data.frame(FrSsSi.stat)


FrHa.stat <- na.omit(FrHa[, c("Site", "Site_bis", "Code.Habitat", "Site.date.sortie", "Annee_semester", "FrHa_Nb.Total")]) %>%
  dplyr::group_by(Site, Site_bis, Code.Habitat, Annee_semester) %>%
  dplyr::summarise(mean.FrHa_Nb.Total = mean(FrHa_Nb.Total), median.FrHa_Nb.Total = median(FrHa_Nb.Total), min.FrHa_Nb.Total = min(FrHa_Nb.Total), max.FrHa_Nb.Total = max(FrHa_Nb.Total), nb.FrHa_Nb.Total = dplyr::n())
FrHa.stat <- data.frame(FrHa.stat)

freq.Nb.Total.stat <- dplyr::full_join(FrSi.stat, FrSsSi.stat, by = c("Site", "Site_bis", "Annee_semester"))
freq.Nb.Total.stat <- dplyr::full_join(freq.Nb.Total.stat, FrHa.stat, by = c("Site", "Site_bis", "Annee_semester"))

#missing Annee_semester value and site value; add dummy observations to fill in gaps

freq.Nb.Total.stat[nrow(freq.Nb.Total.stat)+2, ] <- NA
freq.Nb.Total.stat[nrow(freq.Nb.Total.stat)-1, "Annee_semester"]  <- "2012_s1"
freq.Nb.Total.stat[nrow(freq.Nb.Total.stat), "Annee_semester"]  <- "2017_s1"

freq.Nb.Total.stat$Annee_semester_to.nb <- freq.Nb.Total.stat$Annee_semester
freq.Nb.Total.stat$Annee_semester_to.nb <- as.factor(freq.Nb.Total.stat$Annee_semester_to.nb)

levels(freq.Nb.Total.stat$Annee_semester_to.nb) <- c(1:length(sort(unique(freq.Nb.Total.stat$Annee_semester_to.nb))))
freq.Nb.Total.stat$Annee_semester_to.nb <- as.numeric(freq.Nb.Total.stat$Annee_semester_to.nb)

#saveRDS(freq.Nb.Total.stat, "Freq. & cptmt/freq.Nb.Total.stat.RDS")

op <- par(mar = c(8,4,4,2) + 0.1) ## default is c(5,4,4,2) + 0.1

for (i in c(1:length(na.omit(unique(freq.Nb.Total.stat[, "Site"]))))) {

  # add na.omit cfr I had to add empty observations with missing factor levels
  #i <- 1
  
  dplyr::filter(freq.Nb.Total.stat, Site == unique(freq.Nb.Total.stat[, "Site"])[i]) -> freq.i
  
  # I have to invert plot(x,y) by plot(y,x) in order to work ... ?
  png(paste0("freq_2_", unique(freq.i$Site), ".png"))
  plot(freq.i$Annee_semester_to.nb, rep(0, length(freq.i$Annee_semester_to.nb)), 
       xlim = c(1, length(unique(freq.Nb.Total.stat$Annee_semester_to.nb))),
       ylim = c(-5, 200)#c(0-(
         #(max(c(freq.i[, "max.FrSi_Nb.Total"], freq.i[, "max.FrSsSi_Nb.Total"], freq.i[, "max.FrHa_Nb.Total"]), na.rm = T)*1.05) -
          # (max(c(freq.i[, "max.FrSi_Nb.Total"], freq.i[, "max.FrSsSi_Nb.Total"], freq.i[, "max.FrHa_Nb.Total"]), na.rm = T))),
         #(max(c(freq.i[, "max.FrSi_Nb.Total"], freq.i[, "max.FrSsSi_Nb.Total"], freq.i[, "max.FrHa_Nb.Total"]), na.rm = T)*1.05) )
  
       , main = unique(freq.i$Site), xlab = "", ylab = "fréquentation", xaxt = "n", col = "white")
  
  axis(1, at = 1:length(unique(freq.Nb.Total.stat$Annee_semester_to.nb)), labels = sort(unique(freq.Nb.Total.stat$Annee_semester)), las = 2)
  
  #title(xlab = "Annee_semester", cex.lab = 1, line = 5)
  
  dplyr::filter(freq.i, nb.FrSi_Nb.Total >= 2) -> freq.i.red

  if (nrow(freq.i.red) >= 1) { 
    arrows(as.numeric(freq.i.red$Annee_semester_to.nb), freq.i.red$median.FrSi_Nb.Total, as.numeric(freq.i.red$Annee_semester_to.nb), freq.i.red$max.FrSi_Nb.Total, code = 3, angle = 90, length = 0.00) 
  } else { 
    points(0, 0, col = "white")
  }
  
  if (nrow(freq.i.red) >= 1) { 
    arrows(as.numeric(freq.i.red$Annee_semester_to.nb), freq.i.red$median.FrSi_Nb.Total, as.numeric(freq.i.red$Annee_semester_to.nb), freq.i.red$min.FrSi_Nb.Total, code = 3, angle = 90, length = 0.00) 
  } else { 
    points(0, 0, col = "white")
  }
  
  points(freq.i$Annee_semester_to.nb, freq.i$mean.FrSi_Nb.Total, pch = 19, col = "orange")
  points(freq.i$Annee_semester_to.nb, freq.i$median.FrSi_Nb.Total, pch = 19, col = "red")
  
  rm(freq.i.red)
  
  dplyr::filter(freq.i, nb.FrSsSi_Nb.Total >= 2) -> freq.i.red
  
  if (nrow(freq.i.red) >= 1) { 
    arrows(as.numeric(freq.i.red$Annee_semester_to.nb)+.25, freq.i.red$median.FrSsSi_Nb.Total, as.numeric(freq.i.red$Annee_semester_to.nb)+.25, freq.i.red$max.FrSsSi_Nb.Total, code = 3, angle = 90, length = 0.00) 
  } else { 
    points(0, 0, col = "white")
  }
  
  if (nrow(freq.i.red) >= 1) { 
    arrows(as.numeric(freq.i.red$Annee_semester_to.nb)+.25, freq.i.red$median.FrSsSi_Nb.Total, as.numeric(freq.i.red$Annee_semester_to.nb)+.25, freq.i.red$min.FrSsSi_Nb.Total, code = 3, angle = 90, length = 0.00) 
  } else { 
    points(0, 0, col = "white")
  }
  
  points(as.numeric(freq.i$Annee_semester_to.nb) + .25, freq.i$mean.FrSsSi_Nb.Total, pch = 19, col = "lightblue")
  points(as.numeric(freq.i$Annee_semester_to.nb) + .25, freq.i$median.FrSsSi_Nb.Total, pch = 19, col = "darkblue")
  
  rm(freq.i.red)
  
  dplyr::filter(freq.i, nb.FrHa_Nb.Total >= 2) -> freq.i.red
  
  if (nrow(freq.i.red) >= 1) { 
    arrows(as.numeric(freq.i.red$Annee_semester_to.nb)-.25, freq.i.red$median.FrHa_Nb.Total, as.numeric(freq.i.red$Annee_semester_to.nb)-.25, freq.i.red$max.FrHa_Nb.Total, code = 3, angle = 90, length = 0.00) 
  } else { 
    points(0, 0, col = "white")
  }
  
  if (nrow(freq.i.red) >= 1) { 
    arrows(as.numeric(freq.i.red$Annee_semester_to.nb)-.25, freq.i.red$median.FrHa_Nb.Total, as.numeric(freq.i.red$Annee_semester_to.nb)-.25, freq.i.red$min.FrHa_Nb.Total, code = 3, angle = 90, length = 0.00) 
  } else { 
    points(0, 0, col = "white")
  }
  
  points(as.numeric(freq.i$Annee_semester_to.nb) - .25, freq.i$mean.FrHa_Nb.Total, pch = 19, col = "lightgreen")
  points(as.numeric(freq.i$Annee_semester_to.nb) - .25, freq.i$median.FrHa_Nb.Total, pch = 19, col = "forestgreen")
  
  rm(freq.i.red)
  
  legend("bottom", 
         #inset = c(0,-0.75), 
         inset = c(0,-0.5), 
         legend = c("méd. Si", "moy. Si","méd. SsSi", "moy. SsSi", "méd. Ha", "moy. Ha"), pch = c(19,19,19,19,19,19), col = c("red", "orange", "darkblue", "lightblue", "forestgreen", "lightgreen")#, horiz = TRUE
         , bty = "n", xpd = TRUE,
         ncol = 3)
  #inset = c(-0.45, 0) # You will need to fine-tune the first value depending on the windows size
  # xpd = TRUE # You need to specify this graphical parameter to put the legend outside the plot
  
}

# par(mfrow = c(1,1))
par(op)

# to be able to run the Rmarkdown loop

freq.$Site <- as.factor(freq.$Site)
freq.$Site <- ordered(freq.$Site)
freq.$Site.nb <- freq.$Site
levels(freq.$Site.nb) <- 1:length(unique(freq.$Site))
freq.$Site.nb <- as.integer(freq.$Site.nb)


freq.$Site.nb <- ifelse(freq.$Site %in% c("ARMO_Piegu", "ARMO_Verdelet") == TRUE, 
                     unique(dplyr::filter(freq., Site == "ARMO_Piégu / Verdelet")[,"Site.nb"]),
                     freq.$Site.nb)
freq.$Site.nb <- ifelse(freq.$Site %in% c("BASQ_FlotsBleusZF", "BASQ_FlotsBleusZP"), 
                     unique(dplyr::filter(freq., Site == "BASQ_FlotsBleus")[, "Site.nb"]),
                     freq.$Site.nb)


saveRDS(freq., "freq.RDS")


freq.Nb.Total.stat$Site <- as.factor(freq.Nb.Total.stat$Site)
freq.Nb.Total.stat$Site  <- ordered(freq.Nb.Total.stat$Site)
freq.Nb.Total.stat$Site.nb <- freq.Nb.Total.stat$Site
levels(freq.Nb.Total.stat$Site.nb) <- 1:length(unique(freq.Nb.Total.stat$Site))
freq.Nb.Total.stat$Site.nb <- as.integer(freq.Nb.Total.stat$Site.nb)

#freq.Nb.Total.stat$Site <- as.character(freq.Nb.Total.stat$Site)
freq.Nb.Total.stat$Site.nb <- ifelse(freq.Nb.Total.stat$Site %in% c("ARMO_Piegu", "ARMO_Verdelet") == TRUE, 
                          unique(dplyr::filter(freq.Nb.Total.stat, Site == "ARMO_Piégu / Verdelet")[,"Site.nb"]),
                          freq.Nb.Total.stat$Site.nb)
freq.Nb.Total.stat$Site.nb <- ifelse(freq.Nb.Total.stat$Site %in% c("BASQ_FlotsBleusZF", "BASQ_FlotsBleusZP"), 
                          unique(dplyr::filter(freq.Nb.Total.stat, Site == "BASQ_FlotsBleus")[, "Site.nb"]),
                          freq.Nb.Total.stat$Site.nb)


#saveRDS(freq.Nb.Total.stat, "freq.Nb.Total.stat.RDS")


## Work on comportment data now


obs. <- read.csv2(input_obs, fileEncoding = "Latin1")

fiche <- read.csv2(fiche_val, fileEncoding = "Latin1")

obs. <- dplyr::left_join(obs., fiche[, c("ID.Fiche", "date.sortie", "libellé.campagne", "libellé.sortie", "territoire", "code.site", "site", "sous.site", "zone.habitat", "type.protocole", "version.protocole")], by = "ID.Fiche")

obs.$date.sortie <- as.Date(obs.$date.sortie, origin = "1970-01-01")


obs.$Heure.Debut <- ifelse(obs.$Heure.Debut == "", NA, obs.$Heure.Debut)
obs.$Heure.Fin <- ifelse(obs.$Heure.Debut == "", NA, obs.$Heure.Fin)
obs. <- tibble::add_column(obs., Tps.obs = difftime(strptime(obs.$Heure.Fin, format = "%H:%M"), strptime(obs.$Heure.Debut, format = "%H:%M"), units = "mins"), .after = "Heure.Fin")
table(obs.$Tps.obs)["15"]
obs.$Tps.obs <- as.integer(obs.$Tps.obs)

qecbNew <- readRDS(input_qecb)

# insert site name based on qecb df.

(df. <- unique(qecbNew[, c("code.site", "Site", "Site_bis")]))

(df.join <- unique(obs.[, c("code.site", "zone.habitat"
)]))
(df.join <- dplyr::left_join(df.join, df., by = "code.site"))
df.join <- df.join[!(df.join$code.site == "ARMO_042-043" & df.join$Site == "ARMO_Piegu"),]
df.join <- df.join[!(df.join$zone.habitat == "Les Flots Bleus (champ de blocs) - zone pècheurs" & df.join$Site == "BASQ_FlotsBleusZF"),]
df.join <- df.join[!(df.join$zone.habitat == "Les Flots Bleus (champ de blocs) - zone famille" & df.join$Site == "BASQ_FlotsBleusZP"),]

library(data.table)
for (i in c(1:nrow(df.join))) {
  #i <- 1
  setDT(obs.)[code.site == df.join[i, "code.site"], Site := df.join[i,"Site"]]
}
obs. <- data.frame(obs.)

obs.$Site <- ifelse(obs.$code.site == "BASQ_01" & obs.$zone.habitat == "Les Flots Bleus (champ de blocs) - zone pècheurs", "BASQ_FlotsBleusZP", obs.$Site)

rm(df.join)
rm(df.)

obs. <- tibble::add_column(obs., Site = obs.$Site, .after = "code.site")
obs. <- obs.[ , !names(obs.) %in% c("Site")]
obs. <- dplyr::rename(obs., Site = Site.1)

# There are some missing time data, either because there was no fishers, either because data were not encoded. Has to be completed.
obs.$Tps.obs <- ifelse(!is.na(obs.$Heure.Debut) & is.na(obs.$Tps.obs), 15, obs.$Tps.obs)

obs. <- tibble::add_column(obs., Nb.Blocs.Retournes.remis.norm15min = obs.$Nb.Blocs.Retournes.remis/obs.$Tps.obs*15, .before = "date.sortie")
obs. <- tibble::add_column(obs., Nb.Blocs.Deplaces.non.remis.norm15min = obs.$Nb.Blocs.Deplaces.non.remis/obs.$Tps.obs*15, .before = "date.sortie")
obs. <- tibble::add_column(obs., Nb.Blocs.Retournes.non.remis.norm15min = obs.$Nb.Blocs.Retournes.non.remis/obs.$Tps.obs*15, .before = "date.sortie")

# some issues with encoded data

dplyr::filter(obs., is.na(obs.$Nb.Blocs.Retournes.remis) | is.na(obs.$Nb.Blocs.Retournes.remis.norm15min) | is.na(obs.$Nb.Blocs.Retournes.non.remis) | is.na(obs.$Nb.Blocs.Retournes.non.remis.norm15min) | is.na(obs.$Nb.Blocs.Deplaces.non.remis) | is.na(obs.$Nb.Blocs.Deplaces.non.remis.norm15min)) -> check
obs.$Nb.Blocs.Retournes.non.remis <- ifelse(obs.$ID.Fiche == 373166, 0, obs.$Nb.Blocs.Retournes.non.remis)
obs.$Nb.Blocs.Retournes.non.remis.norm15min <- ifelse(obs.$ID.Fiche == 373166, 0, obs.$Nb.Blocs.Retournes.non.remis.norm15min)
obs.$Nb.Blocs.Deplaces.non.remis <- ifelse(obs.$ID.Fiche == 373166, 0, obs.$Nb.Blocs.Deplaces.non.remis)
obs.$Nb.Blocs.Deplaces.non.remis.norm15min <- ifelse(obs.$ID.Fiche == 373166, 0, obs.$Nb.Blocs.Deplaces.non.remis.norm15min)
dplyr::filter(obs., is.na(obs.$Nb.Blocs.Retournes.remis) | is.na(obs.$Nb.Blocs.Retournes.remis.norm15min) | is.na(obs.$Nb.Blocs.Retournes.non.remis) | is.na(obs.$Nb.Blocs.Retournes.non.remis.norm15min) | is.na(obs.$Nb.Blocs.Deplaces.non.remis) | is.na(obs.$Nb.Blocs.Deplaces.non.remis.norm15min)) -> check
obs. <- dplyr::filter(obs., ID.Fiche %notin% c(check$ID.Fiche))

# add replicate numbers for fishers observed during the same survey

obs. <- tibble::add_column(obs., Site.date.sortie = paste0(obs.$Site, ".", obs.$date.sortie), .after = "Site")

obs. <- obs. %>% 
  dplyr::group_by(Site.date.sortie) %>%
  dplyr::mutate(repl. = dplyr::row_number(Site.date.sortie))
obs. <- data.frame(obs.)

# add a unique variable for the three obs. cptmt., with ponderation.

ratio. <- ((obs.$Nb.Blocs.Retournes.remis*0)+(obs.$Nb.Blocs.Deplaces.non.remis*0.5)+(obs.$Nb.Blocs.Retournes.non.remis*1))/(obs.$Nb.Blocs.Retournes.remis+obs.$Nb.Blocs.Deplaces.non.remis+obs.$Nb.Blocs.Retournes.non.remis)
obs. <- tibble::add_column(obs., ratio.obs = ratio., .after = "Nb.Blocs.Retournes.non.remis")
obs.$ratio.obs <- ifelse(obs.$Nb.Blocs.Retournes.remis == 0 & obs.$Nb.Blocs.Deplaces.non.remis == 0 & obs.$Nb.Blocs.Retournes.non.remis == 0, 0, obs.$ratio.obs)

ratio.norm15min <- ((obs.$Nb.Blocs.Retournes.remis.norm15min*0)+(obs.$Nb.Blocs.Deplaces.non.remis.norm15min*0.5)+(obs.$Nb.Blocs.Retournes.non.remis.norm15min*1))/(obs.$Nb.Blocs.Retournes.remis.norm15min+obs.$Nb.Blocs.Deplaces.non.remis.norm15min+obs.$Nb.Blocs.Retournes.non.remis.norm15min)
obs. <- tibble::add_column(obs., ratio.norm15min.obs = ratio.norm15min, .after = "Nb.Blocs.Retournes.non.remis.norm15min")
obs.$ratio.norm15min.obs <- ifelse(obs.$Nb.Blocs.Retournes.remis.norm15min == 0 & obs.$Nb.Blocs.Deplaces.non.remis.norm15min == 0 & obs.$Nb.Blocs.Retournes.non.remis.norm15min == 0, 0, obs.$ratio.norm15min.obs)


saveRDS(obs., "obs.RDS")


xmin. <- as.Date("2013-01-01", origin = "1970-01-01")
xmax. <- as.Date("2022-01-01", origin = "1970-01-01")

ymax. = max(c(obs.$Nb.Blocs.Deplaces.non.remis, obs.$Nb.Blocs.Deplaces.non.remis.norm15min, obs.$Nb.Blocs.Retournes.non.remis, obs.$Nb.Blocs.Retournes.non.remis.norm15min, obs.$Nb.Blocs.Retournes.remis, obs.$Nb.Blocs.Retournes.remis.norm15min), na.rm = T)


par(mfrow = c(1,1))

for (i in c(1:length(unique(obs.$Site)))) {
  
  #i <- 5
  
  dplyr::filter(obs., Site == unique(obs.$Site)[i]) -> obs.i
  
  ymax. = max(c(obs.i$Nb.Blocs.Deplaces.non.remis, obs.i$Nb.Blocs.Deplaces.non.remis.norm15min, obs.i$Nb.Blocs.Retournes.non.remis, obs.i$Nb.Blocs.Retournes.non.remis.norm15min, obs.i$Nb.Blocs.Retournes.remis, obs.i$Nb.Blocs.Retournes.remis.norm15min), na.rm = T)
  
  png(paste0("Observations_1", unique(obs.i$Site), ".png"))
  plot(obs.i$date.sortie, obs.i$Nb.Blocs.Retournes.remis.norm15min, xlim = c(xmin.,xmax.), ylim = c(0,105)#ymax.*1.05)
, xlab = "", ylab = "Nbr de blocs (normalisé pour 15 min d'obs.)", main = unique(obs.i$Site), col = "darkgreen")
  points(obs.i$date.sortie, obs.i$Nb.Blocs.Retournes.non.remis.norm15min, col = "red")
  points(obs.i$date.sortie, obs.i$Nb.Blocs.Deplaces.non.remis.norm15min, col = "orange")
  
  legend("bottom", inset = c(0,-0.4), legend = c("ret.re.","ret.n.re.", "dépl.n.re."), pch = c(19,19,19), col = c("darkgreen", "red", "orange"), horiz = TRUE, bty = "n", xpd = TRUE)
  
}


for (i in c(1:length(unique(obs.$Site)))) {
  
  #i <- 5
  
  dplyr::filter(obs., Site == unique(obs.$Site)[i]) -> obs.i
  
  png(paste0("Observations_2", unique(obs.i$Site), ".png"))
  plot(obs.i$date.sortie, obs.i$ratio.norm15min.obs*100, xlim = c(xmin.,xmax.), ylim = c(0,105), xlab = "", ylab = "comportement relatif (normalisé pour 15 min d'obs.)", main = unique(obs.i$Site), col = "black")
  
}


