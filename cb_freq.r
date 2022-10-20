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

freq.hab. <- read.csv2(input_hab, header = FALSE, fileEncoding = "Latin1")
names_ <- as.vector(unlist(freq.hab.[1, ]))
names_ <- gsub(" ", ".", names_)
colnames(freq.hab.) <- names_

freq.site <- read.csv2(input_site, header = FALSE, fileEncoding = "Latin1")
names_ <- as.vector(unlist(freq.site[1, ]))
names_ <- gsub(" ", ".", names_)
colnames(freq.site) <- names_

freq.ss.site <- read.csv2(input_soussite, header = FALSE, fileEncoding = "Latin1")
names_ <- as.vector(unlist(freq.ss.site[1, ]))
names_ <- gsub(" ", ".", names_)
colnames(freq.ss.site) <- names_

ficheterrain <- read.csv2(fiche_val, header = FALSE, fileEncoding = "Latin1")
names_ <- as.vector(unlist(ficheterrain[1, ]))
names_ <- gsub(" ", ".", names_)
colnames(ficheterrain) <- names_

freq.hab. <- dplyr::filter(freq.hab., grepl(c("champs | Champs | blocs"), freq.hab.$Libellé.Habitat))


qecbNew <- readRDS(input_qecb)

# two sites are missing in freq.site compared to qecb data: "PNMI_02" = Quéménès & "PNMI_07" = Île de Sein - Goulenez et Île de Sein - Kilaourou. According to Anna Capietto (email Mercredi 19 Mai 2021 15:43:53): "Nous (...) n’avons pas de données de fréquentation associées à ces suivis." (i.e. qecb & ivr).
freq.site <- dplyr::filter(freq.site, freq.site$Code.Site %in% c(sort(unique(qecbNew$code.site))))

freq.ss.site <- dplyr::filter(freq.ss.site, freq.ss.site$Code.Site %in% c(sort(unique(qecbNew$code.site))))

`%notin%` <- Negate(`%in%`)

dplyr::filter(freq.ss.site, Code.Sous.Site %notin% c("ARMO_082", "ARMO_153", "EGMP_114_2", "EGMP_015")) -> freq.ss.site


# some other possibilities to merge df. based on a common variable? obviously not

# investigate data
# obviously Code.Site is the only variable with common observations/values

ficheterrain <- dplyr::filter(ficheterrain, code.site %in% c(sort(unique(qecbNew$code.site))))


# intersect between ficheterrain and data according to ID.Fiche variable, cfr Elodie Gamp discu.
# Obviously, "Libellé.Sortie" could be used as well to bind df.

ficheterrain$date.sortie <- as.Date(ficheterrain$date.sortie, origin = "1970-01-01")
# we have the information, including the date, for almost all the data of the 3 df. freq.hab., freq.site et freq.ss.site in the ficheterrain df. :-)

ficheterrain <- dplyr::rename(ficheterrain, Libellé.Sortie = libellé.sortie)

freq.site <- dplyr::left_join(freq.site, ficheterrain, by = c("ID.Fiche", "Libellé.Sortie"))

freq.ss.site <- dplyr::left_join(freq.ss.site, ficheterrain, by =  c("ID.Fiche", "Libellé.Sortie"))

freq.hab. <- dplyr::left_join(freq.hab., ficheterrain, by = c("ID.Fiche", "Libellé.Sortie"))

# I noticed an issue with -999 data instead of NAs for freq.hab. df. I correct it here before plotting
freq.hab.$Nb.Total <- ifelse(freq.hab.$Nb.Total == min(freq.hab.$Nb.Total), NA, freq.hab.$Nb.Total)

# only Flots Bleus is repeated: 1 site, 2 ss-sites

#par(mfrow = c(3,1))
#plot(freq.site$date.sortie, freq.site$Nb.Total, xlim = c(as.Date("2013-01-01", origin = "1970-01-01"), as.Date("2022-01-01", origin = "1970-01-01")), ylim = c(0,800))
#plot(freq.ss.site$date.sortie, freq.ss.site$Nb.Total, xlim = c(as.Date("2013-01-01", origin = "1970-01-01"), as.Date("2022-01-01", origin = "1970-01-01")), ylim = c(0,800))
#plot(freq.hab.$date.sortie, freq.hab.$Nb.Total, xlim = c(as.Date("2013-01-01", origin = "1970-01-01"), as.Date("2022-01-01", origin = "1970-01-01")), ylim = c(0,800))

#plot(freq.site$date.sortie, log10(freq.site$Nb.Pecheurs.Site+1), xlim = c(as.Date("2013-01-01", origin = "1970-01-01"), as.Date("2022-01-01", origin = "1970-01-01")), ylim = c(log10(1), log10(1000)))
#plot(freq.ss.site$date.sortie, log10(freq.ss.site$Nb.Total+1), xlim = c(as.Date("2013-01-01", origin = "1970-01-01"), as.Date("2022-01-01", origin = "1970-01-01")), ylim = c(log10(1), log10(1000)))
#plot(freq.hab.$date.sortie, log10(freq.hab.$Nb.Total+1), xlim = c(as.Date("2013-01-01", origin = "1970-01-01"), as.Date("2022-01-01", origin = "1970-01-01")), ylim = c(log10(1), log10(1000)))
#par(mfrow = c(1,1))


# insert site name based on qecb df.

df. <- unique(qecbNew[, c("code.site", "Site", "Site_bis")])

###########################################################
# Show df.join table throughout the process to check for site name correspondence.
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!
###########################################################

df.join <- unique(freq.hab.[, c("code.site", "site", "Code.Habitat", "Libellé.Habitat")])
df.join <- dplyr::left_join(df.join, df., by = "code.site")
###########################################################
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# check below kine after running code because lines to be removed might need to be updated !! or code another way with character strings
df.join <- df.join[-c(9,10),] # current
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!
###########################################################
#freq.hab.$Site <- NA
#freq.hab.$Site <- as.character(freq.hab.$Site)

# multiple ifelse statement doesn't work, so I used another function from package data.table
for (i in c(1:nrow(df.join))) {
  #i <- 1
  data.table::setDT(freq.hab.)[code.site == df.join[i, "code.site"] & Code.Habitat == df.join[i, "Code.Habitat"], Site := df.join[i,"Site"]]
}


freq.hab. <- data.frame(freq.hab.)

# NB: from table to dataframe space " " are replace by a ".", therefore colnames changes, e.g. below for "code.site" and "Code.Habitat".
freq.hab. <- tibble::add_column(freq.hab.[, c(1:(ncol(freq.hab.)-1))], Site = freq.hab.$Site, .after = "Libellé.Habitat")


# multiple ifelse statement doesn't work, so I used another function from package data.table
for (i in c(1:nrow(df.join))) {
  #i <- 1
  data.table::setDT(freq.hab.)[code.site == df.join[i, "code.site"] & Code.Habitat == df.join[i, "Code.Habitat"], Site_bis := df.join[i,"Site_bis"]]
}
freq.hab. <- data.frame(freq.hab.)
freq.hab. <- tibble::add_column(freq.hab.[, c(1:(ncol(freq.hab.)-1))], Site_bis = freq.hab.$Site_bis, .after = "Site")
# issue with code.site vs code.habitat ... cfr some Code.Site BASQ_01 for Flots Bleus but 2 Code.Habitat for ZF & ZP ...
# remove Code.Habitat = ss.site for Flos Bleus

rm(df.join)

df.join <- unique(freq.site[, c("Code.Site", "Libellé.Site"
                                 #, "site"
                                 )])
df.join <- dplyr::rename(df.join, code.site = Code.Site)
(df.join <- dplyr::left_join(df.join, df., by = "code.site"))
df.join$Site <- ifelse(df.join$code.site == "BASQ_01", "BASQ_FlotsBleus", df.join$Site)
df.join$Site <- ifelse(df.join$code.site == "ARMO_042-043", "ARMO_Piégu / Verdelet", df.join$Site)
df.join$Site_bis <- ifelse(df.join$code.site == "BASQ_01", "Les Flots Bleus", df.join$Site_bis)
df.join$Site_bis <- ifelse(df.join$code.site == "ARMO_042-043", "Îlot du Verdelet / Piégu", df.join$Site_bis)
df.join <- df.join[!duplicated(df.join), ]
freq.site$Site <- NA
freq.site$Site <- as.character(freq.site$Site)

for (i in c(1:nrow(df.join))) {
  #i <- 1
  data.table::setDT(freq.site)[Code.Site == df.join[i, "code.site"], Site := df.join[i,"Site"]]
}
freq.site <- data.frame(freq.site)

freq.site <- tibble::add_column(freq.site[, c(1:(ncol(freq.site)-1))], Site = freq.site$Site, .after = "Libellé.Site")
# check for Flots Bleus and Piégu / Verdelet ?
freq.site$Site_bis <- NA
freq.site$Site_bis <- as.character(freq.site$Site_bis)

for (i in c(1:nrow(df.join))) {
  #i <- 1
  data.table::setDT(freq.site)[Code.Site == df.join[i, "code.site"], Site_bis := df.join[i,"Site_bis"]]
}
freq.site <- data.frame(freq.site)

freq.site <- tibble::add_column(freq.site[, c(1:(ncol(freq.site)-1))], Site_bis = freq.site$Site_bis, .after = "Site")
# check for Flots Bleus and Piégu / Verdelet ?
rm(df.join)

df.join <- unique(freq.ss.site[, c("Code.Site", "Libellé.Site", "Code.Sous.Site", "Libellé.Sous.Site")])
df.join <- dplyr::rename(df.join, code.site = Code.Site)
df.join <- dplyr::left_join(df.join, df., by = "code.site")
###########################################################
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# check below kine after running code because lines to be removed might need to be updated !! or code another way with character strings
df.join <- df.join[-c(1,5),] # current
###########################################################
freq.ss.site$Site <- NA
freq.ss.site$Site <- as.character(freq.ss.site$Site)

for (i in c(1:nrow(df.join))) {
  #i <- 1
  data.table::setDT(freq.ss.site)[Code.Site == df.join[i, "code.site"] & Code.Sous.Site == df.join[i, "Code.Sous.Site"], Site := df.join[i,"Site"]]
}
freq.ss.site <- data.frame(freq.ss.site)

freq.ss.site <- tibble::add_column(freq.ss.site[, c(1:(ncol(freq.ss.site)-1))], Site = freq.ss.site$Site, .after = "Libellé.Site")
# check for Piégu / Verdelet ?
freq.ss.site$Site_bis <- NA
freq.ss.site$Site_bis <- as.character(freq.ss.site$Site_bis)
library(data.table)
for (i in c(1:nrow(df.join))) {
  #i <- 1
  data.table::setDT(freq.ss.site)[Code.Site == df.join[i, "code.site"] & Code.Sous.Site == df.join[i, "Code.Sous.Site"], Site_bis := df.join[i,"Site_bis"]]
}
freq.ss.site <- data.frame(freq.ss.site)

freq.ss.site <- tibble::add_column(freq.ss.site[, c(1:(ncol(freq.ss.site)-1))], Site_bis = freq.ss.site$Site_bis, .after = "Site")
# check for Piégu / Verdelet ?

rm(df.join)

rm(df.)


# plot



freq.hab. %>% tidyr::separate(date.sortie, into = c("Annee", "Mois", "Jour"), remove = F) -> freq.hab.
freq.hab.$Annee <- as.numeric(freq.hab.$Annee)

freq.site %>% tidyr::separate(date.sortie, into = c("Annee", "Mois", "Jour"), remove = F) -> freq.site
freq.site$Annee <- as.numeric(freq.site$Annee)

freq.ss.site %>% tidyr::separate(date.sortie, into = c("Annee", "Mois", "Jour"), remove = F) -> freq.ss.site
freq.ss.site$Annee <- as.numeric(freq.ss.site$Annee)

Ymin. <- min(c(as.vector(unlist(na.omit(unique(freq.hab.["Annee"])))), as.vector(unlist(na.omit(unique(freq.site["Annee"])))), as.vector(unlist(na.omit(unique(freq.ss.site["Annee"]))))))
Ymax. <- max(c(as.vector(unlist(na.omit(unique(freq.hab.["Annee"])))), as.vector(unlist(na.omit(unique(freq.site["Annee"])))), as.vector(unlist(na.omit(unique(freq.ss.site["Annee"]))))))

x. <- dplyr::filter(freq.ss.site, is.na(freq.ss.site$Site))

fct.plot.raw <- function(df., ylab., var.) {
  
  #test
  #df. <- freq.ss.site
  #var. <- "Nb.Total"
  #ylab. = "fréquentation" 
  #i <- 6
  
  for (i in c(1:length(unique(df.[, "Site"])))) {
    
    dplyr::filter(df., Site == sort(unique(df.[, "Site"]))[i]) -> freq.i
    
    xmin. <- as.Date(ifelse(min(freq.i$Annee, na.rm = T) >= 2014, as.Date("2014-01-01", origin = "1970-01-01"), as.Date(paste0(Ymin., "-01-01"), origin = "1970-01-01")), origin = "1970-01-01")
    xmax. <- as.Date(ifelse(max(freq.i$Annee, na.rm = T) <= 2017, as.Date("2018-01-01", origin = "1970-01-01"), as.Date(paste0((Ymax.+1), "-01-01"), origin = "1970-01-01")), origin = "1970-01-01")
    
    plot(freq.i$date.sortie, freq.i[, var.],
         xlim = c(xmin., xmax.), 
         ylim = c(0, Ymax.),
         pch = 19, main = unique(freq.i$Site), xlab = "Date", ylab = ylab.)
    
  }

}


par(mfrow = c(3,2))
fct.plot.raw(df. = freq.hab., ylab. = "fréquentation habitat - Nb.Total", var. = "Nb.Total")
par(mfrow = c(1,1))

par(mfrow = c(3,2))
fct.plot.raw(df. = freq.ss.site, ylab. = "fréquentation sous-site - Nb.Total", var. = "Nb.Total")
par(mfrow = c(1,1))

par(mfrow = c(3,2))
fct.plot.raw(df. = freq.site, ylab. = "fréquentation site - Nb.Total", var. = "Nb.Total")
par(mfrow = c(1,1))


# Conclusions from below analyses

# for freq.hab., consider $Nb.Total var.
# for freq.site, consider $Nb.Total and $Nb.Pecheurs.Site var. (also eventually add $`Nb Pecheurs Arrivé` to $Nb.Pecheurs.Site to have a more exhaustif nb.); NB: $Nb.Pecheurs.Site is major part of $Nb.Total, but not always specified as such, so better to only consider $Nb.Total 
# Ccl.: all count data in freq.ss.site are within freq.site, and freq.hab. data are little, and no correlation between both site and habitat count ! 


# there are several 'count data' types ... add points to plot to see them.

# habitat

par(mfrow = c(3,2))

for (i in c(1:length(unique(freq.hab.[, "Site"])))) {
  
  #i <- 1
  
  dplyr::filter(freq.hab., Site == sort(unique(freq.hab.[, "Site"]))[i]) -> freq.i
  
  xmin. <- as.Date(ifelse(min(freq.i$Annee, na.rm = T) >= 2014, as.Date("2014-01-01", origin = "1970-01-01"), as.Date(paste0(Ymin., "-01-01"), origin = "1970-01-01")), origin = "1970-01-01")
  xmax. <- as.Date(ifelse(max(freq.i$Annee, na.rm = T) <= 2017, as.Date("2018-01-01", origin = "1970-01-01"), as.Date(paste0((Ymax.+1), "-01-01"), origin = "1970-01-01")), origin = "1970-01-01")
  
  plot(freq.i$`date.sortie`, freq.i[, "Nb.Total"],
       xlim = c(xmin., xmax.), 
       ylim = c(0, Ymax.),
       pch = 19, main = unique(freq.i$Site), xlab = "Date", ylab = "fréquentation habitat")
  
  points(freq.i$`date.sortie`, freq.i$`Nb.Adultes`, col = "orange")
  points(freq.i$`date.sortie`, freq.i$`Nb.Enfants`, col = "red")
  
}

par(mfrow = c(1,1))

freq.hab.[, c("Nb.Total", "Nb.Adultes", "Nb.Enfants")] -> df.1
dplyr::filter(df.1, !is.na(df.1$Nb.Total)) -> df.1

# Nb.Total is indeed sum of Adults + Enfants when distinction is specified
rm(df.1)


# sous-site

par(mfrow = c(3,2))

for (i in c(1:length(unique(freq.ss.site[, "Site"])))) {
  
  #i <- 1
  
  dplyr::filter(freq.ss.site, Site == sort(unique(freq.ss.site[, "Site"]))[i]) -> freq.i
  
  xmin. <- as.Date(ifelse(min(freq.i$Annee, na.rm = T) >= 2014, as.Date("2014-01-01", origin = "1970-01-01"), as.Date(paste0(Ymin., "-01-01"), origin = "1970-01-01")), origin = "1970-01-01")
  xmax. <- as.Date(ifelse(max(freq.i$Annee, na.rm = T) <= 2017, as.Date("2018-01-01", origin = "1970-01-01"), as.Date(paste0((Ymax.+1), "-01-01"), origin = "1970-01-01")), origin = "1970-01-01")
  
  plot(freq.i$`date.sortie`, freq.i[, "Nb.Total"],
       xlim = c(xmin., xmax.), 
       ylim = c(0, Ymax.),
       pch = 19, main = unique(freq.i$Site), xlab = "Date", ylab = "fréquentation sous-site")
  
  points(freq.i$`date.sortie`, freq.i$Nb.Adultes, col = "orange")
  points(freq.i$`date.sortie`, freq.i$Nb.Enfants, col = "red")
  
}

par(mfrow = c(1,1))

(freq.ss.site[, c("Nb.Total", "Nb.Adultes", "Nb.Enfants")]) -> df.1
dplyr::filter(df.1, !is.na(df.1$Nb.Total)) -> df.1
# Nb.Total is indeed sum of Adults + Enfants when distinction is specified
rm(df.1)


# site

#freq.site$Nb.Pecheurs.Départ <- as.numeric(freq.site$Nb.Pecheurs.Départ)
#freq.site$Nb.Pecheurs.Zone.Interdite <- as.numeric(freq.site$Nb.Pecheurs.Zone.Interdite)

par(mfrow = c(3,2))

for (i in c(1:length(unique(freq.site[, "Site"])))) {
  
  #i <- 1
  
  dplyr::filter(freq.site, Site == sort(unique(freq.site[, "Site"]))[i]) -> freq.i
  
  xmin. <- as.Date(ifelse(min(freq.i$Annee, na.rm = T) >= 2014, as.Date("2014-01-01", origin = "1970-01-01"), as.Date(paste0(Ymin., "-01-01"), origin = "1970-01-01")), origin = "1970-01-01")
  xmax. <- as.Date(ifelse(max(freq.i$Annee, na.rm = T) <= 2017, as.Date("2018-01-01", origin = "1970-01-01"), as.Date(paste0((Ymax.+1), "-01-01"), origin = "1970-01-01")), origin = "1970-01-01")
  
  plot(freq.i$`date.sortie`, freq.i[, "Nb.Total"],
       xlim = c(xmin., xmax.), 
       ylim = c(0, Ymax.),
       pch = 19, cex = 0.5, main = unique(freq.i$Site), xlab = "Date", ylab = "fréquentation site")
  
  points(freq.i$date.sortie, freq.i$Nb.Adultes, col = "orange") # Adultes + Enfants = Total
  points(freq.i$date.sortie, freq.i$Nb.Enfants, col = "red")
  points(freq.i$date.sortie, freq.i$Nb.Pecheurs.Site, col = "blue") # Adultes + Enfants = Total
  points(freq.i$date.sortie, freq.i$Nb.Pecheurs.Arrivee, col = "forestgreen")
  points(freq.i$date.sortie, freq.i$Nb.Pecheurs.Départ, col = "lightgreen") # no data so removed
  points(freq.i$date.sortie, freq.i$Nb.Pecheurs.Zone.Interdite, col = "pink") # two data only so removed
  
}

par(mfrow = c(1,1))

#freq.site$Nb.Total - rowSums(freq.site[, c("Nb.Adultes", "Nb.Enfants", "Nb.Pecheurs.Site", "Nb.Pecheurs.Arrivee", "Nb.Pecheurs.Départ", "Nb.Pecheurs.Zone.Interdite")], na.rm = T)
#freq.site$Nb.Total - rowSums(freq.site[, c("Nb.Adultes", "Nb.Enfants")], na.rm = T)


# below df for discussion with Pauline, Christian ans Isabelle

dplyr::filter(na.omit(freq.site[, c("Nb.Adultes", "Nb.Enfants", "Nb.Total", "Nb.Pecheurs.Site", "Nb.Pecheurs.Arrivee")]), Nb.Pecheurs.Arrivee > 0) -> df.1
dplyr::filter(na.omit(freq.site[, c("Nb.Adultes", "Nb.Enfants", "Nb.Total", "Nb.Pecheurs.Site")]), Nb.Pecheurs.Site > 0) -> df.2
freq.site[, c("Nb.Adultes", "Nb.Enfants", "Nb.Total", "Nb.Pecheurs.Site", "Nb.Pecheurs.Arrivee", "Nb.Pecheurs.Départ", "Nb.Pecheurs.Zone.Interdite")] -> df.3

rm(df.1, df.2, df.3)


# see now if df are complementary with regads to frequentation
# NB: Now we consider "Site" according to geographic level, i.e. Site vs Ss.Site vs Habitat (cfr e.g. FlotsBleus, with 2 Ss.Sites).

freq.hab.red. <- freq.hab.[, c("Code.Habitat", "Libellé.Habitat", "code.site", "Site", "Site_bis", "site", "date.sortie", "Nb.Total", "Nb.Adultes", "Nb.Enfants")]

#freq.hab.red. <- dplyr::rename(freq.hab.red., Nb.Total.freq.hab. = Nb.Total)
colnames(freq.hab.red.)[c(3,8:10)] <- c(paste0("FrHa_", names(freq.hab.red.[, c(3,8:10)])))

freq.hab.red.$Site.date.sortie <- paste(freq.hab.red.$Site, freq.hab.red.$date.sortie, sep = ".")

#freq.hab.red. <- dplyr::arrange(freq.hab.red., Site, date.sortie, `Nb.Total freq.hab`)

# add replicate number when several counts the same date

x=data.frame(id=c(1,1,2,3,4,4,5,6,7), value=c(10,10,10,20,20,20,20,20,30))
x %>% 
  dplyr::arrange(id, value) %>%
  dplyr::group_by(value) %>%
  dplyr::mutate(rank=dplyr::row_number(value))
rm(x)

freq.hab.red. %>% 
  dplyr::group_by(Site.date.sortie) %>%
  dplyr::mutate(repl. = dplyr::row_number(Site.date.sortie)) -> freq.hab.red.
freq.hab.red. <- data.frame(freq.hab.red.)

freq.ss.site.red. <- freq.ss.site[, c("Code.Sous.Site", "Libellé.Sous.Site", "Code.Site", "Libellé.Site", "Site", "Site_bis", "site", "date.sortie", "Nb.Total", "Nb.Adultes", "Nb.Enfants")]
#freq.ss.site.red. <- dplyr::rename(freq.ss.site.red., `Nb.Total.freq.ss.site` = `Nb.Total`, `Code.Site.freq.ss.site` = `Code.Site`)
colnames(freq.ss.site.red.)[c(3,4,9:11)] <- c(paste0("FrSsSi_", names(freq.ss.site.red.[, c(3,4,9:11)])))
freq.ss.site.red.$Site.date.sortie <- paste0(freq.ss.site.red.$Site, ".", freq.ss.site.red.$date.sortie)
freq.ss.site.red. <- dplyr::arrange(freq.ss.site.red., Site, date.sortie, FrSsSi_Nb.Total)
freq.ss.site.red. %>% 
  dplyr::group_by(Site.date.sortie) %>%
  dplyr::mutate(repl. = dplyr::row_number(Site.date.sortie)) -> freq.ss.site.red.
freq.ss.site.red. <- data.frame(freq.ss.site.red.)

freq.site.red. <- freq.site[, c("Code.Site", "Libellé.Site", "Site", "Site_bis", "site", "date.sortie", "Nb.Total", "Nb.Adultes", "Nb.Enfants", "Nb.Pecheurs.Site", "Nb.Pecheurs.Arrivee", "Nb.Pecheurs.Départ", "Nb.Pecheurs.Zone.Interdite")]
#freq.site.red. <- dplyr::rename(freq.site.red., Nb.Total.freq.site = Nb.Total, Code.Site.freq.site = Code.Site)
colnames(freq.site.red.)[c(1,2,7:13)] <- c(paste0("FrSi_", names(freq.site.red.[, c(1,2,7:13)])))
freq.site.red.$Site.date.sortie <- paste0(freq.site.red.$Site, ".", freq.site.red.$date.sortie)
freq.site.red. <- dplyr::arrange(freq.site.red., Site, date.sortie, FrSi_Nb.Total)
freq.site.red. %>% 
  dplyr::group_by(Site.date.sortie) %>%
  dplyr::mutate(repl. = dplyr::row_number(Site.date.sortie)) -> freq.site.red.
freq.site.red. <- data.frame(freq.site.red.)


df. <- dplyr::full_join(freq.site.red.#[, c("Site", "date.sortie", "Site.date.sortie", "repl.", "Nb.Total.freq.site")]
                 , freq.ss.site.red.#[, c("Site", "date.sortie", "Site.date.sortie", "repl.", "Nb.Total.freq.ss.site")]
                 , by = c("Site", "Site_bis", "site", "date.sortie", "Site.date.sortie", "repl."))

freq.hab.red.$Site <- as.character(freq.hab.red.$Site)
freq.hab.red.$Site_bis <- as.character(freq.hab.red.$Site_bis)
freq.hab.red.$site <- as.character(freq.hab.red.$site)

freq.hab.red.$Site.date.sortie <- as.character(freq.hab.red.$Site.date.sortie)

df. <- dplyr::full_join(df., freq.hab.red.#[, c("Site", "date.sortie", "Site.date.sortie", "repl.", "Nb.Total.freq.hab.")]
                 , by = c("Site", "Site_bis", "site", "date.sortie", "Site.date.sortie", "repl."))



df. <- dplyr::arrange(df., Site, date.sortie, repl.)
freq. <- data.frame(df.)
freq. <- freq.[, c(3:6,14,15,1,2,7:13,16:28)]
rm(df.)

freq. <- tidyr::separate(freq., date.sortie, into = c("Annee", "Mois", "Jour"), remove = FALSE)
freq.$Annee <- as.numeric(freq.$Annee)
freq.$Mois <- as.numeric(freq.$Mois)
freq.$Jour <- as.numeric(freq.$Jour)


# prior saving, I will create 2 new variables in the freq dataset: one with freq data, one with the corresponding origin variable

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

#freq.$Fr_diff <- NA

#for (i in 1:nrow(freq.)){
#freq.$Fr_diff[i]  <- abs(max(freq.[i, c("FrSi_Nb.Total", "FrSi_Nb.Pecheurs.Site", "FrSsSi_Nb.Total", "FrHa_Nb.Total")], na.rm = T) - freq.$Fr_Nb[i])
#}

saveRDS(freq., "freq.RDS")


# relationship between frequentation ?
par(mfrow = c(1,1))
#plot(freq.$FrSi_Nb.Total, freq.$FrSsSi_Nb.Total)
#plot(freq.$FrSi_Nb.Total, freq.$FrHa_Nb.Total)
#plot(freq.$FrSsSi_Nb.Total, freq.$FrHa_Nb.Total)


# final plot

freq. %>% tidyr::separate(date.sortie, into = c("Annee", "Mois", "Jour"), remove = F) -> freq.
freq.$Annee <- as.numeric(freq.$Annee)
# almost only unique value !
freq. %>%
  dplyr::group_by(repl.) %>%
  dplyr::summarise(nb = dplyr::n())   


#par(mfrow = c(3,2))

for (i in c(1:length(unique(freq.[, "Site"])))) {
  
  #i <- 3
  
  dplyr::filter(freq., Site == unique(freq.[, "Site"])[i]) -> freq.i
  
  xmin. <- as.Date(ifelse(min(freq.i$Annee, na.rm = T) >= 2014, as.Date("2014-01-01", origin = "1970-01-01"), as.Date(paste0(Ymin., "-01-01"), origin = "1970-01-01")), origin = "1970-01-01")
  xmax. <- as.Date(ifelse(max(freq.i$Annee, na.rm = T) <= 2017, as.Date("2018-01-01", origin = "1970-01-01"), as.Date(paste0((Ymax.+1), "-01-01"), origin = "1970-01-01")), origin = "1970-01-01")
  
  # I have to invert plot(x,y) by plot(y,x) in order to work ... ?
  plot(freq.i$date.sortie, rep(0, length(freq.i$date.sortie)), 
       xlim = c(xmin., xmax.), 
       ylim = c(0,100),
#-((max(c(freq.i[, "FrSi_Nb.Total"], freq.i[, "FrSsSi_Nb.Total"], freq.i[, "FrHa_Nb.Total"]), na.rm = T)*1.05)-max(c(freq.i[, "FrSi_Nb.Total"], freq.i[, "FrSsSi_Nb.Total"], freq.i[, "FrHa_Nb.Total"]), na.rm = T)), #log10
                         #max(c(as.numeric(freq.i[, "FrSi_Nb.Total"]), as.numeric(freq.i[, "FrSsSi_Nb.Total"]), as.numeric(freq.i[, "FrHa_Nb.Total"])))*1.05),
                main = unique(freq.i$Site), xlab = "", ylab = "fréquentation", col = "white")
  
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

par(mfrow = c(1,1))

site. <- unique(freq.[, c("Site", "Site_bis", "FrSi_Libellé.Site", "Libellé.Sous.Site", "Libellé.Habitat", "site")])
dplyr::filter(freq., is.na(freq.$site)) -> df.

#par(mfrow = c(3,2))

for (j in c(1:length(na.omit(unique(freq.[, "site"]))))) {
  
  #j <- 3
  dplyr::filter(freq., site == na.omit(unique(freq.[, "site"]))[j]) -> freq.j
  
  for (i in c(1:length(unique(freq.j[, "Site"])))) {
   
  #i <- 3
  dplyr::filter(freq.j, Site == unique(freq.j[, "Site"])[i]) -> freq.i 
    
  xmin. <- as.Date(ifelse(min(freq.i$Annee, na.rm = T) >= 2014, as.Date("2014-01-01", origin = "1970-01-01"), as.Date(paste0(Ymin., "-01-01"), origin = "1970-01-01")), origin = "1970-01-01")
  xmax. <- as.Date(ifelse(max(freq.i$Annee, na.rm = T) <= 2017, as.Date("2018-01-01", origin = "1970-01-01"), as.Date(paste0((Ymax.+1), "-01-01"), origin = "1970-01-01")), origin = "1970-01-01")
  
  # I have to invert plot(x,y) by plot(y,x) in order to work ... ?
  plot(freq.i$date.sortie, rep(0, length(freq.i$date.sortie)), 
       xlim = c(xmin., xmax.), 
       ylim = c(0, Ymax.), 
#-((max(c(freq.i[, "FrSi_Nb.Total"], freq.i[, "FrSsSi_Nb.Total"], freq.i[, "FrHa_Nb.Total"]), na.rm = T)*1.05)-max(c(freq.i[, "FrSi_Nb.Total"], freq.i[, "FrSsSi_Nb.Total"], freq.i[, "FrHa_Nb.Total"]), na.rm = T)), #log10
                #(max(c(freq.i[, "FrSi_Nb.Total"], freq.i[, "FrSsSi_Nb.Total"], freq.i[, "FrHa_Nb.Total"]), na.rm = T)*1.05)),
       main = unique(freq.i$Site_bis), xlab = "", ylab = "fréquentation", col = "white")
  
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
}

par(mfrow = c(1,1))


# summarize data by date then season

# there are few (2 when I wrote the script) without date.sortie value; so remove them.

freq. %>% dplyr::filter(!is.na(freq.$date.sortie)) -> freq.

# median and mean of two values are identical, so dplyr::filter for nb > 2

# and for remaining data, I'll consider the mean value cfr df. above

# first add a semester variable

freq.$Mois <- as.numeric(freq.$Mois)
ifelse(freq.$Mois %in% c(1:6), "s1", "s2") -> freq.$semester
freq.$semester <- as.factor(freq.$semester)
freq.$Annee_semester <- paste0(freq.$Annee, "_", freq.$semester)


na.omit(freq.[, c("Site", "Site_bis", "FrSi_Code.Site", "Site.date.sortie", "Annee_semester", "FrSi_Nb.Total")]) %>%
  dplyr::group_by(Site, Site_bis, FrSi_Code.Site, Site.date.sortie, Annee_semester) %>%
  dplyr::summarise(FrSi_Nb.Total = mean(FrSi_Nb.Total), nb = dplyr::n()) -> FrSi
FrSi <- data.frame(FrSi)

na.omit(freq.[, c("Site", "Site_bis", "Code.Sous.Site", "Site.date.sortie", "Annee_semester", "FrSsSi_Nb.Total")]) %>%
  dplyr::group_by(Site, Site_bis, Code.Sous.Site, Site.date.sortie, Annee_semester) %>%
  dplyr::summarise(FrSsSi_Nb.Total = mean(FrSsSi_Nb.Total), nb = dplyr::n()) -> FrSsSi
FrSsSi <- data.frame(FrSsSi)

na.omit(freq.[, c("Site", "Site_bis", "Code.Habitat", "Site.date.sortie", "Annee_semester", "FrHa_Nb.Total")]) %>%
  dplyr::group_by(Site, Site_bis, Code.Habitat, Site.date.sortie, Annee_semester) %>%
  dplyr::summarise(FrHa_Nb.Total = mean(FrHa_Nb.Total), nb = dplyr::n()) -> FrHa
FrHa <- data.frame(FrHa)


na.omit(FrSi[, c("Site", "Site_bis", "FrSi_Code.Site", "Site.date.sortie", "Annee_semester", "FrSi_Nb.Total")]) %>%
  dplyr::group_by(Site, Site_bis, FrSi_Code.Site, Annee_semester) %>%
  dplyr::summarise(mean.FrSi_Nb.Total = mean(FrSi_Nb.Total), median.FrSi_Nb.Total = median(FrSi_Nb.Total), min.FrSi_Nb.Total = min(FrSi_Nb.Total), max.FrSi_Nb.Total = max(FrSi_Nb.Total), nb.FrSi_Nb.Total = dplyr::n()) -> FrSi.stat
FrSi.stat <- data.frame(FrSi.stat)

na.omit(FrSsSi[, c("Site", "Site_bis", "Code.Sous.Site", "Site.date.sortie", "Annee_semester", "FrSsSi_Nb.Total")]) %>%
  dplyr::group_by(Site, Site_bis, Code.Sous.Site, Annee_semester) %>%
  dplyr::summarise(mean.FrSsSi_Nb.Total = mean(FrSsSi_Nb.Total), median.FrSsSi_Nb.Total = median(FrSsSi_Nb.Total), min.FrSsSi_Nb.Total = min(FrSsSi_Nb.Total), max.FrSsSi_Nb.Total = max(FrSsSi_Nb.Total), nb.FrSsSi_Nb.Total = dplyr::n()) -> FrSsSi.stat
FrSsSi.stat <- data.frame(FrSsSi.stat)

na.omit(FrHa[, c("Site", "Site_bis", "Code.Habitat", "Site.date.sortie", "Annee_semester", "FrHa_Nb.Total")]) %>%
  dplyr::group_by(Site, Site_bis, Code.Habitat, Annee_semester) %>%
  dplyr::summarise(mean.FrHa_Nb.Total = mean(FrHa_Nb.Total), median.FrHa_Nb.Total = median(FrHa_Nb.Total), min.FrHa_Nb.Total = min(FrHa_Nb.Total), max.FrHa_Nb.Total = max(FrHa_Nb.Total), nb.FrHa_Nb.Total = dplyr::n()) -> FrHa.stat
FrHa.stat <- data.frame(FrHa.stat)

freq.Nb.Total.stat <- dplyr::full_join(FrSi.stat, FrSsSi.stat, by = c("Site", "Site_bis", "Annee_semester"))
freq.Nb.Total.stat <- dplyr::full_join(freq.Nb.Total.stat, FrHa.stat, by = c("Site", "Site_bis", "Annee_semester"))

#missing Annee_semester value and site value; add dummy observations to fill in gaps
freq.Nb.Total.stat[nrow(freq.Nb.Total.stat)+2, ] <- NA
freq.Nb.Total.stat[nrow(freq.Nb.Total.stat)-1, "Annee_semester"]  <- "2017_s1"
freq.Nb.Total.stat[nrow(freq.Nb.Total.stat), "Annee_semester"]  <- "2021_s2"

freq.Nb.Total.stat$Annee_semester_to.nb <- freq.Nb.Total.stat$Annee_semester
freq.Nb.Total.stat$Annee_semester_to.nb <- as.factor(freq.Nb.Total.stat$Annee_semester_to.nb)
levels(freq.Nb.Total.stat$Annee_semester_to.nb) <- c(1:length(sort(unique(freq.Nb.Total.stat$Annee_semester_to.nb))))
freq.Nb.Total.stat$Annee_semester_to.nb <- as.numeric(freq.Nb.Total.stat$Annee_semester_to.nb)

saveRDS(freq.Nb.Total.stat, "freq.Nb.Total.stat.RDS")


#par(mfrow = c(3,2))

op <- par(mar = c(8,4,4,2) + 0.1) ## default is c(5,4,4,2) + 0.1

for (i in c(1:length(na.omit(unique(freq.Nb.Total.stat[, "Site"]))))) {

  # add na.omit cfr I had to add empty observations with missing factor levels
  
  dplyr::filter(freq.Nb.Total.stat, Site == unique(freq.Nb.Total.stat[, "Site"])[i]) -> freq.i
  
  # I have to invert plot(x,y) by plot(y,x) in order to work ... ?
  
  plot(freq.i$Annee_semester_to.nb, rep(0, length(freq.i$Annee_semester_to.nb)), 
       xlim = c(1, length(unique(freq.Nb.Total.stat$Annee_semester_to.nb))),
       ylim = c(0, Ymax.)
#-(
#         (max(c(freq.i[, "max.FrSi_Nb.Total"], freq.i[, "max.FrSsSi_Nb.Total"], freq.i[, "max.FrHa_Nb.Total"]), na.rm = T)*1.05) -
 #          (max(c(freq.i[, "max.FrSi_Nb.Total"], freq.i[, "max.FrSsSi_Nb.Total"], freq.i[, "max.FrHa_Nb.Total"]), na.rm = T))
 #        ),
#         (max(c(freq.i[, "max.FrSi_Nb.Total"], freq.i[, "max.FrSsSi_Nb.Total"], freq.i[, "max.FrHa_Nb.Total"]), na.rm = T)*1.05) )
  
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

par(mfrow = c(1,1))
par(op)

# to be able to run the Rmarkdown loop

freq.$Site <- as.factor(freq.$Site)
ordered(freq.$Site, levels = c("ARMO_Bilfot", "ARMO_IlePlate", "ARMO_Piégu / Verdelet", "ARMO_Piegu", "ARMO_Verdelet", "BASQ_FlotsBleus", "BASQ_FlotsBleusZF", "BASQ_FlotsBleusZP", "BRES_Keraliou", "EGMP_BreeBains", "EGMP_Chassiron", "EGMP_GroinCou", "EGMP_PasEmsembert", "EGMP_PerreAntiochat", "FINS_Mousterlin", "FINS_StNicolasGlenan", "FOUR_PlateauFour", "GDMO_BegLann", "GDMO_Locmariaquer", "GONB_IlotStMichel", "PDMO_Perharidy")) -> freq.$Site
freq.$Site.nb <- freq.$Site
levels(freq.$Site.nb) <- 1:length(unique(freq.$Site))
freq.$Site.nb <- as.integer(freq.$Site.nb)
#freq.$Site <- as.character(freq.$Site)
freq.$Site.nb <- ifelse(freq.$Site %in% c("ARMO_Piegu", "ARMO_Verdelet") == TRUE, 
                     unique(dplyr::filter(freq., Site == "ARMO_Piégu / Verdelet")[,"Site.nb"]),
                     freq.$Site.nb)
freq.$Site.nb <- ifelse(freq.$Site %in% c("BASQ_FlotsBleusZF", "BASQ_FlotsBleusZP"), 
                     unique(dplyr::filter(freq., Site == "BASQ_FlotsBleus")[, "Site.nb"]),
                     freq.$Site.nb)

saveRDS(freq., "freq.RDS")


freq.Nb.Total.stat$Site <- as.factor(freq.Nb.Total.stat$Site)
ordered(freq.Nb.Total.stat$Site, levels = c("ARMO_Bilfot", "ARMO_IlePlate", "ARMO_Piégu / Verdelet", "ARMO_Piegu", "ARMO_Verdelet", "BASQ_FlotsBleus", "BASQ_FlotsBleusZF", "BASQ_FlotsBleusZP", "BRES_Keraliou", "EGMP_BreeBains", "EGMP_Chassiron", "EGMP_GroinCou", "EGMP_PasEmsembert", "EGMP_PerreAntiochat", "FINS_Mousterlin", "FINS_StNicolasGlenan", "FOUR_PlateauFour", "GDMO_BegLann", "GDMO_Locmariaquer", "GONB_IlotStMichel", "PDMO_Perharidy")) -> freq.Nb.Total.stat$Site 
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

saveRDS(freq.Nb.Total.stat, "freq.Nb.Total.stat.RDS")


## Work on comportment data now


obs. <- read.csv2(input_obs, header = FALSE, fileEncoding = "Latin1")

fiche <- read.csv2(fiche_val, header = FALSE, fileEncoding = "Latin1")

obs. <- dplyr::left_join(obs., fiche[, c("ID.Fiche", "date.sortie", "libellé.campagne", "libellé.sortie", "territoire", "code.site", "site", "sous.site", "zone.habitat", "type.protocole", "version.protocole")], by = "ID.Fiche")

obs.$date.sortie <- as.Date(obs.$date.sortie, origin = "1970-01-01")

library(lubridate)
obs.$Heure.Debut <- ifelse(obs.$Heure.Debut == "", NA, obs.$Heure.Debut)
obs.$Heure.Fin <- ifelse(obs.$Heure.Debut == "", NA, obs.$Heure.Fin)
obs. <- tibble::add_column(obs., Tps.obs = difftime(strptime(obs.$Heure.Fin, format = "%H:%M"), strptime(obs.$Heure.Debut, format = "%H:%M"), units = "mins"), .after = "Heure.Fin")
obs.$Tps.obs <- as.integer(obs.$Tps.obs)

qecbNew <- readRDS(input_qecb)

 # two sites are missing in freq.site compared to qecb data: "PNMI_02" = Quéménès & "PNMI_07" = Île de Sein - Goulenez et Île de Sein - Kilaourou. According to Anna Capietto (email Mercredi 19 Mai 2021 15:43:53): "Nous (...) n’avons pas de données de fréquentation associées à ces suivis." (i.e. qecb & ivr).
(unique(qecbNew[, c("code.site", "site", "Site_bis")]) -> check.qecb)


# insert site name based on qecb df.

(df. <- unique(qecbNew[, c("code.site", "Site", "Site_bis")]))

(df.join <- unique(obs.[, c("code.site", "zone.habitat"
)]))
(df.join <- dplyr::left_join(df.join, df., by = "code.site"))
df.join <- df.join[!(df.join$code.site == "ARMO_042-043" & df.join$Site == "ARMO_Piegu"),]
df.join <- df.join[!(df.join$zone.habitat == "Les Flots Bleus (champ de blocs) - zone pàªcheurs" & df.join$Site == "BASQ_FlotsBleusZF"),]
df.join <- df.join[!(df.join$zone.habitat == "Les Flots Bleus (champ de blocs) - zone famille" & df.join$Site == "BASQ_FlotsBleusZP"),]

library(data.table)
for (i in c(1:nrow(df.join))) {
  #i <- 1
  setDT(obs.)[code.site == df.join[i, "code.site"], Site := df.join[i,"Site"]]
}
obs. <- data.frame(obs.)

obs.$Site <- ifelse(obs.$code.site == "BASQ_01" & obs.$zone.habitat == "Les Flots Bleus (champ de blocs) - zone pàªcheurs", "BASQ_FlotsBleusZP", obs.$Site)

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

obs. %>% 
  dplyr::group_by(Site.date.sortie) %>%
  dplyr::mutate(repl. = dplyr::row_number(Site.date.sortie)) -> obs.
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

par(mfrow = c(2,1))

ymax. = max(c(obs.$Nb.Blocs.Deplaces.non.remis, obs.$Nb.Blocs.Deplaces.non.remis.norm15min, obs.$Nb.Blocs.Retournes.non.remis, obs.$Nb.Blocs.Retournes.non.remis.norm15min, obs.$Nb.Blocs.Retournes.remis, obs.$Nb.Blocs.Retournes.remis.norm15min), na.rm = T)

plot(obs.$date.sortie, obs.$Nb.Blocs.Retournes.remis, xlim = c(xmin.,xmax.), ylim = c(0,ymax.*1.05), xlab = "", ylab = "Nbr de blocs", col = "darkgreen")
points(obs.$date.sortie, obs.$Nb.Blocs.Retournes.non.remis, col = "red")
points(obs.$date.sortie, obs.$Nb.Blocs.Deplaces.non.remis, col = "orange")

legend("bottom", inset = c(0,-0.3), legend = c("ret. remis","ret. n.remis", "dépl. n.remis"), pch = c(19,19,19), col = c("darkgreen", "red", "orange"), horiz = TRUE, bty = "n", xpd = TRUE)

plot(obs.$date.sortie, obs.$Nb.Blocs.Retournes.remis.norm15min, xlim = c(xmin.,xmax.), ylim = c(0,ymax.*1.05), xlab = "", ylab = "Nbr de blocs (normalisé pour 15 min d'obs.)", col = "darkgreen")
points(obs.$date.sortie, obs.$Nb.Blocs.Retournes.non.remis.norm15min, col = "red")
points(obs.$date.sortie, obs.$Nb.Blocs.Deplaces.non.remis.norm15min, col = "orange")

legend("bottom", inset = c(0,-0.3), legend = c("ret. remis","ret. n.remis", "dépl. n.remis"), pch = c(19,19,19), col = c("darkgreen", "red", "orange"), horiz = TRUE, bty = "n", xpd = TRUE)

par(mfrow = c(1,1))


for (i in c(1:length(unique(obs.$Site)))) {
  
  #i <- 5
  
  dplyr::filter(obs., Site == unique(obs.$Site)[i]) -> obs.i
  
  ymax. = max(c(obs.i$Nb.Blocs.Deplaces.non.remis, obs.i$Nb.Blocs.Deplaces.non.remis.norm15min, obs.i$Nb.Blocs.Retournes.non.remis, obs.i$Nb.Blocs.Retournes.non.remis.norm15min, obs.i$Nb.Blocs.Retournes.remis, obs.i$Nb.Blocs.Retournes.remis.norm15min), na.rm = T)
  
  par(mfrow = c(1,2))
  
  plot(obs.i$date.sortie, obs.i$Nb.Blocs.Retournes.remis, xlim = c(xmin.,xmax.), ylim = c(0,ymax.*1.05), xlab = "", ylab = "Nbr de blocs", main = unique(obs.i$Site), col = "darkgreen")
  points(obs.i$date.sortie, obs.i$Nb.Blocs.Retournes.non.remis, col = "red")
  points(obs.i$date.sortie, obs.i$Nb.Blocs.Deplaces.non.remis, col = "orange")
  
  legend("bottom", inset = c(0,-0.25), legend = c("ret. remis","ret. n.remis", "dépl. n.remis"), pch = c(19,19,19), col = c("darkgreen", "red", "orange"), horiz = TRUE, bty = "n", xpd = TRUE)
  
  plot(obs.i$date.sortie, obs.i$Nb.Blocs.Retournes.remis.norm15min, xlim = c(xmin.,xmax.), ylim = c(0,ymax.*1.05), xlab = "", ylab = "Nbr de blocs (normalisé pour 15 min d'obs.)", main = unique(obs.i$Site), col = "darkgreen")
  points(obs.i$date.sortie, obs.i$Nb.Blocs.Retournes.non.remis.norm15min, col = "red")
  points(obs.i$date.sortie, obs.i$Nb.Blocs.Deplaces.non.remis.norm15min, col = "orange")
  
  legend("bottom", inset = c(0,-0.25), legend = c("ret. remis","ret. n.remis", "dépl. n.remis"), pch = c(19,19,19), col = c("darkgreen", "red", "orange"), horiz = TRUE, bty = "n", xpd = TRUE)

}


par(mfrow = c(3,2))

for (i in c(1:length(unique(obs.$Site)))) {
  
  #i <- 5
  
  dplyr::filter(obs., Site == unique(obs.$Site)[i]) -> obs.i
  
  ymax. = max(c(obs.i$Nb.Blocs.Deplaces.non.remis, obs.i$Nb.Blocs.Deplaces.non.remis.norm15min, obs.i$Nb.Blocs.Retournes.non.remis, obs.i$Nb.Blocs.Retournes.non.remis.norm15min, obs.i$Nb.Blocs.Retournes.remis, obs.i$Nb.Blocs.Retournes.remis.norm15min), na.rm = T)
  
  plot(obs.i$date.sortie, obs.i$Nb.Blocs.Retournes.remis.norm15min, xlim = c(xmin.,xmax.), ylim = c(0,ymax.*1.05), xlab = "", ylab = "Nbr de blocs (normalisé pour 15 min d'obs.)", main = unique(obs.i$Site), col = "darkgreen")
  points(obs.i$date.sortie, obs.i$Nb.Blocs.Retournes.non.remis.norm15min, col = "red")
  points(obs.i$date.sortie, obs.i$Nb.Blocs.Deplaces.non.remis.norm15min, col = "orange")
  
  legend("bottom", inset = c(0,-0.4), legend = c("ret.re.","ret.n.re.", "dépl.n.re."), pch = c(19,19,19), col = c("darkgreen", "red", "orange"), horiz = TRUE, bty = "n", xpd = TRUE)
  
}

par(mfrow = c(1,1))

par(mfrow = c(3,2))

for (i in c(1:length(unique(obs.$Site)))) {
  
  #i <- 5
  
  dplyr::filter(obs., Site == unique(obs.$Site)[i]) -> obs.i
  
  plot(obs.i$date.sortie, obs.i$ratio.obs*100, xlim = c(xmin.,xmax.), ylim = c(0,105), xlab = "", ylab = "comportement relatif", main = unique(obs.i$Site), col = "black")
  
}

par(mfrow = c(1,1))

par(mfrow = c(3,2))

for (i in c(1:length(unique(obs.$Site)))) {
  
  #i <- 5
  
  dplyr::filter(obs., Site == unique(obs.$Site)[i]) -> obs.i
  
  plot(obs.i$date.sortie, obs.i$ratio.norm15min.obs*100, xlim = c(xmin.,xmax.), ylim = c(0,105), xlab = "", ylab = "comportement relatif (normalisé pour 15 min d'obs.)", main = unique(obs.i$Site), col = "black")
  
}

