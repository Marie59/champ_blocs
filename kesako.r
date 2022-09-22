# author: "Jonathan Richir"
# date: "19 April 2021"


## Set environment and install packages


#setwd("C:/Users/jonat/ULiege/RStudio/CB_Concarneau")
#Sys.setenv(TZ = "UTC")
#Sys.setlocale("LC_TIME", "English")


#unwanted_array = list('Š'='S', 'š'='s', 'Ž'='Z', 'ž'='z', 'À'='A', 'Á'='A', 'Â'='A', 'Ã'='A', 'Ä'='A', 'Å'='A', 'Æ'='A', 'Ç'='C', 'È'='E', 'É'='E',
#                      'Ê'='E', 'Ë'='E', 'Ì'='I', 'Í'='I', 'Î'='I', 'Ï'='I', 'Ñ'='N', 'Ò'='O', 'Ó'='O', 'Ô'='O', 'Õ'='O', 'Ö'='O', 'Ø'='O', 'Ù'='U',
#                      'Ú'='U', 'Û'='U', 'Ü'='U', 'Ý'='Y', 'Þ'='B', 'ß'='Ss', 'à'='a', 'á'='a', 'â'='a', 'ã'='a', 'ä'='a', 'å'='a', 'æ'='a', 'ç'='c',
#                      'è'='e', 'é'='e', 'ê'='e', 'ë'='e', 'ì'='i', 'í'='i', 'î'='i', 'ï'='i', 'ð'='o', 'ñ'='n', 'ò'='o', 'ó'='o', 'ô'='o', 'õ'='o',
#                      'ö'='o', 'ø'='o', 'ù'='u', 'ú'='u', 'û'='u', 'ý'='y', 'ý'='y', 'þ'='b', 'ÿ'='y'
#                      )


# Don't necessarily run below code, since we now directly refer to the ficheterrain file to get the info for variables Date and Site ; but if not, there is some adaptation to do later on; WITHOUT changing any variable names since they are used in other subsequent R script !!
#################################################
#Rscript

###############################
##  ##
###############################

#####Packages : 
#               
#               

#####Load arguments

args <- commandArgs(trailingOnly = TRUE)

#####Import the S2 data

if (length(args) < 1) {
    stop("This tool needs at least 1 argument")
}else {
    input_data <- args[1]
    input_data2 <- args[2]
    fiche_val <- args[3]
    fiche_term <- args[4]

}


ivr <- read.csv2(input_data, header = FALSE)
#("raw_data/ESTAMP_CB/données CdB validées/champbloc_ivr.csv", header = FALSE)
class(ivr)
head(ivr)
names. <- as.vector(unlist(ivr[1,]))
names. <- gsub(" ", ".", names.)
colnames(ivr) <- names.
ivr <- ivr[-1,]
ivr <- ivr[, -17]

#for(i in seq_along(unwanted_array))
#  out <- gsub(names(unwanted_array)[i], unwanted_array[i], ivr)

#library(tidyr)
# NB inversion between id and ID.Fiche when compared to ivr
ivr <- tidyr::rename(ivr, XX = ID.Fiche)
ivr <- tidyr::rename(ivr, ID.Fiche = id)
ivr <- tidyr::rename(ivr, id = XX)
ivr %>% separate(ID.Fiche, into = c("X1", "X2", "X3", "X4", "X5", "X6", "X7"), sep = c("_", "-", "."), remove = F) -> ivr

unique(ivr$X1)
unique(ivr$X2)
unique(ivr$X3)
unique(ivr$X4)
unique(ivr$X5)
unique(ivr$X6)
unique(ivr$X7)

ivr <- ivr[ , -which(names(ivr) %in% c("X1", "X2", "X3", "X4", "X5", "X6", "X7"))]


library(stringr)
df. <- data.frame(matrix(ncol = 10, nrow = nrow(ivr)))
for (i in c(1:nrow(ivr))) {
  id <- (unlist(str_extract_all(ivr$ID.Fiche[i], "[A-Za-z-0-9]+")))
  df.[i,1:length(id)] <- id
}
head(df.)

unique(df.$X1)
unique(df.$X2)
unique(df.$X3)
unique(df.$X4)
unique(df.$X5)
unique(df.$X6)
unique(df.$X7)
unique(df.$X8)
unique(df.$X9)
unique(df.$X10)
df.$X10 <- ivr$ID.Fiche  

df. <- select(df., -c(1,2))


# Site

unique(df.$X3)
df.$X3 <- ifelse(df.$X3 %in% c("QECB", "IVR"), NA, df.$X3)   
(letter. <- as.vector(unique((unlist(str_extract_all(df.$X3, "[A-Za-z]+"))))))
df.$X3.bis <- ifelse(df.$X3 %in% letter., df.$X3, NA) 
unique(df.$X3.bis)

unique(df.$X4)
df.$X4.bis <- df.$X4
df.$X4.bis <- ifelse(df.$X4.bis == "20160917-Quemenes", "Quemenes", df.$X4.bis)
(letter. <- as.vector(unique((unlist(str_extract_all(df.$X4.bis, "[A-Za-z]+"))))))
df.$X4.bis <- ifelse(df.$X4.bis %in% letter., df.$X4.bis, NA) 
unique(df.$X4.bis)

unique(df.$X5)
(letter. <- as.vector(unique((unlist(str_extract_all(df.$X5, "[A-Za-z]+"))))))
df.$X5.bis <- ifelse(df.$X5 %in% letter.[c(4:9)], df.$X5, NA) 
unique(df.$X5.bis)

unique(df.$X6)
(letter. <- as.vector(unique((unlist(str_extract_all(df.$X6, "[A-Za-z]+"))))))
df.$X6.bis <- ifelse(df.$X6 %in% letter.[c(3,5)], df.$X6, NA) 
unique(df.$X6.bis)

df.$X3456.bis <- paste0(df.$X3.bis, "_", df.$X4.bis, "_", df.$X5.bis, "_", df.$X6.bis)
unique(df.$X3456.bis)
df.$X3456.bis <- str_remove_all(df.$X3456.bis, "_NA")
unique(df.$X3456.bis)
df.$X3456.bis <- str_remove_all(df.$X3456.bis, "NA_")
unique(df.$X3456.bis)

# For site name cfr Pauline email

#tri des stations d'étude champs de blocs (à chaque fois je précise le territoire en 4 lettres et le nom de la station d'étude) :
# Golfe Normand Breton (GONB) - Îlot Saint-Michel : "IlotStMichel"  
# Côtes d'Armor (ARMO) - Îlot du Verdelet : "Verdelet"
# Côtes d'Armor (ARMO) - Piégu : "Piegu"    
# Côtes d'Armor (ARMO) - Pointe de Bilfot : "PteBilfot"   
# Côtes d'Armor (ARMO) - Île Plate : "IlePlate"   
# Pays de Morlaix (PDMO) - Perharidy : "Perharidy"    
# Rade de Brest (BRES) - Keraliou : "Keraliou"     
# Finistère (FINS) - Pointe de Mousterlin : "Mousterlin", "Moust"     
# Finistère (FINS) - Saint-Nicolas des Glénan : "SNG" 
# Golfe du Morbihan (GDMO) - Locmariaquer : "Locmariaquer"  
# Golfe du Morbihan (GDMO) - Beg Lann : "BegLann"       
# Plateau du Four (FOUR) - Plateau du Four : "PFour"       
# Estuaire de la Gironde et Mer des Pertuis (EGMP) - Grouin du Cou : "Groinducou", "GroinduCou", "GrouinDuCou"  
# Estuaire de la Gironde et Mer des Pertuis (EGMP) - Le Pas d'Ensembert : "Ensembert"   
# Estuaire de la Gironde et Mer des Pertuis (EGMP) - Le Perré d'Antiochat : "PerAnt", "PerAntiochat"
# Estuaire de la Gironde et Mer des Pertuis (EGMP) - La Brée-les-Bains : "LaBree", "La_Bree"
# Estuaire de la Gironde et Mer des Pertuis (EGMP) - Chassiron : "Chassiron"    
# Littoral Basque (BASQ) - Les Flots Bleus / Zone pêcheurs : "FB_ZP", "FlotsBleus_ZP"
# Littoral Basque (BASQ) - Les Flots Bleus / Zone familles : "FlotsBleus_ZF", "FB_ZF"        
#Pour les sites hors Life du Parc naturel marin d'Iroise, je ne suis pas sure de moi car je ne les ai jamais traités. De mémoire, il y en avait deux : une à l'île de Sein et l'autre à Quéménès. Mais c'est bien possible qu'il y en ait d'autres (...)

df.$X3456.bis <- ifelse(df.$X3456.bis == "Locmariaquer", "GDMO_Locmariaquer", df.$X3456.bis)
df.$X3456.bis <- ifelse(df.$X3456.bis == "BegLann", "GDMO_BegLann", df.$X3456.bis)
df.$X3456.bis <- ifelse(df.$X3456.bis %in% c("PFour", "PlateauFour"), "FOUR_PlateauFour", df.$X3456.bis)
df.$X3456.bis <- ifelse(df.$X3456.bis %in% c("Groinducou", "GroinduCou", "GrouinDuCou"), "EGMP_GroinCou", df.$X3456.bis)
df.$X3456.bis <- ifelse(df.$X3456.bis == "Ensembert", "EGMP_PasEmsembert", df.$X3456.bis)
df.$X3456.bis <- ifelse(df.$X3456.bis %in% c("LaBree", "La_Bree"), "EGMP_BreeBains", df.$X3456.bis)
df.$X3456.bis <- ifelse(df.$X3456.bis %in% c("PerAnt", "PerAntiochat"), "EGMP_PerreAntiochat", df.$X3456.bis)
df.$X3456.bis <- ifelse(df.$X3456.bis == "Chassiron", "EGMP_Chassiron", df.$X3456.bis)
df.$X3456.bis <- ifelse(df.$X3456.bis %in% c("FB_ZP", "FlotsBleus_ZP"), "BASQ_FlotsBleusZP", df.$X3456.bis)
df.$X3456.bis <- ifelse(df.$X3456.bis %in% c("FlotsBleus_ZF", "FB_ZF"), "BASQ_FlotsBleusZF", df.$X3456.bis)
df.$X3456.bis <- ifelse(df.$X3456.bis == "IlotStMichel", "GONB_IlotStMichel", df.$X3456.bis)
df.$X3456.bis <- ifelse(df.$X3456.bis == "Quemenes", "FINS_Quemenes", df.$X3456.bis)
df.$X3456.bis <- ifelse(df.$X3456.bis == "Sein_Goulenez", "FINS_SeinGoulenez", df.$X3456.bis)
#df.$X3456.bis <- ifelse(df.$X3456.bis == "Sein", "FINS_Sein", df.$X3456.bis)
df.$X3456.bis <- ifelse(df.$X3456.bis == "Sein", "FINS_SeinKilaourou", df.$X3456.bis)
df.$X3456.bis <- ifelse(df.$X3456.bis == "Sein_kilaourou", "FINS_SeinKilaourou", df.$X3456.bis)
df.$X3456.bis <- ifelse(df.$X3456.bis == "Verdelet", "ARMO_Verdelet", df.$X3456.bis)
df.$X3456.bis <- ifelse(df.$X3456.bis == "Piegu", "ARMO_Piegu", df.$X3456.bis)
df.$X3456.bis <- ifelse(df.$X3456.bis == "PteBilfot", "ARMO_Bilfot", df.$X3456.bis)
df.$X3456.bis <- ifelse(df.$X3456.bis == "IlePlate", "ARMO_IlePlate", df.$X3456.bis)
df.$X3456.bis <- ifelse(df.$X3456.bis %in% c("Perharidy_Import", "Perharidy"), "PDMO_Perharidy", df.$X3456.bis)
df.$X3456.bis <- ifelse(df.$X3456.bis %in% c("Keraliou_Import", "Keraliou"), "BRES_Keraliou", df.$X3456.bis)
df.$X3456.bis <- ifelse(df.$X3456.bis %in% c("Mousterlin", "Moust"), "FINS_Mousterlin", df.$X3456.bis)
df.$X3456.bis <- ifelse(df.$X3456.bis == "SNG", "FINS_StNicolasGlenan", df.$X3456.bis)

unique(df.$X3456.bis)
df. %>% filter(is.na(df.$X3456.bis))
df. <- rename(df., Site = X3456.bis)

df.$Site.bis <- NA
unique(df.$Site)

df.$Site.bis <- ifelse(df.$Site == "GDMO_Locmariaquer", "Locmariaquer", df.$Site.bis)
df.$Site.bis <- ifelse(df.$Site == "GDMO_BegLann", "Beg Lann", df.$Site.bis)
df.$Site.bis <- ifelse(df.$Site == "FOUR_PlateauFour", "Plateau du Four", df.$Site.bis)
df.$Site.bis <- ifelse(df.$Site == "EGMP_GroinCou", "Groin du Cou", df.$Site.bis)
df.$Site.bis <- ifelse(df.$Site == "EGMP_PasEmsembert", "Le Pas d'Emsembert", df.$Site.bis)
df.$Site.bis <- ifelse(df.$Site == "EGMP_BreeBains", "La Brée-les-Bains", df.$Site.bis)
df.$Site.bis <- ifelse(df.$Site == "EGMP_PerreAntiochat", "Le Perré d'Antiochat", df.$Site.bis)
df.$Site.bis <- ifelse(df.$Site == "EGMP_Chassiron", "Chassiron", df.$Site.bis)
df.$Site.bis <- ifelse(df.$Site == "BASQ_FlotsBleusZP", "Les Flots Bleus / zone pêcheurs", df.$Site.bis)
df.$Site.bis <- ifelse(df.$Site == "BASQ_FlotsBleusZF", "Les Flots Bleus / zone familles", df.$Site.bis)
df.$Site.bis <- ifelse(df.$Site == "GONB_IlotStMichel", "Îlot Saint-Michel", df.$Site.bis)
df.$Site.bis <- ifelse(df.$Site == "FINS_Quemenes", "Quéménès", df.$Site.bis)
df.$Site.bis <- ifelse(df.$Site == "FINS_SeinGoulenez", "Île de Sein - Goulenez", df.$Site.bis)
#df.$Site.bis <- ifelse(df.$Site == "FINS_Sein", "Île de Sein", df.$Site.bis)
df.$Site.bis <- ifelse(df.$Site == "FINS_SeinKilaourou", "Île de Sein - Kilaourou", df.$Site.bis)
df.$Site.bis <- ifelse(df.$Site == "ARMO_Verdelet", "Îlot du Verdelet", df.$Site.bis)
df.$Site.bis <- ifelse(df.$Site == "ARMO_Piegu", "Piégu", df.$Site.bis)
df.$Site.bis <- ifelse(df.$Site == "ARMO_Bilfot", "Pointe de Bilfot", df.$Site.bis)
df.$Site.bis <- ifelse(df.$Site == "ARMO_IlePlate", "Île Plate", df.$Site.bis)
df.$Site.bis <- ifelse(df.$Site == "PDMO_Perharidy", "Perharidy", df.$Site.bis)
df.$Site.bis <- ifelse(df.$Site == "BRES_Keraliou", "Keraliou", df.$Site.bis)
df.$Site.bis <- ifelse(df.$Site == "FINS_Mousterlin", "Pointe de Mousterlin", df.$Site.bis)
df.$Site.bis <- ifelse(df.$Site == "FINS_StNicolasGlenan", "Saint-Nicolas des Glénan", df.$Site.bis)


# Date
# NB: two values have to be changed prior selecting according to number, for "20160917-Quemenes" & "oct2014"

unique(df.$X3)
(nb. <- as.vector(unique((unlist(str_extract_all(df.$X3, "[0-9]+"))))))
df.$X3.tris <- ifelse(df.$X3 %in% nb., df.$X3, NA) 
unique(df.$X3.tris)

unique(df.$X4)
df.$X4.tris <- df.$X4
df.$X4.tris <- ifelse(df.$X4.tris == "20160917-Quemenes", "20160917", df.$X4.tris)
unique(df.$X4.tris)
(nb. <- as.vector(unique((unlist(str_extract_all(df.$X4.tris, "[0-9]+"))))))
df.$X4.tris <- ifelse(df.$X4.tris %in% nb., df.$X4.tris, NA) 
unique(df.$X4.tris)

unique(df.$X5)
df.$X5.tris <- df.$X5
df.$X5.tris <- ifelse(df.$X5.tris == "oct2014", "201410", df.$X5.tris)
unique(df.$X5.tris)
(nb. <- as.vector(unique((unlist(str_extract_all(df.$X5.tris, "[0-9]+"))))))
df.$X5.tris <- ifelse(df.$X5.tris %in% nb., df.$X5.tris, NA) 
unique(df.$X5.tris)

unique(df.$X6)
(nb. <- as.vector(unique((unlist(str_extract_all(df.$X6, "[0-9]+"))))))
df.$X6.tris <- ifelse(df.$X6 %in% nb., df.$X6, NA) 
unique(df.$X6.tris)

unique(df.$X7)
(nb. <- as.vector(unique((unlist(str_extract_all(df.$X7, "[0-9]+"))))))
df.$X7.tris <- ifelse(df.$X7 %in% nb., df.$X7, NA) 
unique(df.$X7.tris)

df.$X3456.tris <- paste0(df.$X3.tris, "_", df.$X4.tris, "_", df.$X5.tris, "_", df.$X6.tris, "_", df.$X7.tris)
unique(df.$X3456.tris)


df.$Date <-  NA

unique(df.$X4.tris)
for (i in c(1:nrow(df.))) {
  df.$Date[i] <- ifelse(nchar(as.vector(df.$X4.tris)[i]) %in% c(3:12), df.$X4.tris[i], NA)
}
unique(df.$Date)

unique(df.$X5.tris)
for (i in c(1:length(df.$X5.tris))) {
  df.$Date[i] <- ifelse(nchar(as.vector(df.$X5.tris)[i]) %in% c(3:12), df.$X5.tris[i], df.$Date[i])
}
unique(df.$Date)

unique(df.$X6.tris)
for (i in c(1:length(df.$X6.tris))) {
  df.$Date[i] <- ifelse(nchar(as.vector(df.$X6.tris)[i]) %in% c(3:12), df.$X6.tris[i], df.$Date[i])
}
unique(df.$Date)

df. %>% filter(is.na(df.$Date))

unique(df.$Date)


df.$Date <- ifelse(df.$Date == "22032015", "20150322", df.$Date)
df.$Date <- ifelse(df.$Date == "08092014", "20140908", df.$Date)
df.$Date <- ifelse(df.$Date == "11092014", "20140911", df.$Date)
unique((df. %>% filter(Date == "201410"))[, c("X10", "Date")])     
df.$Date <- ifelse(df.$Date == "201410", "20141001", df.$Date) # cfr EGMP_BreeBains, 2014-10-10/11; change later on.
df.$Date <- ifelse(df.$Date == "23032015", "20150323", df.$Date)
df.$Date <- ifelse(df.$Date == "27102015", "20151027", df.$Date)
df.$Date <- ifelse(df.$Date == "18042015", "20150418", df.$Date)
df.$Date <- ifelse(df.$Date == "2014042830", "20140428", df.$Date)
df.$Date <- ifelse(df.$Date == "17052014", "20140517", df.$Date)
unique((df. %>% filter(Date == "201503"))[, c("X10", "Date")]) # Why 03&05 for PDMO_Perharidy?? actually, 4 quadrats in 03 and quadrat 5th in 05 ! considered as an unique quadrat in 05, make more sense even if only one quadrat in 05; change later on
df. %>% filter(Date == "201503")
df.$Date <- ifelse(df.$Date == "201503", "20150301", df.$Date) 
df.$Date <- ifelse(df.$Date == "2015102728", "20151027", df.$Date)

unique(df.$Date)

#df.$DateYmd <- as.Date(df.$Date, format = "%Y%m%d" , origin = "1970-01-01")


# add the two new variables with new site name and date in the ivr df.

library("tibble")
ivr <- add_column(ivr, df.["Site"], .after = "ID.Fiche")
ivr <- add_column(ivr, df.["Site.bis"], .after = "Site")
ivr <- add_column(ivr, df.["Date"], .after = "Site.bis")

unique(ivr$Site)
unique(ivr$Site.bis)

rm(df., i, id, letter., names., nb.)


#library(tidyr)
#ivr %>% separate(DateYmd, c("Year", "Month", "Day"), sep = "-", remove = F) -> ivr


# change the two dates fixed to the 1st of the considered month

unique(ivr$Site)

ivr %>% filter(Site == "EGMP_BreeBains" & Date == "20141001")
ivr$Date <- ifelse(ivr$Site == "EGMP_BreeBains" & ivr$Date == "20141001", "20141010", ivr$Date)
ivr %>% filter(Site == "EGMP_BreeBains" & Date == "20141001")
ivr %>% filter(Site == "EGMP_BreeBains" & Date == "20141010")

ivr %>% filter(Site == "PDMO_Perharidy" & Date == "20150301")
ivr$Date <- ifelse(ivr$Site == "PDMO_Perharidy" & ivr$Date == "20150301", "20150319", ivr$Date)
ivr %>% filter(Site == "EGMP_BreeBains" & Date == "20141001")
ivr %>% filter(Site == "EGMP_BreeBains" & Date == "20141010")
#NB: obvisouly, no more reason to make the changes below after checking for dates in fiche df.; changed with above lines to the 19th of March 2015
#ivr$Date <- ifelse(ivr$Site == "PDMO_Perharidy" & ivr$Date == "20150301" & ivr$Numero.Quadrat %in% c("1","2","3","4"), "20150319", ivr$Date)
#ivr$Date <- ifelse(ivr$Site == "PDMO_Perharidy" & ivr$Date == "20150301" & ivr$Numero.Quadrat == "5", "20150517", ivr$Date)
#ivr %>% filter(Site == "PDMO_Perharidy" & Date == "20150301")
#ivr %>% filter(Site == "PDMO_Perharidy" & Date == "20150319")
#ivr %>% filter(Site == "PDMO_Perharidy" & Date == "20150517")

library(tidyr)
DateYmd <- as.Date(ivr$Date, format = "%Y%m%d" , origin = "1970-01-01")
ivr <- add_column(ivr, DateYmd, .after = "Date")
rm(DateYmd)


## import suppl. observations ESTAMP 2018+

ivr.next <- read.csv2(input_data2, header = FALSE)
class(ivr.next)
head(ivr.next)
names. <- as.vector(unlist(ivr.next[1,]))
names. <- gsub(" ", ".", names.)
colnames(ivr.next) <- names.
rm(names.)
ivr.next <- ivr.next[-1,]
ivr.next <- ivr.next[, -17]

unique(ivr.next$id)
unique(str_sub(ivr.next$id,12))
unique(ivr.next$ID.Fiche)

ivr.next <- ivr.next[, c(2,1,3:16)]

Site <- rep(NA, nrow(ivr.next))
ivr.next <- add_column(ivr.next, Site, .after = "ID.Fiche")
Site.bis <- rep(NA, nrow(ivr.next))
ivr.next <- add_column(ivr.next, Site.bis, .after = "Site")
Date <- rep(NA, nrow(ivr.next))
ivr.next <- add_column(ivr.next, Date, .after = "Site.bis")
DateYmd <- rep(NA, nrow(ivr.next))
ivr.next <- add_column(ivr.next, DateYmd, .after = "Date")
rm(Site, Site.bis, Date, DateYmd)

ivr.next$DateYmd <- as.Date(str_sub(ivr.next$id, start = 1, end = 10), origin = "1970-01-01") 

unique(ivr.next$ID.Fiche)
ivr.next$Site <- ifelse(ivr.next$ID.Fiche %in% c("392417", "392521", "406229", "424249", "444815", "455567"), "GDMO_Locmariaquer", ivr.next$Site)
ivr.next$Site <- ifelse(ivr.next$ID.Fiche %in% c("477617", "477714", "477750", "477757"), "FINS_SeinGoulenez", ivr.next$Site)
ivr.next$Site <- ifelse(ivr.next$ID.Fiche %in% c("477640",  "477733", "477743"), "FINS_SeinKilaourou", ivr.next$Site)
ivr.next$Site <- ifelse(ivr.next$ID.Fiche %in% c("478379"), "FINS_Quemenes", ivr.next$Site)

ivr.next$Site.bis <- ifelse(ivr.next$Site == "GDMO_Locmariaquer", "Locmariaquer", ivr.next$Site.bis)
ivr.next$Site.bis <- ifelse(ivr.next$Site == "FINS_Quemenes", "Quéménès", ivr.next$Site.bis)
ivr.next$Site.bis <- ifelse(ivr.next$Site == "FINS_SeinGoulenez", "Île de Sein - Goulenez", ivr.next$Site.bis)
ivr.next$Site.bis <- ifelse(ivr.next$Site == "FINS_SeinKilaourou", "Île de Sein - Kilaourou", ivr.next$Site.bis)


## bind ivr ESTAMP dfs.

names(ivr) ; names(ivr.next)
setdiff(names(ivr), names(ivr.next))
# id and ID.Fiche names are inverted between dfs. !!
ivr.next <- rename(ivr.next, x. = id)
ivr.next <- rename(ivr.next, id = ID.Fiche)
ivr.next <- rename(ivr.next, ID.Fiche = x.)
ivr.next <- ivr.next[, c(2,1,3:20)]

ivr <- bind_rows(ivr, ivr.next)

rm(ivr.next)
