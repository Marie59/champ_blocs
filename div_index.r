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
install.packages('adiv')
#####Load arguments

args <- commandArgs(trailingOnly = TRUE)

#####Import data

if (length(args) < 1) {
    stop("This tool needs at least 1 argument")
}else {
    qecnato0 <- args[1]
    bret_egmp_basq_qecb <- args[2]
}

qecnato0 <- readRDS(qecnato0)
bret_egmp_basq_qecb <- readRDS(bret_egmp_basq_qecb)

## Diversity index


# adiv contains two main functions for species diversity indices: speciesdiv, which includes widely used indices such as species richness and the Shannon index, and divparam, which includes indices that have a parameter to control the importance given to rare versus abundant species in diversity measurements (Pavoine (2020) - adiv: An r package to analyse biodiversity in ecology).

# NB: just like for dissimilarity distance matrices, no sense to use the "fishing" variable lists, because either they are present for the bloc mobile and not for the bloc fixe (therefore false higher diversity for bloc mobile), either they are repeated between face supérieure and face inférieure of bloc mobile.

# function in a loop

row.names(qecnato0) <- c(paste0(qecnato0$region.site_year_month_day, "_", qecnato0$Quadrat.bis, "_", qecnato0$Type.Bloc, "_", qecnato0$Numéro.Bloc.échantillon, "_", qecnato0$Face))

# later on I can copy-paste above code to recreate variable names vector
#bret_egmp_basq_qecb
#EGMP.BASQ_qecb
#Bret_EGMP.BASQ_fishing
#EGMP.BASQ_fishing

# remove boulder variables
bret_egmp_basq_qecb <- bret_egmp_basq_qecb[! bret_egmp_basq_qecb %in% c("X..Recouvrement.Sediment", "X..Roche.Nue", "X..Surface.Accolement")]

qecnato0$period <- as.character(qecnato0$period)
qecnato0$Face <- as.character(qecnato0$Face)

div_list <- vector("list", length(unique(qecnato0$site_year_month_day)))

for (i in c(1:nrow(qecnato0))) {
  
  div_i <- dplyr::filter(qecnato0, site_year_month_day == unique(qecnato0$site_year_month_day)[i])
  
  ifelse(unique(div_i$region) == "Bretagne", var. <- c(bret_egmp_basq_qecb), var. <- c(bret_egmp_basq_qecb, EGMP.BASQ_qecb)) # Qu. : Why can't R's ifelse statements return vectors? => you can circumvent the problem if you assign the result inside the ifelse.

  #8 remove empty row cfr: In speciesdiv(div_i[, var.]) & divparam(div_i[, var.]) : empty communities should be discarded
  div_i <- dplyr::filter(div_i, rowSums(div_i[, var.]) > 0)
  
  div_i_speciesdiv <- adiv::speciesdiv(div_i[, var.])
  adiv_i_df <- data.frame(div_i_speciesdiv)
  
  div_i_divparam <- adiv::divparam(div_i[, var.], q = c(0, 0.25, 0.5, 1, 2, 4, 8)) # When q increases, abundant species are overweighted compared to rare species, we thus expect that the evenness in species weights decreases.

  png("diversity.png")
  par(mfrow = c (1, 1))
  plot(adiv::divparam(div_i[, var.], q = 0), main = unique(div_i$site_year_month_day))
  plot(adiv::divparam(div_i[, var.], q = 0:10), legend = FALSE, main = unique(div_i$site_year_month_day))
  
  adiv_i_df$x <- div_i_divparam$div$`1`
  colnames(adiv_i_df)[which(colnames(adiv_i_df) == "x")] <- paste0("Para. ISD, q = ", div_i_divparam$q[1], " (equi. richness)")
  adiv_i_df$x <- div_i_divparam$div$`2`
  colnames(adiv_i_df)[which(colnames(adiv_i_df) == "x")] <- paste0("Para. ISD, q = ", div_i_divparam$q[2])
  adiv_i_df$x <- div_i_divparam$div$`3`
  colnames(adiv_i_df)[which(colnames(adiv_i_df) == "x")] <- paste0("Para. ISD, q = ", div_i_divparam$q[3])
  adiv_i_df$x <- div_i_divparam$div$`4`
  colnames(adiv_i_df)[which(colnames(adiv_i_df) == "x")] <- paste0("Para. ISD, q = ", div_i_divparam$q[4])
  adiv_i_df$x <- div_i_divparam$div$`5`
  colnames(adiv_i_df)[which(colnames(adiv_i_df) == "x")] <- paste0("Para. ISD, q = ", div_i_divparam$q[5], " (equi. Simpson)")
  adiv_i_df$x <- div_i_divparam$div$`6`
  colnames(adiv_i_df)[which(colnames(adiv_i_df) == "x")] <- paste0("Para. ISD, q = ", div_i_divparam$q[6])
  adiv_i_df$x <- div_i_divparam$div$`7`
  colnames(adiv_i_df)[which(colnames(adiv_i_df) == "x")] <- paste0("Para. ISD, q = ", div_i_divparam$q[7])
  
  # plot
  par(mfrow = c(3, 2))
  sapply(names(adiv_i_df[, c(1, 8, 2, 3, 12, 4:7, 9:11, 13:ncol(adiv_i_df))]), 
         function(cname) {
           png(paste0(cname, "_histo.png"))
           hist(adiv_i_df[, c(1, 8, 2, 3, 12, 4:7, 9:11, 13:ncol(adiv_i_df))][[cname]], main = "", xlab = cname, breaks = length(unique(adiv_i_df[, c(1, 8, 2, 3, 12, 4:7, 9:11, 13:ncol(adiv_i_df))][[cname]])))
         }
)
  par(mfrow = c(1,1))
  
  div_list[[i]] <- adiv_i_df
  
  rm(div_i, adiv_i_df, div_i_speciesdiv, div_i_divparam)
  
}

# for the error message due to richness NA data => 35 observations in 21 surveys; no reason to remove these data "...remo", was checked in the complete script.


div_df <- do.call("rbind", div_list)


# There is an issue with region.terri that are merged with no "_" ...

div_df <- tibble::add_column(div_df, rownames. = rownames(div_df), .before = "richness")
div_df <- tidyr::separate(div_df, rownames., into = c("region.terri.", "site_year_month_day", "Quadrat.bis", "Type.Bloc", "Numéro.Bloc.échantillon", "Face"), sep = "_")

# I therefore add these lines to solve that issue

div_df <- tibble::add_column(div_df, terri. = substring(div_df$region.terri., nchar(div_df$region.terri.)-3), .after = "region.terri.")

div_df$region.terri. <- substring(div_df$region.terri., 1, nchar(div_df$region.terri)-4)
div_df <- dplyr::rename(div_df, region = region.terri.)

div_df$site_year_month_day <- paste0(div_df$terri., "_", div_df$site_year_month_day)
div_df <- subset(div_df, select = -c(terri.))

div_df$Type.Bloc <- as.factor(div_df$Type.Bloc)
div_df$Face <- as.factor(div_df$Face)
div_df$Numéro.Bloc.échantillon <- as.integer(div_df$Numéro.Bloc.échantillon)

saveRDS(div_df, "div_df.RDS")
write.table(div_df, "Valeurs_stat.tabular", row.names = FALSE, quote = FALSE, sep = "\t", dec = ".", fileEncoding = "UTF-8")
rm(div_i, div_list, i, var.)
