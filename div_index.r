## Diversity index

library(adiv)
# adiv contains two main functions for species diversity indices: speciesdiv, which includes widely used indices such as species richness and the Shannon index, and divparam, which includes indices that have a parameter to control the importance given to rare versus abundant species in diversity measurements (Pavoine (2020) - adiv: An r package to analyse biodiversity in ecology).

# NB: just like for dissimilarity distance matrices, no sense to use the "fishing" variable lists, because either they are present for the bloc mobile and not for the bloc fixe (therefore false higher diversity for bloc mobile), either they are repeated between face supérieure and face inférieure of bloc mobile.

# function in a loop

row.names(qecbNato0) <- c(paste0(qecbNato0$region.Site.Year.Month.Day, "_", qecbNato0$Quadrat.bis, "_", qecbNato0$Type.Bloc, "_", qecbNato0$Numéro.Bloc.échantillon, "_", qecbNato0$Face))

# later on I can copy-paste above code to recreate variable names vector
Bret_EGMP.BASQ_qecb
EGMP.BASQ_qecb
Bret_EGMP.BASQ_fishing
EGMP.BASQ_fishing

# remove boulder variables
Bret_EGMP.BASQ_qecb <- Bret_EGMP.BASQ_qecb[! Bret_EGMP.BASQ_qecb %in% c("X..Recouvrement.Sediment", "X..Roche.Nue", "X..Surface.Accolement")]

qecbNato0$period <- as.character(qecbNato0$period)
qecbNato0$Face <- as.character(qecbNato0$Face)

div.list <- vector("list", length(unique(qecbNato0$Site.Year.Month.Day)))

for (i in c(1:nrow(qecbNato0))) {
  
  #i <- 7
  
  dplyr::filter(qecbNato0, Site.Year.Month.Day == unique(qecbNato0$Site.Year.Month.Day)[i]) -> div.i
  
  ifelse(unique(div.i$region) == "Bretagne", var. <- c(Bret_EGMP.BASQ_qecb), var. <- c(Bret_EGMP.BASQ_qecb, EGMP.BASQ_qecb)) # Qu. : Why can't R's ifelse statements return vectors? => you can circumvent the problem if you assign the result inside the ifelse.
  var.
  
  #8 remove empty row cfr: In speciesdiv(div.i[, var.]) & divparam(div.i[, var.]) : empty communities should be discarded
  div.i <- dplyr::filter(div.i, rowSums(div.i[, var.]) > 0)
  
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

# for the error message due to richness NA data => 35 observations in 21 surveys; no reason to remove these data "...remo", was checked in the complete script.


div.df. <- do.call("rbind", div.list)


# There is an issue with region.terri that are merged with no "_" ...

div.df. <- tibble::add_column(div.df., rownames. = rownames(div.df.), .before = "richness")
div.df. <- tidyr::separate(div.df., rownames., into = c("region.terri.", "Site.Year.Month.Day", "Quadrat.bis", "Type.Bloc", "Numéro.Bloc.échantillon", "Face"), sep = "_")

# I therefore add these lines to solve that issue
unique(div.df.$region.terri.)
unique(substring(div.df.$region.terri., nchar(div.df.$region.terri.)-3))
div.df. <- tibble::add_column(div.df., terri. = substring(div.df.$region.terri., nchar(div.df.$region.terri.)-3), .after = "region.terri.")
unique(substring(div.df.$region.terri., 1, nchar(div.df.$region.terri.)-4))
div.df.$region.terri. <- substring(div.df.$region.terri., 1, nchar(div.df.$region.terri)-4)
div.df. <- dplyr::rename(div.df., region = region.terri.)

div.df.$Site.Year.Month.Day <- paste0(div.df.$terri., "_", div.df.$Site.Year.Month.Day)
div.df. <- subset(div.df., select = -c(terri.))

div.df.$Type.Bloc <- as.factor(div.df.$Type.Bloc)
div.df.$Face <- as.factor(div.df.$Face)
div.df.$Numéro.Bloc.échantillon <- as.integer(div.df.$Numéro.Bloc.échantillon)

saveRDS(div.df., "div.df.RDS")

rm(div.i, div.list, i, var.)
