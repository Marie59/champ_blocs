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
    input_qecbnato0 <- args[1]
    input_ivr <- args[2]
    input_dissim <- args[3]
    input_div <- args[4]
    input_freq <- args[5]
    input_obs <- args[6]
    input_inter <- args[7]

}

## import dfs

# df below are created via their respective R scripts and stored in their respective file.

qecbnato0 <- readRDS(input_qecbnato0) # last qecb df corrected for surf accollement and missing 0 data

ivr_val_qu <- readRDS(input_ivr)

matri_full <- readRDS(input_dissim)

div_ <- readRDS(input_div)
div_ <- tibble::add_column(div_, Site = stringr::str_sub(div_$site_year_month_day, end = -12), .after = "region")
div_ <- tidyr::separate(div_, site_year_month_day, into = c("Terri", "Site", "Year", "Month", "Day"), remove = FALSE)
div_$Site <- paste0(div_$Terri, "_", div_$Site)
div_ <- subset(dplyr::select(div_, -c("Terri")))

# df below are suppl. df created for modeling purpose. The script that lead to their building is not in the shared CdB project currently; only are giver the resulting df from frequentation, fishing comportment and oceanography data analysis.

freq_  <- readRDS(input_freq)
obs_  <- readRDS(input_obs)

# do not keep Maud Bernard thesis suppl. sites 23-26 in below list df. for oceanography data
list_date_mean  <- readRDS(input_inter)
list_date_mean <- list_date_mean[1:22]

## intersite comparison boxplot, sampling effort & statistics


names(qecbnato0)[which(colnames(qecbnato0) == "période")] <- "semester"
qecbnato0$semester <- ifelse(qecbnato0$semester == "p1", "s1", "s2")
qecbnato0 <- tibble::add_column(qecbnato0, Annee_semester = NA, .after = "semester")
qecbnato0$Annee_semester <- paste0(qecbnato0$Year, "_", qecbnato0$semester)

qecbnato0$Annee_semester <- ifelse(qecbnato0$Annee_semester == "NA_NA", NA, qecbnato0$Annee_semester)

qecbnato0_bm <- dplyr::filter(qecbnato0, Type.Bloc == "Bloc mobile")
qecbnato0_bm_fs <- dplyr::filter(qecbnato0, Type.Bloc == "Bloc mobile", Face == "face supérieure")
qecbnato0_bm_fi <- dplyr::filter(qecbnato0, Type.Bloc == "Bloc mobile", Face == "face inférieure")
qecbnato0_bf <- dplyr::filter(qecbnato0, Type.Bloc %in% c("Bloc fixé", "Roche en place"))

qecbnato0_bm_fs_stats_effort <- na.omit(qecbnato0_bm_fs[, c("Site", "Annee_semester")]) %>% dplyr::group_by(Site, Annee_semester) %>% dplyr::summarize(nb. = dplyr::n())
qecbnato0_bm_fs_stats_effort <- qecbnato0_bm_fs_stats_effort %>% tidyr::spread(Annee_semester, nb.)
qecbnato0_bm_fi_stats_effort <- na.omit(qecbnato0_bm_fi[, c("Site", "Annee_semester")]) %>% dplyr::group_by(Site, Annee_semester) %>% dplyr::summarize(nb. = dplyr::n())
qecbnato0_bm_fi_stats_effort <- qecbnato0_bm_fi_stats_effort %>% tidyr::spread(Annee_semester, nb.)

# https://bookdown.org/Maxine/r4ds/comparing-two-data-frames-tibbles.html
dplyr::all_equal(qecbnato0_bm_fs_stats_effort, qecbnato0_bm_fi_stats_effort)
qecbnato0_bm_stats_effort <- qecbnato0_bm_fs_stats_effort

qecbnato0_bf_stats_effort <- na.omit(qecbnato0_bf[, c("Site", "Annee_semester")]) %>% dplyr::group_by(Site, Annee_semester) %>% dplyr::summarize(nb. = dplyr::n())
qecbnato0_bf_stats_effort <- qecbnato0_bf_stats_effort %>% tidyr::spread(Annee_semester, nb.)


# % accolement

qecbnato0$Site <- factor(qecbnato0$Site)

qecbnato0_bm_fi <- dplyr::filter(qecbnato0, Type.Bloc == "Bloc mobile" & Face == "face inférieure")

p_acco <- ggplot2::ggplot(qecbnato0_bm_fi, ggplot2::aes(x = Site, y = X..Surface.Accolement)) + 
  ggplot2::geom_boxplot() + 
  #ggplot2::geom_jitter(shape = 16, position=position_jitter(0.2)) +
  ggplot2::xlab("") +
  ggplot2::ylab("Surface d'accolement de FI.BM (%)") +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1))

ggplot2::ggsave("8_Accolement.png", p_acco)

qecbnato0_bm_fi_stats_site_accol <- na.omit(qecbnato0_bm_fi[, c("Site", "X..Surface.Accolement")]) %>% dplyr::group_by(Site) %>% dplyr::summarise(min. = min(X..Surface.Accolement), max. = max(X..Surface.Accolement), mean. = mean(X..Surface.Accolement), median. = median(X..Surface.Accolement), nb = dplyr::n())
qecbnato0_bm_fi_stats_site_accol %>% print(n = nrow(qecbnato0_bm_fi_stats_site_accol))
saveRDS(qecbnato0_bm_fi_stats_site_accol, "qecbnato0_bm_fi_stats_site_accol.RDS")


# frequentation

freq_$Site <- as.character(freq_$Site)
#freq_$Site <- ifelse(freq_$Site == "ARMO_Piégu / Verdelet", "ARMO_Piegu.Verdelet", freq_$Site)
#ARMO_Piegu.Verdelet <- dplyr::filter(freq_, Site == "ARMO_Piegu.Verdelet")
#ARMO_Piegu <- ARMO_Piegu.Verdelet
#ARMO_Verdelet <- ARMO_Piegu.Verdelet
#ARMO_Piegu$Site <- "ARMO_Piegu"
#ARMO_Verdelet$Site <- "ARMO_Verdelet"
#ARMO_Piegu.Verdelet <- dplyr::bind_rows(ARMO_Piegu, ARMO_Verdelet)


BASQ_FlotsBleus <- dplyr::filter(freq_, Site == "BASQ_FlotsBleus")
BASQ_FlotsBleusZF <- BASQ_FlotsBleus
BASQ_FlotsBleusZP <- BASQ_FlotsBleus
BASQ_FlotsBleusZF$Site <- "BASQ_FlotsBleusZF"
BASQ_FlotsBleusZP$Site <- "BASQ_FlotsBleusZP"
BASQ_FlotsBleus <- dplyr::bind_rows(BASQ_FlotsBleusZF, BASQ_FlotsBleusZP)

freq_$Site <- as.factor(freq_$Site)
#ARMO_Piegu.Verdelet$Site <- as.factor(ARMO_Piegu.Verdelet$Site)
BASQ_FlotsBleus$Site <- as.factor(BASQ_FlotsBleus$Site)

# create a unique freq_ df. with duplicated Piegu-Verdelet and FlotsBleus frequentation data

freq_$Site <- as.character(freq_$Site)

`%notin%` <- Negate(`%in%`)

freq_new <- freq_ %>% dplyr::filter(Site %notin%  "BASQ_FlotsBleus")

#freq_new <- dplyr::bind_rows(freq_new, ARMO_Piegu.Verdelet)

freq_new <- dplyr::bind_rows(freq_new, BASQ_FlotsBleus)

freq_new <- dplyr::arrange(freq_new, Site)
freq_new$Site <- as.factor(freq_new$Site)


freq_new$Site <- factor(freq_new$Site)

stat_freq_new <- freq_new %>% dplyr::group_by(Site) %>% dplyr::tally()
freq_min5 <- dplyr::filter(freq_new, Site %notin% c(as.vector(unlist((dplyr::filter(stat_freq_new, n < 5))["Site"]))))
freq_inf5 <- dplyr::filter(freq_new, Site %notin% c(as.vector(unlist((dplyr::filter(stat_freq_new, n >= 5))["Site"]))))

p_freq <- ggplot2::ggplot(freq_new, ggplot2::aes(x = Site, y = Fr_Nb)) + 
  ggplot2::geom_blank() +
  ggplot2::xlab("") +
  ggplot2::ylab("Fréquentation") +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1)) + 
  ggplot2::geom_boxplot(data = freq_min5, ggplot2::aes(x = Site, y = Fr_Nb)) + 
  ggplot2::geom_point(data = freq_inf5, ggplot2::aes(x = Site, y = Fr_Nb)) 
ggplot2::ggsave("5_Frequentation.png", p_freq)

# plot frequentation data with missing site

setdiff(qecbnato0$Site, freq_new$Site)
freq_new[nrow(freq_new) + 3, ] <- NA
freq_new$Site <- as.character(freq_new$Site)
freq_new[nrow(freq_new) - 2, "Site"] <- "FINS_Quemenes"
freq_new[nrow(freq_new) - 2, "Site_bis"] <- "Quéménès"
freq_new[nrow(freq_new) - 1, "Site"] <- "FINS_SeinGoulenez"
freq_new[nrow(freq_new) - 1, "Site_bis"] <- "Île de Sein - Goulenez"
freq_new[nrow(freq_new), "Site"] <- "FINS_SeinKilaourou"
freq_new[nrow(freq_new), "Site_bis"] <- "Île de Sein - Kilaourou"


freq_new$Site <- as.character(freq_new$Site)
freq_new$Site_bis <- as.character(freq_new$Site_bis)
freq_new$Site_bis <- ifelse(freq_new$Site == "ARMO_Piegu", "Piégu", freq_new$Site_bis)
freq_new$Site_bis <- ifelse(freq_new$Site == "ARMO_Verdelet", "Îlot du Verdelet", freq_new$Site_bis)
freq_new$Site_bis <- ifelse(freq_new$Site == "BASQ_FlotsBleusZF", "Les Flots Bleus / zone familles", freq_new$Site_bis)
freq_new$Site_bis <- ifelse(freq_new$Site == "BASQ_FlotsBleusZP", "Les Flots Bleus / zone pêcheurs", freq_new$Site_bis)


freq_new <- dplyr::arrange(freq_new, Site)
freq_new$Site <- as.factor(freq_new$Site)

freq_new$Site_bis <- as.factor(freq_new$Site_bis)


freq_new$Site <- factor(freq_new$Site)

stat_freq_new <- freq_new %>% dplyr::group_by(Site) %>% dplyr::tally()
freq_min5 <- dplyr::filter(freq_new, Site %notin% c(as.vector(unlist((dplyr::filter(stat_freq_new, n < 5))["Site"]))))
freq_inf5 <- dplyr::filter(freq_new, Site %notin% c(as.vector(unlist((dplyr::filter(stat_freq_new, n >= 5))["Site"]))))

p_freq <- ggplot2::ggplot(freq_new, ggplot2::aes(x = Site, y = Fr_Nb)) + 
  ggplot2::geom_blank() +
  ggplot2::xlab("") +
  ggplot2::ylab("Fréquentation") +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1)) + 
  ggplot2::geom_boxplot(data = freq_min5, ggplot2::aes(x = Site, y = Fr_Nb)) + 
  ggplot2::geom_point(data = freq_inf5, ggplot2::aes(x = Site, y = Fr_Nb)) 
ggplot2::ggsave("5_Frequentation_na.png", p_freq)

freq_new_stats_site_fr_nb <- na.omit(freq_new[, c("Site", "Fr_Nb")]) %>% dplyr::group_by(Site) %>% dplyr::summarise(min. = min(Fr_Nb), max. = max(Fr_Nb), mean. = mean(Fr_Nb), median. = median(Fr_Nb), nb = dplyr::n())
freq_new_stats_site_fr_nb %>% print(n = nrow(freq_new_stats_site_fr_nb))
#saveRDS(freq_new_stats_site_fr_nb, "freq_new_stats_site_fr_nb.RDS")

freq_new_stats_effort <- na.omit(freq_new[, c("Site", "Annee_semester")]) %>% dplyr::group_by(Site, Annee_semester) %>% dplyr::summarize(nb. = dplyr::n())
freq_new_stats_effort <- freq_new_stats_effort %>% tidyr::spread(Annee_semester, nb.)
setdiff(unique(freq_new$Site), unique(freq_new_stats_effort$Site))
df_add <- data.frame(Site = as.vector(setdiff(unique(freq_new$Site), unique(freq_new_stats_effort$Site))))
freq_new_stats_effort <- dplyr::bind_rows(freq_new_stats_effort, df_add)


freq_new <- dplyr::filter(freq_new, Site %notin% c("FINS_Quemenes", "FINS_SeinGoulenez", "FINS_SeinKilaourou")) # I renamed the freq df. by freq_new, because I modified the freq df.


# fisher"s behaviour

stat_obs_ <- obs_ %>% dplyr::group_by(Site) %>% dplyr::tally()

obs_ <- dplyr::filter(obs_, all(obs_$Nb.Blocs.Retournes.remis.norm15min) >= 0)

p_obs_cpt <- ggplot2::ggplot(obs_, ggplot2::aes(x = Site, y = ratio_norm15min.obs)) +
  ggplot2::geom_boxplot() + 
  ggplot2::xlab("") +
  ggplot2::ylab("Comportement des pêcheurs") +
  ggplot2::labs(subtitle = "1 : Bad behavior don't put the block back \n0 : Good behavior put the block back") +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1))

ggplot2::ggsave("6_Comportement_pecheurs.png", p_obs_cpt)

# plot comportment data with missing site

setdiff(qecbnato0$Site, obs_$Site)
obs_[nrow(obs_) + 6, ] <- NA
obs_$Site <- as.character(obs_$Site)
obs_[nrow(obs_) - 5, "Site"] <- "ARMO_Piegu"
obs_[nrow(obs_) - 4, "Site"] <- "FINS_Quemenes"
obs_[nrow(obs_) - 3, "Site"] <- "FINS_SeinGoulenez"
obs_[nrow(obs_) - 2, "Site"] <- "FINS_SeinKilaourou"
obs_[nrow(obs_) - 1, "Site"] <- "EGMP_Chassiron"
obs_[nrow(obs_), "Site"] <- "EGMP_PasEmsembert"


obs_sitebis <- unique(qecbnato0[, c("Site", "Site_bis")])
obs_ <- dplyr::left_join(obs_, obs_sitebis, by = "Site")

obs_ <- dplyr::arrange(obs_, Site)
obs_$Site <- as.factor(obs_$Site)

obs_$Site <- factor(obs_$Site)

stat_obs_ <- obs_ %>% dplyr::group_by(Site) %>% dplyr::tally()
dplyr::arrange(stat_obs_, n)
obs_min5 <- dplyr::filter(obs_, Site %notin% c(as.vector(unlist((dplyr::filter(stat_obs_, n < 5))["Site"]))))
obs_inf5 <- dplyr::filter(obs_, Site %notin% c(as.vector(unlist((dplyr::filter(stat_obs_, n >= 5))["Site"]))))

p_obs_cpt <- ggplot2::ggplot(obs_, ggplot2::aes(x = Site, y = ratio_norm15min.obs)) + 
  ggplot2::geom_blank() +
  #ggplot2::geom_jitter(shape = 16, position = position_jitter(0.2), color = "gray") +
  ggplot2::xlab("") +
  ggplot2::ylab("comportement (échelle relative)") +
  ggplot2::labs(subtitle = "1 : Bad behavior don't put the block back \n0 : Good behavior put the block back") +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1)) + 
  ggplot2::geom_boxplot(data = obs_min5, ggplot2::aes(x = Site, y = ratio_norm15min.obs)) + 
  ggplot2::geom_point(data = obs_inf5, ggplot2::aes(x = Site, y = ratio_norm15min.obs)) 
ggplot2::ggsave("6_Comportement_pecheurs_na.png", p_obs_cpt)

obs_stats_site_ratio <- na.omit(obs_[, c("Site", "ratio_norm15min.obs")]) %>% dplyr::group_by(Site) %>% dplyr::summarise(min. = min(obs_$ratio_norm15min.obs), max. = max(obs_$ratio_norm15min.obs), mean. = mean(obs_$ratio_norm15min.obs), median. = median(obs_$ratio_norm15min.obs), nb = dplyr::n())
saveRDS(obs_stats_site_ratio, "obs_stats_site_ratio.RDS")

obs_ <- tidyr::separate(obs_, date.sortie, into = c("Year", "Month", "Day"), remove = FALSE)
obs_$semester <- ifelse(obs_$Month %in% c("01", "02", "03", "04", "05", "06"), "s1", NA)
obs_$semester <- ifelse(obs_$Month %in% c("07", "08", "09", "10", "11", "12"), "s2", obs_$semester)
obs_$Year_semester <- paste0(obs_$Year, "_", obs_$semester)

obs_$Year_semester <- ifelse(obs_$Year_semester == "NA_NA", NA, obs_$Year_semester)

obs_stats_effort <- na.omit(obs_[, c("Site", "Year_semester")]) %>% dplyr::group_by(Site, Year_semester) %>% dplyr::summarize(nb. = dplyr::n())
obs_stats_effort <- obs_stats_effort %>% tidyr::spread(Year_semester, nb.)
setdiff(unique(obs_$Site), unique(obs_stats_effort$Site))
df_add <- data.frame(Site = as.vector(setdiff(unique(obs_$Site), unique(obs_stats_effort$Site))))
obs_stats_effort <- dplyr::bind_rows(obs_stats_effort, df_add)

obs_ <- dplyr::filter(obs_, Site %notin% c("ARMO_Piegu", "FINS_Quemenes", "FINS_SeinGoulenez", "FINS_SeinKilaourou", "EGMP_Chassiron", "EGMP_PasEmsembert"))


# IVR

ivr_val_qu$Site <- as.factor(ivr_val_qu$Site)

ivr_val_qu$Site <- factor(ivr_val_qu$Site)

p_ivr <- ggplot2::ggplot(ivr_val_qu, ggplot2::aes(x = Site, y = blocs.retournes.fr.)) + 
  ggplot2::geom_boxplot() + 
  ggplot2::ylim(0,100) +
  ggplot2::xlab("") +
  ggplot2::ylab("Proportion de blocs retournés (%)") +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1))

# coloured quality scale based on quartiles

ivr_val_qu <- tibble::add_column(ivr_val_qu, sc.blocs.retournes.fr. = ivr_val_qu$blocs.retournes.fr./20, .after = "blocs.retournes.fr.")

one <- mean(unlist(dplyr::filter(ivr_val_qu, sc.blocs.retournes.fr. <= quantile(ivr_val_qu$sc.blocs.retournes.fr., 0.25, na.rm = TRUE))["sc.blocs.retournes.fr."]))
two <- mean(unlist(dplyr::filter(ivr_val_qu, sc.blocs.retournes.fr. > quantile(ivr_val_qu$sc.blocs.retournes.fr., 0.25, na.rm = TRUE) & sc.blocs.retournes.fr. <= quantile(ivr_val_qu$sc.blocs.retournes.fr., 0.5, na.rm = TRUE))["sc.blocs.retournes.fr."]))
three <- mean(unlist(dplyr::filter(ivr_val_qu, sc.blocs.retournes.fr. > quantile(ivr_val_qu$sc.blocs.retournes.fr., 0.5, na.rm = TRUE) & sc.blocs.retournes.fr. <= quantile(ivr_val_qu$sc.blocs.retournes.fr., 0.75, na.rm = TRUE))["sc.blocs.retournes.fr."]))
four <- mean(unlist(dplyr::filter(ivr_val_qu, sc.blocs.retournes.fr. > quantile(ivr_val_qu$sc.blocs.retournes.fr., 0.75, na.rm = TRUE))["sc.blocs.retournes.fr."]))

p_ivr <- ggplot2::ggplot(ivr_val_qu, ggplot2::aes(x = Site, y = sc.blocs.retournes.fr.)) + 
  ggplot2::geom_boxplot() + 
  ggplot2::ylim(0,5) +
  ggplot2::xlab("") +
  ggplot2::ylab("Indice visuel de retournement") +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1))


p_ivr <- p_ivr + ggplot2::annotate("rect", 
                          xmin = -Inf, 
                          xmax = Inf, 
                          ymin = c(0, 5/20, 25/20, 45/20, 65/20, 85/20), 
                          ymax = c(5/20, 25/20, 45/20, 65/20, 85/20, 100/20),
                          alpha = .2, 
                          fill = c("blue", "green", "yellow", "orange", "orangered", "red"))

ggplot2::ggsave("1_ivr.png", p_ivr)

ivr_val_qustats_site_sc_blocs_retournes_fr <- na.omit(ivr_val_qu[, c("Site", "sc.blocs.retournes.fr.")]) %>% dplyr::group_by(Site) %>% dplyr::summarise(IVR.min = min(sc.blocs.retournes.fr.), IVR.max = max(sc.blocs.retournes.fr.), IVR.mean = mean(sc.blocs.retournes.fr.), IVR.median = median(sc.blocs.retournes.fr.), IVR.nb = dplyr::n())
saveRDS(ivr_val_qustats_site_sc_blocs_retournes_fr, "ivr_val_qustats_site_sc_blocs_retournes_frRDS")

ivr_val_qu$semester <- ifelse(ivr_val_qu$Month %in% c(1,2,3,4,5,6), "s1", NA)
ivr_val_qu$semester <- ifelse(ivr_val_qu$Month %in% c(7,8,9,10,11,12), "s2", ivr_val_qu$semester)
ivr_val_qu$Year_semester <- paste0(ivr_val_qu$Year, "_", ivr_val_qu$semester)

ivr_val_qu$Year_semester <- ifelse(ivr_val_qu$Year_semester == "NA_NA", NA, ivr_val_qu$Year_semester)

ivr_val_qustats_effort <- na.omit(ivr_val_qu[, c("Site", "Year_semester")]) %>% dplyr::group_by(Site, Year_semester) %>% dplyr::summarize(IVR.nb = dplyr::n())
ivr_val_qustats_effort <- ivr_val_qustats_effort %>% tidyr::spread(Year_semester, IVR.nb)


# dissimilarity distances

matri_full$Site <- as.factor(matri_full$Site)
matri_full$Site <- factor(matri_full$Site)

p_fs_dist <- ggplot2::ggplot(matri_full, ggplot2::aes(x = Site, y = BM.BF_FS_dist.)) +
  ggplot2::geom_boxplot() + 
  #ggplot2::geom_jitter(shape = 16, position=position_jitter(0.2)) +
  ggplot2::xlab("") +
  ggplot2::ylab("distance diss. BM.BF_FS") +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1))

# coloured quality scale based on quartiles

one <- mean(unlist(dplyr::filter(matri_full, BM.BF_FS_dist. <= quantile(matri_full$BM.BF_FS_dist., 0.25, na.rm = TRUE))["BM.BF_FS_dist."]))
two <- mean(unlist(dplyr::filter(matri_full, BM.BF_FS_dist. > quantile(matri_full$BM.BF_FS_dist., 0.25, na.rm = TRUE) & BM.BF_FS_dist. <= quantile(matri_full$BM.BF_FS_dist., 0.5, na.rm = TRUE))["BM.BF_FS_dist."]))
three <- mean(unlist(dplyr::filter(matri_full, BM.BF_FS_dist. > quantile(matri_full$BM.BF_FS_dist., 0.5, na.rm = TRUE) & BM.BF_FS_dist. <= quantile(matri_full$BM.BF_FS_dist., 0.75, na.rm = TRUE))["BM.BF_FS_dist."]))
four <- mean(unlist(dplyr::filter(matri_full, BM.BF_FS_dist. > quantile(matri_full$BM.BF_FS_dist., 0.75, na.rm = TRUE))["BM.BF_FS_dist."]))

p_fs_dist <- p_fs_dist + ggplot2::annotate("rect", 
                                  xmin = -Inf, 
                                  xmax = Inf, 
                                  ymin = c(0, one, two, three, four), 
                                  ymax = c(one, two, three, four, 1),
                                  alpha = .2, 
                                  fill = c("blue", "green", "yellow", "orange", "red"))

ggplot2::ggsave("2_fs_dist.png", p_fs_dist)

matri_full_stats_site_bm_bf_fs_dist <- na.omit(matri_full[, c("Site", "BM.BF_FS_dist.")]) %>% dplyr::group_by(Site) %>% dplyr::summarise(min. = min(BM.BF_FS_dist.), max. = max(BM.BF_FS_dist.), mean. = mean(BM.BF_FS_dist.), median. = median(BM.BF_FS_dist.), nb = dplyr::n())
saveRDS(matri_full_stats_site_bm_bf_fs_dist, "matri_full_stats_site_bm_bf_fs_dist.RDS")

matri_full$semester <- ifelse(matri_full$Month %in% c("01", "02", "03", "04", "05", "06"), "s1", NA)
matri_full$semester <- ifelse(matri_full$Month %in% c("07", "08", "09", "10", "11", "12"), "s2", matri_full$semester)
matri_full$Year_semester <- paste0(matri_full$Year, "_", matri_full$semester)

matri_full$Year_semester <- ifelse(matri_full$Year_semester == "NA_NA", NA, matri_full$Year_semester)

matri_full_stats_bm_bf_fs_dist_effort <- na.omit(matri_full[, c("Site", "Year_semester", "BM.BF_FS_dist.")]) %>% dplyr::group_by(Site, Year_semester) %>% dplyr::summarize(nb. = dplyr::n())
matri_full_stats_bm_bf_fs_dist_effort <- matri_full_stats_bm_bf_fs_dist_effort %>% tidyr::spread(Year_semester, nb.)

p_bm_dist <- ggplot2::ggplot(matri_full, ggplot2::aes(x = Site, y = BM_FS.FI_dist.)) +
  ggplot2::geom_boxplot() + 
  #ggplot2::geom_jitter(shape = 16, position=position_jitter(0.2)) +
  ggplot2::xlab("") +
  ggplot2::ylab("distance diss. BM_FS.FI") +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1))

# coloured quality scale based on quartiles

one <- mean(unlist(dplyr::filter(matri_full, BM_FS.FI_dist. <= quantile(matri_full$BM_FS.FI_dist., 0.25, na.rm = TRUE))["BM_FS.FI_dist."]))
two <- mean(unlist(dplyr::filter(matri_full, BM_FS.FI_dist. > quantile(matri_full$BM_FS.FI_dist., 0.25, na.rm = TRUE) & BM_FS.FI_dist. <= quantile(matri_full$BM_FS.FI_dist., 0.5, na.rm = TRUE))["BM_FS.FI_dist."]))
three <- mean(unlist(dplyr::filter(matri_full, BM_FS.FI_dist. > quantile(matri_full$BM_FS.FI_dist., 0.5, na.rm = TRUE) & BM_FS.FI_dist. <= quantile(matri_full$BM_FS.FI_dist., 0.75, na.rm = TRUE))["BM_FS.FI_dist."]))
four <- mean(unlist(dplyr::filter(matri_full, BM_FS.FI_dist. > quantile(matri_full$BM_FS.FI_dist., 0.75, na.rm = TRUE))["BM_FS.FI_dist."]))

p_bm_dist <- p_bm_dist + ggplot2::annotate("rect", 
                     xmin = -Inf, 
                     xmax = Inf, 
                     ymin = c(0, one, two, three, four), 
                     ymax = c(one, two, three, four, 1),
                     alpha = .2, 
                     fill = c("red", "orange", "yellow", "green", "blue"))

ggplot2::ggsave("2_bm_dist.png", p_bm_dist)

matri_full_stats_site_bm_fs_fi_dist <- na.omit(matri_full[, c("Site", "BM_FS.FI_dist.")]) %>% dplyr::group_by(Site) %>% dplyr::summarise(min. = min(BM_FS.FI_dist.), max. = max(BM_FS.FI_dist.), mean. = mean(BM_FS.FI_dist.), median. = median(BM_FS.FI_dist.), nb = dplyr::n())
saveRDS(matri_full_stats_site_bm_fs_fi_dist, "matri_full_stats_site_bm_fs_fi_dist.RDS")

matri_full_stats_bm_fs_fi_dist_effort <- na.omit(matri_full[, c("Site", "Year_semester", "BM_FS.FI_dist.")]) %>% dplyr::group_by(Site, Year_semester) %>% dplyr::summarize(nb. = dplyr::n())
matri_full_stats_bm_fs_fi_dist_effort <- matri_full_stats_bm_fs_fi_dist_effort %>% tidyr::spread(Year_semester, nb.)


# diversity index

div_$Site <- as.factor(div_$Site)

div_$Site <- factor(div_$Site)

div_bm_fs <- dplyr::filter(div_, Type.Bloc == "Bloc mobile" & Face == "face supérieure")
div_bm_fi <- dplyr::filter(div_, Type.Bloc == "Bloc mobile" & Face == "face inférieure")
div_bf_fs <- dplyr::filter(div_, Type.Bloc %in% c("Bloc fixé", "Roche en place") & Face == "face supérieure")

setdiff(paste0(div_bm_fs$site_year_month_day, "_", div_bm_fs$Quadrat.bis, "_", div_bm_fs$Numéro.Bloc.échantillon), paste0(div_bm_fi$site_year_month_day, "_", div_bm_fi$Quadrat.bis, "_", div_bm_fi$Numéro.Bloc.échantillon))

div_$site_year_month_day_Quadrat.bis_Numéro.Bloc.échantillon <- paste0(div_$site_year_month_day, "_", div_$Quadrat.bis, "_", div_$Numéro.Bloc.échantillon)
div_ <- div_[ , -which(names(div_) %in% c("site_year_month_day_Quadrat.bis_Numéro.Bloc.échantillon"))]

p_rich <- ggplot2::ggplot(div_, ggplot2::aes(x = Site, y = richness)) +
  ggplot2::geom_boxplot() + 
  ggplot2::ylim(0, max(div_$richness)) +
  ggplot2::xlab("") +
  ggplot2::ylab("richness") +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1))

index_fct <- function(index) {
  
  div_bm_fs_df <- div_bm_fs[, c("Site", index)]
  colnames(div_bm_fs_df) <- c("Site", "var.")

  p_var_bm_fs <- ggplot2::ggplot(div_bm_fs_df, ggplot2::aes(x = Site, y = var.)) +
    ggplot2::geom_boxplot() + 
    ggplot2::ylim(min(div_[, index]), max(div_[, index])) +
    ggplot2::xlab("") +
    ggplot2::ylab(paste0(index, " ", "bm_fs")) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1))

  
  div_bm_fi_df. <- div_bm_fi[, c("Site", index)]
  colnames(div_bm_fi_df.) <- c("Site", "var.")
  
  p_var_bm_fi <- ggplot2::ggplot(div_bm_fi_df., ggplot2::aes(x = Site, y = var.)) +
    ggplot2::geom_boxplot() + 
    ggplot2::ylim(min(div_[, index]), max(div_[, index])) +
    ggplot2::xlab("") +
    ggplot2::ylab(paste0(index, " ", "bm_fi")) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1))

  
  div_bf_fs_df <- div_bf_fs[, c("Site", index)]
  colnames(div_bf_fs_df) <- c("Site", "var.")
  
  p_var_bf_fs <- ggplot2::ggplot(div_bf_fs_df, ggplot2::aes(x = Site, y = var.)) +
    ggplot2::geom_boxplot() + 
    ggplot2::ylim(min(div_[, index]), max(div_[, index])) +
    ggplot2::xlab("") +
    ggplot2::ylab(paste0(index, " ", "bm_fs")) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1))


  div_df <- div_[, c("Site", index)]
  colnames(div_df) <- c("Site", "var.")
  
  p_var <- ggplot2::ggplot(div_df, ggplot2::aes(x = Site, y = var.)) +
    ggplot2::geom_boxplot() + 
    #ggplot2::geom_jitter(shape = 16, position=position_jitter(0.2)) +
    ggplot2::ylim(min(div_[, index]), max(div_[, index])) +
    ggplot2::xlab("") +
    ggplot2::ylab(index) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1))

  
  p_var_bm_fs_df <- data.frame(ggplot2::ggplot_build(p_var_bm_fs)$data[[1]])
  p_var_bm_fs_df <- tibble::add_column(p_var_bm_fs_df, Site = sort(unique(div_bm_fs$Site, levels = div_bm_fs$Site)), .before = "ymin")
  p_var_bm_fs_df <- tibble::add_column(p_var_bm_fs_df, bloc.face = "bm_fs", .before = "ymin")
  #p_var_bm_fs_df <- rename(p_var_bm_fs_df, median.bm_fs = middle)

  p_var_bm_fi_df <- data.frame(ggplot2::ggplot_build(p_var_bm_fi)$data[[1]])
  p_var_bm_fi_df <- tibble::add_column(p_var_bm_fi_df, Site = sort(unique(div_bm_fi$Site, levels = div_bm_fi$Site)), .before = "ymin")
  p_var_bm_fi_df <- tibble::add_column(p_var_bm_fi_df, bloc.face = "bm_fi", .before = "ymin")
  #p_var_bm_fi_df <- rename(p_var_bm_fi_df, median.bm_fi = middle)
  
  p_var_bf_fs_df <- data.frame(ggplot2::ggplot_build(p_var_bf_fs)$data[[1]])
  p_var_bf_fs_df <- tibble::add_column(p_var_bf_fs_df, Site = sort(unique(div_bf_fs$Site, levels = div_bf_fs$Site)), .before = "ymin")
  p_var_bf_fs_df <- tibble::add_column(p_var_bf_fs_df, bloc.face = "bf_fs", .before = "ymin")
  #p_var_bf_fs_df <- rename(p_var_bf_fs_df, median.bf_fs = middle)

  median_df <- rbind(p_var_bm_fs_df[, c("Site", "bloc.face", "middle")], p_var_bm_fi_df[, c("Site", "bloc.face", "middle")], p_var_bf_fs_df[, c("Site", "bloc.face", "middle")])
  
p_var <<- p_var + ggplot2::geom_jitter(median_df, mapping = ggplot2::aes(x = Site, y = middle, colour = bloc.face), position = ggplot2::position_jitterdodge(jitter.width = 0, jitter.height = 0), show.legend = TRUE) +
    ggplot2::scale_colour_manual(name = "median",
                        breaks = c("bm_fs", "bm_fi", "bf_fs"),
                        values = c("bm_fs" = "forestgreen", "bm_fi" = "orange", "bf_fs" = "purple"))

}

index_fct(index = "richness")
p_rich <- p_var
ggplot2::ggsave("3_Richness.png", p_rich)

p_shan <- ggplot2::ggplot(div_, ggplot2::aes(x = Site, y = Shannon)) +
  ggplot2::geom_boxplot() + 
  #ggplot2::geom_jitter(shape = 16, position=position_jitter(0.2)) +
  ggplot2::xlab("") +
  ggplot2::ylab("Shannon") +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1))

index_fct(index = "Shannon")
p_shan <- p_var
ggplot2::ggsave("4_Shannon.png", p_shan)

div_stats <- div_
div_stats$Type.Bloc <- as.character(div_stats$Type.Bloc)
div_stats$Type.Bloc <- ifelse(div_stats$Type.Bloc == "Roche en place", "Bloc fixé", div_stats$Type.Bloc)

div_stats_site_richness <- na.omit(div_stats[, c("Site", "richness")]) %>% dplyr::group_by(Site) %>% dplyr::summarise(richness.min = min(richness), richness.max = max(richness), richness.mean = mean(richness), richness.median = median(richness), richness.nb = dplyr::n())
saveRDS(div_stats_site_richness, "div_stats_site_richness.RDS")

div_stats_site_shannon <- na.omit(div_stats[, c("Site", "Face", "Type.Bloc", "Shannon")]) %>% dplyr::group_by(Site, Face, Type.Bloc) %>% dplyr::summarise(Shannon.min = min(Shannon), Shannon.max = max(Shannon), Shannon.mean = mean(Shannon), Shannon.median = median(Shannon), Shannon.nb = dplyr::n(), .groups = "drop")
saveRDS(div_stats_site_shannon, "div_stats_site_shannon.RDS")


# wave height

list_date_mean_ini_ <- list_date_mean

for (i in c(1:length(list_date_mean))) {
  site_ <- rep(names(list_date_mean[i]), nrow(list_date_mean[[i]]))
  list_date_mean[[i]] <- tibble::add_column(list_date_mean[[i]], Site = site_, .before = "Date")
  list_date_mean[[i]] <- list_date_mean[[i]][, c("Site", "Date", "mean", "median", "max")]
  rm(site_, i)
}

oceano_mean <- data.frame(matrix(data = NA, nrow = 1, ncol = 5))
names(oceano_mean) <- c("Site", "Date", "mean", "median", "max")
for (i in c(1:length(list_date_mean))) {
  oceano_mean <- dplyr::bind_rows(oceano_mean, list_date_mean[[i]])
}
oceano_mean <- oceano_mean[-1, ]
oceano_mean <- oceano_mean %>% dplyr::group_by(Site) 
oceano_mean <- oceano_mean %>% dplyr::mutate(mean.ano = mean-mean(mean))
oceano_mean <- oceano_mean %>% dplyr::mutate(median.ano = median-mean(median))
oceano_mean <- oceano_mean %>% dplyr::mutate(max.ano = max-mean(max))
oceano_mean <- dplyr::ungroup(oceano_mean)

oceano_mean$Site <- as.factor(oceano_mean$Site)
oceano_mean$Site <- factor(oceano_mean$Site, levels = c("GONB_IlotStMichel", "ARMO_Piegu", "ARMO_Verdelet", "ARMO_Bilfot", "ARMO_IlePlate", "PDMO_Perharidy", "FINS_Quemenes", "BRES_Keraliou", "FINS_SeinGoulenez", "FINS_SeinKilaourou", "FINS_Mousterlin", "FINS_StNicolasGlenan", "GDMO_Locmariaquer", "GDMO_BegLann", "FOUR_PlateauFour", "EGMP_GroinCou", "EGMP_PasEmsembert", "EGMP_PerreAntiochat", "EGMP_Chassiron", "EGMP_BreeBains", "BASQ_FlotsBleusZP", "BASQ_FlotsBleusZF"))

p_oceano_mean_max <- ggplot2::ggplot(oceano_mean, ggplot2::aes(x = Site, y = max)) +
  ggplot2::geom_boxplot() + 

  ggplot2::xlab("") +
  ggplot2::ylab("daily max hs (for modelled site means)") +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1))
ggplot2::ggsave("7_oceano_mean_max_2021.png", p_oceano_mean_max)

oceano_mean_stats_site_max <- na.omit(oceano_mean[, c("Site", "max")]) %>% dplyr::group_by(Site) %>% dplyr::summarise(oceano_mean.max.min = min(max), oceano_mean.max.max = max(max), oceano_mean.max.mean = mean(max), oceano_mean.max.median = median(max), oceano_mean.max.nb = dplyr::n())
saveRDS(oceano_mean_stats_site_max, "oceano_mean_stats_site_max.RDS")

oceano_mean <- tidyr::separate(oceano_mean, Date, c("Year", "Month", "Day"), remove = FALSE)
oceano_mean$semester <- ifelse(oceano_mean$Month %in% c("01", "02", "03", "04", "05", "06"), "s1", NA)
oceano_mean$semester <- ifelse(oceano_mean$Month %in% c("07", "08", "09", "10", "11", "12"), "s2", oceano_mean$semester)
oceano_mean$Year_semester <- paste0(oceano_mean$Year, "_", oceano_mean$semester)

oceano_mean$Year_semester <- ifelse(oceano_mean$Year_semester == "NA_NA", NA, oceano_mean$Year_semester)

oceano_mean_max_stats_effort <- na.omit(oceano_mean[, c("Site", "Year_semester")]) %>% dplyr::group_by(Site, Year_semester) %>% dplyr::summarize(oceano_mean.max.nb = dplyr::n())
oceano_mean_max_stats_effort <- oceano_mean_max_stats_effort %>% tidyr::spread(Year_semester, oceano_mean.max.nb)


# summary sampling effort table

rm(list = setdiff(ls(), c("div_", "freq_", "freq_new", "ivr_val_qu", "list_date_mean", "matri_full", "obs_", "oceano_mean", "qecbnato0", "qecbNew", 
                          "%notin%",
                          "matri_full_stats_bm_bf_fs_dist_effort", "matri_full_stats_bm_fs_fi_dist_effort",
                          "ivr_val_qustats_effort", "oceano_mean_max_stats_effort", "freq_new_stats_effort", "obs_stats_effort", "qecbnato0_bm_stats_effort", "qecbnato0_bf_stats_effort")))

qecbnato0_bm_stats_effort[, "X"] <- "BM"
qecbnato0_bf_stats_effort[, "X"] <- "BF"
matri_full_stats_bm_bf_fs_dist_effort[, "X"] <- "BM.BF_FS_dist."
matri_full_stats_bm_fs_fi_dist_effort[, "X"] <- "BM_FS.FI_dist."
ivr_val_qustats_effort[, "X"] <- "ivr"
oceano_mean_max_stats_effort[, "X"] <- "oceano."
freq_new_stats_effort[, "X"] <- "freq_"
obs_stats_effort[, "X"] <- "cptmt."

effort <- dplyr::bind_rows(qecbnato0_bm_stats_effort, qecbnato0_bf_stats_effort)
effort <- dplyr::bind_rows(effort, matri_full_stats_bm_bf_fs_dist_effort)
effort <- dplyr::bind_rows(effort, matri_full_stats_bm_fs_fi_dist_effort)
effort <- dplyr::bind_rows(effort, ivr_val_qustats_effort)
effort <- dplyr::bind_rows(effort, oceano_mean_max_stats_effort) 
effort <- dplyr::bind_rows(effort, freq_new_stats_effort) 
effort <- dplyr::bind_rows(effort, obs_stats_effort)

names(effort)[names(effort) == "X"] <- "param."
effort <- effort[, c("param.", "Site", c(sort(names(effort[ , -which(names(effort) %in% c("param.", "Site"))]))))]
effort$param. <- as.factor(effort$param.)
effort$param. <- ordered(effort$param., levels = c("BM", "BF", "BM.BF_FS_dist.", "BM_FS.FI_dist.", "ivr", "oceano.", "freq_", "cptmt."))
effort <- effort[with(effort, order(Site, param.)),]
names(effort)
# add 2012_s1 column
#effort <- tibble::add_column(effort, `2012_s1` = NA, .after = "2011_s2")
#effort$`2012_s1` <- as.integer(effort$`2012_s1`)

effort$semester <- rowSums(!is.na(effort[, 3:ncol(effort)]))
effort$effort <- rowSums(effort[, 3:(ncol(effort) - 1)], na.rm = TRUE)

effort$Site <- as.factor(effort$Site)
effort$Site <- factor(effort$Site)
effort <- dplyr::arrange(effort, Site)

#effort.paper <- dplyr::filter(effort, param. %notin% c("BM.BF_FS_dist.", "BM_FS.FI_dist.", "oceano."))
write.table(effort, file = "effort.tabular", sep = "\t", dec = ".", na = " ", row.names = FALSE, col.names = TRUE, quote = FALSE)
