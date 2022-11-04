{
## vars.short

frmla. <- as.formula(paste("BM_FS.FI_dist.tr", paste(c(model.short, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile")[, c("BM_FS.FI_dist.tr", vars.short)])
get.vars.()

matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM_FS.FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- tbl.
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM_FS.FI_dist.tr", vars.short)])
##get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM_FS.FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM_FS.FI_dist.tr", vars.short)])
##get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM_FS.FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join <- tibble::add_column(tbl.join, vars. = NA, .before = "region")
tbl.join$vars. <- "vars.short"

## vars.medium

frmla. <- as.formula(paste("BM_FS.FI_dist.tr", paste(c(model.medium, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile")[, c("BM_FS.FI_dist.tr", vars.medium)])
#get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM_FS.FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM_FS.FI_dist.tr", vars.medium)])
#get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM_FS.FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM_FS.FI_dist.tr", vars.medium)])
#get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM_FS.FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium", tbl.join$vars.)

## vars.large

frmla. <- as.formula(paste("BM_FS.FI_dist.tr", paste(c(model.large, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile")[, c("BM_FS.FI_dist.tr", vars.large)])
#get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM_FS.FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM_FS.FI_dist.tr", vars.large)])
#get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM_FS.FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM_FS.FI_dist.tr", vars.large)])
#get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM_FS.FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large", tbl.join$vars.)


### BM.BF_FS_dist.tr

## vars.short

frmla. <- as.formula(paste("BM.BF_FS_dist.tr", paste(c(model.short, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile")[, c("BM.BF_FS_dist.tr", vars.short)])
#get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM.BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.BF_FS_dist.tr", vars.short)])
#get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM.BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.BF_FS_dist.tr", vars.short)])
#get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM.BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short", tbl.join$vars.)

## vars.medium

frmla. <- as.formula(paste("BM.BF_FS_dist.tr", paste(c(model.medium, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile")[, c("BM.BF_FS_dist.tr", vars.medium)])
#get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM.BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.BF_FS_dist.tr", vars.medium)])
#get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM.BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.BF_FS_dist.tr", vars.medium)])
#get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM.BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium", tbl.join$vars.)

## vars.large

frmla. <- as.formula(paste("BM.BF_FS_dist.tr", paste(c(model.large, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile")[, c("BM.BF_FS_dist.tr", vars.large)])
#get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM.BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.BF_FS_dist.tr", vars.large)])
#get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM.BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.BF_FS_dist.tr", vars.large)])
#get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM.BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large", tbl.join$vars.)


### richness.tr face supérieure bloc mobile

## vars.short

frmla. <- as.formula(paste("richness.tr", paste(c(model.short, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Face == "face supérieure")[, c("richness.tr", vars.short)])
#get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "richness BM_FS", vars. = vars.list, transfo. = "center_scale", frmla. = frmla.mod.)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne", Face == "face supérieure")[, c("richness.tr", vars.short)])
#get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "richness BM_FS", vars. = vars.list, transfo. = "center_scale", frmla. = frmla.mod.)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ", Face == "face supérieure")[, c("richness.tr", vars.short)])
#get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "richness BM_FS", vars. = vars.list, transfo. = "center_scale", frmla. = frmla.mod.)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short", tbl.join$vars.)

## vars.medium

frmla. <- as.formula(paste("richness.tr", paste(c(model.medium, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Face == "face supérieure")[, c("richness.tr", vars.medium)])
#get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "richness BM_FS", vars. = vars.list, transfo. = "center_scale", frmla. = frmla.mod.)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne", Face == "face supérieure")[, c("richness.tr", vars.medium)])
#get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "richness BM_FS", vars. = vars.list, transfo. = "center_scale", frmla. = frmla.mod.)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ", Face == "face supérieure")[, c("richness.tr", vars.medium)])
#get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "richness BM_FS", vars. = vars.list, transfo. = "center_scale", frmla. = frmla.mod.)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium", tbl.join$vars.)

## vars.large

frmla. <- as.formula(paste("richness.tr", paste(c(model.large, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Face == "face supérieure")[, c("richness.tr", vars.large)])
#get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "richness BM_FS", vars. = vars.list, transfo. = "center_scale", frmla. = frmla.mod.)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne", Face == "face supérieure")[, c("richness.tr", vars.large)])
#get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "richness BM_FS", vars. = vars.list, transfo. = "center_scale", frmla. = frmla.mod.)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ", Face == "face supérieure")[, c("richness.tr", vars.large)])
#get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "richness BM_FS", vars. = vars.list, transfo. = "center_scale", frmla. = frmla.mod.)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large", tbl.join$vars.)


### richness.tr face inférieure bloc mobile

## vars.short

frmla. <- as.formula(paste("richness.tr", paste(c(model.short, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Face == "face inférieure")[, c("richness.tr", vars.short)])
#get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "richness BM_FI", vars. = vars.list, transfo. = "center_scale", frmla. = frmla.mod.)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne", Face == "face inférieure")[, c("richness.tr", vars.short)])
#get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "richness BM_FI", vars. = vars.list, transfo. = "center_scale", frmla. = frmla.mod.)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ", Face == "face inférieure")[, c("richness.tr", vars.short)])
#get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "richness BM_FI", vars. = vars.list, transfo. = "center_scale", frmla. = frmla.mod.)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short", tbl.join$vars.)

## vars.medium

frmla. <- as.formula(paste("richness.tr", paste(c(model.medium, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Face == "face inférieure")[, c("richness.tr", vars.medium)])
#get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "richness BM_FI", vars. = vars.list, transfo. = "center_scale", frmla. = frmla.mod.)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne", Face == "face inférieure")[, c("richness.tr", vars.medium)])
#get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "richness BM_FI", vars. = vars.list, transfo. = "center_scale", frmla. = frmla.mod.)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ", Face == "face inférieure")[, c("richness.tr", vars.medium)])
#get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "richness BM_FI", vars. = vars.list, transfo. = "center_scale", frmla. = frmla.mod.)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium", tbl.join$vars.)

## vars.large

frmla. <- as.formula(paste("richness.tr", paste(c(model.large, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Face == "face inférieure")[, c("richness.tr", vars.large)])
#get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "richness BM_FI", vars. = vars.list, transfo. = "center_scale", frmla. = frmla.mod.)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne", Face == "face inférieure")[, c("richness.tr", vars.large)])
#get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "richness BM_FI", vars. = vars.list, transfo. = "center_scale", frmla. = frmla.mod.)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ", Face == "face inférieure")[, c("richness.tr", vars.large)])
#get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "richness BM_FI", vars. = vars.list, transfo. = "center_scale", frmla. = frmla.mod.)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large", tbl.join$vars.)


### richness.tr face supérieure bloc fixé

## vars.short

frmla. <- as.formula(paste("richness.tr", paste(c(model.short, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(dplyr::filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Face == "face supérieure")[, c("richness.tr", vars.short)])
#get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "richness BF_FS", vars. = vars.list, transfo. = "center_scale", frmla. = frmla.mod.)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(dplyr::filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Region == "Bretagne", Face == "face supérieure")[, c("richness.tr", vars.short)])
#get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "richness BF_FS", vars. = vars.list, transfo. = "center_scale", frmla. = frmla.mod.)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(dplyr::filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Region == "EGMP.BASQ", Face == "face supérieure")[, c("richness.tr", vars.short)])
#get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "richness BF_FS", vars. = vars.list, transfo. = "center_scale", frmla. = frmla.mod.)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short", tbl.join$vars.)

## vars.medium

frmla. <- as.formula(paste("richness.tr", paste(c(model.medium, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(dplyr::filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Face == "face supérieure")[, c("richness.tr", vars.medium)])
#get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "richness BF_FS", vars. = vars.list, transfo. = "center_scale", frmla. = frmla.mod.)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(dplyr::filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Region == "Bretagne", Face == "face supérieure")[, c("richness.tr", vars.medium)])
#get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "richness BF_FS", vars. = vars.list, transfo. = "center_scale", frmla. = frmla.mod.)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(dplyr::filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Region == "EGMP.BASQ", Face == "face supérieure")[, c("richness.tr", vars.medium)])
#get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "richness BF_FS", vars. = vars.list, transfo. = "center_scale", frmla. = frmla.mod.)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium", tbl.join$vars.)

## vars.large

frmla. <- as.formula(paste("richness.tr", paste(c(model.large, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(dplyr::filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Face == "face supérieure")[, c("richness.tr", vars.large)])
#get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "richness BF_FS", vars. = vars.list, transfo. = "center_scale", frmla. = frmla.mod.)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(dplyr::filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Region == "Bretagne", Face == "face supérieure")[, c("richness.tr", vars.large)])
#get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "richness BF_FS", vars. = vars.list, transfo. = "center_scale", frmla. = frmla.mod.)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(dplyr::filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Region == "EGMP.BASQ", Face == "face supérieure")[, c("richness.tr", vars.large)])
#get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "richness BF_FS", vars. = vars.list, transfo. = "center_scale", frmla. = frmla.mod.)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large", tbl.join$vars.)


### Shannon.tr face supérieure bloc mobile

## vars.short

frmla. <- as.formula(paste("Shannon.tr", paste(c(model.short, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Face == "face supérieure")[, c("Shannon.tr", vars.short)])
#get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Shannon BM_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne", Face == "face supérieure")[, c("Shannon.tr", vars.short)])
#get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Shannon BM_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ", Face == "face supérieure")[, c("Shannon.tr", vars.short)])
#get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Shannon BM_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short", tbl.join$vars.)

## vars.medium

frmla. <- as.formula(paste("Shannon.tr", paste(c(model.medium, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Face == "face supérieure")[, c("Shannon.tr", vars.medium)])
#get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Shannon BM_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne", Face == "face supérieure")[, c("Shannon.tr", vars.medium)])
#get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Shannon BM_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ", Face == "face supérieure")[, c("Shannon.tr", vars.medium)])
#get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Shannon BM_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium", tbl.join$vars.)

## vars.large

frmla. <- as.formula(paste("Shannon.tr", paste(c(model.large, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Face == "face supérieure")[, c("Shannon.tr", vars.large)])
#get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Shannon BM_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne", Face == "face supérieure")[, c("Shannon.tr", vars.large)])
#get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Shannon BM_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ", Face == "face supérieure")[, c("Shannon.tr", vars.large)])
#get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Shannon BM_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large", tbl.join$vars.)


### Shannon.tr face inférieure bloc mobile

## vars.short

frmla. <- as.formula(paste("Shannon.tr", paste(c(model.short, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Face == "face inférieure")[, c("Shannon.tr", vars.short)])
#get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Shannon BM_FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne", Face == "face inférieure")[, c("Shannon.tr", vars.short)])
#get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Shannon BM_FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ", Face == "face inférieure")[, c("Shannon.tr", vars.short)])
#get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Shannon BM_FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short", tbl.join$vars.)

## vars.medium

frmla. <- as.formula(paste("Shannon.tr", paste(c(model.medium, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Face == "face inférieure")[, c("Shannon.tr", vars.medium)])
#get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Shannon BM_FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne", Face == "face inférieure")[, c("Shannon.tr", vars.medium)])
#get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Shannon BM_FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ", Face == "face inférieure")[, c("Shannon.tr", vars.medium)])
#get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Shannon BM_FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium", tbl.join$vars.)

## vars.large

frmla. <- as.formula(paste("Shannon.tr", paste(c(model.large, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Face == "face inférieure")[, c("Shannon.tr", vars.large)])
#get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Shannon BM_FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne", Face == "face inférieure")[, c("Shannon.tr", vars.large)])
#get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Shannon BM_FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ", Face == "face inférieure")[, c("Shannon.tr", vars.large)])
#get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Shannon BM_FI", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large", tbl.join$vars.)


### Shannon.tr face supérieure bloc fixé

## vars.short

frmla. <- as.formula(paste("Shannon.tr", paste(c(model.short, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(dplyr::filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Face == "face supérieure")[, c("Shannon.tr", vars.short)])
#get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Shannon BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(dplyr::filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Region == "Bretagne", Face == "face supérieure")[, c("Shannon.tr", vars.short)])
#get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Shannon BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(dplyr::filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Region == "EGMP.BASQ", Face == "face supérieure")[, c("Shannon.tr", vars.short)])
#get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Shannon BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short", tbl.join$vars.)

## vars.medium

frmla. <- as.formula(paste("Shannon.tr", paste(c(model.medium, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(dplyr::filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Face == "face supérieure")[, c("Shannon.tr", vars.medium)])
#get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Shannon BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(dplyr::filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Region == "Bretagne", Face == "face supérieure")[, c("Shannon.tr", vars.medium)])
#get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Shannon BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(dplyr::filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Region == "EGMP.BASQ", Face == "face supérieure")[, c("Shannon.tr", vars.medium)])
#get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Shannon BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium", tbl.join$vars.)

## vars.large

frmla. <- as.formula(paste("Shannon.tr", paste(c(model.large, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(dplyr::filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Face == "face supérieure")[, c("Shannon.tr", vars.large)])
#get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Shannon BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(dplyr::filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Region == "Bretagne", Face == "face supérieure")[, c("Shannon.tr", vars.large)])
#get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Shannon BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(dplyr::filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Region == "EGMP.BASQ", Face == "face supérieure")[, c("Shannon.tr", vars.large)])
#get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Shannon BF_FS", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large", tbl.join$vars.)
}


# Let's add ivr modeling next, if removed from the model.large.vs.medium.vs.short vectors

{
### blocs.retournes.fr.tr

## vars.short

frmla. <- as.formula(paste("blocs.retournes.fr.tr", paste(c(model.short.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile")[, c("blocs.retournes.fr.tr", vars.short.ivr)])
#get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "blocs.retournes.fr.", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("blocs.retournes.fr.tr", vars.short.ivr)])
#get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "blocs.retournes.fr.", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("blocs.retournes.fr.tr", vars.short.ivr)])
#get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "blocs.retournes.fr.", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short.ivr", tbl.join$vars.)

## vars.medium

frmla. <- as.formula(paste("blocs.retournes.fr.tr", paste(c(model.medium.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile")[, c("blocs.retournes.fr.tr", vars.medium.ivr)])
#get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "blocs.retournes.fr.", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("blocs.retournes.fr.tr", vars.medium.ivr)])
#get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "blocs.retournes.fr.", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("blocs.retournes.fr.tr", vars.medium.ivr)])
#get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "blocs.retournes.fr.", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium.ivr", tbl.join$vars.)

## vars.large

frmla. <- as.formula(paste("blocs.retournes.fr.tr", paste(c(model.large.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile")[, c("blocs.retournes.fr.tr", vars.large.ivr)])
#get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "blocs.retournes.fr.", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# Bretagne
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("blocs.retournes.fr.tr", vars.large.ivr)])
#get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "blocs.retournes.fr.", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

# EGMP.BASQ
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("blocs.retournes.fr.tr", vars.large.ivr)])
##get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "blocs.retournes.fr.", vars. = vars.list, transfo. = "orderNorm", frmla. = frmla.mod.)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod., vars.list, tbl., buildglmm.)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large.ivr", tbl.join$vars.)
}

