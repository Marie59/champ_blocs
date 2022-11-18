{
### BM_FS.FI_dist.tr

## vars.short

frmla. <- as.formula(paste("BM_FS.FI_dist.tr", paste(c(model.short, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile")[, c("BM_FS.FI_dist.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM_FS.FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- tbl.
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM_FS.FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM_FS.FI_dist.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM_FS.FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM_FS.FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM_FS.FI_dist.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM_FS.FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM_FS.FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join <- tibble::add_column(tbl.join, vars. = NA, .before = "region")
tbl.join$vars. <- "vars.short"
tbl.join <- tibble::add_column(tbl.join, M.1vs2 = NA, .before = "region")
tbl.join$M.1vs2 <- c("M1","M2","M1","M2","M1","M2")

## vars.medium

frmla. <- as.formula(paste("BM_FS.FI_dist.tr", paste(c(model.medium, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile")[, c("BM_FS.FI_dist.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM_FS.FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM_FS.FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM_FS.FI_dist.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM_FS.FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM_FS.FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM_FS.FI_dist.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM_FS.FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM_FS.FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)

## vars.large

frmla. <- as.formula(paste("BM_FS.FI_dist.tr", paste(c(model.large, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile")[, c("BM_FS.FI_dist.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM_FS.FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM_FS.FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM_FS.FI_dist.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM_FS.FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM_FS.FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM_FS.FI_dist.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM_FS.FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM_FS.FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)


### BM.BF_FS_dist.tr

## vars.short


frmla. <- as.formula(paste("BM.BF_FS_dist.tr", paste(c(model.short, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile")[, c("BM.BF_FS_dist.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM.BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM.BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.BF_FS_dist.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM.BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM.BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.BF_FS_dist.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM.BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM.BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)

## vars.medium

frmla. <- as.formula(paste("BM.BF_FS_dist.tr", paste(c(model.medium, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile")[, c("BM.BF_FS_dist.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM.BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM.BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.BF_FS_dist.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM.BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM.BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.BF_FS_dist.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM.BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM.BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)

## vars.large

frmla. <- as.formula(paste("BM.BF_FS_dist.tr", paste(c(model.large, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile")[, c("BM.BF_FS_dist.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM.BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "BM.BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("BM.BF_FS_dist.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM.BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "BM.BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("BM.BF_FS_dist.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM.BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "BM.BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)


### richness.tr face supérieure bloc mobile

## vars.short

frmla. <- as.formula(paste("richness.tr", paste(c(model.short, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Face == "face supérieure")[, c("richness.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "richness BM_FS", vars. = vars.list.M1, transfo. = "center_scale", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "richness BM_FS", vars. = vars.list.M2, transfo. = "center_scale", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne", Face == "face supérieure")[, c("richness.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "richness BM_FS", vars. = vars.list.M1, transfo. = "center_scale", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "richness BM_FS", vars. = vars.list.M2, transfo. = "center_scale", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ", Face == "face supérieure")[, c("richness.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "richness BM_FS", vars. = vars.list.M1, transfo. = "center_scale", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "richness BM_FS", vars. = vars.list.M2, transfo. = "center_scale", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)

## vars.medium

frmla. <- as.formula(paste("richness.tr", paste(c(model.medium, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Face == "face supérieure")[, c("richness.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "richness BM_FS", vars. = vars.list.M1, transfo. = "center_scale", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "richness BM_FS", vars. = vars.list.M2, transfo. = "center_scale", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne", Face == "face supérieure")[, c("richness.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "richness BM_FS", vars. = vars.list.M1, transfo. = "center_scale", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "richness BM_FS", vars. = vars.list.M2, transfo. = "center_scale", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ", Face == "face supérieure")[, c("richness.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "richness BM_FS", vars. = vars.list.M1, transfo. = "center_scale", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "richness BM_FS", vars. = vars.list.M2, transfo. = "center_scale", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)

## vars.large

frmla. <- as.formula(paste("richness.tr", paste(c(model.large, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Face == "face supérieure")[, c("richness.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "richness BM_FS", vars. = vars.list.M1, transfo. = "center_scale", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "richness BM_FS", vars. = vars.list.M2, transfo. = "center_scale", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne", Face == "face supérieure")[, c("richness.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "richness BM_FS", vars. = vars.list.M1, transfo. = "center_scale", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "richness BM_FS", vars. = vars.list.M2, transfo. = "center_scale", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ", Face == "face supérieure")[, c("richness.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "richness BM_FS", vars. = vars.list.M1, transfo. = "center_scale", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "richness BM_FS", vars. = vars.list.M2, transfo. = "center_scale", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)


### richness.tr face inférieure bloc mobile

## vars.short

frmla. <- as.formula(paste("richness.tr", paste(c(model.short, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Face == "face inférieure")[, c("richness.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "richness BM_FI", vars. = vars.list.M1, transfo. = "center_scale", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "richness BM_FI", vars. = vars.list.M2, transfo. = "center_scale", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne", Face == "face inférieure")[, c("richness.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "richness BM_FI", vars. = vars.list.M1, transfo. = "center_scale", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "richness BM_FI", vars. = vars.list.M2, transfo. = "center_scale", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ", Face == "face inférieure")[, c("richness.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "richness BM_FI", vars. = vars.list.M1, transfo. = "center_scale", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "richness BM_FI", vars. = vars.list.M2, transfo. = "center_scale", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)

## vars.medium

frmla. <- as.formula(paste("richness.tr", paste(c(model.medium, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Face == "face inférieure")[, c("richness.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "richness BM_FI", vars. = vars.list.M1, transfo. = "center_scale", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "richness BM_FI", vars. = vars.list.M2, transfo. = "center_scale", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne", Face == "face inférieure")[, c("richness.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "richness BM_FI", vars. = vars.list.M1, transfo. = "center_scale", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "richness BM_FI", vars. = vars.list.M2, transfo. = "center_scale", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ", Face == "face inférieure")[, c("richness.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "richness BM_FI", vars. = vars.list.M1, transfo. = "center_scale", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "richness BM_FI", vars. = vars.list.M2, transfo. = "center_scale", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)

## vars.large

frmla. <- as.formula(paste("richness.tr", paste(c(model.large, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Face == "face inférieure")[, c("richness.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "richness BM_FI", vars. = vars.list.M1, transfo. = "center_scale", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "richness BM_FI", vars. = vars.list.M2, transfo. = "center_scale", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne", Face == "face inférieure")[, c("richness.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "richness BM_FI", vars. = vars.list.M1, transfo. = "center_scale", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "richness BM_FI", vars. = vars.list.M2, transfo. = "center_scale", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ", Face == "face inférieure")[, c("richness.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "richness BM_FI", vars. = vars.list.M1, transfo. = "center_scale", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "richness BM_FI", vars. = vars.list.M2, transfo. = "center_scale", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)


### richness.tr face supérieure bloc fixé

## vars.short

frmla. <- as.formula(paste("richness.tr", paste(c(model.short, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(dplyr::filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Face == "face supérieure")[, c("richness.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "richness BF_FS", vars. = vars.list.M1, transfo. = "center_scale", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "richness BF_FS", vars. = vars.list.M2, transfo. = "center_scale", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(dplyr::filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Region == "Bretagne", Face == "face supérieure")[, c("richness.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "richness BF_FS", vars. = vars.list.M1, transfo. = "center_scale", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "richness BF_FS", vars. = vars.list.M2, transfo. = "center_scale", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(dplyr::filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Region == "EGMP.BASQ", Face == "face supérieure")[, c("richness.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "richness BF_FS", vars. = vars.list.M1, transfo. = "center_scale", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "richness BF_FS", vars. = vars.list.M2, transfo. = "center_scale", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)

## vars.medium

frmla. <- as.formula(paste("richness.tr", paste(c(model.medium, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(dplyr::filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Face == "face supérieure")[, c("richness.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "richness BF_FS", vars. = vars.list.M1, transfo. = "center_scale", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "richness BF_FS", vars. = vars.list.M2, transfo. = "center_scale", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(dplyr::filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Region == "Bretagne", Face == "face supérieure")[, c("richness.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "richness BF_FS", vars. = vars.list.M1, transfo. = "center_scale", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "richness BF_FS", vars. = vars.list.M2, transfo. = "center_scale", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(dplyr::filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Region == "EGMP.BASQ", Face == "face supérieure")[, c("richness.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "richness BF_FS", vars. = vars.list.M1, transfo. = "center_scale", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "richness BF_FS", vars. = vars.list.M2, transfo. = "center_scale", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)

## vars.large

frmla. <- as.formula(paste("richness.tr", paste(c(model.large, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(dplyr::filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Face == "face supérieure")[, c("richness.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "richness BF_FS", vars. = vars.list.M1, transfo. = "center_scale", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "richness BF_FS", vars. = vars.list.M2, transfo. = "center_scale", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(dplyr::filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Region == "Bretagne", Face == "face supérieure")[, c("richness.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "richness BF_FS", vars. = vars.list.M1, transfo. = "center_scale", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "richness BF_FS", vars. = vars.list.M2, transfo. = "center_scale", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(dplyr::filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Region == "EGMP.BASQ", Face == "face supérieure")[, c("richness.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "richness BF_FS", vars. = vars.list.M1, transfo. = "center_scale", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "richness BF_FS", vars. = vars.list.M2, transfo. = "center_scale", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)


### Shannon.tr face supérieure bloc mobile

## vars.short

frmla. <- as.formula(paste("Shannon.tr", paste(c(model.short, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Face == "face supérieure")[, c("Shannon.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Shannon BM_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Shannon BM_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne", Face == "face supérieure")[, c("Shannon.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Shannon BM_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Shannon BM_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ", Face == "face supérieure")[, c("Shannon.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Shannon BM_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Shannon BM_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)

## vars.medium

frmla. <- as.formula(paste("Shannon.tr", paste(c(model.medium, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Face == "face supérieure")[, c("Shannon.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Shannon BM_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Shannon BM_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne", Face == "face supérieure")[, c("Shannon.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Shannon BM_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Shannon BM_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ", Face == "face supérieure")[, c("Shannon.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Shannon BM_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Shannon BM_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)

## vars.large

frmla. <- as.formula(paste("Shannon.tr", paste(c(model.large, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Face == "face supérieure")[, c("Shannon.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Shannon BM_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Shannon BM_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne", Face == "face supérieure")[, c("Shannon.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Shannon BM_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Shannon BM_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ", Face == "face supérieure")[, c("Shannon.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Shannon BM_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Shannon BM_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)


### Shannon.tr face inférieure bloc mobile

## vars.short

frmla. <- as.formula(paste("Shannon.tr", paste(c(model.short, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Face == "face inférieure")[, c("Shannon.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Shannon BM_FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Shannon BM_FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne", Face == "face inférieure")[, c("Shannon.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Shannon BM_FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Shannon BM_FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ", Face == "face inférieure")[, c("Shannon.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Shannon BM_FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Shannon BM_FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)

## vars.medium

frmla. <- as.formula(paste("Shannon.tr", paste(c(model.medium, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Face == "face inférieure")[, c("Shannon.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Shannon BM_FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Shannon BM_FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne", Face == "face inférieure")[, c("Shannon.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Shannon BM_FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Shannon BM_FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ", Face == "face inférieure")[, c("Shannon.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Shannon BM_FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Shannon BM_FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)

## vars.large

frmla. <- as.formula(paste("Shannon.tr", paste(c(model.large, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Face == "face inférieure")[, c("Shannon.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Shannon BM_FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Shannon BM_FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne", Face == "face inférieure")[, c("Shannon.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Shannon BM_FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Shannon BM_FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ", Face == "face inférieure")[, c("Shannon.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Shannon BM_FI", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Shannon BM_FI", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)


### Shannon.tr face supérieure bloc fixé

## vars.short

frmla. <- as.formula(paste("Shannon.tr", paste(c(model.short, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(dplyr::filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Face == "face supérieure")[, c("Shannon.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Shannon BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Shannon BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(dplyr::filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Region == "Bretagne", Face == "face supérieure")[, c("Shannon.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Shannon BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Shannon BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(dplyr::filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Region == "EGMP.BASQ", Face == "face supérieure")[, c("Shannon.tr", vars.short)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Shannon BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Shannon BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)

## vars.medium

frmla. <- as.formula(paste("Shannon.tr", paste(c(model.medium, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(dplyr::filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Face == "face supérieure")[, c("Shannon.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Shannon BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Shannon BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(dplyr::filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Region == "Bretagne", Face == "face supérieure")[, c("Shannon.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Shannon BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Shannon BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(dplyr::filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Region == "EGMP.BASQ", Face == "face supérieure")[, c("Shannon.tr", vars.medium)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Shannon BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Shannon BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)

## vars.large

frmla. <- as.formula(paste("Shannon.tr", paste(c(model.large, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(dplyr::filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Face == "face supérieure")[, c("Shannon.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Shannon BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "Shannon BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(dplyr::filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Region == "Bretagne", Face == "face supérieure")[, c("Shannon.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Shannon BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "Shannon BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(dplyr::filter(df., Type.Bloc %in% c("Bloc fixé", "Roche en place"), Region == "EGMP.BASQ", Face == "face supérieure")[, c("Shannon.tr", vars.large)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Shannon BF_FS", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "Shannon BF_FS", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)
}


# Let's add ivr modeling next, if removed from the model.large.vs.short vectors

{
### blocs.retournes.fr.tr

## vars.short

frmla. <- as.formula(paste("blocs.retournes.fr.tr", paste(c(model.short.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile")[, c("blocs.retournes.fr.tr", vars.short.ivr)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "blocs.retournes.fr.", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "blocs.retournes.fr.", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("blocs.retournes.fr.tr", vars.short.ivr)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "blocs.retournes.fr.", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "blocs.retournes.fr.", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("blocs.retournes.fr.tr", vars.short.ivr)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "blocs.retournes.fr.", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "blocs.retournes.fr.", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.short.ivr", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)

## vars.medium

frmla. <- as.formula(paste("blocs.retournes.fr.tr", paste(c(model.medium.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile")[, c("blocs.retournes.fr.tr", vars.medium.ivr)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "blocs.retournes.fr.", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "blocs.retournes.fr.", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("blocs.retournes.fr.tr", vars.medium.ivr)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "blocs.retournes.fr.", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "blocs.retournes.fr.", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("blocs.retournes.fr.tr", vars.medium.ivr)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "blocs.retournes.fr.", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "blocs.retournes.fr.", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.medium.ivr", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)

## vars.large

frmla. <- as.formula(paste("blocs.retournes.fr.tr", paste(c(model.large.ivr, "(1|Site/Numero.Quadrat)"), sep = "", collapse = " + "), sep = " ~ "))

# Atlantique
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile")[, c("blocs.retournes.fr.tr", vars.large.ivr)])
get.vars.()
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "blocs.retournes.fr.", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Atlantique", dataset. = "blocs.retournes.fr.", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# Bretagne
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Region == "Bretagne")[, c("blocs.retournes.fr.tr", vars.large.ivr)])
get.vars.()
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "blocs.retournes.fr.", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "Bretagne", dataset. = "blocs.retournes.fr.", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

# EGMP.BASQ
df.red <- na.omit(dplyr::filter(df., Type.Bloc == "Bloc mobile", Region == "EGMP.BASQ")[, c("blocs.retournes.fr.tr", vars.large.ivr)])
get.vars.()
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "blocs.retournes.fr.", vars. = vars.list.M1, transfo. = "orderNorm", frmla. = frmla.mod.M1) ; sw_to_tbl.(M1.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
matri.fct(data. = df.red, region. = "EGMP.BASQ", dataset. = "blocs.retournes.fr.", vars. = vars.list.M2, transfo. = "orderNorm", frmla. = frmla.mod.M2) ; sw_to_tbl.(M2.sw)
tbl.join <- dplyr::bind_rows(tbl.join, tbl.)
rm(df.red, frmla.mod.M1, frmla.mod.M2, M1, M2, vars.list.M1, vars.list.M2, tbl., DredgeOutput, M1.sw, M2.sw)

tbl.join$vars. <- ifelse(is.na(tbl.join$vars.), "vars.large.ivr", tbl.join$vars.)
tbl.join$M.1vs2[(length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2)] <- rep(c("M1","M2"), length((length(tbl.join$M.1vs2)-5):length(tbl.join$M.1vs2))/2)
}

