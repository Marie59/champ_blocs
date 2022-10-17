## do calculate QECB values now

# create lists

qecb_val_qu_list <- vector("list", length(unique(qecbnew$site_year_month_day)))
qecb_val_list <- vector("list", length(unique(qecbnew$site_year_month_day)))

for (i in c(1:length(unique(qecbnew$site_year_month_day)))) {

  qecb_i <- qecbnew  %>% dplyr::filter(site_year_month_day == unique(qecbnew$site_year_month_day)[[i]])

  (terri_ <- unique(substr(qecb_i$Site, 1, 4)))

  nb. <- as.vector(unlist(intersect(dplyr::filter(qecb_i, Type.Bloc == "Bloc mobile" & Face == "face supérieure")["Numéro.Bloc.échantillon"], dplyr::filter(qecb_i, Type.Bloc == "Bloc mobile" & Face == "face inférieure")["Numéro.Bloc.échantillon"])))

  bloc_nb <- c(
    paste0("Bloc mobile", " - ", "face supérieure", " - ", nb.),
    paste0("Bloc mobile", " - ", "face inférieure", " - ", nb.),
    paste0(as.vector(unlist(dplyr::filter(qecb_i, Type.Bloc != "Bloc mobile")["Type.Bloc"])), " - ", as.vector(unlist(dplyr::filter(qecb_i, Type.Bloc != "Bloc mobile")["Face"])), " - ", as.vector(unlist(dplyr::filter(qecb_i, Type.Bloc != "Bloc mobile")["Numéro.Bloc.échantillon"])))
  )

  qecb_i$Bloc <- paste0(qecb_i$Type.Bloc, " - ", qecb_i$Face, " - ", qecb_i$Numéro.Bloc.échantillon)

  qecb_i <- qecb_i %>% subset(Bloc %in% bloc_nb)
    ## qebm_1


    # vfs_bm Bloc mobile

{
  df_bm_fs <- qecb_i %>% dplyr::filter(qecb_i$Type.Bloc == "Bloc mobile" & qecb_i$Face == "face supérieure") # to keep a version of it for later on correction for accollement for BM FI
    df_ <- df_bm_fs
    df_ <- dplyr::arrange(df_, Numéro.Bloc.échantillon)

    a_bms <- df_$X..algues.brunes + df_$X..algues.rouges + df_$X..Cladophora
    b_bms <-  df_$X..Lithophyllum
    c_bms <- df_$Nb.Littorina.obtusata + df_$Nb.Gibbula.cineraria + df_$Nb.Gibbula.pennanti + df_$Nb.Gibbula.umbilicalis
    d_bms <- df_$X..Eponges + df_$X..Ascidies.Coloniales + df_$X..Ascidies.Solitaires + df_$X..Bryozoaires.Dresses
    e_bms <- df_$X..algues.vertes
    f_bms <- df_$X..Roche.Nue

    vfs_bm <- ((df_$X..algues.brunes + df_$X..algues.rouges + df_$X..Cladophora)
      +  df_$X..Lithophyllum
      + (df_$Nb.Littorina.obtusata + df_$Nb.Gibbula.cineraria + df_$Nb.Gibbula.pennanti + df_$Nb.Gibbula.umbilicalis)
      + (df_$X..Eponges + df_$X..Ascidies.Coloniales + df_$X..Ascidies.Solitaires + df_$X..Bryozoaires.Dresses)
    ) -
      (df_$X..algues.vertes
          +  df_$X..Roche.Nue
      )
    vfs_bm


    # vfi_bm Bloc mobile

    df_ <- qecb_i %>% dplyr::filter(Type.Bloc == "Bloc mobile" & Face == "face inférieure")
    df_ <- dplyr::arrange(df_, Numéro.Bloc.échantillon)

    df_bm_fi <- df_

    # accolement function according to recent 'retournement'

    `%notin%` <- Negate(`%in%`)

    acco_fct <- function(var_) {

      if (terri_ %notin% c("EGMP", "BASQ")) {
        ifelse(#df_$Couleur.dominante %in% c("Rouge", "Brune", "Brune-Rouge") ||
          df_bm_fs$Couleur.dominante %in% c("Blanche", "Verte", "Blanche-Verte", "Colorée"), df_bm_fi[, var_] / (100 - df_bm_fi$X..Surface.Accolement) * 100, df_bm_fi[, var_])
      } else {
        ifelse(df_bm_fs$Couleur.dominante %in% c("Blanche", "Verte", "Blanche-Verte", "Colorée")
               & df_bm_fi$X..Surface.Accolement != 0 # I have to use it in dplyr::filter this time as well for EGMP- BASQ (but not for Bretagne, altough could be added, same result); identical/repeated measure for BM.FI and BM.FS
               & df_bm_fs$X..Mytilus.sp. == 0, df_bm_fi[, var_] / (100 - df_bm_fi$X..Surface.Accolement) * 100, df_bm_fi[, var_])
      }

    }

    # I would only consider colors in c("Rouge", "Brune", "Brune-Rouge") for BM.FI correction [ and not the series c("Blanche-Brune", "Rouge", "Brune", "Blanche-Rouge", "Brune-Rouge", "Rouge-Verte", "Brune-Verte") ] ; and for BM.FS, the list c("Blanche", "Verte", "Colorée") => we do the correction for BM.FI accollement based on BM.FS color !!!

    df_bm_fs$Couleur.dominante
    df_bm_fs$X..Mytilus.sp.

    df_[, "X..Eponges"]
    df_[, "X..Surface.Accolement"]
    df_[, "X..Eponges"] <- acco_fct("X..Eponges")
    df_[, "X..Eponges"] <- as.numeric(ifelse(as.character(df_[, "X..Eponges"]) %in% c(NA, "NaN", "-Inf", "Inf"), "0", as.character(df_[, "X..Eponges"])))
    df_[, "X..Eponges"] <- ifelse(df_[, "X..Eponges"] > 100, 100, df_[, "X..Eponges"])
    df_[, "X..Eponges"]

    df_[, "X..Ascidies.Coloniales"]
    df_[, "X..Surface.Accolement"]
    df_[, "X..Ascidies.Coloniales"] <- acco_fct("X..Ascidies.Coloniales")
    df_[, "X..Ascidies.Coloniales"] <- as.numeric(ifelse(as.character(df_[, "X..Ascidies.Coloniales"]) %in% c(NA, "NaN", "-Inf", "Inf"), "0", as.character(df_[, "X..Ascidies.Coloniales"])))
    df_[, "X..Ascidies.Coloniales"] <- ifelse(df_[, "X..Ascidies.Coloniales"] > 100, 100, df_[, "X..Ascidies.Coloniales"])
    df_[, "X..Ascidies.Coloniales"]

    df_[, "X..Ascidies.Solitaires"]
    df_[, "X..Surface.Accolement"]
    df_[, "X..Ascidies.Solitaires"] <- acco_fct("X..Ascidies.Solitaires")
    df_[, "X..Ascidies.Solitaires"] <- as.numeric(ifelse(as.character(df_[, "X..Ascidies.Solitaires"]) %in% c(NA, "NaN", "-Inf", "Inf"), "0", as.character(df_[, "X..Ascidies.Solitaires"])))
    df_[, "X..Ascidies.Solitaires"] <- ifelse(df_[, "X..Ascidies.Solitaires"] > 100, 100, df_[, "X..Ascidies.Solitaires"])
    df_[, "X..Ascidies.Solitaires"]

    df_[, "X..Bryozoaires.Dresses"]
    df_[, "X..Surface.Accolement"]
    df_[, "X..Bryozoaires.Dresses"] <- acco_fct("X..Bryozoaires.Dresses")
    df_[, "X..Bryozoaires.Dresses"] <- as.numeric(ifelse(as.character(df_[, "X..Bryozoaires.Dresses"]) %in% c(NA, "NaN", "-Inf", "Inf"), "0", as.character(df_[, "X..Bryozoaires.Dresses"])))
    df_[, "X..Bryozoaires.Dresses"] <- ifelse(df_[, "X..Bryozoaires.Dresses"] > 100, 100, df_[, "X..Bryozoaires.Dresses"])
    df_[, "X..Bryozoaires.Dresses"]

    df_[, "X..Lithophyllum"]
    df_[, "X..Surface.Accolement"]
    df_[, "X..Lithophyllum"] <- acco_fct("X..Lithophyllum")
    df_[, "X..Lithophyllum"] <- as.numeric(ifelse(as.character(df_[, "X..Lithophyllum"]) %in% c(NA, "NaN", "-Inf", "Inf"), "0", as.character(df_[, "X..Lithophyllum"])))
    df_[, "X..Lithophyllum"] <- ifelse(df_[, "X..Lithophyllum"] > 100, 100, df_[, "X..Lithophyllum"])
    df_[, "X..Lithophyllum"]

    d_bmi <- df_$X..Eponges + df_$X..Ascidies.Coloniales + df_$X..Ascidies.Solitaires + df_$X..Bryozoaires.Dresses
    b_bmi <- df_$X..Lithophyllum
    a_bmi <- df_$X..algues.brunes + df_$X..algues.rouges + df_$X..Cladophora
    c_bmi <- df_$Nb.Littorina.obtusata + df_$Nb.Gibbula.cineraria + df_$Nb.Gibbula.pennanti + df_$Nb.Gibbula.umbilicalis
    e_bmi <- df_$X..algues.vertes
    f_bmi <- df_$X..Roche.Nue

    vfi_bm <- ((df_$X..Eponges + df_$X..Ascidies.Coloniales + df_$X..Ascidies.Solitaires + df_$X..Bryozoaires.Dresses)
      +  df_$X..Lithophyllum
    ) -
      ((df_$X..algues.brunes + df_$X..algues.rouges + df_$X..Cladophora)
         + (df_$Nb.Littorina.obtusata + df_$Nb.Gibbula.cineraria + df_$Nb.Gibbula.pennanti + df_$Nb.Gibbula.umbilicalis)
         +  df_$X..algues.vertes
         +  df_$X..Roche.Nue
      )
    vfi_bm

    # vfsi_bm Bloc mobile

    df_ <- qecb_i %>% dplyr::filter(Type.Bloc == "Bloc mobile")
    df_ <- dplyr::arrange(df_, desc(Face), Numéro.Bloc.échantillon)
    num_bloc <- as.vector(sort(unique(df_$Numéro.Bloc.échantillon)))

    g_bmsi <- NA
    h_bmsi <- NA
    l_bmsi <- NA

    df_bm_fs <- dplyr::filter(df_, Face == "face inférieure")
    df_bm_fi <- dplyr::filter(df_, Face == "face inférieure")

    df_bm_fs$Couleur.dominante
    df_bm_fs$X..Mytilus.sp.

    df_[, "X..Balanes.Vivantes"]
    df_[, "X..Surface.Accolement"]
    df_ <- dplyr::mutate(df_, row.nb = dplyr::row_number())
    dplyr::filter(df_, Face == "face inférieure")["row.nb"]
    df_[c(dplyr::filter(df_, Face == "face inférieure")[1, "row.nb"]:unlist(tail(dplyr::filter(df_, Face == "face inférieure"), n = 1)["row.nb"])), "X..Balanes.Vivantes"] <- acco_fct("X..Balanes.Vivantes")
    df_[, "X..Balanes.Vivantes"] <- as.numeric(ifelse(as.character(df_[, "X..Balanes.Vivantes"]) %in% c(NA, "NaN", "-Inf", "Inf"), "0", as.character(df_[, "X..Balanes.Vivantes"])))
    df_[, "X..Balanes.Vivantes"] <- ifelse(df_[, "X..Balanes.Vivantes"] > 100, 100, df_[, "X..Balanes.Vivantes"])
    df_[, "X..Balanes.Vivantes"]

    for (k in c(1:length(na.omit(num_bloc)))) {

      j_ <- num_bloc[k]

      gin_ <- unname(unlist(
        (dplyr::filter(df_, Numéro.Bloc.échantillon == j_ & Face == "face supérieure")["Nb.spirorbis.total"]
         + dplyr::filter(df_, Numéro.Bloc.échantillon == j_ & Face == "face inférieure")["Nb.spirorbis.total"]
        ) / 1000))

      g_bmsi <<- c(g_bmsi, gin_)

      hin_ <- unname(unlist(
        (dplyr::filter(df_, Numéro.Bloc.échantillon == j_ & Face == "face supérieure")["X..Balanes.Vivantes"]
         + dplyr::filter(df_, Numéro.Bloc.échantillon == j_ & Face == "face inférieure")["X..Balanes.Vivantes"]
        ) / 100))

      h_bmsi <<- c(h_bmsi, hin_)

      lin_ <- unname(unlist(
        (dplyr::filter(df_, Numéro.Bloc.échantillon == j_ & Face == "face supérieure")["Nb.Spirobranchus.lamarckii.total"]
         + dplyr::filter(df_, Numéro.Bloc.échantillon == j_ & Face == "face inférieure")["Nb.Spirobranchus.lamarckii.total"]
        ) / 100))

      l_bmsi <<- c(l_bmsi, lin_) # To avoid error message "Error in I <<- c(I, IIn.) : cannot change value of locked binding for 'I'"

    }

    g_bmsi <- g_bmsi[2:length(g_bmsi)]
    g_bmsi
    h_bmsi <- h_bmsi[2:length(h_bmsi)]
    h_bmsi
    i_bmsi <- l_bmsi[2:length(l_bmsi)]
    i_bmsi

    vfsi_bm <- NA

    for (k in c(1:length(na.omit(num_bloc)))) {

      j_ <- num_bloc[k]

      vfsin_ <- unname(unlist(
        ((dplyr::filter(df_, Numéro.Bloc.échantillon == j_ & Face == "face supérieure")["Nb.spirorbis.total"]
           + dplyr::filter(df_, Numéro.Bloc.échantillon == j_ & Face == "face inférieure")["Nb.spirorbis.total"]
        ) / 1000)
        -
          (((dplyr::filter(df_, Numéro.Bloc.échantillon == j_ & Face == "face supérieure")["X..Balanes.Vivantes"]
               + dplyr::filter(df_, Numéro.Bloc.échantillon == j_ & Face == "face inférieure")["X..Balanes.Vivantes"]
            ) / 100)
            + ((dplyr::filter(df_, Numéro.Bloc.échantillon == j_ & Face == "face supérieure")["Nb.Spirobranchus.lamarckii.total"]
                 + dplyr::filter(df_, Numéro.Bloc.échantillon == j_ & Face == "face inférieure")["Nb.Spirobranchus.lamarckii.total"]
            ) / 100)
          )
      ))

      vfsi_bm <<- c(vfsi_bm, vfsin_)

    }

    vfsi_bm <- vfsi_bm[2:length(vfsi_bm)]
    vfsi_bm


    # qebm_1

    (qebm_1 <- vfs_bm + vfi_bm + vfsi_bm)

    ## qebm_2


    # vrfs_bf moyenne Bloc fixé ; = VDRmoyenne in excel file

    df_ <- qecb_i %>% dplyr::filter(Type.Bloc %in% c("Bloc fixé", "Roche en place"))
    df_ <- dplyr::arrange(df_, Numéro.Bloc.échantillon)

    a_bf <- df_$X..algues.brunes + df_$X..algues.rouges + df_$X..Cladophora
    b_bf <- df_$X..Lithophyllum
    c_bf <- df_$Nb.Littorina.obtusata + df_$Nb.Gibbula.cineraria + df_$Nb.Gibbula.pennanti + df_$Nb.Gibbula.umbilicalis
    d_bf <- df_$X..Eponges + df_$X..Ascidies.Coloniales + df_$X..Ascidies.Solitaires + df_$X..Bryozoaires.Dresses
    e_bf <- df_$X..algues.vertes
    f_bf <- df_$X..Roche.Nue

    a_bf <- c(a_bf, rep(NA, length(unique(dplyr::filter(qecb_i, Type.Bloc == "Bloc mobile")["Numéro.Bloc.échantillon"])[, 1]) - length(a_bf)))
    b_bf <- c(b_bf, rep(NA, length(unique(dplyr::filter(qecb_i, Type.Bloc == "Bloc mobile")["Numéro.Bloc.échantillon"])[, 1]) - length(b_bf)))
    c_bf <- c(c_bf, rep(NA, length(unique(dplyr::filter(qecb_i, Type.Bloc == "Bloc mobile")["Numéro.Bloc.échantillon"])[, 1]) - length(c_bf)))
    d_bf <- c(d_bf, rep(NA, length(unique(dplyr::filter(qecb_i, Type.Bloc == "Bloc mobile")["Numéro.Bloc.échantillon"])[, 1]) - length(d_bf)))
    e_bf <- c(e_bf, rep(NA, length(unique(dplyr::filter(qecb_i, Type.Bloc == "Bloc mobile")["Numéro.Bloc.échantillon"])[, 1]) - length(e_bf)))
    f_bf <- c(f_bf, rep(NA, length(unique(dplyr::filter(qecb_i, Type.Bloc == "Bloc mobile")["Numéro.Bloc.échantillon"])[, 1]) - length(f_bf)))

    vrfs_bf <- ((df_$X..algues.brunes + df_$X..algues.rouges + df_$X..Cladophora)
      +  df_$X..Lithophyllum
      + (df_$Nb.Littorina.obtusata + df_$Nb.Gibbula.cineraria + df_$Nb.Gibbula.pennanti + df_$Nb.Gibbula.umbilicalis)
      + (df_$X..Eponges + df_$X..Ascidies.Coloniales + df_$X..Ascidies.Solitaires + df_$X..Bryozoaires.Dresses)
    ) -
      (df_$X..algues.vertes
          +  df_$X..Roche.Nue
      ) # different from Pauline, check with her
    vrfs_bf
    vrfs_bf <- c(vrfs_bf, rep(NA, length(unique(dplyr::filter(qecb_i, Type.Bloc == "Bloc mobile")["Numéro.Bloc.échantillon"])[, 1]) - length(vrfs_bf)))

    # (G - (H + I)) Bloc fixé & Roche en place

    g_bf <- df_$Nb.spirorbis.total / 1000
    h_bf <- df_$X..Balanes.Vivantes / 100
    i_bf <- df_$Nb.Spirobranchus.lamarckii.total / 100

    g_bf <- c(g_bf, rep(NA, length(unique(dplyr::filter(qecb_i, Type.Bloc == "Bloc mobile")["Numéro.Bloc.échantillon"])[, 1]) - length(g_bf)))
    h_bf <- c(h_bf, rep(NA, length(unique(dplyr::filter(qecb_i, Type.Bloc == "Bloc mobile")["Numéro.Bloc.échantillon"])[, 1]) - length(h_bf)))
    i_bf <- c(i_bf, rep(NA, length(unique(dplyr::filter(qecb_i, Type.Bloc == "Bloc mobile")["Numéro.Bloc.échantillon"])[, 1]) - length(i_bf)))

    `(G - (H + I))BF` <- (df_$Nb.spirorbis.total / 1000
      - (df_$X..Balanes.Vivantes / 100 + df_$Nb.Spirobranchus.lamarckii.total / 100)
    )
    `(G - (H + I))BF`
    `(G - (H + I))BF` <- c(`(G - (H + I))BF`, rep(NA, length(unique(dplyr::filter(qecb_i, Type.Bloc == "Bloc mobile")["Numéro.Bloc.échantillon"])[, 1]) - length(`(G - (H + I))BF`)))


    # vrfs_bf_moy

    (vrfs_bf_moy <- mean(vrfs_bf + `(G - (H + I))BF`, na.rm = TRUE))


    # (G - (H + I)S.BM) Bloc mobile face supérieure

    df_ <- qecb_i %>% dplyr::filter(Type.Bloc == "Bloc mobile" & Face == "face supérieure")
    df_ <- dplyr::arrange(df_, Numéro.Bloc.échantillon)

    g_bms <- df_$Nb.spirorbis.total / 1000
    h_bms <- df_$X..Balanes.Vivantes / 100
    i_bms <- df_$Nb.Spirobranchus.lamarckii.total / 100

    `(G - (H + I))S.BM` <- (df_$Nb.spirorbis.total / 1000
      - (df_$X..Balanes.Vivantes / 100 + df_$Nb.Spirobranchus.lamarckii.total / 100)
    )
    `(G - (H + I))S.BM`


    # vrfs_bm

    (vrfs_bm <- vfs_bm + `(G - (H + I))S.BM`)


    # vrfs_bm_moy

    (vrfs_bm_moy <- mean(vrfs_bm#[val.vrfs_bm_moy.i]
          , na.rm = TRUE))


    # ||vrfs_bm_moy/vrfs_bf_moy||

    (`||vrfs_bm_moy/vrfs_bf_moy||` <- abs(mean(vrfs_bm#[val.vrfs_bm_moy.i]
              , na.rm = TRUE)/vrfs_bf_moy))


    # qebm_2

    (qebm_2 <- qebm_1 * `||vrfs_bm_moy/vrfs_bf_moy||`)


    ## QECB

    (QECB <- mean(qebm_2#[val.qecb_i]
          , na.rm = TRUE))

  }

  qecb_val_qu_list[[i]] <- data.frame(id_qecb = rep(unique(qecb_i$id_qecb), length(qebm_2)),
    Site = rep(unique(qecb_i$Site), length(qebm_2)),
    Site_bis = rep(unique(qecb_i$Site_bis), length(qebm_2)),
    site_year_month_day = rep(unique(qecb_i$site_year_month_day), length(qebm_2)),
    Boulder.nb_bms = sort(unique(dplyr::filter(qecb_i, Type.Bloc == "Bloc mobile" & Face == "face supérieure")["Numéro.Bloc.échantillon"])[, 1]),
    Boulder.nb_bmi = sort(unique(dplyr::filter(qecb_i, Type.Bloc == "Bloc mobile" & Face == "face inférieure")["Numéro.Bloc.échantillon"])[, 1]),
    Boulder.nb_bf = c(sort(unique(dplyr::filter(qecb_i, Type.Bloc %in% c("Bloc fixé", "Roche en place"))["Numéro.Bloc.échantillon"])[, 1]), rep(NA, length(unique(dplyr::filter(qecb_i, Type.Bloc == "Bloc mobile" & Face == "face supérieure")["Numéro.Bloc.échantillon"])[, 1]) - length(unique(dplyr::filter(qecb_i, Type.Bloc %in% c("Bloc fixé", "Roche en place"))["Numéro.Bloc.échantillon"])[, 1]))),
    quadrat.bmS = sort(dplyr::filter(qecb_i, Type.Bloc == "Bloc mobile" & Face == "face supérieure")["quadrat_bis"][, 1]),
    quadrat.bmI = sort(dplyr::filter(qecb_i, Type.Bloc == "Bloc mobile" & Face == "face inférieure")["quadrat_bis"][, 1]),
    quadrat.bf = c(sort(dplyr::filter(qecb_i, Type.Bloc %in% c("Bloc fixé", "Roche en place"))["quadrat_bis"][, 1]), rep(NA, length(unique(dplyr::filter(qecb_i, Type.Bloc == "Bloc mobile")["Numéro.Bloc.échantillon"])[, 1]) - length(dplyr::filter(qecb_i, Type.Bloc %in% c("Bloc fixé", "Roche en place"))["quadrat_bis"][, 1]))),
    a_bms,
    b_bms,
    c_bms,
    d_bms,
    e_bms,
    f_bms,
    a_bmi,
    b_bmi,
    c_bmi,
    d_bmi,
    e_bmi,
    f_bmi,
    g_bmsi,
    h_bmsi,
    i_bmsi,
    a_bf,
    b_bf,
    c_bf,
    d_bf,
    e_bf,
    f_bf,
    g_bf,
    h_bf,
    i_bf,
    g_bms,
    h_bms,
    i_bms,
    vfs_bm,
    vfi_bm,
    vfsi_bm,
    qebm_1,
    vrfs_bf,
    `(G - (H + I))S.BM`,
    `(G - (H + I))BF`,
    vrfs_bm,
    qebm_2)

  qecb_val_list[[i]] <- data.frame(id_qecb = unique(qecb_i$id_qecb),
    Site = unique(qecb_i$Site),
    Site_bis = unique(qecb_i$Site_bis),
    site_year_month_day = unique(qecb_i$site_year_month_day),
    vrfs_bm_moy,
    vrfs_bf_moy,
    `||vrfs_bm_moy/vrfs_bf_moy||`,
    QECB)

  rm(qecb_i)
  rm(df_bm_fs, df_bm_fi)
  rm("(G - (H + I))BF", "(G - (H + I))S.BM", "||vrfs_bm_moy/vrfs_bf_moy||", "a_bf", "a_bmi", "a_bms", "b_bf", "b_bmi", "b_bms", "bloc_nb", "c_bf", "c_bmi", "c_bms", "d_bf", "d_bmi", "d_bms", "e_bf", "e_bmi", "e_bms", "f_bf", "f_bmi", "f_bms", "g_bf", "g_bms", "g_bmsi", "gin_", "h_bf", "h_bms", "h_bmsi", "hin_", "i", "i_bf", "i_bms", "i_bmsi", "j_", "k", "l_bmsi", "lin_", "nb.", "num_bloc", "qebm_1", "qebm_2", "QECB", "vfi_bm", "vfs_bm", "vfsi_bm", "vfsin_", "vrfs_bf", "vrfs_bf_moy", "vrfs_bm", "vrfs_bm_moy", "terri_")

}

qecb_val_qu_ <- do.call("rbind", qecb_val_qu_list)
qecb_val_qu_ <- dplyr::arrange(qecb_val_qu_, site_year_month_day, Boulder.nb_bms)
Date <- as.Date(stringr::str_sub(qecb_val_qu_$site_year_month_day, -10, -1), format = "%Y.%m.%d", origin = "1970-01-01")
qecb_val_qu_ <- tibble::add_column(qecb_val_qu_, Date, .after = "Site_bis")
rm(Date)

qebm_2_list <- vector("list", length(unique(qecb_val_qu_$site_year_month_day)))

for (i in c(1:length(unique(qecb_val_qu_$site_year_month_day)))) {

qebm_2_i <- qecb_val_qu_ %>% dplyr::filter(site_year_month_day == unique(qecb_val_qu_$site_year_month_day)[[i]])

  qebm_2_bis <- qebm_2_i$qebm_1 *
  abs((mean(((qebm_2_i$a_bms
      + qebm_2_i$b_bms
      + qebm_2_i$c_bms
      + qebm_2_i$d_bms)
    -
      (qebm_2_i$e_bms
      + qebm_2_i$f_bms)
    )
    +
    (qebm_2_i$g_bms
     - (qebm_2_i$h_bms
        + qebm_2_i$i_bms)
    )
    , na.rm = TRUE)
  )
  /
  (mean(
    (
    (qebm_2_i$a_bf
     + qebm_2_i$b_bf
     + qebm_2_i$c_bf
     + qebm_2_i$d_bf)
    -
    (qebm_2_i$e_bf
     + qebm_2_i$f_bf)
    )
    +
    (qebm_2_i$g_bf
     -
    (qebm_2_i$h_bf
     + qebm_2_i$i_bf)
    )
    , na.rm = TRUE)
  )
  )

qebm_2_list[[i]] <- data.frame(site_year_month_day = unique(qebm_2_i$site_year_month_day), qebm_2_bis)

rm(i, qebm_2_i, qebm_2_bis)

}

qecb_val_qu_[, ncol(qecb_val_qu_) + 1] <- do.call("rbind", qebm_2_list)[2]

qecb_val_ <- do.call("rbind", qecb_val_list)
qecb_val_ <- dplyr::arrange(qecb_val_, site_year_month_day)
Date <- as.Date(stringr::str_sub(qecb_val_$site_year_month_day, -10, -1), format = "%Y.%m.%d", origin = "1970-01-01")
qecb_val_ <- tibble::add_column(qecb_val_, Date, .after = "Site_bis")
rm(Date)

rm(list = ls()[!ls() %in% c("fiche", "qecb", "qecbnew", "qecb_val_qu_")])

qecb_val_qu_ <- tidyr::separate(qecb_val_qu_, Date, c("Annee", "Mois", "Jour"), remove = FALSE)
qecb_val_qu_$Annee <- as.integer(qecb_val_qu_$Annee)
qecb_val_qu_$Mois <- as.integer(qecb_val_qu_$Mois)
qecb_val_qu_$Jour <- as.integer(qecb_val_qu_$Jour)

dplyr::filter(qecb_val_qu_, qebm_2 %in% c("Inf", "NaN"))

qecb_val_qu_nan <- qecb_val_qu_
qecb_val_qu_nan$qebm_2 <- ifelse(qecb_val_qu_nan$qebm_2 %in% c("-Inf", "NaN"), NA, qecb_val_qu_nan$qebm_2)

qecb_val_qu_stat_ <- qecb_val_qu_nan %>% dplyr::group_by(id_qecb, Site, Site_bis, Annee, Mois, Jour) %>% dplyr::summarize(qecb.moy = mean(qebm_2, na.rm = TRUE), qecb.et = sd(qebm_2, na.rm = TRUE), qecb.med = median(qebm_2, na.rm = TRUE), qecb.min = min(qebm_2, na.rm = TRUE), qecb.max = max(qebm_2, na.rm = TRUE), nb. = dplyr::n(), nb.notNa = sum(!is.na(qebm_2)))

Date <- as.Date(paste0(qecb_val_qu_stat_$Annee, "-", qecb_val_qu_stat_$Mois, "-", qecb_val_qu_stat_$Jour), origin = "1970-01-01")
qecb_val_qu_stat_ <- tibble::add_column(qecb_val_qu_stat_, Date, .after = "Site_bis")
rm(Date)

qecb_val_qu_stat_ <- as.data.frame(qecb_val_qu_stat_)
indic <- qecb_val_qu_stat_
saveRDS(qecb_val_qu_stat_, "qecb_val_qu_stat.RDS")


survey_list <- vector("list", length(unique(qecb_val_qu_$site_year_month_day)))

for (i in c(1:length(unique(qecb_val_qu_$site_year_month_day)))) {

  qecb_i <- qecb_val_qu_  %>% dplyr::filter(site_year_month_day == unique(qecb_val_qu_$site_year_month_day)[[i]])

  survey_list[[i]] <- data.frame(
    site_year_month_day = rep(unique(qecb_i$site_year_month_day), nrow(qecb_i)),
    survey_nb = rep(i, nrow(qecb_i))
  )

}

survey <- do.call("rbind", survey_list)

qecb_val_qu_ <- tibble::add_column(qecb_val_qu_, survey_nb = survey$survey_nb, .after = "site_year_month_day")
indic_full <- qecb_val_qu_
rm(i, survey_list, survey, qecb_i)

survey_nb <- c(1:nrow(qecb_val_qu_))
qecb_val_ <- tibble::add_column(qecb_val_qu_, survey_nb, .after = "site_year_month_day")

rm(survey_nb, qecb_val_qu_nan)

saveRDS(qecb_val_, "qecb_val.RDS")
saveRDS(qecb_val_qu_, "qecb_val_qu.RDS")


## Plots qecb

qecb_val_qu_nan <- qecb_val_qu_
qecb_val_qu_stat_nan <- qecb_val_qu_stat_

`%notin%` <- Negate(`%in%`)

qecb_val_qu_nan$qebm_2 <- ifelse(qecb_val_qu_nan$qebm_2 %in% c("-Inf", "NaN"), NA, qecb_val_qu_nan$qebm_2)

qecb_val_qu_stat_nan[, c("qecb.moy", "qecb.et", "qecb.med", "qecb.min", "qecb.max")] <- ifelse(qecb_val_qu_stat_nan[, c("qecb.moy", "qecb.et", "qecb.med", "qecb.min", "qecb.max")] %in% c("-Inf", "NaN"), NA, qecb_val_qu_stat_nan[, c("qecb.moy", "qecb.et", "qecb.med", "qecb.min", "qecb.max")])



for (i in c(1:length(unique(qecb_val_qu_stat_nan$Site)))) {

  df1 <- dplyr::filter(qecb_val_qu_stat_nan, Site == unique(qecb_val_qu_stat_nan$Site)[i])


  xmin_ <- as.Date(ifelse(min(df1$Annee) >= 2014, "2014-01-01", paste0(min(df$Annee), "-01-01")), origin = "1970-01-01")
  xmax_ <- as.Date(ifelse(max(df1$Annee) <= 2017, "2018-01-01", #paste0(max(qecb_val_eg$Annee)+1,
                          "2023-01-01")
                   #)
                   , origin = "1970-01-01")

  png(paste0("old_qecb_", unique(qecb_val_qu_stat_nan$Site), ".png"))
  plot(qecb_val_qu_stat_nan$Date, qecb_val_qu_stat_nan$qecb.med, xlim = c(xmin_, xmax_), ylim = c(-360, 360), pch = 19, cex = 1, main = unique(df1$Site_bis), xlab = "Année",
       ylab = "QECB", col = "grey")
  points(df1$Date, df1$qecb.med, pch = 19, cex = 1.5)
  arrows(df1$Date, df1$qecb.med, df1$Date, df1$qecb.max, code = 3, angle = 90, length = 0.00)
  arrows(df1$Date, df1$qecb.med, df1$Date, df1$qecb.min, code = 3, angle = 90, length = 0.00)

  abline(h = c(-216, -72, 72, 216), lty = "dashed")
  text(xmax_, -288, "1")
  text(xmax_, -146, "2")
  text(xmax_, 0, "3")
  text(xmax_, 146, "4")
  text(xmax_, 288, "5")

}

# New quality scale based on quartiles

dt_ <- dplyr::filter(qecb_val_qu_nan, qebm_2 >= -360)
dt_ <- dplyr::filter(dt_, qebm_2 <= 360)
dt_bis <- dplyr::filter(dt_, qebm_2 >= quantile(dt_$qebm_2, c(0.05), na.rm = TRUE))
dt_bis <- dplyr::filter(dt_bis, qebm_2 <= quantile(dt_bis$qebm_2, c(0.95), na.rm = TRUE))

one <- round(mean(unlist(dplyr::filter(dt_bis, qebm_2 <= quantile(dt_bis$qebm_2, 0.25, na.rm = TRUE))["qebm_2"])), digits = 0)
two <- round(mean(unlist(dplyr::filter(dt_bis, qebm_2 > quantile(dt_bis$qebm_2, 0.25, na.rm = TRUE) & qebm_2 <= quantile(dt_bis$qebm_2, 0.5, na.rm = TRUE))["qebm_2"])), digits = 0)
three <- round(mean(unlist(dplyr::filter(dt_bis, qebm_2 > quantile(dt_bis$qebm_2, 0.5, na.rm = TRUE) & qebm_2 <= quantile(dt_bis$qebm_2, 0.75, na.rm = TRUE))["qebm_2"])), digits = 0)
four <- round(mean(unlist(dplyr::filter(dt_bis, qebm_2 > quantile(dt_bis$qebm_2, 0.75, na.rm = TRUE))["qebm_2"])), digits = 0)


# I have unactivated the model line drawing because aberant for some sites with bad qecb values
for (i in c(1:length(unique(qecb_val_qu_stat_nan$Site)))) {

  df1 <- dplyr::filter(qecb_val_qu_stat_nan, Site == unique(qecb_val_qu_stat_nan$Site)[i])

  xmin_ <- as.Date(ifelse(min(df1$Annee) >= 2014, "2014-01-01", paste0(min(df$Annee), "-01-01")), origin = "1970-01-01")
  xmax_ <- as.Date(ifelse(max(df1$Annee) <= 2017, "2018-01-01", #paste0(max(qecb_val_eg$Annee)+1,
                          "2022-01-01")
                   #)
                   , origin = "1970-01-01")

  ymin_ <- ifelse(min(df1$qecb.med, na.rm = TRUE) < -70, -360, -70)
  ymax_ <- ifelse(max(df1$qecb.med, na.rm = TRUE) > 200, 360, 200)

  png(paste0("new_qecb_", unique(qecb_val_qu_stat_nan$Site), ".png"))
  plot(qecb_val_qu_stat_nan$Date, qecb_val_qu_stat_nan$qecb.med, xlim = c(xmin_, xmax_), ylim = c(ymin_, ymax_), pch = 19, main = "", xlab = "", ylab = "", type = "n", axes = FALSE)

  rect(as.Date("2013-01-01", origin = "1970-01-01"), -400, as.Date("2023-01-01", origin = "1970-01-01"), one, col = "red", border = NA)
  rect(as.Date("2013-01-01", origin = "1970-01-01"), one, as.Date("2023-01-01", origin = "1970-01-01"), two, col = "orange", border = NA)
  rect(as.Date("2013-01-01", origin = "1970-01-01"), two, as.Date("2023-01-01", origin = "1970-01-01"), three, col = "yellow", border = NA)
  rect(as.Date("2013-01-01", origin = "1970-01-01"), three, as.Date("2023-01-01", origin = "1970-01-01"), four, col = "olivedrab", border = NA)
  rect(as.Date("2013-01-01", origin = "1970-01-01"), four, as.Date("2023-01-01", origin = "1970-01-01"), 400, col = "blue", border = NA)

  par(new = TRUE)
  plot(qecb_val_qu_stat_nan$Date, qecb_val_qu_stat_nan$qecb.med, xlim = c(xmin_, xmax_), ylim = c(ymin_, ymax_), pch = 19, cex = 1, main = unique(df1$Site_bis), xlab = "Année",
       ylab = "QECB", col = "grey")
  points(df1$Date, df1$qecb.med, pch = 19, cex = 1.5)
  arrows(df1$Date, df1$qecb.med, df1$Date, df1$qecb.max, code = 3, angle = 90, length = 0.00)
  arrows(df1$Date, df1$qecb.med, df1$Date, df1$qecb.min, code = 3, angle = 90, length = 0.00)

}
rm(df1, dt_, dt_bis, four, i, one, three, two, xmax_, xmin_, ymax_, ymin_)


args <- commandArgs(trailingOnly = TRUE)

#####Import data

if (length(args) < 1) {
    stop("This tool needs at least 1 argument")
}else {
    report <- args[3]
    loop_file <- source(args[4])
}
