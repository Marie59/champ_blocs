library("rmarkdown")

df <- indic
df_full <- indic_full

slices <- unique(df$Site)[!is.na(unique(df$Site))]

for (v in slices) {
    rmarkdown::render(report,
         output_file = paste0(v, ".docx"),
         output_dir = file.path(getwd(), paste0(v, ".docx")),  
         params = list(Site = v)
    )
  }
