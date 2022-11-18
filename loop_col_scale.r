library("rmarkdown")

df <- indic
dir.create("results")
slices <- unique(df$Site)[!is.na(unique(df$Site))]

for (v in slices) {
    rmarkdown::render(report,
         output_file = paste0(v, ".docx"),
         output_dir = file.path("results"),
         params = list(Site = v)
    )
  }
