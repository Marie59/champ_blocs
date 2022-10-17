library("rmarkdown")

df <- indic
df_full <- indic_full

slices <- unique(df$Site)[!is.na(unique(df$Site))]

#if (choice == "pdf") {
#  for (v in slices) {
#    pdf(paste0(v, ".pdf"))
#    rmarkdown::render(report,
#         output_file = paste0(v, ".docx"),
#         output_dir = getwd(),
#         params = list(Site = v)
#    )
#    dev.off()
#  }
#}else {
  for (v in slices) {
    rmarkdown::render(report,
         output_file = paste0(v, ".docx"),
         params = list(Site = v)
    )
  }
#}
