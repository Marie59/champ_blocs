library("rmarkdown")

df <- ivr_test
df_full <- ivr_test_full

slices <- unique(df$Site)[!is.na(unique(df$Site))]

for (v in slices) {
  rmarkdown::render(report,
         output_file = paste0(v, ".docx"),
         params = list(Site = v)
  )
  dev.off()
}
