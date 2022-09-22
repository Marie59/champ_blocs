library("rmarkdown")

df <- ivr_test
df_full <- ivr_test_full

slices <- unique(df$Site)[!is.na(unique(df$Site))]

for (v in slices) {
  render("cb_ivr_site_report_col_scale_loop.Rmd",
         output_file = paste0(v, ".docx"),
         params = list(Site = v)
  )
}
