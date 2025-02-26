##################################################################
# Create the template for the quillback rockfish stock assessment
# See the repo for instruction: https://github.com/nmfs-ost/asar
# Successfully created a skeleton
# Missing Brian and also not sure it's all correct
# Saving to a dummy downloads file for now

install.packages("asar", repos = c("https://nmfs-ost.r-universe.dev", "https://cloud.r-project.org"))

#library(here)

#create a test working directory for now
setwd("C:/Users/melissa.monk/Downloads/qlbk_test")
model.dir <- "C:/Users/melissa.monk/Documents/GitHub/CA_quillback_rockfish_2025/models/0_0_1_2021base"
#output_file <- here::here("Report.sso")
output_file <- file.path(model.dir, "Report.sso")
#convert model results
asar::convert_output(
  output_file = output_file,
  outdir = here::here(), # or wherever you saved the example output file
  model = "SS3",
  file_save = TRUE,
  savedir = here::here(),
  save_name = "ca_qlbk_std_output"
)

#create the template
asar::create_template(
  format = "pdf",
  office = "SWFSC",
  region = "California",
  species = "Quillback rockfish",
  spp_latin = "Sebastes maliger",
  year = 2025,
  author = c("Brian J. Langseth", "Melissa H. Monk", "Julia H. Coates"),
  include_affiliation = TRUE,
  simple_affiliation = FALSE,
  param_names = c("cf","rf"),
  param_values = c("Commercial fleet", "Recreational fleet"),
  resdir = getwd(),
  model_results = "ca_qlbk_std_output.csv",
  model = "SS3",
  rda_dir = file.path(model.dir, "report"),
  end_year = 2024,
  ref_line = "msy",
  ref_line_sb = "msy",
  indices_unit_label = ""
)
