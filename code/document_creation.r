##################################################################
# Create the template for the quillback rockfish stock assessment
# See the repo for instruction: https://github.com/nmfs-ost/asar
# Successfully created a skeleton but that is updated
# with an updated template that matches our TOR
# Missing Brian and also not sure it's all correct
##################################################################

#install.packages("asar", repos = c("https://nmfs-ost.r-universe.dev", "https://cloud.r-project.org"))
#install.packages("stockplotr", repos = c("https://nmfs-ost.r-universe.dev", "https://cloud.r-project.org"))

library(here)
library(asar)
library(stockplotr)

##
#Set up directory structure
##

report.dir <- here::here("documents") #where to save the report files

model.name <- "0_0_1_2021base" #name of the base model
model.dir <- here::here("models", model.name)

##
#Set up model files
##

#Step 1 - Convert base model results
#This is currently set up to save the results into the model's folder
step1 <- asar::convert_output(
  output_file = "Report.sso",
  outdir = model.dir, # or wherever you saved the example output file
  model = "SS3",
  file_save = TRUE,
  savedir = model.dir,
  save_name = "ca_qlbk_std_output"
)

#Step 2 - Create the rda files of figures and tables
#This is currently set up to save the results in the model's folder
#Note that to do this you need to run Step 1 with file_save = FALSE
stockplotr::exp_all_figs_tables(
  dat = step1,
  rda_dir = model.dir,
  ref_line = "msy",
  ref_line_sb = "msy",
  indices_unit_label = ""
)

##
#Set up template - Only need to do this once
##

# #Step 3 - Create the template
# #This creates figure and table output to an rda at rda_dir, if not already done
# #in step 2.
# #This is currently set up to save (or read) the .rda results into (or from) the
# #model's folder and save the report into the report directory folder
# asar::create_template(
#   format = "pdf",
#   office = c("SWFSC", "NWFSC"),
#   region = "California",
#   species = "Quillback rockfish",
#   spp_latin = "Sebastes maliger",
#   year = 2025,
#   author = c("Brian J. Langseth", "Melissa H. Monk", "Julia H. Coates"),
#   include_affiliation = TRUE,
#   simple_affiliation = FALSE,
#   param_names = c("cf","rf"),
#   param_values = c("Commercial fleet", "Recreational fleet"),
#   resdir = model.dir,
#   model_results = "ca_qlbk_std_output.csv",
#   model = "SS3",
#   file_dir = report.dir,
#   rda_dir = model.dir,
#   end_year = 2024,
#   ref_line = "msy",
#   ref_line_sb = "msy",
#   indices_unit_label = ""
# )

#Step 4 is to render the file created within the create_template step
#named "SAR_C_Quillback_rockfish_skeleton". 

#Once step 3 is done it does not need to be repeated. To make changes with a new
#model you will need to  
#1. recreate the results file produced in step1. 
#2. Edit the parameter 'output' in the file "SAR_C_Quillback_rockfish_skeleton" 
#to reflect the different model location. 
#3. Recreate the rda files produced in step2. 
#4. Edit the directory location within the figure and table report files.

