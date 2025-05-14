###################################################
#  Render the 2025 California Quillback report
# 
#
#
#
###################################################

library(here)

# ============================================================================ #
# Steps to update a model and render the document
# 1. Update base model in document: In SAR_C_Quillback_rockfish_skeleton.qmd
#     Update line 77: base_model <- '4_2_1_propBase'
# 2. Rerun the r4ss figures and tables to ensure they're up to date
#      code/make_r4ss_figs_tables.R  
#      If you've already run the template during your session, the base_model
#       already exists

# render the document
quarto::quarto_render(here("report", "SAR_C_Quillback_rockfish_skeleton.qmd"))


