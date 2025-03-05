###################################################
#  Render the 2025 California Quillback report
#
#
#
#
###################################################

library(here)

quarto::quarto_render(here("documents","report", "SAR_C_Quillback_rockfish_skeleton.qmd"))
