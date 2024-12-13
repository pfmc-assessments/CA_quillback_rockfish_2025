##########################################################################################
#
# Model runs for 2025 California Quillback Rockfish 
#   By: Brian Langseth, Melissa Monk, Julia Coates
#
##########################################################################################
#Alternative to devtools if it doesn't work on your machine
#pak::pkg_install("pfmc-assessments/PEPtools")

#Load packages
library(r4ss)
library(PEPtools)
library(here)
library(dplyr)
library(tictoc)


##########################################################################################-
#                                 ---- TEMPLATE ---- 
#                                 ---- COPY ONLY --- 
##########################################################################################-

#Use this structure to set up model runs below

####------------------------------------------------#
# [model number_model name  ----]
####------------------------------------------------#

new_name <- "model number_model name"

##
#Copy inputs
##

copy_SS_inputs(dir.old = here('models', old_name), 
               dir.new = here('models', new_name),
               overwrite = TRUE)

mod <- SS_read(here('models', new_name))


##
#Make Changes
##

Coded changes go here


##
#Output files and run
##

SS_write(mod,
         dir = here('models', new_name),
         overwrite = TRUE)

r4ss::run(dir = here('models', new_name), 
          exe = here('models/ss3_win.exe'), 
          extras = '-nohess',
          show_in_console = TRUE, #comment out if you dont want to watch model iterations
          skipfinished = FALSE)

pp <- SS_output(here('models', new_name))
SS_plots(pp, plot = c(1:26))


##
#Comparison plots
##

xx <- SSgetoutput(dirvec = glue::glue("{models}/{subdir}", models = here('models'),
                                      subdir = c('name1',
                                                 'name2',
                                                 new_name)))
SSsummarize(xx) |>
  SSplotComparisons(legendlabels = c('label for model 1',
                                     'label for model 2',
                                     'label for new model'),
                    subplots = c(1,3), print = TRUE, plotdir = here('models', new_name))

dev.off()

##########################################################################################-
#                                 ---- END TEMPLATE ---- 
##########################################################################################-



##########################################################################################-
#                     ---- Set up from 2021 base to 2025 version ----
##########################################################################################-

####------------------------------------------------#
# 0_0_1_2021base  ----
####------------------------------------------------#

new_name <- "0_0_1_2021base"

##
#Copy inputs
##

copy_SS_inputs(dir.old = "//nwcfile.nmfs.local/FRAM/Assessments/Archives/QuillbackRF/QuillbackRF_2021/2_base_model/CA/10_0_0_postNov_base", 
               dir.new = here('models', new_name),
               overwrite = TRUE)

mod <- SS_read(here('models', new_name))


##
#Make Changes
##

##
#Output files and run
##

SS_write(mod,
         dir = here('models', new_name),
         overwrite = TRUE)

tictoc::tic()
r4ss::run(dir = here('models',new_name), 
          exe = here('models/ss3_win.exe'), 
          extras = '-nohess',
          show_in_console = TRUE, 
          skipfinished = FALSE)
tictoc::toc()

pp <- SS_output(here('models', new_name))
SS_plots(pp, plot = c(1:26))


##
#Comparison plots
##

xx <- SSgetoutput(dirvec = glue::glue("{models}/{subdir}", models = c("//nwcfile.nmfs.local/FRAM/Assessments/Archives/QuillbackRF/QuillbackRF_2021/2_base_model/CA", here('models')),
                                      subdir = c('10_0_0_postNov_base',
                                                 new_name)))
SSsummarize(xx) |>
  SSplotComparisons(legendlabels = c('2021 base',
                                     'update exe'),
                    subplots = c(1,3), print = TRUE, plotdir = here('models', new_name))

dev.off()




