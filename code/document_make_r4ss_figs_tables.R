# code to set base model directory and update r4ss plots and tables as needed
# From yellowtail 2025 and modified for quillback

# location of base model (TODO: change as needed)
# path is relative to /report/
# base_mod should match what's set in /report/SAR_C_Quillback_rockfish_skeleton.qmd
# so is not set here to avoid accidental mismatch
if (!exists("base_model")) {
  cli::cli_abort(
    "base_mod not set. Please set base_model to match what's set in /report/SAR_quillback....qmd."
  )
}

# read model output using r4ss
pp <- r4ss::SS_output(
  dir = here('models', base_model),
  SpawnOutputLabel = "Spawning output (Billions of eggs)",
  printstats = FALSE,
  verbose = FALSE
)

# make new r4ss plots
# TODO: add better fleetnames if desired
r4ss::SS_plots(pp, printfolder = "", dir= here("report","r4ss_plots"), uncertainty = TRUE)

# make new tables
# TODO: add better fleetnames if desired
r4ss::table_all(replist = pp, dir = here::here("report"))

#Add csv of numbers at age table
load(here::here("report", "tables", "numbers_at_age.rda"))
write.csv(numbers_at_age$table, here::here("report", "tables", "natage.csv"), row.names = FALSE)

# custom calls to r4ss functions

# taller biology plot to be easier to see and 
r4ss::SSplotBiology(
  pp,
  subplots = 3,
  plotdir = here("report","r4ss_plots"),
  plot = FALSE,
  print = TRUE,
  pheight = 5.5,
  pwidth = 6.5)


# make custom selectivity plot
source(here('code/selexComp.R'))
plot_sel_all(pp) #NEED to be able to change the file path to save this plot

# # make custom index plot
# source("Rscripts/plot_indices.R")
# plot_indices(
#   model,
#   dir = "report/Figures",
#   fit = TRUE,
#   log = FALSE,
#   fleets = c(6, 5, 4, 7)
# )

# # make custom parameter prior/est plot TODO: modify to exclude male M?
# r4ss::SSplotPars(
#   model,
#   strings = "NatM_uniform_Fem_GP_1",
#   ncols = 1,
#   nrows = 1,
#   plot = FALSE,
#   print = TRUE,
#   plotdir = "report/Figures",
#   pheight = 3.5,
#   newheaders = "Female natural mortality (M)"
# )

# # NOTE: manually update comparisons with Canada via script:
# Rscripts\explore_Canadian_comparisons.R

