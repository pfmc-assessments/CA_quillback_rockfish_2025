####
#
#This function plots selectivities for each fleet on one panel, including all the blocks for 
#that fleet
#
#This pulls heavily from Ian Taylor and Kelli Johnson lingcod 2021 model plots at
#https://github.com/pfmc-assessments/lingcod/blob/main/R/plot_selex.R and
#https://github.com/pfmc-assessments/lingcod/blob/main/R/make_r4ss_plots_ling.R
#
#I pulled this from the canary 2023 repository and have updated to apply for
#CA quillback
####

#' Plot time-varying selectivity or selectivity for a single fleet
#'
#' @param mod A model object created by [get_mod()] or
#' `r4ss::SS_output()`
#' @param input A list object created by `r4ss::SS_read`
#' @param fleet a single fleet number
#' @param Factor a factor from mod$sizeselex$Factor
#' @param sex sex 1 for females, 2 for males
#' @export
#' @author Ian G. Taylor
plot_sel_ret <- function(mod,
                         fleet = 1,
                         Factor = "Lsel",
                         sex = 1,
                         legloc = "topleft", fleetnames = "default") {

  #input = r4ss::SS_read(mod$inputs$dir)

  years <- mod$startyr:mod$endyr
  # run selectivity function to get table of info on time blocks etc.
  # NOTE: this writes a png file to unfit/sel01_multiple_fleets_length1.png
  infotable <- r4ss::SSplotSelex(mod,
                                 fleets = fleet,
                                 sexes = sex,
                                 sizefactors = Factor,
                                 years = years,
                                 subplots = 1,
                                 plot = FALSE,
                                 print = TRUE,
                                 plotdir = mod$inputs$dir,
                                 fleetnames = fleetnames
  )$infotable
  # remove extra file (would need to sort out the relative path stuff)
  file.remove(file.path(mod$inputs$dir, "sel01_multiple_fleets_length1.png"))
  nlines <- nrow(infotable)
  infotable$col <- r4ss::rich.colors.short(max(6,nlines), alpha = 0.7) %>%
    rev() %>% tail(nlines)
  infotable$pch <- NA
  infotable$lty <- nrow(infotable):1
  infotable$lwd <- 3
  infotable$longname <- infotable$Yr_range
  # #Manually change block structure for fleet 2 because fleets with blocks in 
  # #the last two years are not read well
  # if(fleet == 2){ 
  #   #Update last year of last block first
  #   infotable[nrow(infotable), c("longname", "Yr_range")] <-
  #     paste0(c(infotable[nrow(infotable) - 1, c("longname", "Yr_range")]), "-2024")
  #   #Update the second year of last block
  #   infotable[nrow(infotable) - 1, c("longname", "Yr_range")] <-
  #     infotable[nrow(infotable), c("longname", "Yr_range")]
  #   #Set elements to those from later blocks to overcome extra block added at end
  #   infotable$col[-nrow(infotable)] <- infotable$col[-1]
  #   infotable$lty[-nrow(infotable)] <- infotable$lty[-1]
  # }
  # run plot function again, passing in the modified infotable
  r4ss::SSplotSelex(mod,
                    fleets = fleet,
                    sexes = sex,
                    sizefactors = Factor,
                    labels = c(
                      "Length (cm)",
                      "Age (yr)",
                      "Year",
                      ifelse(Factor == "Lsel", "Selectivity", "Retention"),
                      "Retention",
                      "Discard mortality"
                    ),
                    legendloc = legloc,
                    years = mod$startyr:mod$endyr,
                    subplots = 1,
                    plot = TRUE,
                    print = FALSE,
                    infotable = infotable,
                    mainTitle = FALSE,
                    mar = c(2,2,2,1),
                    plotdir = mod$inputs$dir,
                    fleetnames = fleetnames
  )
  mtext(infotable$FleetName, side = 3, line = 0.1)
}

#' Plot selectivity and retention for all fleets. This is hard coded
#' for our CA quillback rockfish assessment fleet structure
#'
#' @param mod A model object created by [get_mod()] or
#' `r4ss::SS_output()`
#' @param sex Either 1 (females) or 2 (males)
#' @export
#' @author Ian G. Taylor
plot_sel_all <- function(mod, sex = 1, fleetnames = "default") {
  
  graphics.off()
  
  filename <- "_selectivityPlot.png"
  if (sex == 2) {
    filename <- gsub(".png", "_males.png", filename)
  }
  filepath <- file.path(mod$inputs$dir, filename)
  png(filepath, width = 6.5, height = 6.5, units = "in", res = 300, pointsize = 10)
  par(mfrow = c(3,2), oma = c(2,2,0,0), las = 1)
  
  #For each fleet
  plot_sel_ret(mod, Factor = "Lsel", fleet = 1, sex = sex, fleetnames = fleetnames)
  mtext("Selectivity", side = 2, line = 3, las = 0)
  plot_sel_ret(mod, Factor = "Lsel", fleet = 2, sex = sex, fleetnames = fleetnames)
  plot_sel_ret(mod, Factor = "Lsel", fleet = 3, sex = sex, fleetnames = fleetnames)
  mtext("Selectivity", side = 2, line = 3, las = 0)
  plot_sel_ret(mod, Factor = "Lsel", fleet = 4, sex = sex, fleetnames = fleetnames)
  mtext("Length (cm)", side = 1, line = 2.5)
  plot_sel_ret(mod, Factor = "Lsel", fleet = 5, sex = sex, fleetnames = fleetnames)
  mtext("Length (cm)", side = 1, line = 2.5)

  
  dev.off()
  
  print(paste0("Plot in ", filepath))
}


plot_sel_all_faa <- function(mod, sex = 1, fleetnames = "default") {
  
  graphics.off()
  
  filename <- "_selectivityPlot_FAA.png"
  if (sex == 2) {
    filename <- gsub(".png", "_males.png", filename)
  }
  filepath <- file.path(mod$inputs$dir, filename)
  png(filepath, width = 6.5, height = 6.5, units = "in", res = 300, pointsize = 10)
  par(mfrow = c(4,2), oma = c(2,2,0,0), las = 1)
  
  #For each fleet
  plot_sel_ret(mod, Factor = "Lsel", fleet = 1, sex = sex, fleetnames = fleetnames)
  mtext("Selectivity", side = 2, line = 3, las = 0)
  plot_sel_ret(mod, Factor = "Lsel", fleet = 2, sex = sex, fleetnames = fleetnames)
  plot_sel_ret(mod, Factor = "Lsel", fleet = 3, sex = sex, fleetnames = fleetnames)
  mtext("Selectivity", side = 2, line = 3, las = 0)
  plot_sel_ret(mod, Factor = "Lsel", fleet = 4, sex = sex, fleetnames = fleetnames)
  plot_sel_ret(mod, Factor = "Lsel", fleet = 5, sex = sex, fleetnames = fleetnames)
  plot_sel_ret(mod, Factor = "Lsel", fleet = 6, sex = sex, fleetnames = fleetnames)
  mtext("Length (cm)", side = 1, line = 2.5)
  plot_sel_ret(mod, Factor = "Lsel", fleet = 7, sex = sex, fleetnames = fleetnames)
  mtext("Length (cm)", side = 1, line = 2.5)
  
  dev.off()
  
  print(paste0("Plot in ", filepath))
}
