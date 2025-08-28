library(r4ss)
library(here)
library(dplyr)

source(here('code/table_decision_format.R'))

#base_mod_name <- '6_0_1_postStarBase' 
base_mod_name <- '6_0_2_postStarBase_sigma075' 
base_mod <- SS_read(here('models', base_mod_name))

pstar <- 0.45

#need to run this before ForeCatch to get the catches
base45 <- SS_output(here('models', base_mod_name))

#####-
## Low decision table M = 0.0525 ---------------------------------
## Need to fix the forecast catches at the base case
mod <- base_mod

#Update catches based on base model
fore_catch <- r4ss::SS_ForeCatch(base45, yrs = 2025:2036, digits = 4)
mod$fore$ForeCatch <- fore_catch[,c(1:4)]

#Turn off buffers
mod$fore$Flimitfraction <- 1 #dont have years of buffer applied
mod$fore$FirstYear_for_caps_and_allocations <- 2037 #these should be overwritten with the fixed catch but putting here anyway

#turn off control rule
mod$fore$ControlRuleMethod <- 0

#Change M value
mod$ctl$MG_parms["NatM_p_1_Fem_GP_1", c("INIT", "PRIOR")] <- c(0.0525, round(log(0.0525),2))


##
#Output files and run
##

SS_write(mod,
         dir = here('models', paste0(base_mod_name, "_low_M_pstar_",pstar)),
         overwrite = TRUE)

r4ss::run(dir = here('models', paste0(base_mod_name, "_low_M_pstar_",pstar)),
          exe = here('models/ss3_win.exe'),
          extras = '-nohess',
          show_in_console = TRUE,
          skipfinished = FALSE)


#####-
## High decision table M = 0.08 ---------------------------------

mod <- base_mod

#Upate catches based on base model
fore_catch <- r4ss::SS_ForeCatch(base45, yrs = 2025:2036, digits = 4)
mod$fore$ForeCatch <- fore_catch[,c(1:4)]

#Turn off buffers
mod$fore$Flimitfraction <- 1 #dont have years of buffer applied
mod$fore$FirstYear_for_caps_and_allocations <- 2037 #these should be overwritten with the fixed catch but putting here anyway

#Change M value
mod$ctl$MG_parms["NatM_p_1_Fem_GP_1", c("INIT", "PRIOR")] <- c(0.08, round(log(0.08),2))


##
#Output files and run
##

SS_write(mod,
         dir = here('models', paste0(base_mod_name, "_high_M_pstar_",pstar)),
         overwrite = TRUE)

r4ss::run(dir = here('models', paste0(base_mod_name, "_high_M_pstar_",pstar)),
          exe = here('models/ss3_win.exe'),
          extras = '-nohess',
          show_in_console = TRUE,
          skipfinished = FALSE)



#####-------------------------------------------####-
#Decision Table --------------------------------Moved into 002_load_tables and 01_executive summary
#####-------------------------------------------####-

# low45 <- SS_output(here('models', paste0(base_mod_name, "_low_M_pstar_",pstar)))
# base45 <- SS_output(here('models', base_mod_name))
# high45 <- SS_output(here('models', paste0(base_mod_name, "_high_M_pstar_",pstar)))

# low40 <- SS_output(here('models','decision_tables',"low_0.4"))
# base40 <- SS_output(here('models','decision_tables',"base_0.4"))
# high40 <- SS_output(here('models','decision_tables',"high_0.4"))

# caption <- "Decision table with 10-year projections beginning in 2027 for alternative states of nature based around
# modeling natural mortality. 'Mgmt' refers to the two management scenarios (A) the default harvest control rule
# $P^* = 0.45$, and (B) harvest control rule with a lower $P^* = 0.40$. Catch (in mt) is from the projections from the
# base model for each management scenario, and is applied to each state of nature. Catches in 2025
# and 2026 are fixed at the ACLs and have been set for that year with values provided by the GMT. The alternative
# states of nature ('Low', 'Base', and 'High') are provided in the columns, and assume natural mortality is fixed either
# a low value (Low M; low state), or a high value (High M; high state). Spawning output ('Spawn', in billions of eggs) and
# fraction of unfished ('Frac') is provided for each state of nature."

# tab <- table_decision(
#   caption = caption,
#   label = "es-decision",
#   list(low45, base45, high45)
# )
# writeLines(tab,here('report', "tables", "decision_table_es.tex"))

# tab <- table_decision(
#   caption = caption,
#   label = "dec-tab",
#   list(low45, base45, high45),
#   list(low40, base40, high40)
# )
# writeLines(tab,here('report', "tables", "decision_table.tex"))

#####-------------------------------------------####-
#Decision Table Figure - Not in Report
#####-------------------------------------------####-
low45 <- SS_output(here('models', paste0(base_mod_name, "_low_M_pstar_",pstar)))
base45 <- SS_output(here('models', base_mod_name))
high45 <- SS_output(here('models', paste0(base_mod_name, "_high_M_pstar_",pstar)))

xx <- SSgetoutput(dirvec = glue::glue("{models}/{subdir}", models = here('models'),
                                      subdir = c(base_mod_name,
                                                 file.path(paste0(base_mod_name, "_low_M_pstar_",pstar)),
                                                 file.path(paste0(base_mod_name, "_high_M_pstar_",pstar)))))
SSsummarize(xx) |>
  SSplotComparisons(legendlabels = c('Base (M = 0.068)',
                                     'Low state of nature (M = 0.0525)',
                                     'High state of nature (M = 0.08)'),
                    subplots = c(1, 3, 9, 11, 18, 2, 4), print = TRUE, 
                    legendloc = "topright", endyrvec = 2036,
                    plotdir = here('models', paste0(base_mod_name, "_high_M_pstar_",pstar)),
                    labels = c(
                      "Year", # 1
                      "Spawning biomass (mt)", # 2
                      "Fraction of unfished spawning biomass", # 3 automatically updated
                      "Age-0 recruits (1,000s)", # 4
                      "Recruitment deviations", # 5
                      "Index", # 6
                      "Log index", # 7
                      "SPR-related quantity", # 8 automatically updated when consistent
                      "Density", # 9
                      "Management target", # 10
                      "Minimum stock size threshold", # 11
                      "Spawning output", # 12 automatically updated when consistent
                      "Harvest rate", # 13
                      "Summary biomass (mt)", # 14
                      "Age X+ biomass (mt)" # 15
                    ))







