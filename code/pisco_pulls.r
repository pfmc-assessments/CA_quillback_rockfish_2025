################################################################################
# Contains SQL connections to read the onboard observer data through 2019
#                         copper rockfish assessment
#	
# Melissa Monk 4/8/23
################################################################################

rm(list = ls(all = TRUE))
graphics.off()
library(here)
library(ggplot2)
library(dplyr)
library(DBI)
library(here)
library(glue)

speciesNODC = 8826010108

dir <- file.path(here(), "data", "rec_indices", "crfs_cpfv_onboard")
setwd(dir)

# connect to all of the databases with windows authentication
db_driver <- "SQL Server"
swc_server <- "pinniger"

# onboard connection
con_onboard <- DBI::dbConnect(odbc::odbc(),
                                driver = db_driver,
                                server = swc_server,
                                database = "Monk_index_manuscript",
                                Trusted_Connection = "yes"
)


#Get the SQL queries for onboard catch and effort information
query_onboard_catch <- glue::glue_sql(
  "SELECT * from DriftCatch", 
  .con = con_onboard
)
query_onboard_effort <- glue::glue_sql(
  "SELECT * from DriftEffort_forR", 
  .con = con_onboard
)

onboard_catch <- dbGetQuery(con_onboard, query_onboard_catch)
onboard_effort <- dbGetQuery(con_onboard, query_onboard_effort)