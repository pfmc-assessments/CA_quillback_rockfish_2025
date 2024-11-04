################################################################################
# Contains SQL connections to read the PISCO data for the 
#                        2025 quillback rockfish assessment
#	
# Melissa Monk 9/3/2024
################################################################################

rm(list = ls(all = TRUE))
graphics.off()
library(here)
library(ggplot2)
library(dplyr)
library(DBI)
library(here)
library(glue)



dir <- file.path("C:/users/melissa.monk/documents/stock_assessments/CA_quillback_2025")
setwd(dir)

# connect to all of the databases with windows authentication
db_driver <- "SQL Server"
swc_server <- "SWC-Estrella-S"

# onboard connection
con_pisco <- DBI::dbConnect(odbc::odbc(),
                                driver = db_driver,
                                server = swc_server,
                                database = "PISCO_Surveys",
                                Trusted_Connection = "yes"
)


#Get the SQL queries for onboard catch and effort information
query_pisco <- glue::glue_sql(
  "SELECT *
  FROM Transects
  inner join FishCounts on Transects.TransectID=FishCounts.TransectID
  where classcode = 'smal'
  order by method, year", 
  .con = con_onboard
)
query_pisco_lengths <- glue::glue_sql(
  "SELECT * 
  FROM FishLengths  
  where classcode = 'smal'
", 
  .con = con_onboard
)

pisco_catch <- dbGetQuery(con_pisco, query_pisco)
pisco_lengths <- dbGetQuery(con_pisco, query_pisco_lengths)


save(pisco_catch, pisco_lengths, file = "pisco_pulls.RData")
