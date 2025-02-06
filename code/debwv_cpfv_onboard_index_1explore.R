################################################################################
# Contains SQL connections to read Deb's onboard data
#                         2025 assessment of CA quillback rockfish
#	
# Written by Melissa Monk
# Note - only Monterey samples in 1987
################################################################################

rm(list = ls(all = TRUE))
graphics.off()
library(here)
library(ggplot2)
library(dplyr)
library(DBI)
library(here)
library(glue)

dir <- file.path(here(), "data-raw", "rec_indices", "debwv_cpfv_onboard")
setwd(dir)

# connect to all of the databases with windows authentication
db_driver <- "SQL Server"
swc_server <- "SWC-Estrella-S"

# DebWV database connection
con_debwv <- DBI::dbConnect(odbc::odbc(),
                            driver = db_driver,
                            server = swc_server,
                            database = "CDFW_CPFV_Onboard_1987_1998",
                            Trusted_Connection = "yes"
)


#Get the SQL queries for deb's data - had to remove comments for R - they are in the SQL script
query_debwv <- glue::glue_sql(
  "SELECT LOCATION_TRUNC.TRIP_ID AS ASSN,			
		   LOCATION_TRUNC.FSHNG_STOP AS LOCNUM,		
		   CNTY,								
		   DISTRICT,																								
		   MAX_OBSANG,
		   MIN_OBSANG,
		   AVG_OBSANG,
		   SUM_FISHTIME,
		   SUM_ANGHRS,
		   BOAT.percent_groundfish,
		   YEAR(TRPDATE) AS YEAR,					
		   MONTH(TRPDATE) AS MONTH,					
     		   DEPTH =   CASE					
					 WHEN AVG_MINDEPTHfm IS NOT NULL AND AVG_MAXDEPTHfm IS NOT NULL				
					   THEN ((AVG_MINDEPTHfm+AVG_MAXDEPTHfm)/2)
					 WHEN AVG_MINDEPTHfm IS NULL AND AVG_MAXDEPTHfm IS NOT NULL
					   THEN   AVG_MAXDEPTHfm
					 WHEN AVG_MINDEPTHfm IS NOT NULL AND AVG_MAXDEPTHfm IS  NULL
			 			THEN AVG_MINDEPTHfm
					 WHEN AVG_MINDEPTHfm IS NULL AND AVG_MAXDEPTHfm IS NULL
			 			THEN ((luLOC_CODE.MINFMS)+(luLOC_CODE.MAXFMS))/2
			 			END,
			  DEPTHTYPE = CASE 
					 WHEN AVG_MINDEPTHfm IS NOT NULL AND AVG_MAXDEPTHfm IS NOT NULL 
					   THEN 'AVERAGE'
					 WHEN AVG_MINDEPTHfm IS NULL AND AVG_MAXDEPTHfm IS NOT NULL
					   THEN  'MAXDEPTHfm'
					 WHEN AVG_MINDEPTHfm IS NOT NULL AND AVG_MAXDEPTHfm IS  NULL
			 			THEN 'MINDEPTHfm'
					 WHEN AVG_MINDEPTHfm IS NULL AND AVG_MAXDEPTHfm IS NULL
			 			THEN 'LOC_CODE'
			 			END, 	
			 	
		   REEFID,							
		   SUM(case when CDFGSP = 2326 then NUMENC else 0 end) as NUMENC,  
		   SUM(case when CDFGSP = 2326 then KEPT else 0 end) as KEPT,     
           SUM(case when CDFGSP = 2326 then DISCD else 0 end) as DISCD     


	FROM LOCATION_TRUNC  
		   INNER JOIN BOAT		 ON LOCATION_TRUNC.Trip_ID = BOAT.Trip_ID
		   INNER JOIN LUPORT	 ON BOAT.PORT = LUPORT.PORT
		   INNER JOIN LULOC_CODE ON LULOC_CODE.LOC_CODE = LOCATION_TRUNC.LOC_CODE
		   INNER JOIN LUREEF	 ON LOCATION_TRUNC.LOC_CODE = LUREEF.LOC_CODE
		   LEFT  JOIN CATCHES	 ON LOCATION_TRUNC.Trip_ID = CATCHES.Trip_ID 
									and LOCATION_TRUNC.FSHNG_STOP = CATCHES.FSHNG_STOP
		  
	WHERE 
			  FSHNGTYP_GFISH>0						
		  and SUM_FISHTIME is not NULL			
		  and AVG_OBSANG is not NULL			
		  and LOCATION_TRUNC.LOC_CODE is not NULL	
		  and ReefID is not NULL				
	 
			
    GROUP BY		
			  LOCATION_TRUNC.TRIP_ID,
			  LOCATION_TRUNC.FSHNG_STOP, 
			  CNTY,
			  AVG_MINDEPTHfm,
			  AVG_MAXDEPTHfm, 
			  SUM_FISHTIME, 
			  TRPDATE, 
 			  BOAT.percent_groundfish,
 			  REEFID,
 			  DISTRICT,
 			  MINFMS,
			  MAXFMS, 
			  MAX_OBSANG,
			  MIN_OBSANG,
			  AVG_OBSANG,
			  SUM_ANGHRS


   ORDER BY NUMENC desc ", 
  .con = con_debwv
)

debwv <- dbGetQuery(con_debwv, query_debwv)
dbDisconnect(con_debwv)

#Look at how many quillback rockfish retained and discarded
debwv %>%
  group_by(YEAR) %>%
  summarise(kept = sum(KEPT),
            discard = sum(DISCD)) 
			
			
debwv <- debwv	%>%
mutate(FISHTIME = SUM_FISHTIME,
ANGHRS  = SUM_ANGHRS)
#very few discards so keep them

#look at the percent of trips and groundfish
ggplot(debwv %>% filter(NUMENC>0), aes(percent_groundfish, NUMENC)) + 
  geom_point() + xlab("Percent of trip catch groundfish") +
  ylab("Number of quillback encountered")
ggsave(file.path(getwd(),"percent_gfish.png"))


save(debwv, file = "debwv_quillback_data.R")
