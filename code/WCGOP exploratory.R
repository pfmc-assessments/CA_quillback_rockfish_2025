## Data: WCGOP and EM catch and composition data

# Read in entire WCGOP catch and biological data to reduce to quillback only
# as well as to figure out total domain of samples to figure out where quillback
# were not caught

library(dplyr)
library(here)

load(here("WCGOP/CONFIDENTIAL_EMLogbook_Catch_Data_2022.Rdat"))
catchEM <- EMCatch
load(here("WCGOP/CONFIDENTIAL_Observer_Catch_Data_2002_2022.Rdat"))
catch <- OBCatch
load(here("WCGOP/CONFIDENTIAL_Observer_Biological_Data_2002_2022.Rdat"))
bio <- OBBio2

#Subset to quillback
quill_catch <- catch[grep("quillback|Quillback",catch$spc.name),]
quill_catch_sci <- catch[grep("maliger",catch$sci.name),]

quill_catchEM <- catchEM[grep("quillback|Quillback",catchEM$spc.name),]
quill_catchEM_sci <- catchEM[grep("maliger",catchEM$sci.name),]

quill_bio <- bio[grep("quillback|Quillback",bio$species),]
quill_bio_sci <- bio[grep("maliger",bio$scientific_name),]

#Group by
quill_catch %>%
  group_by(YEAR, sector, D_STATE) %>%
  summarise(tot = sum(RET_LBS, na.rm = TRUE), dis = sum(DIS_LBS, na.rm = TRUE))

quill_catchEM %>%
  group_by(YEAR, sector, D_STATE) %>%
  summarise(tot = sum(RET), dis = sum(DIS))

quill_bio %>%
  group_by(DYEAR, FISHERY, GEAR_TYPE, D_STATE) %>%
  summarise(tot = sum(CATCH_WEIGHT, na.rm = FALSE), dis = sum(TOTAL_CATCH_DISCARD_SAMPLE_LBS))

