#############################################################################
#  Look at the MRFSS PR data for the quillback rockfish assessment
#  Determine if an index of abundance could be created from these data
#
#  MRFSS files from the SWFSC Sept 2022
#   Melissa Monk 1/27/2025
#############################################################################

library(dplyr)
setwd("A:/RecFIN MRFSS Raw Tables/R workspace with tables")

#load a workspace with all mrfss data
load("MRFSS_TYPES 1_2_3_6_AND_ESTIMATES.RData")

#mrfss angler interviews
with(MRFSS_ANG_INFO_TYPE1.df, table(MODE_F, MODE_FX))

#filter the type1 data to just california, pr mode
ca_type1_modef8 <- MRFSS_ANG_INFO_TYPE1.df %>%
        filter(MODE_F == 8, ST == 6)
with(ca_type1_modef8, table(CNTY))
with(ca_type1_modef8, table(SUB_REG, CNTY))
#see why there are nulls in num_typ3


#looks like cuplicate ID_CODES so you can't just use that to
#assign a trip
aa <- ca_type1_modef8 %>%
 filter(is.na(NUM_TYP3))

#test to see if any of these are in the observed catch
#there are, but guessing it's one of the duplicates or error
bb <- MRFSS_SAMP_EXAM_CATCH.df %>%
filter(ID_CODE %in% aa$ID_CODE)

#In the type 1 data
# use sub region as a filter
#1 = southern ca, 2 = northern ca
ca_type1_modef8 <- ca_type1_modef8 %>%
filter(SUB_REG == 2)

#southern CA counties still remain
#remove southern CA counties + SLO
socal <- c(73, 59, 37, 111, 83, 79)
ca_type1_modef8 <- ca_type1_modef8 %>%
filter(!CNTY %in% socal)


#get the unique columns to merge into sampler examined catch
# ID_CODE, PRIM1, MODE_F, AREA_X, AREA, DIST, TIME_,ID_CODE
# CNTY, GEAR, HRSF, CNTRBTS, NUM_TYP3

ca_typ1_unique <- ca_type1_modef8 %>%
dplyr::select(c(ID_CODE, PRIM1, MODE_F, AREA_X, 
                AREA, DIST, TIME_,ID_CODE,
                CNTY, GEAR, HRSF, CNTRBTRS, NUM_TYP3)) %>%
            unique()

#look at gears
with(ca_typ1_unique, table(GEAR))

#limit to hook and line
ca_typ1_unique <- ca_typ1_unique %>%
                    filter(GEAR == 1)

# look at num_typ3
summary(ca_typ1_unique$NUM_TYP3)

#remove the NA and 0's and limit to just ocean areas fished
ca_typ1_unique <- ca_typ1_unique %>%
 filter(!is.na(NUM_TYP3), 
         NUM_TYP3 >0,
         AREA_X %in% c(1,2))
###########################################################################
#look at the sampler examined catch
ca_pr_exam_catch <- MRFSS_SAMP_EXAM_CATCH.df %>%
filter(ST == 6, MODE_F == 8, SUB_REG == 2, AREA_X %in% c(1,2))

#check counties
summary(as.factor(ca_pr_exam_catch$CNTY))

#remove the rest of socal
ca_pr_exam_catch <- ca_pr_exam_catch %>%
filter(!CNTY %in% socal)

#take a look at quillback catches
qlbk <- ca_pr_exam_catch %>%
filter(SP_CODE == 8826010120)
#quillback 8826010120
summary(as.factor(qlbk$CNTY))
summary(as.factor(ca_pr_exam_catch$CNTY))
summary(as.factor(qlbk$AREA_X))


# merge ca_typ1_unique with the sampler examined catch
test <- inner_join(ca_pr_exam_catch, ca_typ1_unique)
#try and remove columns with just NA - not as many as I'd hoped
test <- test[,colSums(is.na(test))<nrow(test)]

#salmon trips?
salmon <- test %>% filter(SALMON==1)
qlbk_salmon <- salmon %>%
filter(SP_CODE == 8826010120)
#only 36 trips

