#############################################################################
## DebVW index of abundance for quillback rockfish for the 2025 assessment
## Data pulled from Deb's database  in the explore file
## Melissa Monk
##        Note - only Monterey sampled in 1987
# USING NUMBER ENCOUNTERED AND NOT KEPT ONLY
#############################################################################

rm(list=ls(all=TRUE))
graphics.off()

#libraries
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(here)
library(glue)

dir <- file.path(here(), "data-raw", "rec_indices", "debwv_cpfv_onboard")
setwd(dir)
out.dir <- file.path(here(),"data_explore_figs", "rec_indices","debwv_cpfv_onboard")
#load data
load("debwv_quillback_data.R")
#####################################################################################
#Data filter dataframe 
data_filters <- data.frame(
  matrix(vector(), 12, 4,
         dimnames=list(c(), c("Filter", "Description",
                             "Samples", "Positive_Samples"))),
                          stringsAsFactors=F)
data_filters$Filter[1:2] = c("All")
filter.num = 1
###################################################################################
#quillbacks

quill <- subset(debwv, NUMENC>0)
dim(quill)
#282 observations
sum(quill$NUMENC)
#594 total quillback retained

with(quill, table(REEFID, DISTRICT))
with(quill, table(DISTRICT, YEAR))
with(quill, table(YEAR))
with(quill, table(DISTRICT))/with(debwv, table(DISTRICT))

#look at the percent positive by year and district
with(quill, table(YEAR, DISTRICT))/with(debwv, table(YEAR, DISTRICT))

#Add initial values to dataframe of sample sizes
#Add to filter dataframe
data_filters$Samples[filter.num] = dim(debwv)[1]
data_filters$Positive_Samples[filter.num] = dim(subset(debwv, NUMENC>0))[1]
data_filters$Filter[filter.num] = "All"
data_filters$Description[filter.num] = 'None'
filter.num = filter.num + 1

#Remove numenc  = null
debwv <- subset(debwv, NUMENC!='NULL')

##########################################################################################################
#Add to filter dataframe
data_filters$Samples[filter.num] = dim(debwv)[1]
data_filters$Positive_Samples[filter.num] = dim(subset(debwv, NUMENC>0))[1]
data_filters$Filter[filter.num] = "No catch"
data_filters$Description[filter.num] = 'Remove no catch trips'
filter.num = filter.num + 1
#########################################################################################################

##############################Remove 1987 (only sampled Monterey Bay)
#Remove 1987 and also depth <80 fm
debwv  <- debwv %>%
  filter(YEAR>1987) %>%
  filter(DEPTH<80)

#########################################################################################################

#Add to filter dataframe
data_filters$Samples[filter.num] = dim(debwv)[1]
data_filters$Positive_Samples[filter.num] = dim(subset(debwv, NUMENC>0))[1]
data_filters$Filter[filter.num] = "Only sampled Monterey"
data_filters$Description[filter.num] = 'Remove 1987 and depths >80fm'
filter.num = filter.num + 1
#########################################################################################################


#mutate columns to numeric
debwv <- debwv %>% mutate_at(c('KEPT', 'DISCD', 'NUMENC','REEFID'), as.numeric)
dim(subset(debwv, NUMENC>0))

#Read in SuperReef info - assignments for quillback, but see how they match
#reconsider for quillback and look at the REEFID - so few have quillback
reef_info <- read.csv('SuperReefs.csv',header=T)
reef_info_needed <- reef_info %>% 
                  dplyr::select(ReefID,QLBKDebRe, Area, Cnty_Major)  %>%
                  rename(REEFID=ReefID,SuperReef = QLBKDebRe) %>%
                  group_by(REEFID, SuperReef) %>%
                  summarise(tot_Area = sum(Area)) 
#Join super reefs to main data
debwv <- dplyr::left_join(debwv, reef_info_needed, by = 'REEFID')


#look at distributions of NUMENC over reefs
qlbk_reefs <- debwv %>% filter(NUMENC>0) %>% group_by(REEFID) %>% summarise(NUMENC = sum(NUMENC))


#assign a column for presence
debwv<- debwv %>%  mutate(spp_present = 
                        case_when(NUMENC>0 ~ T,
                                  TRUE ~ F))

#Add a column for wave - bimonthly sampling
debwv <- debwv %>%
  mutate(WAVE = case_when(MONTH %in% c(1,2) ~ 1,
                          MONTH %in% c(3,4) ~ 2,
                          MONTH %in% c(5,6) ~ 3,
                          MONTH %in% c(7,8) ~ 4,
                          MONTH %in% c(9,10) ~ 5,
                          MONTH %in% c(11,12) ~ 6)) %>%
  mutate_at(vars(WAVE), as.factor)

#Create catch per angler house
debwv <- debwv %>%
  mutate(CPUE = NUMENC/(AVG_OBSANG*(FISHTIME/70)))
summary(debwv$CPUE)

#make a copy of the original to reference
dat <- debwv
###################################################
#Look at district
dat %>% 
  group_by(DISTRICT) %>%
  count(spp_present)

with(dat, table(YEAR,DISTRICT))  
#District 5 and 6 not too much data


##############################FISHTIME
summary(dat$FISHTIME)
fishtime_quantile <- quantile(dat$FISHTIME, seq(0,1,.025))
fishtime_quantile

png(filename = paste0(out.dir,'/Histogram of time fished.png'), width = 6, height = 6, units = "in", res = 600)
hist(dat$FISHTIME, breaks=50)
dev.off()
#A lot of really small fish times, but I checked the data sheet
#logically anything less than ~7 minutes isn't a representative drift
#Not removing the upper quantile bc it can be a combo of a number of drifts
#Removes 5% of the data
dat <- dat %>%
      filter(FISHTIME >= 6,
      FISHTIME<=218) #, FISHTIME < fishtime_quantile[40])

#########################################################################################################
#Add to filter dataframe
data_filters$Samples[filter.num] = dim(dat)[1]
data_filters$Positive_Samples[filter.num] = dim(subset(dat, NUMENC>0))[1]
data_filters$Filter[filter.num] = "Time fished"
data_filters$Description[filter.num] = 'Remove upper and lower 2.5% of time fished; keep 6-218 minutes'
filter.num = filter.num + 1
#########################################################################################################

summary(dat$AVG_OBSANG)
OBSANG_quantile <- quantile(dat$AVG_OBSANG, seq(0,1,.025))
OBSANG_quantile

png(filename = paste0(out.dir,'/Histogram of observed anglers.png'), 
width = 6, height = 6, units = "in", res = 600)
hist(dat$AVG_OBSANG, breaks=50)
dev.off()

dat <- dat %>%
      filter(AVG_OBSANG >= 4,
      AVG_OBSANG <= 15) #, FISHTIME < fishtime_quantile[40])

#########################################################################################################
#Add to filter dataframe
data_filters$Samples[filter.num] = dim(dat)[1]
data_filters$Positive_Samples[filter.num] = dim(subset(dat, NUMENC>0))[1]
data_filters$Filter[filter.num] = "Observed anglers"
data_filters$Description[filter.num] = 'Remove upper and lower 2.5% of observed anglers; keep 4-15'
filter.num = filter.num + 1
#########################################################################################################




###############################DEPTH
#Look at depths and create depth bins
#BATHY DATA IN METERS, OBSERVER DATA IN FEET, MANAGEMENT IN FATHOMS 
#Convert feet to fathoms for the analyses


#Look at depth by SuperReef
#png(filename = paste0(out.dir,'/Histogram of depth (fm) by reef.png'), width = 6, height = 6, units = "in", res = 600)
ggplot(dat %>% filter(NUMENC>0), aes(DEPTH, fill=SuperReef)) +
  geom_histogram() + xlab("Depth (fm)") +ylab("Number of drifts")
ggsave(file.path(out.dir,"depthfm_byreef_pos.png"), width = 7, height = 7)

#Look at positive depth
dat %>% filter(NUMENC>0) %>% do(data.frame(quantile(.$DEPTH, seq(0,1,.025))))
#Look at depths with no quillback
dat  %>% filter(NUMENC==0) %>% do(data.frame(quantile(.$DEPTH, seq(0,1,.025))))

#REMOVE anything deeper than  meters (~60 fathoms)
 dat <- dat %>%
   filter(DEPTH<=50, DEPTH >= 10) 
# ########################################################################################################
#Add to filter dataframe
data_filters$Samples[filter.num] = dim(dat)[1]
data_filters$Positive_Samples[filter.num] = dim(subset(dat, NUMENC>0))[1]
data_filters$Filter[filter.num] = "Depth"
data_filters$Description[filter.num] = 'Retain drifts between 10-50 fm'
filter.num = filter.num + 1
########################################################################################################
#percent groundfish filter
qlbk <- dat %>% filter(NUMENC > 0)
quantile(qlbk$percent_groundfish, seq(0,1,.025), na.rm=T)
pgfish <- quantile(dat$percent_groundfish, seq(0,1,.025), na.rm=T)

dat <- dat %>% filter(percent_groundfish>= pgfish[[2]])
# ########################################################################################################
#Add to filter dataframe
data_filters$Samples[filter.num] = dim(dat)[1]
data_filters$Positive_Samples[filter.num] = dim(subset(dat, NUMENC>0))[1]
data_filters$Filter[filter.num] = "Target"
data_filters$Description[filter.num] = 'Retain trips with at least 77.5% groundfish catch (97.5% of trips)'
filter.num = filter.num + 1
########################################################################################################


#png(filename = paste0(out.dir,'/Histogram of depth fathoms after filter.png'), width = 6, height = 6, units = "in", res = 600)
hist(dat$DEPTH)
#dev.off()

ggplot(dat, aes(x=SuperReef, y = DEPTH, colour = SuperReef)) + 
geom_boxplot() +ylab("Depth (fm)")
ggsave(file = file.path(getwd(), "depth_by_reef.png"), width = 7, height = 7)




dat = dat %>%
      mutate(DEPTH_bin = cut(DEPTH,
                       breaks = c(0,20,30,40,50))) %>%
           mutate_at(vars(DEPTH_bin), as.factor)
           
           

#Look at distribution of data by depth bins
with(dat, table(spp_present,DEPTH_bin))

#dat = droplevels(dat)


###############################################################
###################################################
#Look at reefs and species presence
with(dat, table(spp_present, SuperReef))

#Look at reefs and species presence
with(dat, table(spp_present, DISTRICT))

#Find reefs that were fished, and encountered qlbk
pos_qlbk_reefs <- dat %>% filter(NUMENC>0) %>% dplyr::select(REEFID) %>% unique()
reefs_fished <- dat %>% dplyr::select(REEFID) %>% unique()
#find reefs never fished
reef_fished_info <- reef_info_needed %>%
               mutate(reef_pos_qlbk = case_when(REEFID %in% pos_qlbk_reefs$REEFID ~ 'QLBK_present')) %>%
               mutate(reef_fished = case_when(REEFID %in% reefs_fished$REEFID ~ 'Fished'))
View(reef_fished_info)

#Count how many years a reef was visited
reef_years <- dat %>%
              group_by(REEFID) %>%
              summarise(n_years = n_distinct(YEAR))

#Count how many drifts on a reef
reef_drifts <- dat %>%
              group_by(REEFID) %>%
              count(name="n_drifts")

#join sample sizes to the same data frame
with(dat, table(YEAR, REEFID))
with(dat %>% filter(NUMENC>0), table(YEAR, REEFID))
#x11();with(reef_sample_size,plot(n_years, n_drifts))

                        
#check for temporal coverage
with(dat, table(SuperReef))
with(dat, table(SuperReef,YEAR))
with(dat %>% filter(NUMENC>0), table(SuperReef,YEAR))



#check for temporal coverage of positives
pos <- subset(dat, NUMENC>0)
with(pos, table(SuperReef,YEAR))


###NEED TO MODIFY THIS BC now using reef ID
#Combine some of the super reefs
dat$SuperReef = as.factor(dat$SuperReef)
########################################################################################################
#SuperReef filter
#remove Moss Landing to Conception
reef.rm <- c("10_MossLanding_BigSur", "13_SLOCnty_Morro", "15_Morro_Conception")
dat <- dat %>% filter(!SuperReef %in% reef.rm)

#get the total area where quillback are present and look at 
#what fraction is in the farallons
#Find reefs that were fished, and encountered qlbk
pos_qlbk_reefs <- dat %>% filter(NUMENC>0) %>% dplyr::select(REEFID) %>% unique()
reefs_fished <- dat %>% dplyr::select(REEFID) %>% unique()
pos_reefs_dat <- reef_info_needed %>%
               mutate(reef_pos_qlbk = case_when(REEFID %in% pos_qlbk_reefs$REEFID ~ 'QLBK_present')) %>%
               mutate(reef_fished = case_when(REEFID %in% reefs_fished$REEFID ~ 'Fished')) %>%              
filter(!is.na(reef_pos_qlbk))

(31+56)/sum(pos_reefs_dat$tot_Area)
#At this point in the filtering, the Farallons contain 18% of the total available habitat


# ########################################################################################################
#Add to filter dataframe
data_filters$Samples[filter.num] = dim(dat)[1]
data_filters$Positive_Samples[filter.num] = dim(subset(dat, NUMENC>0))[1]
data_filters$Filter[filter.num] = "Target"
data_filters$Description[filter.num] = 'Remove trips south of Moss Landing'
filter.num = filter.num + 1
########################################################################################################

#Look at the infividual reefs left
with(dat, table(REEFID))
with(dat %>% filter(NUMENC>0), table(REEFID))

#remove reefs with at least 5  quillback
qlbk_reef_min <- dat %>% 
                filter(NUMENC>0) %>%
                group_by(REEFID) %>%
                summarise(ENC = sum(NUMENC))
qlbk_reef_keep <- qlbk_reef_min %>%
filter(ENC > 5)


########################################################################################################
#Finer scale filter of reefs
#Remove reefs with less than 5 quillback quillback

dat <- dat %>% filter(REEFID %in% qlbk_reef_keep$REEFID)
# ########################################################################################################

# ########################################################################################################
#Add to filter dataframe
data_filters$Samples[filter.num] = dim(dat)[1]
data_filters$Positive_Samples[filter.num] = dim(subset(dat, NUMENC>0))[1]
data_filters$Filter[filter.num] = "Target"
data_filters$Description[filter.num] = 'Remove reefs with fewer than 5 observed quillback'
filter.num = filter.num + 1
########################################################################################################

#look at what's left
with(dat, table(REEFID, YEAR))
with(dat %>% filter(NUMENC>0), table(REEFID, YEAR))
#5 is big reef off half moon bay
#26, 136 are Farallons
#40 and 49 are Mendocino, off Stillwell Point and Laguna Point near Ft Bragg

#Whittle down to just the three locations
dat <- dat %>%
filter(SuperReef %in% c("2_Farallons", "1_Mendocino", "3_HalfMoonBay")) %>% droplevels
with(dat %>% filter(NUMENC>0), table(SuperReef, YEAR))

dat$MegaReef <- dat$SuperReef

########################################################################################################
#Filter out 1990 and 1991
#Remove reefs with less than 5 quillback quillback
yr.rm = c(1990, 1991)
dat <- dat %>% filter(!YEAR %in% yr.rm)
# ########################################################################################################

# ########################################################################################################
#Add to filter dataframe
data_filters$Samples[filter.num] = dim(dat)[1]
data_filters$Positive_Samples[filter.num] = dim(subset(dat, NUMENC>0))[1]
data_filters$Filter[filter.num] = "Years"
data_filters$Description[filter.num] = 'Remove 1990 and 1991 with fewer than 20 samples each'
filter.num = filter.num + 1
########################################################################################################




with(subset(dat, NUMENC>0),table(MegaReef))/with(dat, table(MegaReef))
percent_pos_depth <- round(with(subset(dat, NUMENC>0),table(YEAR,MegaReef))/with(dat, table(YEAR,MegaReef)),2)
write.csv(percent_pos_depth, file = file.path(getwd(), "percent_pos_depth.csv"), 
          row.names = FALSE)
#plot CPUE by year again with combined reef
CPUE_reef_year <- dat %>%
  group_by(MegaReef,YEAR) %>%
  summarise(mean_CPUE = mean(CPUE))

ggplot(CPUE_reef_year, aes(x=YEAR, y = mean_CPUE, color = MegaReef, group=MegaReef)) +
  geom_point() + geom_line(linewidth = 1) + xlab("Year") + 
  ylab("Average CPUE") + scale_color_viridis_d()
ggsave(file = file.path(getwd(), "cpue_by_reef.png"), height = 7, width = 7)

#Look at depths again
round(with(subset(dat, NUMENC>0),table(YEAR,DEPTH_bin))/with(dat, table(YEAR,DEPTH_bin)),2)

#Join MegaReef to ReefInfo table
#MegaReef_info <- dat %>% dplyr::select(SuperReef,MegaReef) %>% unique()
#aa = left_join(reef_areas, reef_fished_info)
#bb = left_join(aa, reef_sample_size)
#QLBK_reef_info = left_join(bb, MegaReef_info)




#Arithmetic index
CPUE_year <-  dat %>% group_by(YEAR) %>% summarise(Avg_CPUE = mean(CPUE)) %>% as.data.frame()
print(CPUE_year)

#Plot the average cpue by year and reef
png(filename = paste0(out.dir,'/Average CPUE by Year and Region.png'), width = 6, height = 4, 
    units = "in", pointsize = 10, res=300)
with(dat, interaction.plot(YEAR, MegaReef, CPUE,
                           col=1:7, lty=1, lwd=2, ylab="CPAH", ylim=c(0,.2), 
                           legend=F))

dev.off()


#-------------------------------------------------------------------------------

ggplot(dat, aes(x=as.factor(MegaReef), y = DEPTH, fill = MegaReef)) + 
  geom_boxplot() +ylab("Depth (fm)") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank() 
  ) + scale_fill_viridis_d()
ggsave(file = file.path(getwd(), "depth_by_reef.png"), width = 7, height = 7)




#save.image(paste0(getwd(),'/Filtered_data_DebWV_onboard.RData'))




#################################################3
##additional looks at the data

pos <- subset(dat, NUMENC>0)
with(pos, table(REEFID))
with(pos, table(YEAR))
with(dat, table(REEFID))
with(dat, table(YEAR, MegaReef))
summary(dat$percent_groundfish)

 quantile(dat$percent_groundfish, seq(0,1,.1), na.rm=T)

pos_reefs <- pos %>%
dplyr::select(REEFID) %>%
unique()
dat1 <- subset(dat, REEFID %in% pos_reefs$REEFID)
pos <- subset(dat1, NUMENC>0)
with(pos, table(YEAR))
with(dat1, table(YEAR))
with(dat1, table(YEAR, MegaReef))
summary(dat1$percent_groundfish)


ggplot(dat %>% filter(NUMENC>0), aes(x = CPUE, y = FISHTIME)) + 
geom_point()


summary(dat$ANGHRS)
summary(dat$FISHTIME)
summary(dat$AVG_OBSANG)
dev.off()


save(dat, data_filters, file = file.path(dir,'QLBK_filtered_data.RData'))
