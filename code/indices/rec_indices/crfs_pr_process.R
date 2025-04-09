#########################################################################################
### Process the CDFW PR data for an index of abundance incuding a Stephens-MacCall Filter
### Removing filter on species targets before the SM filter
### and removing sites with <=5 quillback
### Quillback assessment 2025
### Julia Coates adapted from Melissa Monk
#########################################################################################
rm(list = ls(all = TRUE))
graphics.off()
library(RColorBrewer)
library(dplyr)
library(tidyr)
library(ggplot2)
library(here)
library(glue)

#species and area identifiers - eventually put in function
pacfinSpecies <- 'RFQIL'
speciesName <- "quillback"
out.dir <- here("data-raw", "PRData")

#load data for processing
load(here("data-raw", "PRData", "all_pr_data_SM.RData"))
all_pr_data <- all_pr_wide
all_pr_data <- relocate(.data=all_pr_data, target, .before=1)

# Data filter dataframe
filter.num <- 1
dataFilters <- data.frame(matrix(vector(), 10, 4,
                                 dimnames = list(c(), c(
                                   "Filter", "Description", "Samples",
                                   "Positive_Samples"
                                 ))), stringsAsFactors = F)
#-------------------------------------------------------------------------------
#Filter to north of district 2
cdfwpr <- all_pr_data %>% 
  filter(district > 2) %>%
  mutate(cpue = target/anglers)



# bag limit exploration---------------------------------------------------------
#look at total sample sizes by year to see if there is a post-covid difference
cdfwpr_samplesize <- all_pr_data %>%
  filter(district > 2) %>%
  group_by(year) %>%
  summarize(trips = n_distinct(ID))
write.csv(cdfwpr_samplesize, file=file.path(here("data-raw", "PRData", "pr_trip_sample_size_by_year.csv")))


#----cpue over time----- 
# Median of trips with at least one quillback
q = c(.25, .5, .75)
cpue_year <- cdfwpr %>%
filter(target>0) %>%
group_by(year) %>%
 summarize(
  min = min(cpue),
  quant25 = quantile(cpue, probs = q[1]), 
            quant50 = quantile(cpue, probs = q[2], na.rm=T),
            quant75 = quantile(cpue, probs = q[3]),
            max = max(cpue))
cpue_year

# Median of all trips
cpue_year2 <- cdfwpr %>%
#  filter(target>0) %>%
  group_by(year) %>%
  summarize(
    min = min(cpue),
    quant25 = quantile(cpue, probs = q[1]), 
    quant50 = quantile(cpue, probs = q[2], na.rm=T),
    quant75 = quantile(cpue, probs = q[3]),
    max = max(cpue))
cpue_year2  # So few that medians and quantiles are 0

# Mean of trips with at least one quillback
cpue_year3 <- cdfwpr %>%
  filter(target>0) %>%
  group_by(year) %>%
  summarize(
    min = min(cpue),
    quant25 = quantile(cpue, probs = q[1]), 
    mean = mean(cpue, na.rm=T),
    sd = sd(cpue),
    quant75 = quantile(cpue, probs = q[3]),
    max = max(cpue))
cpue_year3

write.csv(cpue_year, file=file.path("data-raw", "PRData","observed_cpue_by_year.csv"),
row.names = FALSE)

ggplot(cpue_year3, aes(x=year, y=mean)) +
  geom_line() +
  geom_ribbon(aes(ymin=mean - sd, ymax=mean + sd), alpha=0.5) +
  ylab("mean cpue")

#histogram of the number of fish per bag on trips catching quillback

baglim <- cdfwpr %>% filter(target>0)

hist(baglim$cpue, xlab="cpue bin", main="Histogram of Quillback CPUE, PR1 Observed")

ggplot(baglim, aes(x=year, y=cpue)) + 
  geom_point(alpha=1/10)

ggplot(baglim, aes(x=year, y=cpue)) +
  geom_boxplot(aes(group=year))

#-------------------------------------------------------------------------------
# Add to filter dataframe
dataFilters$Filter[filter.num] <- c("District")
dataFilters$Description[filter.num] <- c("District > 2")
dataFilters$Samples[filter.num] <- length(unique(cdfwpr$ID))
dataFilters$Positive_Samples[filter.num] <- cdfwpr %>% filter(target>0) %>% n_distinct("ID")
filter.num <- filter.num + 1
#-------------------------------------------------------------------------------

#Get the number of available samples by distrcit and year
samples_year_district <- cdfwpr %>%
group_by(year, district) %>%
summarize(n = n_distinct(ID)) %>%
pivot_wider(names_from=district, values_from = n)

#Remove 2020 due to covid and 2023 due to rule change
cdfwpr <- cdfwpr %>%
filter(!year %in% c(2020, 2023)) 

#-------------------------------------------------------------------------------
# Add to filter dataframe
dataFilters$Filter[filter.num] <- c("Year")
dataFilters$Description[filter.num] <- c("Remove 2020 due to COVID & 2023 due to rule change")
dataFilters$Samples[filter.num] <- length(unique(cdfwpr$ID))
dataFilters$Positive_Samples[filter.num] <- cdfwpr %>% filter(target>0) %>% n_distinct("ID")
filter.num <- filter.num + 1
#-------------------------------------------------------------------------------

# Look at positive samples by site and county
target <- cdfwpr %>% filter(target>0)
unique(cdfwpr$site)
unique(cdfwpr$county)

target %>%
  group_by(year, site) %>%
  summarize(n = n_distinct(ID)) %>%
  pivot_wider(names_from=site, values_from = n)

# Sites with the fewest positive samples are 101 (Avila), 104 (Moss Landing), 105, and 107 (Monterey)

target %>%
  group_by(year, county) %>%
  summarize(n = n_distinct(ID)) %>%
  pivot_wider(names_from=county, values_from = n)

# Create a county-site combo field
cdfwpr$county_site <- paste(cdfwpr$county, cdfwpr$site, sep="_")

target <- cdfwpr %>% filter(target>0)

county_site <- target %>%
  group_by(year, county_site) %>%
  summarize(n = n_distinct(ID)) %>%
  pivot_wider(names_from=county_site, values_from = n)

# Among sites with positive observations, the fewest quillback have occured in SLO_Avila, SLO_Morro Bay, Marin_Loch Lomond
# How about among all sites?
county_site_all <- cdfwpr %>%
  group_by(county_site) %>%
  summarise(pos_samp = sum(target>0))

county_site_5less <- filter(county_site_all, pos_samp < 6)
county_site_5less <- as.vector(county_site_5less$county_site)

target %>%
  group_by(year, district) %>%
  summarize(n = n_distinct(ID)) %>%
  pivot_wider(names_from=district, values_from = n)

# Try removing Avila and Morro and combining remaining district 3 sites in with district 4
cdfwpr <- cdfwpr %>% filter(! county_site %in% c("79_101", "79_100"))
cdfwpr$district_new <- cdfwpr$district
cdfwpr$district_new[cdfwpr$district_new==3] <- 4

# Try removing sites with <=5 quillback and combining remaining district 3 sites in with district 4
cdfwpr <- cdfwpr %>% filter(! county_site %in% county_site_5less)
cdfwpr$district_new <- cdfwpr$district
cdfwpr$district_new[cdfwpr$district_new==3] <- 4

#-------------------------------------------------------------------------------
# Add to filter dataframe
dataFilters$Filter[filter.num] <- c("Interview Site")
#dataFilters$Description[filter.num] <- c("Remove Avila & Morro Bay, Add Remaining SLO County Sites to District 4")
dataFilters$Description[filter.num] <- c("Remove Sites <=5 Quillback, Add Remaining SLO County Sites to District 4")
dataFilters$Samples[filter.num] <- length(unique(cdfwpr$ID))
dataFilters$Positive_Samples[filter.num] <- cdfwpr %>% filter(target>0) %>% n_distinct("ID")
filter.num <- filter.num + 1
#-------------------------------------------------------------------------------

#Look at where the target is found within the possible remaining data
target <- cdfwpr %>% filter(target>0)
summary(as.factor(cdfwpr$area_x))
summary(as.factor(target$area_x))
#remove mexico, na, and bays and harbors
area.to.remove <- c("Bay/estuary/harbor")

cdfwpr <- cdfwpr %>%
  filter(!area_x %in% area.to.remove,
         !is.na(area_x))
#Look at where the target is found within the possible remaining data
target <- cdfwpr %>% filter(target>0)
summary(as.factor(cdfwpr$area_x))
summary(as.factor(target$area_x))
#-------------------------------------------------------------------------------
# Add to filter dataframe
dataFilters$Filter[filter.num] <- c("Areas fished")
dataFilters$Description[filter.num] <- c("Retain trips occuring in ocean areas")
dataFilters$Samples[filter.num] <- length(unique(cdfwpr$ID))
dataFilters$Positive_Samples[filter.num] <- cdfwpr %>% filter(target>0) %>% n_distinct("ID")
filter.num <- filter.num + 1
#-------------------------------------------------------------------------------
#remove uncommon gears and then look at the target
gears <- cdfwpr %>%
  group_by(geara) %>%
  tally() %>%
  filter(n>100)

#remove uncommon gears
cdfwpr <- cdfwpr %>%
  filter(geara %in% gears$geara)

#new target table
target <- cdfwpr %>% filter(target>0)
summary(as.factor(target$geara))
summary(as.factor(cdfwpr$geara))
#1 = Hook & line
#2 = Dip net
#3 = Cast net
#4 = Gill net
#5 = Seine
#6 = Trawl
#7 = Trap
#8 = Spear/spear gun
#9 = Hand
#10 = Other
#Blank = no gear recorded or not the first species and location in a PR1 sample


#keep only hook and line - explore keeping troll as well
gears.to.keep <- c('H','Hook and Line', 'T', 'Troll')
cdfwpr <- cdfwpr %>%
  filter(geara %in% gears.to.keep) %>%
  mutate(geara = case_when(geara == "H" ~ "Hook and Line",
                           geara == "T" ~ "Troll",
                           TRUE ~ geara))

target <- cdfwpr %>% filter(target>0)
summary(as.factor(target$geara))
summary(as.factor(cdfwpr$geara))
#-------------------------------------------------------------------------------
# Add to filter dataframe
dataFilters$Filter[filter.num] <- c("Gear")
dataFilters$Description[filter.num] <- c("Retain trips with primary gear of hook-and-line or troll")
dataFilters$Samples[filter.num] <- length(unique(cdfwpr$ID))
dataFilters$Positive_Samples[filter.num] <- cdfwpr %>% filter(target>0) %>% n_distinct("ID")
filter.num <- filter.num + 1
#-------------------------------------------------------------------------------

#number of anglers
summary(cdfwpr$anglers)
#seems reasonable

#months
summary(as.factor(cdfwpr$month))
target <- cdfwpr %>% filter(target>0)
summary(as.factor(target$month))
#remove Jan-March in the north - no rockfishing these months

cdfwpr <- cdfwpr %>% filter(month > 3)

#-------------------------------------------------------------------------------
# Add to filter dataframe
dataFilters$Filter[filter.num] <- c("Months fished")
dataFilters$Description[filter.num] <- c("Remove Jan-March; recreational rockfish fishery closed")
dataFilters$Samples[filter.num] <- length(unique(cdfwpr$ID))
dataFilters$Positive_Samples[filter.num] <- cdfwpr %>% filter(target>0) %>% n_distinct("ID")
filter.num <- filter.num + 1
#-------------------------------------------------------------------------------

library(stringr)
cdfwpr$prim1Common <- str_trim(cdfwpr$prim1Common, "right")

# Look at positive samples by district
pos_samples_year_district <- cdfwpr %>%
  filter(target>0) %>%
  group_by(year, district_new) %>%
  summarize(n = n_distinct(ID)) %>%
  pivot_wider(names_from=district_new, values_from = n)
# All year/districts now have positive samples

#------------------------------------------------------
# ----Stephens-MacCall Filtering----
#------------------------------------------------------
# Remove species comprising <0.1% of the records
uncommon.spp <- names(cdfwpr[16:286])[colSums(cdfwpr[16:286]>0)/length(cdfwpr$ID)<0.001]
cdfwpr2 <- cdfwpr[, -which(colnames(cdfwpr) %in% uncommon.spp)]
cdfwpr2 <- as.data.frame(cdfwpr2)

# Remove species that never co-occur with quillback
cdfwpr2quill <- filter(cdfwpr2, target>0)
#u.species <- names(cdfwpr2[,17:53])
#extreme.counterindicators <- u.species[colSums(subset(cdfwpr2[u.species], target>0)>0)==0] #Not sure why, but this doesn't work
extreme.counterindicators <- as.data.frame(colSums(cdfwpr2quill[16:69]))
extreme.counterindicators$species <- rownames(extreme.counterindicators)
colnames(extreme.counterindicators) <- c("count", "species")
extreme.counterindicators <- filter(extreme.counterindicators, count<1)
extreme.counterindicators <- rownames(extreme.counterindicators)
cdfwpr2 <- cdfwpr2[, -which(names(cdfwpr2) %in% extreme.counterindicators)]

# Create a binary version of cdfwpr2

cdfwpr3 <- cdfwpr2
cdfwpr3[,16:67][cdfwpr3[,16:67]>0] <- 1
cdfwpr3[,1][cdfwpr3[,1]>0] <- 1

predictors <- cdfwpr3[,c(16:67)]
target <- cdfwpr3[,1]
bin.data <- cbind(target, predictors)
#colnames(bin.data)[1] <- "target"
prednames <- colnames(predictors)
prednames <- make.names(prednames)
colnames(bin.data)[2:53] <- prednames
prednames <- paste(prednames, collapse="+")
prednames


# set up formula for GLM
fish.form <- as.formula(paste("target ~ ",prednames))
fish.form

# Regress on all species
my.glm <- glm(formula=fish.form, family=binomial(link='logit'), data=bin.data)
summary(my.glm)
pred.props <- predict(my.glm, type='response')

# ROC analysis
library(pROC)
roccurve <- roc(target ~ pred.props)
# define another cut-off threshold for 'random' zeros based on balancing false negatives and false positives,
# followed by inclusion of all trips which caught the target
# this is calculated by sorting the trips in descending order of their predicted probs, and
# then keeping a number of observations equal to the number of positive trips
pos.trips <- sum(target) # same as sum of predicted props
SM.thresh <- sort(pred.props, decreasing=T)[pos.trips]
SM.thresh
#  105517 
#0.1860326   

# ROC plot
png(filename="data-raw/PRData/Quillback_PR_Index_S-M_ROC.png", units="in", width=5, height=5, res=600)
plot(roccurve)
text(0.25, 0.25, paste("AUC =",round(auc(roccurve),3)))
dev.off()

fishprob <- cbind.data.frame(prob=pred.props, cdfwpr2)  # Reverting to cdfwpr2 here to go back to count data rather than binary
fishprob <- droplevels(fishprob)
names(fishprob)

# add column with classifications based on SM thresh
fishprob$SM_classifier <- as.numeric(pred.props > SM.thresh)
write.table(fishprob, quote=F, row=F, sep=',', file=file.path(here("data-raw", "PRData", 'Probs_and_Data.csv')))

# show classification table
with(fishprob, table(Quillback=target>0, Keep_Trip=SM_classifier, useNA='ifany'))

# plot regression coefficients +/- 2 se
bin.coefs <- data.frame(summary(my.glm)$coef[,1:2])
bin.coefs <- bin.coefs[order(-bin.coefs$Estimate),]
png(filename="data-raw/PRData/Quill_PR_Index_S-M_coef.png", units='in', width=8, height=10, res=600)
par(mar=c(5,8,4,2))
ggplot(bin.coefs, aes(x=factor(rownames(bin.coefs), levels=rownames(bin.coefs)[order(Estimate)]), y=Estimate)) + 
  geom_bar(position=position_dodge(), stat="identity", fill='blue') +
  geom_errorbar(aes(ymin=Estimate-1.96*Std..Error, ymax=Estimate+1.96*Std..Error),
                width=.6,                    # Width of the error bars
                position=position_dodge(.9)) +
  labs(y = "Binomial GLM Coef. w/ 95% C.I.", x = "Species") +
  coord_flip()
dev.off()

# DROP ONLY TRUE NEGATIVES
# keep all trips with prob >= best.thresh ***OR*** with positive Blue catch (correct habitat, by def'n)
drop_only_true_neg.df <- fishprob[fishprob$prob>=SM.thresh | fishprob$target>0,
                                  c("ID", "target","anglers", "prim1Common", "year", "month","county","district_new","prob", "cpue")]
dim(drop_only_true_neg.df)
summary(drop_only_true_neg.df)
# save filtered data
write.table(drop_only_true_neg.df, quote=F, row=F, sep=',', file=file.path(here("data-raw", "PRData", "filtered_quillback_pr_DROP_ONLY_TRUE_NEGS.csv")))

# classification table
table(QuillCaught=drop_only_true_neg.df$target>0, KeepTrip=drop_only_true_neg.df$prob>=SM.thresh)

# look at the spatial and temporal distribution of samples
with(drop_only_true_neg.df, table(year, district_new, useNA='ifany'))

#-------------------------------------------------------------------------------
# Add to filter dataframe
dataFilters$Filter[filter.num] <- c("Stephens-MacCall")
dataFilters$Description[filter.num] <- c("Remove predicted false negatives")
dataFilters$Samples[filter.num] <- nrow(drop_only_true_neg.df)
dataFilters$Positive_Samples[filter.num] <- nrow(subset(drop_only_true_neg.df, target>0))
filter.num <- filter.num + 1
#-------------------------------------------------------------------------------

# make the filename simpler... jeez.
cdfwpr4 <- drop_only_true_neg.df


#############################################################################################################
#  End Stephens-MacCall
#############################################################################################################

# Look at positive samples by district and month
pos_samples_year_district <- cdfwpr4 %>%
  filter(target>0) %>%
  group_by(year, district_new) %>%
  summarize(n = n_distinct(ID)) %>%
  pivot_wider(names_from=district_new, values_from = n)
# Some year/districts without positive samples
write.csv(pos_samples_year_district, 
          file.path(out.dir,"pos_samples_by_year_district.csv"),
          row.names=FALSE)

pos_samples_year_month <- cdfwpr4 %>%
  filter(target>0) %>%
  group_by(year, month) %>%
  summarize(n = n_distinct(ID)) %>%
  pivot_wider(names_from=month, values_from = n)
# Lots of year/months have no positive samples.  Add a 3-month grouping variable.  
write.csv(pos_samples_year_month, 
          file.path(out.dir,"pos_samples_by_year_month.csv"),
          row.names=FALSE)

cdfwpr4$wave3 <- NA
cdfwpr4$wave3[cdfwpr4$month %in% c(4, 5, 6)] <- 1
cdfwpr4$wave3[cdfwpr4$month %in% c(7, 8, 9)] <- 2
cdfwpr4$wave3[cdfwpr4$month %in% c(10, 11, 12)] <- 3

pos_samples_year_wave <- cdfwpr4 %>%
  filter(target>0) %>%
  group_by(year, wave3) %>%
  summarize(n = n_distinct(ID)) %>%
  pivot_wider(names_from=wave3, values_from = n)
# Lots of year/months have no positive samples.  Add a 3-month grouping variable.  
write.csv(pos_samples_year_wave, 
          file.path(out.dir,"pos_samples_by_year_wave3.csv"),
          row.names=FALSE)

#final tables and visualizations
#Get the number of available samples by year
samples_year_district <- cdfwpr4 %>%
group_by(year, district_new) %>%
tally() %>%
tidyr::pivot_wider(names_from=district_new, values_from = n)
samples_year_district
write.csv(samples_year_district, 
file.path(out.dir,"samples_by_year_district.csv"),
row.names=FALSE)

#sample sizes by month
samples_month_year <- cdfwpr4 %>%
group_by(year, month) %>%
tally() %>%
tidyr::pivot_wider(names_from = month, values_from = n)
samples_month_year

write.csv(samples_month_year, 
          file.path(out.dir,"samples_by_month.csv"),
          row.names=FALSE)
# Some year/months have no samples.  Need to use wave?  

#average cpue by county
#might want to add a look up table and have the 
#county names
cpue_by_county <- cdfwpr4 %>%
group_by(year, county, district_new) %>%
summarise(average_cpue = mean(cpue)) %>%
mutate_at(vars(county,district_new), as.factor)

cpue_by_district <- cdfwpr4 %>%
group_by(year,  district_new) %>%
summarise(average_cpue = mean(cpue)) %>%
mutate_at(vars(district_new), as.factor)

#didn't see anything with county-too messy - look at district
ggplot(cpue_by_district, aes(x = year, 
y = average_cpue, colour = district_new)) +
geom_point(size = 3)  + theme_bw() +
geom_line(aes(x = year, y = average_cpue, 
colour = district_new)) +
xlab("Year") + ylab("Average CPUE") + 
scale_color_viridis_d()
ggsave(file = file.path(out.dir, "average_cpue_by_district.png"), width = 7, height = 7)

cpue_raw <- cdfwpr4 %>%
  group_by(year) %>%
  summarise(average_cpue = mean(cpue), sd_cpue=sd(cpue)) 

ggplot(cpue_raw, aes(x=year, y=average_cpue)) +
  geom_line() +
  geom_ribbon(aes(ymin=average_cpue - sd_cpue, ymax=average_cpue + sd_cpue), alpha=0.5) +
  ylab("mean cpue")

ggsave(file = file.path(out.dir, "average_cpue.png"), width = 7, height = 7)

#save the datafile and filters for the run file
save(cdfwpr4, dataFilters, 
file = file.path(out.dir, "data_for_glm.RData"))

write.csv(dataFilters, 
          file.path(out.dir, "dataFilters.csv"),
          row.names=FALSE)
