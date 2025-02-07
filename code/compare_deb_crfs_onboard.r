##########################################################
# Look at the filtered data for Deb's survey and the 
# more contemporary CDFW onboard observer data
#
# Quillback rockfish 2025
# Melissa Monk 2/6/2025
#
##########################################################

rm(list=ls(all=TRUE))
graphics.off()

library(here)
library(tidyr)
library(dplyr)
library(ggplot2)

dir <- file.path(here(),"data-raw", "rec_indices", "compare_onboard")
setwd(dir)

#load Deb's final dataset
load(file.path(here(), "data-raw","rec_indices", "debwv_cpfv_onboard", "QLBK_filtered_data.RData"))


#load  the cdfw onboard dataset
load(file.path(here(), "data-raw","rec_indices", "crfs_cpfv_onboard", "data_for_glm.RData"))



# get column names and compare
str(dat)
str(onboard)

deb <- dat %>%
  mutate(ID = paste0(ASSN,'_', LOCNUM),
   dbase = "Deb") %>%
        rename(county = CNTY, 
                district = DISTRICT, 
                year = YEAR, month = MONTH, 
                fishtime = FISHTIME, number.fish = NUMENC, 
                reef = REEFID, effort = ANGHRS ) %>%
  dplyr::select(dbase, ID, year, month, county, district, fishtime, reef, number.fish, effort)


crfs <- onboard %>%
 dplyr::select(dbase, ID, year, month, county, district, fishtime, reef, number.fish, effort)

both <- rbind(deb, crfs)

#filter to district 4
both <- both %>% filter(district == 4)
both$cpue <- both$number.fish/both$effort
both$anglers <- both$effort/(both$fishtime/60)

ggplot(both, aes(x = as.factor(year), y = fishtime)) + 
geom_boxplot() #+ ylab("Depth (fm)")
#ggsave(file = file.path(getwd(), "depth_by_reef.png"), width = 7, height = 7)



ggplot(both, aes(x = year, y = cpue)) + 
geom_jitter() 

with(both %>% filter(number.fish>0), table(reef))
with(both %>% filter(number.fish>0), table(reef, year))
with(both %>% filter(number.fish>0), table(year))
with(both %>% filter(number.fish>0), table(year)) / with(both, table(year))
ggplot(both %>% filter(number.fish > 0), aes(x = year, y = number.fish)) + 
geom_jitter() 


pos <- both %>% filter(number.fish > 0)


cpue_reef_year <- both %>%
   group_by(year, reef) %>%
   summarise(avg_cpue = mean(cpue))

ggplot(cpue_reef_year, aes(x = year, y = avg_cpue, colour = reef)) +
geom_line(lwd = 2)
