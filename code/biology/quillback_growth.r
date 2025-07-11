###############################################################
#Script look at the available age at length data for quillback
#rockfish for the 2025 California stock assessment
#
#Author: Melissa Monk SWFSC
#      1/17/2025
#updated 4/1/25 with a single file for lengths created by
#age_length_cleanup_by_area.R
###############################################################
rm(list = ls(all = TRUE))
graphics.off()

library(ggplot2)
library(dplyr)
library(here)
library(nwfscSurvey)
library(FSA)

setwd(here())
#fig.dir <- file.path(here(),"data_explore_figs")
#read in the data
qlbk <- read.csv(file.path(here(), "data-raw", "ages","QLBK_faa_age_length.csv"))

#model_fleet
qlbk <- qlbk %>%
mutate(fleet = case_when(source == "pacfin" ~ "Commercial", 
                         source == "CCFRPNotFarallons" ~ "CCFRP",
                         TRUE ~ "Growth")) %>% mutate(project = source)
summary(as.factor(qlbk$source))
summary(as.factor(qlbk$fleet))
#  CCFRP Commercial     Growth
#  103        302        865
summary(as.factor(qlbk$faa_area))
#north south 
#  738   532 
qlbk %>% filter(age !=0) %>% group_by(faa_area) %>% tally()
#without age 0 fish
# north      738
# south      340

#Assign a flag if not included in the base model
qlbk <- qlbk %>%
mutate(InModel = case_when(fleet == "Commercial" & year %in% c(2007, 2023, 2024) ~ "External fit only",
                           fleet == "Growth" & year %in% c(1985, 2004, 2007, 2014, 2019, 2020, 2025) ~ "External fit only",
                           age == 0 ~ "External fit only",
                           TRUE ~ "Included in base model"))
qlbk$InModel <- as.factor(qlbk$InModel)
qlbk <- qlbk %>% mutate(fleet = case_when(fleet =="CCFRP" ~ "Growth", 
TRUE ~ fleet))

write.csv(qlbk, here("data", "growth_data_inmodel_forplots.csv"))


#For postSTAR version assign an updated flag if not included in the base model
#Adding a new csv because other scripts pull earlier versions and may not work if update the csv
qlbk_postSTAR <- qlbk %>%
  mutate(InModel = case_when(source %in% c("Abrams", "CCFRPFarallons", "CCFRPNotFarallons") ~ "Included in base model",
                             fleet == "Commercial" & year %in% c(2007, 2011, 2012, 2023, 2024) ~ "External fit only",
                             fleet == "Growth" & year %in% c(1985, 2004, 2007, 2014, 2019, 2020, 2025) ~ "External fit only",
                             fleet == "Growth" & !(source %in% c("Abrams", "CCFRPFarallons", "CCFRPNotFarallons")) ~ "External fit only",
                             age == 0 ~ "External fit only",
                             TRUE ~ "Included in base model"))
qlbk_postSTAR$InModel <- as.factor(qlbk_postSTAR$InModel)
qlbk_postSTAR <- qlbk_postSTAR %>% mutate(fleet = case_when(fleet =="CCFRP" ~ "Growth", 
                                                            TRUE ~ fleet))

write.csv(qlbk_postSTAR, here("data", "growth_data_inmodel_forplots_postSTAR.csv"))

##############################################################################################################
#make a copy to keep the code the same
ca <- qlbk
######Data plots
#age length by sex and project
ggplot(ca, aes(y = length_cm, x = age, color = sex)) +
	geom_point(alpha = 0.5) + 
  theme_bw() + 
  xlim(0, 60) + ylim(0, 60) +
  theme(panel.grid.major = element_blank(), 
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        strip.text.y = element_text(size = 14),
        panel.grid.minor = element_blank()) + 
	facet_grid(project~.) + 
	xlab("Age") + ylab("Length (cm)") +
  scale_color_viridis_d()
ggsave(filename = file.path(here(), "data_explore_figs", "bio_figs", "age_at_length_sex_project.png"),
       width = 10, height = 8)


#plot by faceted year
ggplot(ca, aes(y = length_cm, x = age, color = project)) +
	geom_point(alpha = 0.1) + 
  theme_bw() + 
  geom_jitter() + 
  xlim(0, 60) + ylim(0, 60) +
  theme(panel.grid.major = element_blank(), 
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        strip.text.y = element_text(size = 16),
         legend.text = element_text(size = 20),
        panel.grid.minor = element_blank()) + 
	facet_grid(rows = vars(year)) + 
	xlab("Age") + ylab("Length (cm)") +
  scale_color_viridis_d()
ggsave(filename = file.path(here(), "data_explore_figs", "bio_figs", "age_at_length_byproject.png"),
       width = 10, height = 8)

#plot by area and project
 #+
  #scale_color_viridis_d()#begin = .05, end = .8)
ggsave(filename = file.path(here(), "data_explore_figs", "bio_figs", "faa_age_at_length_and_project.png"),
       width = 10, height = 8)


#plot by faceted area
ggplot(ca, aes(y = length_cm, x = age, colour = fleet, shape = faa_area)) +
	geom_point(alpha = 0.3, size = 4) + 
  theme_bw() +  
  xlim(0, 60) + ylim(0, 60) +
  theme(panel.grid.major = element_blank(), 
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        strip.text.y = element_text(size = 16),
         legend.text = element_text(size = 20),
        panel.grid.minor = element_blank()) + 
	xlab("Age") + ylab("Length (cm)") +
  scale_color_viridis_d(begin = .05, end = .8)
ggsave(filename = file.path(here(), "data_explore_figs", "bio_figs", "faa_age_at_length_byfleet.png"),
       width = 10, height = 8)



#plot by faceted area
ggplot(ca, aes(y = length_cm, x = year, colour = factor(year))) +
	geom_boxplot() + 
  theme_bw() +  
  xlim(0, 60) + ylim(0, 60) +
  theme(panel.grid.major = element_blank(), 
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        strip.text.y = element_text(size = 16),
         legend.text = element_text(size = 20),
        panel.grid.minor = element_blank()) + 
	xlab("Age") + ylab("Length (cm)") +
  scale_color_viridis_d(begin = .05, end = .8)
ggsave(filename = file.path(here(), "data_explore_figs", "bio_figs", "faa_age_at_length_byfleet.png"),
       width = 10, height = 8)

####################################################################################
# Data summary tables
 ca %>% group_by(faa_area) %>% summarise(max = max(length_cm))


with(ca, table(project, faa_area))
####################################################################################
# Additional data summaries

#look at the sample sizes for a given age for north and south
faa_laa_summary0 <- ca %>%
filter(age !=0) %>% 
mutate(floor_len = floor(length_cm)) %>%
group_by(age) %>%
tally()
View(faa_laa_summary0)


#look at the mean and median age at a given length for north and south
faa_laa_summary <- ca %>%
filter(age !=0) %>% 
mutate(floor_len = floor(length_cm)) %>%
group_by(faa_area, floor_len) %>%
summarise(mean_age = round(mean(age),0)) %>% # %>%min_age = min(age), max_age = max(age), 
tidyr::pivot_wider(names_from = faa_area, values_from = mean_age)
View(faa_laa_summary)

faa_laa_summary1 <- ca %>%
filter(age !=0) %>% 
mutate(floor_len = floor(length_cm)) %>%
group_by(faa_area, floor_len) %>%
summarise(median_age = round(median(age),0)) %>% # %>%min_age = min(age), max_age = max(age), 
tidyr::pivot_wider(names_from = faa_area, values_from = median_age)
View(faa_laa_summary1)


#box plots of the ages at length
#look at the mean age at a given length for north and south
ca %>%
mutate(floor_len = floor(length_cm)) %>%
#group_by(faa_area) %>%
ggplot(aes(x = factor(age), y = length_cm, fill = faa_area)) +
geom_boxplot()

#box plots of the ages at length no commercial 
#look at the ages at a given length for north and south
ca %>% filter(project != "pacfin") %>%
mutate(floor_len = floor(length_cm)) %>%
#group_by(faa_area) %>%
ggplot(aes(x = factor(age), y = length_cm, fill = faa_area)) +
geom_boxplot()

#box plots of the ages at length flipped 
#look at the ages at a given length for north and south
ca %>% 
mutate(floor_len = floor(length_cm)) %>%
#group_by(faa_area) %>%
ggplot(aes(y = age, x = factor(floor_len), fill = faa_area)) +
geom_boxplot()


#box plots of the ages at length no commercial 
#look at the ages at a given length for north and south
ca %>% filter(project != "pacfin") %>%
mutate(floor_len = floor(length_cm)) %>%
#group_by(faa_area) %>%
ggplot(aes(x = factor(age), y = length_cm, fill = faa_area)) +
geom_boxplot()


##look at the birth year for each age
births <- ca %>% filter(age > 0) %>%
mutate(birthyear = year - age) %>% filter(birthyear > 1969)
View(births)

births$projects <- as.factor(births$project)
ggplot(births, aes(x = birthyear)) +
geom_histogram(breaks=seq(1970, 2021,by=1), aes(fill=project)) +
theme_bw()+
theme(text = element_text(size = 20)) +
scale_x_continuous(breaks = seq(1970, 2021,by=1)) +
scale_fill_viridis(discrete = TRUE) +
theme(axis.text.x=element_text(angle=90, hjust=1))+
xlab("Birth Year") + ylab("Number of aged fish")
ggsave(filename = file.path(here(), "data_explore_figs", "bio_figs", "age_birthyears_by_project.png"),
       width = 16, height = 10)

births$year <- as.factor(births$year)
ggplot(births, aes(x = birthyear)) +
geom_histogram(breaks=seq(1970, 2021,by=1), aes(fill=year)) +
theme_bw()+
theme(text = element_text(size = 20)) +
scale_x_continuous(breaks = seq(1970, 2021,by=1)) +
scale_fill_viridis(discrete = TRUE) +
guides(fill = guide_legend(title = "Capture Year")) +
theme(axis.text.x=element_text(angle=90, hjust=1)) +
xlab("Birth Year") + ylab("Number of aged fish")
ggsave(filename = file.path(here(), "data_explore_figs", "bio_figs", "age_birthyears_by_captureyear.png"),
       width = 16, height = 10)

with(births, table(birthyear))
###########################################################################################################
#############################################
#estimate growth
#2021 estimate Linf = 43.04 k = .199 to = -0.067
#2025 CA only est: 

age_df <- ca #%>% filter(age !=0)
age_df$Age <- age_df$age
age_df$Length_cm <- age_df$length_cm
age_df <- age_df

vb_est_all<- est_vbgrowth(
  dir = NULL, 
  dat = age_df,
  col_length = "length_cm",
  col_age = "age",
  init_params = data.frame(K = 0.17, Linf = 39, L0 = 2, CV0 = 0.10, CV1 = 0.10))
vb_est_all$all_growth

#keep for reading into model runs files
#         K        Linf          L0         CV0         CV1 
# 0.109943348 42.890509548 16.414008963  0.162235382  0.009453519
#write.csv(data.frame("ests" = vb_est_all$all_growth), here("data", "vonb_ests.csv"))

#         K        Linf          L0         CV0         CV1 #
# 0.17820030 41.18119888  3.98945906  0.20322534  0.06373563
write.csv(data.frame("ests" = vb_est_all$all_growth), here("data", "vonb_ests_withAge0.csv"))

#Value for t0 from this parameterization based on Schnute and Fournier 1980
0 + 1/vb_est_all$all_growth[1] * 
  log((vb_est_all$all_growth[2] - vb_est_all$all_growth[3])/
        (vb_est_all$all_growth[2] - vb_est_all$all_growth[1] * exp(-vb_est_all$all_growth[1] * (80 - 0))))

#############################################################################################
#Estimate growth to explore why the initial parameters matter
age_df <- qlbk #%>% filter(age !=0) %>% filter(project != "CCFRP")
age_df$Age <- age_df$age
age_df$Length_cm <- age_df$length_cm
age_df <- age_df

options(scipen=999)
vb_est_all<- est_vbgrowth(
  dir = NULL, 
  dat = age_df,
  col_length = "length_cm",
  col_age = "age",
  init_params = data.frame(K = 0.11, Linf = 42, L0 = 1, CV0 = 0.10, CV1 = 0.10))  #change k from .17 to .11 and Linf from 39 to 42
vb_est_all$all_growth

#no age 0 fish and removing north .17 and 39
#           K         Linf           L0          CV0          CV1
# 0.23884808 36.28061633  2.58123426  0.29903738  0.03112365
# .11 and 42
# 0.25080628 36.05249185  0.95627644  0.34434123  0.03802425

#no age 0 fish and removing south .17 and 39
#  0.17024331 41.72027852  7.05904427  0.17441680  0.06113371
# .11 and 42
#  0.15183062 42.13785308 10.39984989  0.17391668  0.04310851

#no age 0 fish and no pacfin .17 and 39
#          K        Linf          L0         CV0         CV1
#  0.12958463 41.99008304 13.11715487  0.16082135  0.04801135
# .11 and 42
#  0.10805548 43.25176211 15.68451378  0.16116508  0.01641002

#no age 0 fish and no cooperative .17 and 39
# 0.14355298 41.90940832 11.82784979  0.15554780  0.04877026
# .11 and 42
# 0.12157404 42.67588636 14.38494331  0.16133371  0.02750198

#no age 0 fish and no ccfrp .17 and 39
# 0.12201192 42.51757120 13.75858328  0.16741423  0.02352261
# .11 and 42
# 0.1887471 41.0842952  1.1903998  0.2640057  0.0688376

##################################################
#Get the predictions
ages <- seq(0, 60, by = 0.2)
vb_fn <- function(age, Linf, L0, k) {
    #vec <- Linf * (1 - exp( -k * (age - t0)))
    vec <- Linf - (Linf - L0) * exp(-age * k)
    return(vec)
}
vb_fn2 <- function(age, Linf, t0, k) {
    vec <- Linf * (1 - exp( -k * (age - t0)))
   # vec <- Linf - (Linf - L0) * exp(-age * k)
    return(vec)
}
#2025 external estimate
preds1 <- data.frame(ages,
                 fit = vb_fn(ages, Linf = vb_est_all[[3]][2], L0 = vb_est_all[[3]][3], k = vb_est_all[[3]][1]))

#2021 estimate
preds2 <- data.frame(ages,
                 fit = vb_fn2(ages,  Linf = 43.04, t0 = -0.067, k = 0.199))

#2025 Internal estimate preSTAR
preds3 <- data.frame(ages,
                     fit = vb_fn(ages,  Linf = 42.7486, L0 = 4, k = 0.126145))

#2025 Internal estimate postSTAR
preds4 <- data.frame(ages,
                     fit = vb_fn(ages,  Linf = 42.8202, L0 = 4, k = 0.12666)) #keep L0 at 4 even though its nearer to 4.87 to get L1 of 9.34673


#Plot data with fit 
 ggplot(age_df, aes(y = length_cm, x = age)) +
  #geom_point(data = age_df %>% filter(InModel == "yes"), aes(y = length_cm, x = age), shape = 21, size = 4,  colour = "darkgray") + 
  #geom_point(data = age_df %>% filter(InModel == "no"), aes(y = length_cm, x = age), shape = 4, size = 4, colour = "deeppink4") + 
   geom_point(data = age_df, aes(y = length_cm, x = age, shape = factor(InModel), colour = factor(InModel), size = factor(InModel))) + 
  scale_shape_manual(values=c(4, 1), guide = "none") +
  scale_fill_manual(values=c('darkgray', 'deeppink4'), guide = "none") +
  scale_size_manual(values = c(8, 4), guide = "none") +
  geom_line(data = preds1, aes(y = fit, x = ages, colour = "2025 fitted external est."), linewidth = 1.2) +
  geom_line(data = preds2, aes(y = fit, x = ages, colour = "2021 model values"), linewidth = 1.2) +
  geom_line(data = preds3, aes(y = fit, x = ages, colour = "2025 model fitted est."), linewidth = 1.2) +
  theme_bw() + 
  labs(colour = c("Legend")) +
  guides(colour = guide_legend(position  = "inside")) +
  scale_x_continuous(breaks = seq(0,60,10)) +
  scale_y_continuous(breaks = seq(0,55,5)) +
  theme(panel.grid.major = element_blank(), 
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        strip.text.y = element_text(size = 16),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        panel.grid.minor = element_blank(),
        legend.position.inside = c(0.8, 0.25)) + 
  	xlab("Age (years)") + ylab("Length (cm)") #+
   #scale_color_viridis_d(begin = 0, end = .9) 
ggsave(filename = file.path(here(),"report", "figures", "bio_growth.png"),
       width = 10, height = 8)


## 
#If want postSTAR version use the following code
#This copies over all data into "External fit" so facet wrap includes all data
##
age_df <- qlbk_postSTAR #%>% filter(age !=0) %>% filter(project != "CCFRP")
age_df$Age <- age_df$age
age_df$Length_cm <- age_df$length_cm
age_df <- age_df

levels(age_df$InModel) <- c("External fit", "Included in base model")
temp <- age_df[which(age_df$InModel == "Included in base model"),]
temp$InModel <- "External fit"
age_df_adjust <- dplyr::bind_rows(age_df, temp)

ggplot(age_df_adjust, aes(y = length_cm, x = age)) +
  #geom_point(data = age_df_adjust %>% filter(InModel == "yes"), aes(y = length_cm, x = age), shape = 21, size = 4,  colour = "darkgray") + 
  #geom_point(data = age_df_adjust %>% filter(InModel == "no"), aes(y = length_cm, x = age), shape = 4, size = 4, colour = "deeppink4") + 
  geom_point(data = age_df_adjust, aes(y = length_cm, x = age, shape = factor(InModel), colour = factor(InModel), size = factor(InModel))) + 
  scale_shape_manual(values=c(1, 1), guide = "none") +
  scale_fill_manual(values=c('darkgray', 'deeppink4'), guide = "none") +
  scale_size_manual(values = c(4, 4), guide = "none") +
  geom_line(data = preds1, aes(y = fit, x = ages, colour = "2025 fitted external est."), linewidth = 1.2) +
  geom_line(data = preds2, aes(y = fit, x = ages, colour = "2021 model values"), linewidth = 1.2) +
  geom_line(data = preds4, aes(y = fit, x = ages, colour = "2025 model fitted est."), linewidth = 1.2) +
  theme_bw() + 
  facet_wrap(~ InModel, ncol=1) + 
  labs(colour = c("Legend")) +
  guides(colour = guide_legend(position  = "inside")) +
  scale_x_continuous(breaks = seq(0,60,10)) +
  scale_y_continuous(breaks = seq(0,55,5)) +
  theme(panel.grid.major = element_blank(), 
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        strip.text.y = element_text(size = 16),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        panel.grid.minor = element_blank(),
        legend.position.inside = c(0.75, 0.15)) + 
  xlab("Age (years)") + ylab("Length (cm)") #+
#scale_color_viridis_d(begin = 0, end = .9) 
ggsave(filename = file.path(here(),"report", "figures", "bio_growth_postSTAR.png"),
       width = 8, height = 10)


###############################################################################
###############################################################################
#Vonbert Models estimated with a different package - to see if its still
#sensitive to start values - the answer is yes
###############################################################################
Startval = vbStarts(Length_cm~Age, data=age_df)
Startval=list(Linf=42,K=.12,t0=-1)
####fit model to all data 
vbTypical <- Length_cm~Linf*(1-exp(-K*(Age-t0)))
fitTyp = nls(vbTypical, data=age_df , start=Startval)

#make changes to the dataframe to match
age_df < - age_df# %>% filter(age!=0) #add or remove the age 0 fish and it matters
age_df$Age = age_df$age
age_df$Length = age_df$length_cm

fitGen <- nls(vbTypical, data = age_df, start = Startval)
fitGen

#Schnute parameterization
SchStarts = FSA::vbStarts(Length~Age, data=age_df,type='Schnute')
SchStarts

vb3 <- FSA::vbFuns("Schnute",simple=FALSE)

fit <- nls(Length~vb3(Age,L0, L3,K, t1=1,t3=40),
                   data = age_df,start=SchStarts)
fit






#Plot data with fit fromt he model and the data points color coded by the fleets
ages <- seq(0, 60, by = 0.2)
vb_fn <- function(age, Linf, L1, k) {
    #vec <- Linf * (1 - exp( -k * (age - t0)))
    vec <- Linf - (Linf - L1) * exp(-age * k)
    return(vec)
}
preds3 <- data.frame(ages,
                     fit = vb_fn(ages,  Linf = 42.7486, L1 = 9.80221 , k = 0.126145))


new_name <- "5_1_3_preStarBase"
pp <- SS_output(here('models', new_name))

aa<- pp$endgrowth

  # normal distribution of length at age
        growth_high <- as.data.frame(cbind(aa$Age_Beg, qnorm(
          0.975,
          mean = aa$Len_Beg,
          sd = aa$SD_Beg
        )))
        colnames(growth_high) <- c("Age", "Length")
         growth_high <- growth_high %>% filter(Age <61)
        growth_low <- as.data.frame(cbind(aa$Age_Beg, qnorm(
          0.025,
          mean = aa$Len_Beg,
          sd = aa$SD_Beg
        )))
       colnames(growth_low) <- c("Age", "Length")
       growth_low <- growth_low %>% filter(Age <61)

 ggplot(qlbk %>% filter(InModel=="Included in base model"), aes(y = length_cm, x = age)) +
   # geom_point(data = age_df, aes(y = length_cm, x = age, shape = factor(InModel), colour = factor(InModel), size = factor(InModel))) + 
    #geom_point(aes(y = length_cm, x = age, shape = factor(fleet), alpha = .5, colour = factor(fleet), size = factor(fleet))) + 
    geom_point(aes(y = length_cm, x = age, alpha = .5, colour = factor(fleet))) + 
  #scale_shape_manual(values=c(4, 1), guide = "none") +
  #scale_fill_manual(values=c('darkgray', 'deeppink4'), guide = "none") +
  #scale_size_manual(values = c(8, 4), guide = "none") +
  geom_line(data = aa %>% filter(Age_Beg < 61), aes(y = Len_Beg, x = Age_Beg), linewidth = 1.2) +
  geom_line(data = growth_high, aes(y = Length, x = Age), linewidth = 1, linetype = 2) +
  geom_line(data = growth_low, aes(y = Length, x = Age), linewidth = 1, linetype = 2) +
  theme_bw() +
  #labs(colour = c("Legend")) +
  #guides(colour = guide_legend(position  = "inside")) +
  scale_x_continuous(breaks = seq(0,60,10)) +
  scale_y_continuous(breaks = seq(0,55,5)) +
  theme(panel.grid.major = element_blank(), 
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 20),
         strip.text.y = element_text(size = 20),
         strip.text.x = element_text(size = 20),
        # legend.text = element_text(size = 20),
        # legend.title = element_text(size = 20),
         panel.grid.minor = element_blank(),
        # legend.position.inside = c(0.8, 0.25),
          legend.position = 'none') +
  	xlab("Age (years)") + ylab("Length (cm)") +
    facet_wrap(.~ fleet) +
   scale_color_viridis_d(begin = 0, end = .7) 
ggsave(filename = file.path(here(),"report", "figures", "star_modelgrowth_data.png"),
       width = 16, height = 8)




#3/31/25
#     ages       fit
#    0.0  3.985882
#    0.2  5.290169
#    0.4  6.548696
#    0.6  7.763066
#    0.8  8.934831
#    1.0 10.065485

#Age 0 here is estimated as 3.9, but that's really a July 1 length
# Look at using between 6 and 8 as a January 1 length at L1

###########################################################################
# Data biased and not collected randomly to look at sex

# #males only
# length_age_males <- est_vbgrowth(
#  dir = file.path(here(),"data-raw"),
#   dat = subset(age_df, sex == 1), 
#   col_length = "length_cm",
#   col_age = "age",
#    init_params = data.frame(K = 0.12, Linf = 55, L0 = 15, CV0 = 0.10, CV1 = 0.10))
# length_age_males$all_growth
# #males
# #           K         Linf           L0          CV0          CV1
# # 0.131052985 41.724881311 13.105886995  0.192873891  0.006705831

# #females only
# length_age_females <- est_vbgrowth(
#  dir = file.path(here(),"data-raw"),
#   dat = subset(age_df, sex == 2), 
#   col_length = "length_cm",
#   col_age = "age",
#    init_params = data.frame(K = 0.12, Linf = 55, L0 = 15, CV0 = 0.10, CV1 = 0.10))
# length_age_females$all_growth
# #females
# #           K         Linf           L0          CV0          CV1
# # 0.093386826 44.362969045 16.842822351  0.168755023  0.001117803

# #unsexed only
# length_age_unsexed <- est_vbgrowth(
#   dir = file.path(here(),"data-raw"),
#   dat = subset(age_df, sex == 3), 
#   col_length = "length_cm",
#   col_age = "age",
#   init_params = data.frame(K = 0.12, Linf = 55, L0 = 15, CV0 = 0.10, CV1 = 0.10))
# length_age_unsexed$all_growth
# #unsexed
# #        K         Linf           L0          CV0          CV1
# 0.14501755 41.62102296 12.58916457  0.11829281  0.07596045


#Data were not sampled in a manner that sex is unbiased 
# #Plot data with fits by sex
# ggplot(ca, aes(y = length_cm, x = age, col = sex)) +
#   geom_point(alpha = 0.1) + 
#   theme_bw() + 
#   xlim(1, 60) + ylim(1, 60) +
#   theme(panel.grid.major = element_blank(), 
#         axis.text = element_text(size = 16),
#         axis.title = element_text(size = 16),
#         strip.text.y = element_text(size = 16),
#         legend.text = element_text(size = 20),
#         panel.grid.minor = element_blank()) + 
#   facet_grid(rows = vars(sex)) +
#   geom_function(data = data.frame(age = 0, length_cm = 0, sex = "1"),
#                 fun = vb_fn,
#                 args = list(Linf = length_age_males$all_growth["Linf"], 
#                             L0 = length_age_males$all_growth["L0"],
#                             k = length_age_males$all_growth["K"])) +
#   # geom_text(data = data.frame(age = 40, length_cm = 20, sex = "1"), 
#   #           label = paste0("Linf = ", length_age_males$all_growth["Linf"])) +
#   geom_function(data = data.frame(age = 0, length_cm = 0, sex = "2"),
#                 fun = vb_fn,
#                 args = list(Linf = length_age_females$all_growth["Linf"], 
#                             L0 = length_age_females$all_growth["L0"],
#                             k = length_age_females$all_growth["K"])) +
#   geom_function(data = data.frame(age = 0, length_cm = 0, sex = "3"),
#                 fun = vb_fn,
#                 args = list(Linf = length_age_unsexed$all_growth["Linf"], 
#                             L0 = length_age_unsexed$all_growth["L0"],
#                             k = length_age_unsexed$all_growth["K"])) +
#   geom_function(data = data.frame(age = 0, length_cm = 0), aes(col = "All"),
#                 fun = vb_fn,
#                 args = list(Linf = vb_est_all$all_growth["Linf"], 
#                             L0 = vb_est_all$all_growth["L0"],
#                             k = vb_est_all$all_growth["K"])) +
#   xlab("Age") + ylab("Length (cm)")
# ggsave(filename = file.path(here(), "data_explore_figs", "bio_figs", "age_at_length_bysex_withFits.png"),
#        width = 6, height = 8)
