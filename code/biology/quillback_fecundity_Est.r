#########################################################################
### A look at fecundity
### CA quillback rockfish assessment 2025
### comparing estimate to E.J.'s estimate from the meta-analysis
### Melissa Monk
### 3/19/25
#########################################################################
rm(list = ls(all = TRUE))
graphics.off()

library(RColorBrewer)
library(dplyr)
library(tidyr)
library(ggplot2)
library(RODBC)
library(here)


fec.dir <- here("data-raw","fecundity")
plot.dir <- here("data_explore_figs", "bio_figs")


dat <- read.csv(file.path(fec.dir, "QLBK_fecundity.csv"))
dfo.dat <- read.csv(file.path(fec.dir,"qlbk_fecundity_dfo.csv"))
#fecundity equation F = aL^b (F is number of eggs, L is length)
#meta-analysis estimate: 3.93e-07*L^3.7 in millions of eggs where L is length in cm

#check to see if the data needs filtering
summary(dat)
dat <- dat %>%  
        filter(!is.na(SS1_COUNT))


#get the number of eggs per gram, average the two samples and then multiple by the gonad weight
dat <- dat %>% 
    mutate(fec_samp1 = SS1_COUNT/SS1_WEIGHT_G, 
            fec_samp2 = SS2_COUNT/SS2_WEIGHT_G) %>%
   # mutate(fec_cv = cv(c(fec_samp1, fec_samp2)))
   mutate(fec_per_gram = (fec_samp1 + fec_samp1)/2) %>%
    mutate(fec = fec_per_gram*OVERALL_GONAD_WEIGHT_G) %>%
    mutate(length_cm = FORK_LENGTH_MM/10) %>%
    mutate(fec_mil = fec/1000000)

#length weight relationship as a check
ggplot(dat, aes(length_cm, OVERALL_TOTAL_WEIGHT_G)) + geom_point(size = 10)

ggplot(dfo.dat, aes(length_cm, somatic_weight))  + geom_point(size = 10)

add_curve <- function(plot, i){
        return(plot + stat_function(aes(x = 30:40), function(x) {3.93e-07*x^3.7 })) #
}

ej_Est <- function(x) {3.93e-07*x^3.7 }
#look at the data
ggplot(dat, aes(x = length_cm, y = fec_mil, xmin = 0, xmax = 42)) + 
geom_point(size = 10) + geom_line(stat = 'function', fun = ej_Est, color = "blue")


###################################################################################################
#California only data fork length
# log(fecundity) = log(alpha) + beta * log(length) + some noise
# from Sabrina's code
ca.mod         <- lm(log(fec_mil)~log(length_cm), data = dat)
log.alpha.ca.mod  <- coef(ca.mod)[1]
alpha.ca.mod      <- exp(log.alpha.ca.mod) * exp(0.5 * sd(ca.mod$residuals)^2) # note, this is the median
beta.ca.mod       <- coef(ca.mod)[2]

alpha.ca.mod
beta.ca.mod

# power function form 
# fecundity = alpha * length ^ beta
x <- seq(28,46, by = 1)


# plot the data and the model fits; E.J.'s and the new one
ca_est <- function(x) {alpha.ca.mod*x^beta.ca.mod}
ggplot(dat, aes(x = length_cm, y = fec_mil, colour = PORT,  xmin = 0, xmax = 42)) + 
geom_point(size = 5) + #geom_line(stat = 'function', fun = ej_Est, color = "blue") +
geom_line(stat = 'function', fun = ca_est, color = "red")



####################################################################################################
#Canada only data fork length compared to E.J.'s
dfo.dat <- dfo.dat %>%
mutate(fec_mil = fecundity/1000000,
       length_cm = Fork_Length/10)



dfo.mod         <- lm(log(fec_mil)~log(length_cm), data = dfo.dat)
log.alpha.dfo.mod  <- coef(dfo.mod)[1]
alpha.dfo.mod      <- exp(log.alpha.dfo.mod) * exp(0.5 * sd(dfo.mod$residuals)^2)  # note, this is the median
beta.dfo.mod       <- coef(dfo.mod)[2]

alpha.dfo.mod
beta.dfo.mod


# plot the data and the model fits; E.J.'s and the canadian one
dfo_est <- function(x) {alpha.dfo.mod * x^beta.dfo.mod}
ggplot(dfo.dat, aes(x = length_cm, y = fec_mil)) + 
geom_point() + geom_line(stat = 'function', fun = ej_Est, color = "blue") +
geom_line(stat = 'function', fun = dfo_est, color = "purple")



####################################################################################################
#Canada only data total length
dfo.dat1 <- dfo.dat %>%
mutate(fec_mil = fecundity/1000000,
       length_cm = TL_pred/10)



dfo.mod1        <- lm(log(fec_mil)~log(length_cm), data = dfo.dat1)
log.alpha.dfo.mod1  <- coef(dfo.mod1)[1]
alpha.dfo.mod1      <- exp(log.alpha.dfo.mod1) * exp(0.5 * sd(dfo.mod1$residuals)^2)  # note, this is the median
beta.dfo.mod1       <- coef(dfo.mod1)[2]

alpha.dfo.mod1
beta.dfo.mod1


# plot the data and the model fits; E.J.'s and the canadian one
dfo_est1 <- function(x) {alpha.dfo.mod1 * x^beta.dfo.mod1}
ggplot(dfo.dat1, aes(x = length_cm, y = fec_mil)) + 
geom_point(size = 5) + geom_line(stat = 'function', fun = ej_Est, color = "blue") +
geom_line(stat = 'function', fun = dfo_est1, color = "purple")



####################################################################################################
#look at CA and DFO fork length data together
 dat <- dat %>%
       mutate(area = "CA") %>% rename(total_weight = OVERALL_TOTAL_WEIGHT_G) %>%
          mutate(somatic_weight = total_weight - OVERALL_GONAD_WEIGHT_G)
ca <- dat %>% dplyr::select(fec_mil, length_cm, total_weight, somatic_weight, area)

dfo <- dfo.dat %>% dplyr::select(fec_mil, length_cm, Round_Weight, somatic_weight) %>%
mutate(area = "DFO") %>% rename(total_weight = Round_Weight)
dfo_ca <- rbind(ca, dfo)

ggplot(dfo_ca, aes(length_cm, somatic_weight, colour = area))  + 
geom_jitter(size = 6, alpha = .5)

ggplot(dfo_ca, aes(length_cm, total_weight, colour = area))  + 
geom_jitter(size = 6, alpha = .5)

lw_ests <- nwfscSurvey::estimate_weight_length(data = ca %>% mutate(weight_kg = OVERALL_TOTAL_WEIGHT_G/1000))


ggplot(dfo_ca, aes(x = length_cm, y = fec_mil, colour = area)) + 
geom_point(size = 3)  + scale_colour_viridis_d(begin = 0, end = .6) +
geom_line(stat = 'function', fun = ej_Est, color = "blue") +
geom_line(stat = 'function', fun = dfo_est1, color = "purple") +
geom_line(stat = 'function', fun = ca_est, color = "red")


3.93e-07*L^3.7
alpha.ca.mod
alpha.dfo.mod

beta.ca.mod
beta.dfo.mod


ggplot(dat, aes(fec_samp1, fec_samp2, color = PORT )) +
 geom_point(size = 5) + geom_abline()
