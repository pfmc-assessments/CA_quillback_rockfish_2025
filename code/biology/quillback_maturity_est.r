##############################################################################################################
#
# 	Purpose: Evaluate quillback rockfish maturity estimates
#
#			  by Melissa Monk on January 13, 2025
#   Data collected by the SWFSC and analyzed by Melissa Head (NWFSC)    
#
#
##############################################################################################################

#Remove all variables and turn off open graphics
rm(list=ls(all=TRUE))
graphics.off()


library(tidyr)
library(dplyr)
library(here)
library(car)
library(ggplot2)

# dir<- file.path(here())
# setwd("C:/Users/melissa.monk/Documents/Stock_assessments/CA_quillback_2025")
# 
# Quill.mat<-read.csv("Quillbackmaturity.csv")

Quill.mat <- read.csv(here("data-raw", "Quillbackmaturity_update02282025.csv"))
data <- Quill.mat
View(data)
summary(as.factor(data$Certainty))


# using 'certainty=1' data, functional maturity only
#removes 17 quillback 
data_cert <- data %>%
  filter(Certainty == 1, 
         !is.na(Certainty),
         !is.na(Functional_maturity)) %>%
           dplyr::select(Functional_maturity,
                           Length_cm, Latitude_DD, Year, Month,
                                 Port) %>%
  mutate(area = ifelse(Latitude_DD <= 40.2,"south", "north")) %>%
  mutate(FL_2cm = floor(Length_cm/2)*2) %>%
  mutate_at(vars(Port), as.factor)



with(data_cert, table(area, Functional_maturity))
#Functional_maturity
#area   0  1
#north  0  6
#south 15 60
#all quillback from the "north" above 40-10 were mature

ggplot(data_cert, aes(x = Length_cm, y = Functional_maturity)) +
  geom_point(cex = 5, shape = 1 )


###########Biological Maturity###########
data.glm <- glm(Functional_maturity ~ Length_cm, data=data_cert, 
                family = binomial(link ="logit"))
summary(data.glm)

##see if area is significant - yes, but low sample sizes in the north
data.glm1 <- glm(Functional_maturity ~ Length_cm + area, data=data_cert, 
                family = binomial(link ="logit"))
summary(data.glm1)

# results above match the 'deltaMethod' function in the 'car' package,
# which also appears to be using first-order Taylor Series approximation
L50 <- car::deltaMethod(data.glm, "-b0/b1", parameterNames= paste("b", 0:1, sep=""))
L50

obs.prop.df <- as.data.frame(data_cert %>% group_by(FL_2cm) %>% 
                               summarize(n=length(Functional_maturity),
                                         s=sum(Functional_maturity),
                                         obs.prop=mean(Functional_maturity)))
obs.prop.df

f <- function(x, a=coef(data.glm)[1], b=coef(data.glm)[2])
{
  eta <- a+b*x
  exp(eta) / (1 + exp(eta))
}

# plot the predicted curve against binned proportions
# added 1 to the lower edge to 'place' the proportion in the middle of each 2cm bin
#png(filename = "ilion_functional_maturity.png", width = 7, height = 5, units = "in", res = 600)
with(obs.prop.df, plot(FL_2cm+1, obs.prop, 
                       xlim=c(0,50), bty='l', 
                       xlab="Fork Length [cm]", 
                       ylab="Proportion Mature", 
                       main = "Predicted Proportion Mature",
                       cex.lab = 2, cex.main = 2,
                       cex.axis = 3))
curve(f,0,50,add=TRUE)
text(10,0.8,paste0("L50 = ",round(L50[[1]],2)))
#dev.off()
#----------------------------------------------------------------------------
ggplot(data_cert, aes(x = Length_cm, y = Functional_maturity)) +
  geom_point(cex = 6, shape = 1 ) +
  theme_bw() +
  theme(text=element_text(size=21)) +
  geom_function( fun = f, xlim = c(0,50), lwd = 2, colour  = "red")



######
ggplot(data_cert, aes(x = Length_cm, 
                      color = Month,
                      fill = Month)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~Port, ncol = 1)
  
with(data_cert, table(Month, Port))


####E.J. did this all by hand to make sure the car package gave the correct 
####answer
# # get bits and pieces for delta method "by hand"
# vcov(data.glm)
# a <- coef(data.glm)[[1]]
# b <- coef(data.glm)[[2]]
# var.a <- vcov(data.glm)[1,1]
# var.b <- vcov(data.glm)[2,2]
# cov.ab <- vcov(data.glm)[2,1]
# a
# b
# var.a
# var.b
# cov.ab
# 
# # first-order delta method for ratio of correlated random variables
# e.first <- -a/b
# v.first <- (a/b)^2 * (var.a/(a^2) + var.b/(b^2) - 2*cov.ab/(a*b))
# s.first <- sqrt(v.first)
# int.first.95 <- c(e.first - qnorm(0.975)*s.first, e.first + qnorm(0.975)*s.first)
# e.first
# v.first
# s.first
# int.first.95

mid.len <- 1:50
ohm3 <- -0.425; ohm4 <- 33.7
mature.len <- 1 / (1 + exp((ohm3) * (mid.len-ohm4)))
mature.len.alt <- 1 / (1 + exp((-0.60) * (mid.len-ohm4)))
plot(mid.len, mature.len)
lines(mid.len, mature.len.alt, col = 'green')



# ##
# #Melissa Monk analyzed some Oregon fish too (all from Astoria), and found that a combined 
# #maturity estimate is very similar. 
# #Here is a snippet from her code sent to us via email on Jan 31, 2025. 
# #We did not add here entire script because we dont ultimately use. 
# ##
#
# Quill.matODFW<-read.csv(here("data-raw", "Quillbackmaturity_update.csv"))
# QuillODFW.cert <- subset(Quill.matODFW,Certainty==1) ###Remove uncertain samples###
# 
# maturityfxnglm <- glm (maturity ~ 1 + length, 
#                        data <-data.frame(length = QuillODFW.cert$Length_cm, 
#                                          maturity <- QuillODFW.cert$Functional_maturity),
#                        family = binomial(link ="logit"))
# 
# vector.fxn.MatGLM<-c(maturityfxnglm$coefficients)
# A= vector.fxn.MatGLM[1]
# B= vector.fxn.MatGLM[2]
# sA<- summary(maturityfxnglm)$coef[1,2]
# sB<-summary(maturityfxnglm)$coef[2,2]
# r <- summary(maturityfxnglm, corr = TRUE)$correlation[1,2]
# 
# #Difference format of delta method but same answer as EJ's approach above
# deltamethod <- ((sA^2)/(B^2))- ((2*A*sA*sB*r)/(B^3))+ (((A^2)*(sB^2))/(B^4))
# deltamethod
# 
# -A/B #50% mat = 29.30 cm with Oregon and California samples
# 1.96*sqrt(deltamethod) 
# ###95% CI = 1.30##
