###############################################################
## This function plots the growth curve estimated from multiple 
## stock synthesis runs

plot_compare_growth <- function(models = xx,

                                new_name = "STAR_request13_collapseGrowth_reweight",
                                legend_names = c('Base',
                                     'STAR_request13_collapseGrowth_reweight',
                                     'STAR_request13_nogrowthfleet_reweight'),
                                max_age = 60){

qlbk <- read.csv(here("data", "growth_data_inmodel_forplots.csv"))
qlbk_temp <- qlbk %>%
        filter(project %in% c('CCFRPFarallons', 'CCFRPNotFarallons', 'pacfin', 'Abrams')) %>%
        filter(InModel == 'Included in base model')
qlbk <- qlbk_temp
#Figure out how many runs you're looking at
 n_runs <- length(xx)

#Vector of ages
ages <- seq(0, max_age, by = 0.5)
vb_fn <- function(age, Linf, L1, k) {
    #vec <- Linf * (1 - exp( -k * (age - t0)))
    vec <- Linf - (Linf - L1) * exp(-age * k)
    return(vec)
}
#age_length <- data.frame(Ages = NA,
#                         model_names = NA,
#                        length = NA)

for(i in 1:n_runs){

preds <- data.frame(Ages = ages)
                            
preds$model_names = rep(legend_names[i], length(ages))

              preds$length <- vb_fn(ages, 
                             Linf = xx[[i]]$Growth_Parameters$L_a_A2,
                             L1 = xx[[i]]$endgrowth$Len_Beg[1],
                             k = xx[[i]]$Growth_Parameters$K)

# normal distribution of length at age
   growth_high <- as.data.frame(cbind(xx[[1]]$endgrowth$Age_Beg, qnorm(
          0.975,
          mean = xx[[i]]$endgrowth$Len_Beg,
          sd = xx[[i]]$endgrowth$SD_Beg
        )))
    
        growth_low <- as.data.frame(cbind(xx[[i]]$endgrowth$Age_Beg, qnorm(
          0.025,
          mean = xx[[i]]$endgrowth$Len_Beg,
          sd = xx[[i]]$endgrowth$SD_Beg
        )))

     colnames(growth_high) <- c("Age", "Length")
     growth_high <- growth_high %>% filter(Age <61)
     colnames(growth_low) <- c("Age", "Length")
     growth_low <- growth_low %>% filter(Age <61)
growth_low$model_names = legend_names[i]
growth_high$model_names = legend_names[i]


if(i == 1) {age_length = preds
            age_length_low = growth_low
            age_length_high = growth_high
            }
else {age_length = rbind(age_length, preds)
     age_length_low = rbind(age_length_low, growth_low)
     age_length_high = rbind(age_length_high, growth_high)
}

}

 ggplot(qlbk %>% filter(InModel=="Included in base model"), aes(y = length_cm, x = age)) +
      geom_point(aes(y = length_cm, x = age, alpha = .5), ) + 
  geom_line(data = age_length, aes(y = length, x = Ages,  colour = factor(model_names)), linewidth = 2) +
  geom_line(data = age_length_high, aes(y = Length, x = Age, colour = factor(model_names)), linewidth = 1, linetype = 2) +
  geom_line(data = age_length_low, aes(y = Length, x = Age,  colour = factor(model_names)), linewidth = 1, linetype = 2) +
  theme_bw() +
  labs(colour = c("Legend")) +
  guides(colour = guide_legend(position  = "inside")) +
  scale_x_continuous(breaks = seq(0,60,10)) +
  scale_y_continuous(breaks = seq(0,55,5)) +
  theme(panel.grid.major = element_blank(), 
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 20),
         strip.text.y = element_text(size = 20),
        strip.text.x = element_text(size = 20),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        panel.grid.minor = element_blank(),
        legend.position.inside = c(0.6, 0.25)) +
  	xlab("Age (years)") + ylab("Length (cm)") +
   scale_color_viridis_d(begin = 0, end = .7) 

   ggsave(filename = file.path(sens_dir, new_name,"compare_growth_inmodel_only.jpg"),
       width = 16, height = 8)

}




       
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