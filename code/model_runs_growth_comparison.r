###############################################################
## This function plots the growth curve estimated from multiple 
## stock synthesis runs

plot_compare_growth <- function(models = xx,
                                new_name = "",
                                legend_names = c("base","Request 8"),
                                max_age = 60) {

#Figure out how many runs you're looking at
 n_runs <- length(xx)

#Vector of ages
ages <- seq(0, max_age, by = 0.2)
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
preds$model_names = rep(legend_names[i],length(ages))
              preds$length <- vb_fn(ages, 
                             Linf = xx[[i]]$Growth_Parameters$L_a_A2,
                             L1 = xx[[i]]$Growth_Parameters$L_a_A1,
                             k = xx[[i]]$Growth_Parameters$K)
if(i == 1) {age_length = preds}
else {age_length = rbind(age_length, preds)}

}


ggplot(age_length, aes(y = length, x = Ages, colour = factor(model_names))) +
  geom_line(data = age_length, aes(y = length, x = Ages,
            colour = factor(model_names)), linewidth = 2) +
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
        legend.position.inside = c(0.8, 0.25)) +
  	xlab("Age (years)") + ylab("Length (cm)") +
   scale_color_viridis_d(begin = 0, end = .7) 

   ggsave(filename = file.path(sens_dir, new_name,"compare_growth.jpg"),
       width = 16, height = 8)

}
