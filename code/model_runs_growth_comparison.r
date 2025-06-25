###############################################################
## This function plots the growth curve estimated from multiple 
## stock synthesis runs


xx <- SSgetoutput(dirvec = c(glue::glue("{models}/{subdir}", models = here('models'),
                                        subdir = c(base_mod_name,
                                                   file.path('_sensitivities', 'STAR_request8_growthselex')))))
dim(xx)


ages <- seq(0, 60, by = 0.2)
vb_fn <- function(age, Linf, L1, k) {
    #vec <- Linf * (1 - exp( -k * (age - t0)))
    vec <- Linf - (Linf - L1) * exp(-age * k)
    return(vec)
}
preds3 <- data.frame(ages,
                     fit = vb_fn(ages,  Linf = 42.7486, L1 = 9.80221 , k = 0.126145))


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