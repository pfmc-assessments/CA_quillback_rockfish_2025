#Copy figures from the pfoeils and retros over to the figures folder


base_model <- '5_1_3_preStarBase'
doc_figures_folder <- here('report','figures')

#jitters
jitter_fig <- "jitter.png"
file.copy(from = here('models', glue::glue(base_model, '_jitter_0.5'), jitter_fig),
          to = file.path(doc_figures_folder, jitter_fig), 
          overwrite = TRUE, recursive = FALSE)



#retro figs
retro_figs <- c('retro_percent_difference_4_panel.png', 'compare2_spawnbio_uncertainty.png', 'compare4_Bratio_uncertainty.png')
for (a in length(retro_figs)){
file.copy(from = here('models', glue::glue(base_model, '_retro_15_yr_peel'), retro_figs[a]),
          to = file.path(doc_figures_folder, retro_figs[a]), 
          overwrite = TRUE, recursive = FALSE)
}

#profile M
Mname <- 'NatM_uniform_Fem_GP_1'
M_profile_figs <- c(glue::glue('piner_panel_', Mname,'.png'), 
                    glue::glue(Mname, '_trajectories_compare3_Bratio.png'),
                    glue::glue(Mname,'_trajectories_compare1_spawnbio.png'))
for (a in length(M_profile_figs)){
file.copy(from = here('models', glue::glue(base_model, '_profile_', Mname), M_profile_figs[a]),
          to = file.path(doc_figures_folder, M_profile_figs[a]), 
          overwrite = TRUE, recursive = FALSE)
}

#profile h
hname <- 'SR_BH_steep'
h_profile_figs <- c(glue::glue('piner_panel_', hname,'.png'), 
                    glue::glue(hname, '_trajectories_compare3_Bratio.png'),
                    glue::glue(hname,'_trajectories_compare1_spawnbio.png'))
for (a in length(h_profile_figs)){
file.copy(from = here('models', glue::glue(base_model, '_profile_', hname), h_profile_figs[a]),
          to = file.path(doc_figures_folder, h_profile_figs[a]), 
          overwrite = TRUE, recursive = FALSE)
}

#profile SR_LN
R0name <- 'SR_LN(RO)'
R0_profile_figs <- c(glue::glue('piner_panel_', R0name,'.png'), 
                    glue::glue(R0name, '_trajectories_compare3_Bratio.png'),
                    glue::glue(R0name,'_trajectories_compare1_spawnbio.png'))
for (a in length(R0_profile_figs)){
file.copy(from = here('models', glue::glue(base_model, '_profile_', R0name), R0_profile_figs[a]),
          to = file.path(doc_figures_folder, R0_profile_figs[a]), 
          overwrite = TRUE, recursive = FALSE)
}

