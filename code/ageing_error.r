################################################################################
#      Script to estimate ageing error for CA quillback rockfish 2025
#
#    Melissa Monk 3/11/25
#
#
#
################################################################################

rm(list=ls(all=TRUE))
graphics.off()
#install latest version of ageing error package
#pak::pkg_install("pfmc-assessments/AgeingError")

dir <- here("data-raw", "ageing_error")
setwd(dir)




#read in double reads
#Patrick McDonald and Jamie Hale 
dubreads <- read.csv(file.path(dir,"QLBK_Double_Reads.csv"))
