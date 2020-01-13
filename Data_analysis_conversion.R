##12/27/2018##
##Dataset is finally cleaned. Pulling in the master data

setwd("C:/Users/dlars/Dropbox/R_Clackamas/Dissertation/Ch.2")
mastrdta = read.csv("ch2master_current.csv")
dta = mastrdta

##Now run some summary analysis and print to a table##
##Strike that previous command#


##Will need to do an EFA/PCA on all water conservation belief variables to see which ones hang together 
## to compute into one variable.

require(dplyr)
library(tidyverse)

dta =  dta %>%
  mutate(sqm_prop = GIS_ACRES * 4046.86)

dta = dta %>%
  mutate(litrJAN = ((JAN * 2831.685)/31))

dta = dta %>%
  mutate(litrFEB = ((FEB * 2831.685)/28))

dta = dta %>%
  mutate(litrMAR = ((MAR * 2831.685)/31))

dta = dta %>%
  mutate(litrAPR = ((APR * 2831.685)/30))

dta = dta %>%
  mutate(litrMAY = ((MAY * 2831.685)/31))

dta = dta %>%
  mutate(litrJUN = ((JUNE * 2831.685)/30))

dta = dta %>%
  mutate(litrJUL = ((JULY * 2831.685)/31))

dta = dta %>%
  mutate(litrAUG = ((AUG * 2831.685)/31))

dta = dta %>%
  mutate(litrSEPT = ((SEPT * 2831.685)/30))

dta = dta %>%
  mutate(litrOCT = ((OCT * 2831.685)/31))

dta = dta %>%
  mutate(litrNOV = ((NOV * 2831.685)/30))

dta = dta %>%
  mutate(litrDEC = ((DEC * 2831.685)/31))

dta = dta %>%
  mutate(Annual_con_litr = Total_Annual_CCF * 2831.685)

dta = dta %>%
  mutate(Annual_con_litr_day = Annual_con_litr/365)

###The above variable was calculated incorrectly###################


dta = dta %>%
  mutate(Winter_litr_tot = Winter_Total* 2831.685)

dta = dta %>%
  mutate(Winter_con_litr_day = Winter_litr_tot/59)

dta = dta %>%
  mutate(Summer_litr_tot = Summer_Total* 2831.685)

dta = dta %>%
  mutate(Summer_con_litr_day = Summer_litr_tot/62)

#winav = dta %>%
 # summarise(avg = mean(Winter_con_litr_day))

#sumrav = dta %>%
 # summarise(sumrav, avg = mean(Summer_con_litr_day))

##The above is bunk dplyr code##

#> duration = faithful$eruptions     # the eruption durations 
#> mean(duration)                    # apply the mean function 

Summer_av_cons = dta$Summer_con_litr_day
Summer_avr_cons=mean(Summer_av_cons)

dta$Summer_avr_cons <- Summer_avr_cons

Winter_av_cons = dta$Winter_con_litr_day
Winter_avr_cons=mean(Winter_av_cons)

dta$Winter_avr_cons <- Winter_avr_cons


##Now do EFA to determine variables for pro-conservation##

watercon<-dta[,c(52:65)]


watercon2<- watercon[complete.cases(watercon), ]

library(psych)
#library(GPArotation) #Package does not exist##

nofactors = fa.parallel(watercon2, fm="ml", fa="fa")


round1 = fa(watercon2, nfactors=1, rotate = "oblimin", fm = "ml")
round1
round2 = fa(watercon2, nfactors=2, rotate = "oblimin", fm = "ml")
round2
round3 = fa(watercon2, nfactors=3, rotate = "oblimin", fm = "ml")
round3
round4 = fa(watercon2, nfactors=4, rotate = "oblimin", fm = "ml")
round4

#####################################################################
##RMSEA and RMSR are looking good for the 4 factor solution <0.06
##TLI is accpetable greater than 0.9##
########################################################################

watercon3<-watercon2[,-c(4,5,15)]

round5 = fa(watercon3, nfactors=4, rotate = "oblimin", fm = "ml")
round5

round6 = fa(watercon3, nfactors=3, rotate = "oblimin", fm = "ml")
round6

##RMSEA AND RMSR are acceptable for 3 factor solution between 0.6-0.8. TLI is bad, below .90 for 3 factor.
watercon3<-watercon3[,-c(12)]

round7 = fa(watercon3, nfactors=2, rotate = "oblimin", fm = "ml")
round7

watercon4<-watercon3[,-c(4,10)]
nofactors2 = fa.parallel(watercon4, fm="ml", fa="fa")
round8 = fa(watercon4, nfactors=3, rotate = "oblimin", fm = "ml")
round8

######################################################################
##I beleive this is the best solution with a 3 factor model.
#####################################################################
##RMSEA and RMSR are looking good for the 3 factor solution <0.06
##TLI is accpetable greater than 0.9##
########################################################################
factor1 = c(1:3)
factor2 = c(4:7)
factor3 = c(8,9)
alpha(watercon4 [ , factor1], check.keys = TRUE)
alpha(watercon4 [ , factor2], check.keys = TRUE)
alpha(watercon4 [ , factor3], check.keys = TRUE)

##So alpha is good for the first factor but not the second factor##

round9 = fa(watercon4, nfactors=1, rotate = "oblimin", fm = "ml")
round9

##################### 12/30/2018 ########################################################

##Creating Water Conservation Variables 12/30/18##

dta$pro_water_con <- rowSums(dta[, c('Water.Conserv.App', 'Water.Conserv.Daily','Water.Conserv..Understnd','Water.Conserv.PgrmSign','Water.Conserv.PgrmNeigh','Lawn.Native','Lawn.Drought') ], na.rm=TRUE) * ifelse(
  rowSums(is.na(dta[, c('Water.Conserv.App', 'Water.Conserv.Daily','Water.Conserv..Understnd','Water.Conserv.PgrmSign','Water.Conserv.PgrmNeigh','Lawn.Native','Lawn.Drought') ])) == 
    ncol(dta[, c('Water.Conserv.App', 'Water.Conserv.Daily','Water.Conserv..Understnd','Water.Conserv.PgrmSign','Water.Conserv.PgrmNeigh','Lawn.Native','Lawn.Drought') ]), NA, 1)

dta$pro_water_con<-(dta$pro_water_con/7)


##Creating Conservation Variables from EFA ############

dta$water_conscious <- rowSums(dta[, c('Water.Conserv.Daily', 'Water.Conserv..Understnd','Water.Conserv.PgrmSign') ], na.rm=TRUE) * ifelse(
  rowSums(is.na(dta[, c('Water.Conserv.Daily', 'Water.Conserv..Understnd','Water.Conserv.PgrmSign') ])) == 
    ncol(dta[, c('Water.Conserv.Daily', 'Water.Conserv..Understnd','Water.Conserv.PgrmSign') ]), NA, 1)

dta$water_conscious<-(dta$water_conscious/3)

dta$lawn_obligations <- rowSums(dta[, c('Lawn.Pressure', 'Lawn.HOA','Lawn.Grass','Lawn.Native') ], na.rm=TRUE) * ifelse(
  rowSums(is.na(dta[, c('Lawn.Pressure', 'Lawn.HOA','Lawn.Grass','Lawn.Native') ])) == 
    ncol(dta[, c('Lawn.Pressure', 'Lawn.HOA','Lawn.Grass','Lawn.Native') ]), NA, 1)

dta$lawn_obligations<-(dta$lawn_obligations/4)

dta$conservative_lawn_watering <- rowSums(dta[, c('Lawn.Brown', 'Lawn.Summer') ], na.rm=TRUE) * ifelse(
  rowSums(is.na(dta[, c('Lawn.Brown', 'Lawn.Summer') ])) == 
    ncol(dta[, c('Lawn.Brown', 'Lawn.Summer') ]), NA, 1)

dta$conservative_lawn_watering<-(dta$conservative_lawn_watering/2)

###################################################################
####### ---------->   Now export master dataset <-----------------------

write.csv(dta, file = "CH2_Master_Allvar.csv")


#############################################################################################
# ----------> Now subset and create analysis dataset with conversions to metric <-----------------------
dta2<-dta
dta2 = dta2[,-c(3:25,27:34,85,86,90:97)]

write.csv(dta2, file = "CH2_Master_subset_analysis.csv")


#############################################################################################
# ----------> Fixing some errors on 1/8/19. Based on what Heejun pointed out. <-----------------------

mastrdta = read.csv("CH2_Master_subset_analysis.csv")
dta = mastrdta
require(dplyr)
dta %>% select(litrJAN:litrDEC) %>% rowSums(na.rm=TRUE) -> dta$Annual_con_litr_perday

###The above dplyr code is fucken siiiiiicccckkkkkk!!!!!##########

dta = dta %>%
  mutate(Winter_con_litr_perday = Winter_con_litr_day/2)


dta = dta %>%
  mutate(Summer_con_litr_perday = Summer_con_litr_day/2)

dta = dta %>%
  mutate(Annual_avg_monthly_con_litr_perday = Annual_con_litr_perday/12)

dta = dta %>%
  mutate(Annual_con_control_prop_size = Annual_avg_monthly_con_litr_perday/sqm_prop)

dta = dta %>%
  mutate(water_savings = Winter_con_litr_perday-Summer_con_litr_perday)

dta = dta %>%
  mutate(water_savings = Winter_con_litr_perday-Summer_con_litr_perday)

dta = dta %>%
  mutate(water_savings2 = water_savings*-1)

dta = dta %>% 
  select (-c(Annual_con_litr_day, Winter_con_litr_day, Summer_con_litr_day))

dta = dta %>% 
  select (-c(water_savings))

write.csv(dta, file = "CH2_Master_subset_analysis_current.csv")

dta = read.csv("CH2_Master_subset_analysis_current.csv")
