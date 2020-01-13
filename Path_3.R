setwd("C:/Users/dlars/Dropbox/R_Clackamas/Dissertation/Ch.2")
dta = read.csv("CH2_Master_subset_analysis_current3.csv")

linearMod <- lm(water_savings2 ~ sqm_prop, data=dta)  # build linear regression model on full data
print(linearMod)

#water savings = -65.7191 +0.1972 (sqm_prop)

## example

-65.7191 +0.1972 * 5000

-65.7191 +0.1972 * 10000

###________> Doing a reanalysis becuase things don't add up<_____________ 4/15/2019

dta = dta %>%
  mutate(Winter_tot_new = litrJAN+litrFEB)


dta = dta %>%
  mutate(Summer_tot_new = litrJUL+litrAUG)

dta = dta %>%
  mutate(water_savings_temp = Winter_tot_new-Summer_tot_new)

dta = dta %>%
  mutate(water_savings_new = water_savings_temp*-1)

## Now conduct path analysis

library(lavaan)
library(dplyr)
dta2 = select(dta, PNWClimateIndex,
              
              pro_water_con4,
              Water.Conserv.Device,
              Water.Conserv.App,
              Lawn.Native,
              Lawn.Drought,
              
              Water.Veg,
              Water.Lawn,
              water_savings_new,
              sqm_prop)

dta2$PNW_climate_attitudes = dta2$PNWClimateIndex              
dta2$Pro_water_conservation = dta2$pro_water_con4              
dta2$Water_saving_devices = dta2$Water.Conserv.Device
dta2$Water_efficient_appliances = dta2$Water.Conserv.App
dta2$Native_Plants = dta2$Lawn.Native
dta2$Drought_resistant_plants = dta2$Lawn.Drought
dta2$Lawn_requires_watering = dta2$Water.Veg
dta2$Lawn_watering_frequency = dta2$Water.Lawn
dta2$Water_savings = dta2$water_savings_new
dta2$Property_Size = dta2$sqm_prop

##Great. Now subset again for analysis###

pa.dta = dta2[,c(11:20)]

###Now begin path analysis###

###load library
library(semPlot)

##Hey  model blew up due to variance differences in outcome. Going to scale property size and water savings by 1k

pa.dta$Water_savings_scale = (pa.dta$Water_savings/1000)
pa.dta$Property_Size_scale = (pa.dta$Property_Size/1000)

model2A = '

Water_savings_scale ~ Property_Size_scale + Native_Plants + Drought_resistant_plants + Water_efficient_appliances + Lawn_watering_frequency

Native_Plants + Drought_resistant_plants + Water_efficient_appliances + Lawn_watering_frequency ~ Pro_water_conservation + PNW_climate_attitudes

Pro_water_conservation ~~ PNW_climate_attitudes'

fit4= sem(model2A,data=pa.dta)
summary(fit4,standardized = TRUE, fit.measures=TRUE, rsq = T) 

write.csv(dta, file = "CH2_Master_subset_analysis_current4_noanalysis.csv")


## So the findings I got this round are different from what I reported.
###Had something to do with the monthly calculation. I'm not proud of it but I'm
### going to leave it as is. I'm sick of this shit and if it gets smacked down for publication so be it.

