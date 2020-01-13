###Creating indexes of multi-item variables and joining water use data##
##11/12/2018##
setwd("C:/Users/dlars/Dropbox/R_Clackamas/Dissertation/Ch.2")
##Now it's time to compute an index for Water Conservation Attitudes 11/12/18#

master3$waterconindex <- rowSums(master3[, c('Water..Conserv..Device', 'Water..Conserv..App','Water..Conserv..Daily','Water..Conserv..Understnd','Water..Conserv..PgrmSign','Water..Conserv..PgrmNeigh') ], na.rm=TRUE) * ifelse(
  rowSums(is.na(master3[, c('Water..Conserv..Device', 'Water..Conserv..App','Water..Conserv..Daily','Water..Conserv..Understnd','Water..Conserv..PgrmSign','Water..Conserv..PgrmNeigh') ])) == 
    ncol(master3[, c('Water..Conserv..Device', 'Water..Conserv..App','Water..Conserv..Daily','Water..Conserv..Understnd','Water..Conserv..PgrmSign','Water..Conserv..PgrmNeigh') ]), NA, 1)

master3$waterconindex<-(master3$waterconindex/6)

##Now it's time to compute an index for Lawn Conservation Attitudes 11/12/18#

master3$lawnconindex <- rowSums(master3[, c('Lawn..Pressure', 'Lawn..HOA','Lawn..Grass','Lawn..Native','Lawn..Brown') ], na.rm=TRUE) * ifelse(
  rowSums(is.na(master3[, c('Lawn..Pressure', 'Lawn..HOA','Lawn..Grass','Lawn..Native','Lawn..Brown') ])) == 
    ncol(master3[, c('Lawn..Pressure', 'Lawn..HOA','Lawn..Grass','Lawn..Native','Lawn..Brown') ]), NA, 1)

master3$lawnconindex<-(master3$waterconindex/5)

##Should probably export a CSV at this point##
master3 <- master3[-c(12:19)]

write.csv(master3, file = "ch2_allvar_all_resp.csv")

##Now left join the water consumption data##
cons<-read.csv('Cons_sum_miss.csv')
require(dplyr)
master3 = master3 %>% 
  left_join(cons, by = 'ID.x')

master3 <- master3[-c(74)]

write.csv(master3, file = "ch2_allvar_all_resp_cons.csv")

##11/14/2018 Adding the full monthly data as per Heejuns suggestion##
##Still awaiting the data missing issue with Sunrise.
#Will append this data and begin analysis for lab meet on 11/16/18##
##I can always go back reappend everything and run the analysis again. I don't think the added respondents will make much of a difference statistically.

annualcons<-read.csv('Cons_Data_All_Districts.csv')
master4 = master3 %>% 
  left_join(annualcons, by = 'ID.x')

master4 <- master4[-c(74:78)]
write.csv(master4, file = "ch2_allvar_all_resp_cons_allmonths.csv")
