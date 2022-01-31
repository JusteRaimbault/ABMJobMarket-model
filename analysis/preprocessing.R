
setwd(paste0(Sys.getenv("CS_HOME"),"/UrbanDynamics/Models/ABMJobMarket-model"))

library(readstata13)
library(reshape2)
library(dplyr)

datadir = paste0(Sys.getenv("CS_HOME"),"Misc/BiblioDocs/UrbanDynamics/ABMJobs/STATA_20220128/DATA/Cleaning/")


######
# Workers data

workers <- read.dta13(paste0(datadir,"Leverhulme_worker_clean.dta"))

columns = c("works","income","hours_worked","time_job","social_security_cat","insurance","contract","nation_HH","permit")

for(c in columns){
  workers[,c] <- as.character(workers[,c])
  workers[is.na(workers[,c]),c] = "0"
}

write.csv(workers[,columns], file = paste0(datadir,"worker_DCE.csv"),row.names = F, quote = F)


######
# Jobs data

employers <- read.dta13(paste0(datadir,"Leverhulme_employer_clean.dta"))
dcetasks <- read.dta13(paste0(datadir,"dce_tasks.dta"))

employer_choices = reshape(employers, direction="long", varying=paste0("task",1:18),v.names=c("choice"), times=1:18, timevar="task")

choices = left_join(employer_choices,dcetasks, by = c("task"="task"))
choices$choice = as.numeric(choices$choice)

jobs = choices %>% 
       group_by(dce_salary, dce_diversity, dce_hours, dce_skills, 
                dce_social_security, dce_insurance, dce_contract) %>%
       summarise(choice=mean(choice))

write.csv(jobs,file=paste0(datadir, "jobs.csv"),row.names = F, quote = F)

