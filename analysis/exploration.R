
setwd(paste0(Sys.getenv('CS_HOME'),'/UrbanDynamics/Models/ABMJobMarket-model'))

library(ggplot2)
library(readr)
library(dplyr)


source(paste0(Sys.getenv('CS_HOME'),'/Organisation/Models/Utils/R/plots.R'))


####
# Single run - old model version

library(reshape2)

resprefix = '20210830_174307_EXPLORATION_REPLICATIONS' #! no id / replication

res <- as.tbl(read.csv(paste0('openmole/exploration/',resprefix,'.csv')))
resdir <- paste0('analysis/results/',resprefix,'/');dir.create(resdir,recursive = T)

data <- melt(res,id.vars = c("unemploymentShare","workersNumber","jobSeekingNumber","jobsNumber","perceivedInformalityCoef"),
             variable.name = "var"
             )
data$varname = sapply(strsplit(as.character(data$var),split = ".",fixed=T),function(l){l[1]})
data$time = sapply(strsplit(as.character(data$var),split = ".",fixed=T),function(l){as.numeric(l[2])})
data$id = paste0(data$unemploymentShare,data$workersNumber,data$jobSeekingNumber,data$jobsNumber,data$perceivedInformalityCoef)

#g = ggplot(data[data$varname=="unemployment"&data$workersNumber==100&data$jobsNumber==200,],aes(x=time,y=value,group=id,color = perceivedInformalityCoef))
#g+geom_point(pch=".")+facet_grid(unemploymentShare~jobSeekingNumber,scales="free")
#g+geom_smooth()+facet_grid(unemploymentShare~jobSeekingNumber,scales="free")
# not readable -> aggregate with average

sdata = data %>% group_by(unemploymentShare,workersNumber,jobSeekingNumber,jobsNumber,perceivedInformalityCoef,time,varname) %>% summarise(average=mean(value))#,unemploymentShare=mean(unemploymentShare),workersNumber=mean(workersNumber),jobSeekingNumber=mean(jobSeekingNumber),jobsNumber=mean(jobsNumber),perceivedInformalityCoef=mean(perceivedInformalityCoef))

workersNumber=100;perceivedInformalityCoef=1.0
g=ggplot(sdata[sdata$varname=="unemployment"&sdata$workersNumber==workersNumber&sdata$perceivedInformalityCoef==perceivedInformalityCoef,],aes(x=time,y=average,color = jobsNumber))
g+geom_line()+facet_grid(unemploymentShare~jobSeekingNumber,scales="free")+ylab("Unemployment")+xlab("Time")+stdtheme
ggsave(file=paste0(resdir,'unemployment-time_workersNumber',workersNumber,'_perceivedInformalityCoef',perceivedInformalityCoef,'_color-jobsNumber_facet-unemploymentShare-jobSeekingNumber.png'),width=18,height=15,units='cm')

g=ggplot(sdata[sdata$varname=="informality"&sdata$workersNumber==workersNumber&sdata$perceivedInformalityCoef==perceivedInformalityCoef,],aes(x=time,y=average,color = jobsNumber))
g+geom_line()+facet_grid(unemploymentShare~jobSeekingNumber,scales="free")+ylab("Informality")+xlab("Time")+stdtheme
ggsave(file=paste0(resdir,'informality-time_workersNumber',workersNumber,'_perceivedInformalityCoef',perceivedInformalityCoef,'_color-jobsNumber_facet-unemploymentShare-jobSeekingNumber.png'),width=18,height=15,units='cm')



#######
# Stochasticity

indics = c("informality","unemployment")

resprefix = '20220201_150826_STOCHASTICITY'

res <- read_csv(paste0('openmole/exploration/',resprefix,'.csv'))
resdir <- paste0('analysis/results/',resprefix,'/');dir.create(resdir,recursive = T)

summary(res %>% group_by(id) %>% summarise(count=n()))

# histograms
#seed=42;set.seed(seed);ids = sample(unique(res$id),size = 20,replace = F)
# 24 points: can plot all
res$id = as.character(res$id)

for(indic in indics){
  g=ggplot(res,aes_string(x=indic,color="id",group="id"))
  g+geom_density()+stdtheme
  ggsave(file=paste(resdir,indic,'_distribution.png'),width=20,height=15,units='cm')
  
  g=ggplot(res[res$informality<0.9,],aes_string(x=indic,color="id",group="id"))
  g+geom_density()+stdtheme
  ggsave(file=paste(resdir,indic,'_distribution_cond-inf-0-9.png'),width=20,height=15,units='cm')
}

# sharpes
sres = res %>% group_by(id) %>% summarise(
  sdinformality=sd(informality),meaninformality=mean(informality),medianinformality=median(informality),sharpeinformality=abs(meaninformality/sdinformality),
  sdunemployment=sd(unemployment),meanunemployment=mean(unemployment),medianunemployment=median(unemployment),sharpeunemployment=abs(meanunemployment/sdunemployment)
)
summary(sres)

# rel distance
reldistance <- function(indic,sdindic){
  c(2*abs(matrix(rep(sres[[indic]],nrow(res)),nrow = nrow(res),byrow = T) - matrix(rep(sres[[indic]],nrow(res)),nrow = nrow(res),byrow = F))/(matrix(rep(sres[[sdindic]],nrow(res)),nrow = nrow(res),byrow = T) + matrix(rep(sres[[sdindic]],nrow(res)),nrow = nrow(res),byrow = F)))
}

summary(reldistance("meaninformality","sdinformality"))
summary(reldistance("medianinformality","sdinformality"))

summary(reldistance("meanunemployment","sdunemployment"))
summary(reldistance("medianunemployment","sdunemployment"))





