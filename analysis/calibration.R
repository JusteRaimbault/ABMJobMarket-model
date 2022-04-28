setwd(paste0(Sys.getenv('CS_HOME'),'/UrbanDynamics/Models/ABMJobMarket-model'))

library(ggplot2)
library(dplyr)
source(paste0(Sys.getenv('CS_HOME'),'/Organisation/Models/Utils/R/plots.R'))

resprefix='CALIBRATION_20220201_161055'; generation='9800';
#resprefix='CALIBRATION_20220207_150221'; generation='21000'
dir.create(paste0('analysis/results/',resprefix))

res <- as_tibble(read.csv(paste0('openmole/calibration/',resprefix,'/population',generation,'.csv'),stringsAsFactors = F))

resm = res[res$unemploymentError<0.01&res$informalityError<0.01,]
mean(resm$socialNetworkCoef); sd(resm$socialNetworkCoef)
mean(resm$perceivedInformalityCoef); sd(resm$perceivedInformalityCoef)

resf = res[res$objective.unemploymentError<0.1&res$objective.informalityError<0.1&res$evolution.samples>=20,]

g=ggplot(resf,
         aes(x=objective.unemploymentError, y=objective.informalityError, color=unemploymentShare, size=perceivedInformalityCoef))
ggsave(
  g+geom_point(alpha=0.6)+stdtheme,
  file=paste0('analysis/results/',resprefix,'/pareto-unemploymentError-informalityError_color-unemploymentShare_size-perceivedInformalityCoef.png'),
  width=27,height=20,units='cm' 
)


g=ggplot(resf,
         aes(x=objective.unemploymentError, y=objective.informalityError, color=socialNetworkCoef, size=perceivedInformalityCoef))
ggsave(
  g+geom_point(alpha=0.6)+stdtheme,
  file=paste0('analysis/results/',resprefix,'/pareto-unemploymentError-informalityError_color-socialNetworkCoef_size-perceivedInformalityCoef.png'),
  width=27,height=20,units='cm' 
)

# values of coeff parameters for smallest 
summary(resf[resf$objective.unemploymentError<0.01&resf$objective.informalityError<0.01,])


## regressions to link params to errors
# rq: macro values have large confidence intervals -> does not make sense to have a precise calib here? PSE would be better?

lm_aggr_informality = lm(data=res,formula = objective.informalityError ~ unemploymentShare + workPermitShare + jobSeekingNumber + perceivedInformalityCoef + jobSimilarityHierarchy + socialNetworkCoef + socialNetworkHierarchy + socialNetworkMode)
summary(lm_aggr_informality) # nothing significant

lm_aggr_unemployment = lm(data=res,formula = objective.unemploymentError ~ unemploymentShare + workPermitShare + jobSeekingNumber + perceivedInformalityCoef + jobSimilarityHierarchy + socialNetworkCoef + socialNetworkHierarchy + socialNetworkMode)
summary(lm_aggr_unemployment) # unemp share and job seeking number signif (as expected?)






