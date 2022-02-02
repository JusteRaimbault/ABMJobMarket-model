setwd(paste0(Sys.getenv('CS_HOME'),'/UrbanDynamics/Models/ABMJobMarket-model'))

library(ggplot2)
library(dplyr)
source(paste0(Sys.getenv('CS_HOME'),'/Organisation/Models/Utils/R/plots.R'))

resprefix='CALIBRATION_20220201_161055'; generation='9800';
dir.create(paste0('analysis/results/',resprefix))

res <- as_tibble(read.csv(paste0('openmole/calibration/',resprefix,'/population',generation,'.csv'),stringsAsFactors = F))


g=ggplot(res[res$unemploymentError<0.05&res$informalityError<0.05,],aes(x=unemploymentError, y=informalityError, color=unemploymentShare, size=perceivedInformalityCoef))
ggsave(
  g+geom_point(alpha=0.6)+stdtheme,
  file=paste0('analysis/results/',resprefix,'/pareto-unemploymentError-informalityError_color-unemploymentShare_size-perceivedInformalityCoef.png'),
  width=27,height=20,units='cm' 
)


g=ggplot(res[res$unemploymentError<0.05&res$informalityError<0.05,],
         aes(x=unemploymentError, y=informalityError, color=socialNetworkCoef, size=perceivedInformalityCoef))
ggsave(
  g+geom_point(alpha=0.6)+stdtheme,
  file=paste0('analysis/results/',resprefix,'/pareto-unemploymentError-informalityError_color-socialNetworkCoef_size-perceivedInformalityCoef.png'),
  width=27,height=20,units='cm' 
)
