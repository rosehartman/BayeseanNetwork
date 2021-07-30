#Let's try a structural equation modeling

library(lavaan)
library(tidyverse)
library(readxl)
library(tidySEM)
library(car)

dat <- read_excel("FLOATdataset.xlsx", sheet = "FLOATlong term", na = "NA")

#create a new variable for population grouth
dat = mutate(dat, lagFMWT = lag(FMWT), leadSTN = lead(STN), leadFMWT = lead(FMWT),
             logFMWT = log(FMWT +1),
             growth = FMWT/lagFMWT, 
             loggrow = log(growth),
             growth2 = FMWT/STN,
             loggrow2 = log(growth2),
             growth3 = leadSTN/FMWT,
             loggrow3 = log(growth3),
             growth4 = leadFMWT/FMWT,
             loggrow4 = log(growth4),
             logPred = log(Predators),
             logzoops = log(FallZoops),
             clams = Clams,
             Clams = NULL) %>%
  rename(Secchi = `Turb (Fall Secchi)`) %>%
  filter(Year >1974)

#scale all the variable so they work better
datscaled = mutate(dat, across(SprNDOI:logzoops, ~scale(.x))) %>%
  dplyr::select(-SKT, -DaysX2, -`20mm`)

vifdat = filter(datscaled, !is.na(FMWT), Year <2010) %>%
  dplyr::select(-growth, -loggrow, -leadFMWT, -FallChla, -growth4, -loggrow4, -lagFMWT)
vif(as.data.frame(vifdat))

t1 = lm(logFMWT ~ SprNDOI+ FallNDOI + FallX2 + FallChla2 + SpX2 + SummerTemp + 
          logzoops + FallTemp + Secchi + logPred + clams, 
        data = datscaled)

summary(t1)
vif(t1)

#very high variance inflation factor for X2, proably too highly correlated with FallNDOI
t2 = lm(logFMWT ~ SprNDOI+ FallNDOI + FallChla2 + SummerTemp + 
          logzoops + FallTemp + Secchi + logPred + clams, 
        data = datscaled)

summary(t2)
vif(t2)

#fall chla-a and clams also highly colinear
t3 = lm(logFMWT ~ SprNDOI+ FallNDOI + FallChla2 + SummerTemp + 
          logzoops + FallTemp + Secchi + logPred, 
        data = datscaled)

summary(t3)
vif(t3)
#I think this just means I can't have NDOI and X2 in the same model, and I can't
#have Chlorophyll and clams in the same model. But I think I can use NDOI to predict
#X2 and I can use clams to predict chlorophyll if i want.

#definte the model structure
# I created a latent variable for "smelt conditions" (smelt)
# plus one for "smelt habitat", but I"m really just playing around. 
smeltmod = 'smelt =~ loggrow +Secchi + FallTemp 
Secchi ~ FallNDOI
loggrow ~ logzoops
logzoops ~ FallChla2'

smeltmod2 = 'smelt =~ loggrow + logFMWT + Secchi
logzoops ~ FallChla2
loggrow ~ logzoops + logPred+ FallTemp + FallNDOI'

#Calculate the fit
fit <- sem(smeltmod, data = datscaled)
fit2 <- sem(smeltmod2, data = datscaled)
summary(fit, standardized = TRUE)
varTable(fit)
AIC(fit)
AIC(fit2)
# compute the model-implied slope
implied.cor = lavInspect(fit, what="cor.ov")
implied.cov = lavInspect(fit, what="cov.ov")
stdev_ov = sqrt(diag(implied.cov))
estimated.slope = implied.cor["FMWT","FallZoops"]*(stdev_ov["FallZoops"]/stdev_ov["FMWT"])

#plot the model implied slope versus the actual slope
ggplot(data=datscaled, aes(x=FMWT,y=FallZoops)) +
  geom_point() +
  geom_abline(slope = estimated.slope, intercept=0, col="red") +
  geom_smooth(method="lm") +
  labs(x="FMWT index (scaled)", y="Zooplankton biomass (scaled)") +
  theme_bw()

#So the model-implied slope is alittle idfferent than just the graph. 

#Let's look at it visually

graph_sem(fit)
get_nodes(fit)
get_edges(fit)



###################################################33
#Try to do one as close as possible to the netowork diagram
mod2 = 'logFMWT~ FallTemp + logzoops + Secchi
Secchi ~ FallNDOI
FallChla2 ~ FallNDOI + clams
logzoops ~ FallNDOI +  FallChla2'

#is there anything equivelent to a variance inflation factor
#we could use to eliminate factors?

#we could also put in clams.\

#send code to Gonzalo.

#put dates on my calendar - finish this by end of aughts
#send out smelt conditions report ot everyone else
#five page summary by end of the year.

#Calculate the fit
fit2 <- sem(mod2, data = datscaled, fixed.x = FALSE)
summary(fit2, standardized = TRUE)
varTable(fit2)
graph_sem(fit2)
get_nodes(fit2)
get_edges(fit2)
fit2results = table_results(fit2)
AIC(fit2)


ggplot(datscaled, aes(x = loggrow3, y = SummerTemp)) + geom_point()
ggplot(datscaled, aes(x = loggrow, y = FallTemp)) + geom_point()
ggplot(datscaled, aes(y = loggrow3, x = FallX2)) + geom_point() + geom_smooth(method = "lm")
ggplot(datscaled, aes(y = loggrow, x = FallX2)) + geom_point() + geom_smooth(method = "lm")

ggplot(datscaled, aes(x = Year, y = loggrow3)) + geom_point()

###############################################################
#Short-term FLOAT analysis

datshort <- read_excel("FLOATdataset.xlsx", sheet = "FLOATshort time", na = "NA")

#create a new variable for population grouth
datshort = mutate(datshort,  growthSKT = FMWT/SKT) %>%
  rename(Secchi = `Turb (Fall Secchi)`) 

#scale all the variable so they work better
datscshort = mutate(datshort, across(SprNDOI:growthSKT, ~scale(.x))) %>%
  filter(!is.na(Condition))

#check for colinearity (probably hard with such a short data set)
s1 = lm(Condition ~ Secchi + FallZoops + FallTemp + Microcystis + FallChla + FallNDOI, 
        data = datscshort)
summary(s1)
vif(s1)
#so, there are issues
s2 = lm(Condition ~  FallTemp +  FallZoops + FallChla + Secchi, 
        data = datscshort)
summary(s2)
vif(s2)
#Huh. So Fall Temp and Fall zoops are highly colinear. So is secchi and NDOI? 
#That is unfortunate.

modsh = 'Condition ~ Secchi + FallZoops + FallTemp
Secchi ~ FallNDOI
FallZoops ~ FallChla + FallNDOI'


#Calculate the fit
fitsh <- sem(modsh, data = datscshort)

summary(fitsh, standardized = TRUE)
varTable(fitsh)
graph_sem(fitsh)
get_nodes(fitsh)
get_edges(fitsh)
fitSHresults = table_results(fitsh)
