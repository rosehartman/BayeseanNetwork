
### Get some sample data and show the first few values
### Use `dat()` to get the active data set
library(bnlearn)
library(networkD3)
library(heatmaply)
library(visNetwork)


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
  dplyr::select(-SKT, -DaysX2, -`20mm`, -FallChla, -clams)


### Learn the structure of the network
dattest = select(datscaled, FallChla2, FallNDOI, FallTemp,logFMWT, Secchi, logzoops,
                 SprNDOI, logPred)
dag <- cextend(gs(dattest))


### Plot the force directed network
networkData <- data.frame(arcs(dag))

simpleNetwork(
  networkData,
  Source = "from",
  Target = "to"
)

### Print the network score

score(dag, dattest)


### Fit the model parameters and show the CPT for node A
fit = bn.fit(dag, dattest)
fit$FallChla2
fit$zoopBPUE
fit$secchi

### Plot the model parameters for node A

bn.fit.barchart(fit[["chla"]])


### Get the Markov blanket for node A
bnlearn::mb(dag, "A")

### Plot a d3 heatmap of the adjacency matrix
heatmaply(
  amat(dag),
  grid_gap = 1,
  colors = blues9,
  dendrogram = "both",
  symm = TRUE,
  margins = c(100, 100, NA, 0),
  hide_colorbar = TRUE
)

##############################################
#try creating teh network myself

plot.network <- function(structure, ht = "400px"){
  nodes.uniq <- unique(c(structure$arcs[,1], structure$arcs[,2]))
  nodes <- data.frame(id = nodes.uniq,
                      label = nodes.uniq,
                      color = "darkturquoise",
                      shadow = TRUE)
  edges <- data.frame(from = structure$arcs[,1],
                      to = structure$arcs[,2],
                      arrows = "to",
                      smooth = TRUE,
                      shadow = TRUE,
                      color = "black")
  return(visNetwork(nodes, edges, height = ht, width = "100%"))
}

# create an empty graph
structure <- empty.graph(c("logzoops","FallChla2" ,"FallNDOI","FallTemp","logFMWT",
                            "Secchi"))

# set relationships manually
modelstring(structure) <- "[logzoops|FallChla2:FallNDOI:FallTemp][FallChla2|FallNDOI][FallNDOI][FallTemp][logFMWT|logzoops:Secchi:FallTemp][Secchi|FallNDOI]"

# subset and fit

dattest = dplyr::select(datscaled, logzoops, FallChla2, FallNDOI, FallTemp, logFMWT, Secchi) %>%
  filter(!is.na(logFMWT))
bn.mod <- bn.fit(structure, data =dattest)
bn.mod

plot.network(structure)

cat("P(logFMWT >2 | FallTemp <20 and Secchi <5) =", cpquery(bn.mod, (logFMWT > 2), (FallTemp < 20 & Secchi < 5 )), "\n")

#what is the probability of a FMWT index greater than 20 when secchi is less than 50 and temp is greater than 15?
cpquery(bn.mod, (FMWTIndex > 20), (temp > 15 & secchi < 50 ))

#what is the probability of a FMWT index greater than 100 when secchi is less than 50 and temp is greater than 18?
cpquery(bn.mod, (FMWTIndex > 100), (temp > 18 & secchi < 50 ))

#what is the probability of a FMWT index greater than 100 when zoop biomass is less than 6000 and temp is greater than 18?
cpquery(bn.mod, (FMWTIndex > 100), (temp > 18 & zoopBPUE < 6000 ))

#what is the probability of a FMWT index greater than 100 when chla is less than 3and temp is greater than 18?
cpquery(bn.mod, (FMWTIndex > 100), (temp > 18 & chla < 3 ))

#Maybe instead of FMWTIndex above a certain level, it is more useful to test whether FMWT index is higher than the previous year.
#Or whether the ratio of SKT:FMWT index is big. 

#Could have fall temp versus summer temp or something. 
#mybe cut the last few years in "training the model"
#model validation or uncertainty
#structural equation modeling.
#Figure out how to test model accuracy and significance. Need to test model assumptions and distributions. 
#lavaan - structural equation modeling. 

#Not all impacts of non-native species are negative. 

#https://www.bnlearn.com/examples/xval/

#Loss is predictions are computed from an arbitrary set of nodes using likelihood weighting to obtain Bayesian posterior estimates.
eval = bn.cv(data = dattest, bn = structure, loss = "cor-lw", loss.args = list(target = "logFMWT"))
loss(eval)

