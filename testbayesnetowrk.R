
### Get some sample data and show the first few values
### Use `dat()` to get the active data set
library(bnlearn)

dat <- read.csv("testdata.csv")
head(dat)

### Learn the structure of the network

dag <- cextend(bnlearn::gs(dat))


### Plot the force directed network
networkData <- data.frame(bnlearn::arcs(dag))

networkD3::simpleNetwork(
  networkData,
  Source = "from",
  Target = "to"
)

### Print the network score

bnlearn::score(dag, dat)


### Fit the model parameters and show the CPT for node A
fit = bn.fit(dag, dat)
fit$chla


### Plot the model parameters for node A

bnlearn::bn.fit.barchart(fit[["chla"]])


### Get the Markov blanket for node A
```{r}
bnlearn::mb(dag, "A")
```

### Plot a d3 heatmap of the adjacency matrix
```{r}
heatmaply::heatmaply(
  bnlearn::amat(dag),
  grid_gap = 1,
  colors = blues9,
  dendrogram = "both",
  symm = TRUE,
  margins = c(100, 100, NA, 0),
  hide_colorbar = TRUE
)
````

### Generate some random data from the network and show the first few values
```{r}
set.seed(1)
simData <- bnlearn::rbn(fit, n = 100, dat)
head(simData)
```

```{r}
# Put your own code here...
