#################################
### KERNEL DENSITY ESTIMATION ###
#################################
library(vegan)
library(ks)

pca12<-species_scores[,1:2]
H <- Hpi(x=pca12)      # optimal bandwidth estimation
est<- kde(x=pca12, H=H, compute.cont=TRUE)     # kernel density estimation

# set contour probabilities for drawing contour levels
cl<-contourLevels(est, prob=c(0.5, 0.05, 0.001), approx=TRUE)
fit<-envfit(pca12,traits)

tiff(filename="figures/Figure3.tiff",units="in",width=4,height=4,res=600)
plot(est,cont=seq(1,100,by=1),display="filled.contour2",add=FALSE,ylab="PC2 (20.49%)",xlab="PC1 (28.33%)",cex.axis=0.75,las=1) 
plot(est,abs.cont=cl[1], labels=c(0.5),labcex=0.75, add=TRUE, lwd=0.75, col="grey30")
plot(est,abs.cont=cl[2], labels=c(0.95),labcex=0.75, add=TRUE, lwd=0.5, col="grey60")
plot(est,abs.cont=cl[3], labels=c(0.99),labcex=0.75, add=TRUE, lwd=0.5, col="grey60")
plot(fit, cex=0.90, col=1, labels=list(vectors = c("Osf","Ops", "Eps", "Bsh", "Bsf", "PFps","PFar","CPt","CFar","Frt","Fsf")))
points(pca12[,], pch=16, cex=0.25, col="black")
dev.off()



#################################
### Null modelling approaches ###
#################################
library(ade4)
library(geometry)
source("utils.R")

summary(PCA_traits)
pc <- species_scores[,1:2]
pc95 <- subselect.data(pc, 0.95)
blob95 <- convhulln(pc95,"FA")
obs.vol95 <- blob95$vol

# Díaz et al. (2015) apply this model using the original data on plant traits. Using all
# the 11 traits measured herein would take too long, so I opted to run it using the
# first 2 axes of the PCA, which explain nearly 90% of the ecomorphological variation.

###################################################
###              Null Model #1
###################################################
npermute <- 999
sim.vol1.95 <- rep(0,npermute)
for(i in 1:npermute){
  newpc <- (apply(pc, 2, function(x) runif(nrow(pc), min = min(x), max = max(x))))
  newpc95 <- subselect.data(newpc, 0.95)
  blob95 <- convhulln(newpc95,"FA")
  sim.vol1.95[i] <- blob95$vol
}

test6d1.95 <- as.randtest(obs=obs.vol95,sim=sim.vol1.95, alter="less")
test6d1.95

###################################################
###       Null Model #2
###################################################
sim.vol2.95 <- rep(0,npermute)
for(i in 1:npermute){
  newpc <- scale(matrix(rnorm(ncol(pc) * nrow(pc)), nrow(pc), ncol(pc)))
  newpc95 <- subselect.data(newpc, 0.95)
  blob95 <- convhulln(newpc95,"FA")  
  sim.vol2.95[i] <- blob95$vol
}

test6d2.95 <- as.randtest(obs=obs.vol95,sim=sim.vol2.95, alter="less")
test6d2.95


###################################################
###               Null Model #3
###################################################
sim.vol3.95 <- rep(0,npermute)
for(i in 1:npermute){
  newpc <- apply(pc,2,sample)
  newpc95 <- subselect.data(newpc, 0.95)
  blob95 <- convhulln(newpc95,"FA")
  sim.vol3.95[i] <- blob95$vol
}

test6d3.95 <- as.randtest(obs=obs.vol95,sim=sim.vol3.95, alter="less")
test6d3.95


###################################################
###             Null Model #4
###################################################
corM <- cor(pc)
sim.vol4.95 <- rep(0,npermute)
for(i in 1:npermute){
  newpc <- scale(matrix(rnorm(ncol(pc) * nrow(pc)), nrow(pc), ncol(pc)))
  newpc <- scale(newpc%*%chol(corM))
  newpc95 <- subselect.data(newpc, 0.95)
  blob95 <- convhulln(newpc95,"FA")
  sim.vol4.95[i] <- blob95$vol  
}

test6d4.95 <- as.randtest(obs=obs.vol95,sim=sim.vol4.95, alter="less")

test6d4.95$expvar[2]


###################################################
###    Plot results of observed vs null models
###################################################
par(mfrow=c(2,2))
plot(test6d1.95, main = "Ho: Uniform distr. (rdz) / Orthogonal")
plot(test6d2.95, main = "Ho: Normal distr./ Orthogonal")
plot(test6d3.95, main = "Ho: Observed distr. / Orthogonal")
plot(test6d4.95, main = "Ho: Normal distr. / Non-Orthogonal")


###################################################
###  Ratio between observed volume and null models
###################################################
100 - obs.vol95/test6d1.95$expvar[2] * 100
100 - obs.vol95/test6d2.95$expvar[2] * 100
100 - obs.vol95/test6d3.95$expvar[2] * 100
100 - obs.vol95/test6d4.95$expvar[2] * 100



###################################################
###    Analysis of concentration of species
###################################################

tab.null1 <- lapply(1:npermute, function(x) nullmodel(pc, 1))
tab.null2 <- lapply(1:npermute, function(x) nullmodel(pc, 2))
tab.null3 <- lapply(1:npermute, function(x) nullmodel(pc, 3))
tab.null4 <- lapply(1:npermute, function(x) nullmodel(pc, 4))

obs <- myfunc(pc)
null1 <- lapply(tab.null1, myfunc)
null2 <- lapply(tab.null2, myfunc)
null3 <- lapply(tab.null3, myfunc)
null4 <- lapply(tab.null4, myfunc)

par(mfrow = c(2,2))
plot.curves(obs, null1, main = "Null model #1")
plot.curves(obs, null2, main = "Null model #2")
plot.curves(obs, null3, main = "Null model #3")
plot.curves(obs, null4, main = "Null model #4")
