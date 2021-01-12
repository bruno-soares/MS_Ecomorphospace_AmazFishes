# Script for plotting the Blob (PCA biplot and kernel densities) in Diaz et. al #
library(vegan)
library(ks)

################ KERNEL DENSITY ESTIMATION ##############################
pca12<-species_scores[,1:2]
H <- Hpi(x=pca12)      # optimal bandwidth estimation
est<- kde(x=pca12, H=H, compute.cont=TRUE)     # kernel density estimation

# set contour probabilities for drawing contour levels
cl<-contourLevels(est, prob=c(0.5, 0.05, 0.001), approx=TRUE)
fit<-envfit(pca12,traits)

tiff(filename="Kernel density.tiff",units="in",width=4,height=4,res=300)
plot(est,cont=seq(1,100,by=1),display="filled.contour2",add=FALSE,ylab="PC2 (20.49%)",xlab="PC1 (28.32%)",cex.axis=0.75,las=1) 
plot(est,abs.cont=cl[1], labels=c(0.5),labcex=0.75, add=TRUE, lwd=0.75, col="grey30")
plot(est,abs.cont=cl[2], labels=c(0.95),labcex=0.75, add=TRUE, lwd=0.5, col="grey60")
plot(est,abs.cont=cl[3], labels=c(0.99),labcex=0.75, add=TRUE, lwd=0.5, col="grey60")
plot(fit, cex=0.90, col=1, labels=list(vectors = c("Osf","Ops", "Eps", "Bsh", "Bsf", "PFps","PFar","CPt","CFar","Frt","Fsf")))
points(pca12[,], pch=16, cex=0.25, col="black")
dev.off()
