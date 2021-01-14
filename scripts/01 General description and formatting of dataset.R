### Importing dataset ###
data<-read.table("data/dataset.txt",header=T)
data$Species<-as.factor(data$Species)
data$Family<-as.factor(data$Family)
data$Order<-as.factor(data$Order)

################################
### ICHTHYOFAUNA DESCRIPTION ###
################################
# How many species, taxonomic families and orders? #
nrow(as.data.frame(levels(data$Species)))
nrow(as.data.frame(levels(data$Family)))
nrow(as.data.frame(levels(data$Order)))

# Observed and relative richness for each taxonomic order #
t(as.data.frame(lapply(split(aggregate(data$Order,list(data$Species),unique),aggregate(data$Order,list(data$Species),unique)$x),nrow)))
t(as.data.frame(lapply(split(aggregate(data$Order,list(data$Species),unique),aggregate(data$Order,list(data$Species),unique)$x),nrow)))[,1]/nrow(as.data.frame(levels(data$Species)))

# Observed and relative richness for each taxonomic family #
t(as.data.frame(lapply(split(aggregate(data$Family,list(data$Species),unique),aggregate(data$Family,list(data$Species),unique)$x),nrow)))
t(as.data.frame(lapply(split(aggregate(data$Family,list(data$Species),unique),aggregate(data$Family,list(data$Species),unique)$x),nrow)))[,1]/nrow(as.data.frame(levels(data$Species)))

##############################
### Formatting the dataset ###
##############################
traits<-aggregate(data[c(17:27)],list(data$Species),mean)
rownames(traits)<-traits[,1]
traits<-traits[,-1]
orders<-aggregate(data$Order,list(data$Species),unique)
rownames(orders)<-orders[,1]
families<-aggregate(data$Family,list(data$Species),unique)
rownames(families)<-families[,1]
