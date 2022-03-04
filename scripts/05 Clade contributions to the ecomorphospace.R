### Loading packages ###
library(geometry)
library(scater)

### Functional richness for each taxonomic order ###
F_richness<-c(convhulln(data.matrix(split(species_scores,orders$x)$Characiformes)[,c(1:2)],option="FA")$vol,
            convhulln(data.matrix(split(species_scores,orders$x)$Cichliformes)[,c(1:2)],option="FA")$vol,
            convhulln(data.matrix(split(species_scores,orders$x)$Cyprinodontiformes)[,c(1:2)],option="FA")$vol,
            convhulln(data.matrix(split(species_scores,orders$x)$Gymnotiformes)[,c(1:2)],option="FA")$vol,
            convhulln(data.matrix(split(species_scores,orders$x)$Siluriformes)[,c(1:2)],option="FA")$vol,
            convhulln(data.matrix(split(species_scores,orders$x)$Synbranchiformes)[,c(1:2)],option="FA")$vol)
F_richness
S_richness<-c(nrow(split(species_scores,orders$x)$Characiformes),
           nrow(split(species_scores,orders$x)$Cichliformes),
           nrow(split(species_scores,orders$x)$Cyprinodontiformes),
           nrow(split(species_scores,orders$x)$Gymnotiformes),
           nrow(split(species_scores,orders$x)$Siluriformes),
           nrow(split(species_scores,orders$x)$Synbranchiformes))
S_richness
plot(log(F_richness)~S_richness)


### Null models ###
Characiformes_data<-data.matrix(split(species_scores,orders$x)$Characiformes)
Characiformes_FRic_Null<-data.frame()
for(i in 1:10000){
  axes<-Characiformes_data[sample(nrow(Characiformes_data),10),]
  volume<-convhulln(axes[,c(1:2)],option="FA")$vol
  Characiformes_FRic_Null<-rbind(Characiformes_FRic_Null,volume)
}

Siluriformes_data<-data.matrix(split(species_scores,orders$x)$Siluriformes)
Siluriformes_FRic_Null<-data.frame()
for(i in 1:10000){
  axes<-Siluriformes_data[sample(nrow(Siluriformes_data),10),]
  volume<-convhulln(axes[,c(1:2)],option="FA")$vol
  Siluriformes_FRic_Null<-rbind(Siluriformes_FRic_Null,volume)
}

Gymnotiformes_data<-data.matrix(split(species_scores,orders$x)$Gymnotiformes)
Gymnotiformes_FRic_Null<-data.frame()
for(i in 1:10000){
  axes<-Gymnotiformes_data[sample(nrow(Gymnotiformes_data),10),]
  volume<-convhulln(axes[,c(1:2)],option="FA")$vol
  Gymnotiformes_FRic_Null<-rbind(Gymnotiformes_FRic_Null,volume)
}

Cichliformes_data<-data.matrix(split(species_scores,orders$x)$Cichliformes)
Cichliformes_FRic_Null<-data.frame()
for(i in 1:10000){
  axes<-Cichliformes_data[sample(nrow(Cichliformes_data),10),]
  volume<-convhulln(axes[,c(1:2)],option="FA")$vol
  Cichliformes_FRic_Null<-rbind(Cichliformes_FRic_Null,volume)
}

Cyprinodontiformes_data<-data.matrix(split(species_scores,orders$x)$Cyprinodontiformes)
Cyprinodontiformes_FRic_Null<-data.frame()
for(i in 1:10000){
  axes<-Cyprinodontiformes_data[sample(nrow(Cyprinodontiformes_data),10),]
  volume<-convhulln(axes[,c(1:2)],option="FA")$vol
  Cyprinodontiformes_FRic_Null<-rbind(Cyprinodontiformes_FRic_Null,volume)
}

Characiformes_FRic_error<-(qnorm(0.0975)*sd(Characiformes_FRic_Null[,1]))/sqrt(mean(Characiformes_FRic_Null[,1]))
Characiformes_FRic_leftci<-mean(Characiformes_FRic_Null[,1])+Characiformes_FRic_error
Characiformes_FRic_rightci<-mean(Characiformes_FRic_Null[,1])-Characiformes_FRic_error

Siluriformes_FRic_error<-(qnorm(0.0975)*sd(Siluriformes_FRic_Null[,1]))/sqrt(mean(Siluriformes_FRic_Null[,1]))
Siluriformes_FRic_leftci<-mean(Siluriformes_FRic_Null[,1])+Siluriformes_FRic_error
Siluriformes_FRic_rightci<-mean(Siluriformes_FRic_Null[,1])-Siluriformes_FRic_error

Gymnotiformes_FRic_error<-(qnorm(0.0975)*sd(Gymnotiformes_FRic_Null[,1]))/sqrt(mean(Gymnotiformes_FRic_Null[,1]))
Gymnotiformes_FRic_leftci<-mean(Gymnotiformes_FRic_Null[,1])+Gymnotiformes_FRic_error
Gymnotiformes_FRic_rightci<-mean(Gymnotiformes_FRic_Null[,1])-Gymnotiformes_FRic_error

Cichliformes_FRic_error<-(qnorm(0.0975)*sd(Cichliformes_FRic_Null[,1]))/sqrt(mean(Cichliformes_FRic_Null[,1]))
Cichliformes_FRic_leftci<-mean(Cichliformes_FRic_Null[,1])+Cichliformes_FRic_error
Cichliformes_FRic_rightci<-mean(Cichliformes_FRic_Null[,1])-Cichliformes_FRic_error

Cyprinodontiformes_FRic_error<-(qnorm(0.0975)*sd(Cyprinodontiformes_FRic_Null[,1]))/sqrt(mean(Cyprinodontiformes_FRic_Null[,1]))
Cyprinodontiformes_FRic_leftci<-mean(Cyprinodontiformes_FRic_Null[,1])+Cyprinodontiformes_FRic_error
Cyprinodontiformes_FRic_rightci<-mean(Cyprinodontiformes_FRic_Null[,1])-Cyprinodontiformes_FRic_error

pd <- position_dodge(0.1)

Figure4a<-ggplot()+
  geom_errorbar(aes(x="Cyprinodontiformes",min=Cyprinodontiformes_FRic_leftci,max=Cyprinodontiformes_FRic_rightci),width=0.5,position=pd,size=1.2)+
  geom_errorbar(aes(x="Cichliformes",min=Cichliformes_FRic_leftci,max=Cichliformes_FRic_rightci),width=0.5,position=pd,size=1.2)+
  geom_errorbar(aes(x="Characiformes",min=Characiformes_FRic_leftci,max=Characiformes_FRic_rightci),width=.5,position=pd,size=1.2)+
  geom_errorbar(aes(x="Siluriformes",min=Siluriformes_FRic_leftci,max=Siluriformes_FRic_rightci),width=.5,position=pd,size=1.2)+
  geom_errorbar(aes(x="Gymnotiformes",min=Gymnotiformes_FRic_leftci,max=Gymnotiformes_FRic_rightci),width=.5,position=pd,size=1.2)+
  geom_point(aes(x="Cyprinodontiformes",y=mean(Cyprinodontiformes_FRic_Null[,1])),position=pd,size=2.5)+
  geom_point(aes(x="Cichliformes",y=mean(Cichliformes_FRic_Null[,1])),position=pd,size=2.5)+
  geom_point(aes(x="Characiformes",y=mean(Characiformes_FRic_Null[,1])),position=pd,size=2.5)+
  geom_point(aes(x="Siluriformes",y=mean(Siluriformes_FRic_Null[,1])),position=pd,size=2.5)+
  geom_point(aes(x="Gymnotiformes",y=mean(Gymnotiformes_FRic_Null[,1])),position=pd,size=2.5)+
  xlab("Lineages")+
  ylab("FRIc")+
  theme(panel.background = element_rect(fill = "white", colour = "black", size = 0.5), # opcoes graficas
        panel.grid.major = element_line(colour = NA),
        panel.grid.minor = element_line(colour = NA),
        axis.text = element_text(colour = "black", size = 10),
        axis.title = element_text(colour = "black", size = 15, face = "bold"),
        legend.title = element_blank(),
        legend.background = element_rect(fill = "white"),
        legend.text = element_text(face = "bold", colour = "black", size = 12))
Figure4a



### Functional specialization for each taxonomic order ###
# Taxonomic orders with less than 3 species cannot be calculated
# Null modelling was not applied for Synbranchiformes due to the low number of species
trait_axes<-species_scores[,1:2]
centroid<-apply(trait_axes,2,mean)
Total_speS<-apply(trait_axes,1,function(x){(sum((x-centroid)^2))^0.5})
Total_speS_rel<-Total_speS/max(Total_speS)

speS_Characiformes<-apply(data.matrix(split(species_scores,orders$x)$Characiformes)[,c(1:2)],1,function(x){(sum((x-centroid)^2))^0.5})
speS_rel_Characiformes<-speS_Characiformes/max(Total_speS)
mean(speS_rel_Characiformes)

speS_Cichliformes<-apply(data.matrix(split(species_scores,orders$x)$Cichliformes)[,c(1:2)],1,function(x){(sum((x-centroid)^2))^0.5})
speS_rel_Cichliformes<-speS_Cichliformes/max(Total_speS)
mean(speS_rel_Cichliformes)

speS_Cyprinodontiformes<-apply(data.matrix(split(species_scores,orders$x)$Cyprinodontiformes)[,c(1:2)],1,function(x){(sum((x-centroid)^2))^0.5})
speS_rel_Cyprinodontiformes<-speS_Cyprinodontiformes/max(Total_speS)
mean(speS_rel_Cyprinodontiformes)

speS_Gymnotiformes<-apply(data.matrix(split(species_scores,orders$x)$Gymnotiformes)[,c(1:2)],1,function(x){(sum((x-centroid)^2))^0.5})
speS_rel_Gymnotiformes<-speS_Gymnotiformes/max(Total_speS)
mean(speS_rel_Gymnotiformes)

speS_Siluriformes<-apply(data.matrix(split(species_scores,orders$x)$Siluriformes)[,c(1:2)],1,function(x){(sum((x-centroid)^2))^0.5})
speS_rel_Siluriformes<-speS_Siluriformes/max(Total_speS)
mean(speS_rel_Siluriformes)

speS_Synbranchiformes<-apply(data.matrix(split(species_scores,orders$x)$Synbranchiformes)[,c(1:2)],1,function(x){(sum((x-centroid)^2))^0.5})
speS_rel_Synbranchiformes<-speS_Synbranchiformes/max(Total_speS)
mean(speS_rel_Synbranchiformes)

speS_Gobiiformes<-apply(data.matrix(split(species_scores,orders$x)$Gobiiformes)[,c(1:2)],1,function(x){(sum((x-centroid)^2))^0.5})
speS_rel_Gobiiformes<-speS_Gobiiformes/max(Total_speS)
mean(speS_rel_Gobiiformes)

Characiformes_data<-data.matrix(split(species_scores,orders$x)$Characiformes)
Characiformes_FSpe_Null<-data.frame()
for(i in 1:10000){
  axes<-Characiformes_data[sample(nrow(Characiformes_data),10),]
  axes<-axes[,1:2]
  speS<-apply(axes,1,function(x){(sum((x-centroid)^2))^0.5})
  speS_rel<-mean(speS/max(Total_speS))
  Characiformes_FSpe_Null<-rbind(Characiformes_FSpe_Null,speS_rel)
}

Siluriformes_data<-data.matrix(split(species_scores,orders$x)$Siluriformes)
Siluriformes_FSpe_Null<-data.frame()
for(i in 1:10000){
  axes<-Siluriformes_data[sample(nrow(Siluriformes_data),10),]
  axes<-axes[,1:2]
  speS<-apply(axes,1,function(x){(sum((x-centroid)^2))^0.5})
  speS_rel<-mean(speS/max(Total_speS))
  Siluriformes_FSpe_Null<-rbind(Siluriformes_FSpe_Null,speS_rel)
}

Gymnotiformes_data<-data.matrix(split(species_scores,orders$x)$Gymnotiformes)
Gymnotiformes_FSpe_Null<-data.frame()
for(i in 1:10000){
  axes<-Gymnotiformes_data[sample(nrow(Gymnotiformes_data),10),]
  axes<-axes[,1:2]
  speS<-apply(axes,1,function(x){(sum((x-centroid)^2))^0.5})
  speS_rel<-mean(speS/max(Total_speS))
  Gymnotiformes_FSpe_Null<-rbind(Gymnotiformes_FSpe_Null,speS_rel)
}

Cichliformes_data<-data.matrix(split(species_scores,orders$x)$Cichliformes)
Cichliformes_FSpe_Null<-data.frame()
for(i in 1:10000){
  axes<-Cichliformes_data[sample(nrow(Cichliformes_data),10),]
  axes<-axes[,1:2]
  speS<-apply(axes,1,function(x){(sum((x-centroid)^2))^0.5})
  speS_rel<-mean(speS/max(Total_speS))
  Cichliformes_FSpe_Null<-rbind(Cichliformes_FSpe_Null,speS_rel)
}

Cyprinodontiformes_data<-data.matrix(split(species_scores,orders$x)$Cyprinodontiformes)
Cyprinodontiformes_FSpe_Null<-data.frame()
for(i in 1:10000){
  axes<-Cyprinodontiformes_data[sample(nrow(Cyprinodontiformes_data),10),]
  axes<-axes[,1:2]
  speS<-apply(axes,1,function(x){(sum((x-centroid)^2))^0.5})
  speS_rel<-mean(speS/max(Total_speS))
  Cyprinodontiformes_FSpe_Null<-rbind(Cyprinodontiformes_FSpe_Null,speS_rel)
}

Characiformes_FSpe_error<-(qnorm(0.0975)*sd(Characiformes_FSpe_Null[,1]))/sqrt(mean(Characiformes_FSpe_Null[,1]))
Characiformes_FSpe_leftci<-mean(Characiformes_FSpe_Null[,1])+Characiformes_FSpe_error
Characiformes_FSpe_rightci<-mean(Characiformes_FSpe_Null[,1])-Characiformes_FSpe_error

Siluriformes_FSpe_error<-(qnorm(0.0975)*sd(Siluriformes_FSpe_Null[,1]))/sqrt(mean(Siluriformes_FSpe_Null[,1]))
Siluriformes_FSpe_leftci<-mean(Siluriformes_FSpe_Null[,1])+Siluriformes_FSpe_error
Siluriformes_FSpe_rightci<-mean(Siluriformes_FSpe_Null[,1])-Siluriformes_FSpe_error

Gymnotiformes_FSpe_error<-(qnorm(0.0975)*sd(Gymnotiformes_FSpe_Null[,1]))/sqrt(mean(Gymnotiformes_FSpe_Null[,1]))
Gymnotiformes_FSpe_leftci<-mean(Gymnotiformes_FSpe_Null[,1])+Gymnotiformes_FSpe_error
Gymnotiformes_FSpe_rightci<-mean(Gymnotiformes_FSpe_Null[,1])-Gymnotiformes_FSpe_error

Cichliformes_FSpe_error<-(qnorm(0.0975)*sd(Cichliformes_FSpe_Null[,1]))/sqrt(mean(Cichliformes_FSpe_Null[,1]))
Cichliformes_FSpe_leftci<-mean(Cichliformes_FSpe_Null[,1])+Cichliformes_FSpe_error
Cichliformes_FSpe_rightci<-mean(Cichliformes_FSpe_Null[,1])-Cichliformes_FSpe_error

Cyprinodontiformes_FSpe_error<-(qnorm(0.0975)*sd(Cyprinodontiformes_FSpe_Null[,1]))/sqrt(mean(Cyprinodontiformes_FSpe_Null[,1]))
Cyprinodontiformes_FSpe_leftci<-mean(Cyprinodontiformes_FSpe_Null[,1])+Cyprinodontiformes_FSpe_error
Cyprinodontiformes_FSpe_rightci<-mean(Cyprinodontiformes_FSpe_Null[,1])-Cyprinodontiformes_FSpe_error

pd <- position_dodge(0.1)
Figure4b<-ggplot()+
  geom_errorbar(aes(x="Cichliformes",min=Cichliformes_FSpe_leftci,max=Cichliformes_FSpe_rightci),width=0.5,position=pd,size=1.2)+
  geom_errorbar(aes(x="Cyprinodontiformes",min=Cyprinodontiformes_FSpe_leftci,max=Cyprinodontiformes_FSpe_rightci),width=0.5,position=pd,size=1.2)+
  geom_errorbar(aes(x="Characiformes",min=Characiformes_FSpe_leftci,max=Characiformes_FSpe_rightci),width=.5,position=pd,size=1.2)+
  geom_errorbar(aes(x="Siluriformes",min=Siluriformes_FSpe_leftci,max=Siluriformes_FSpe_rightci),width=.5,position=pd,size=1.2)+
  geom_errorbar(aes(x="Gymnotiformes",min=Gymnotiformes_FSpe_leftci,max=Gymnotiformes_FSpe_rightci),width=.5,position=pd,size=1.2)+
  geom_point(aes(x="Cyprinodontiformes",y=mean(Cyprinodontiformes_FSpe_Null[,1])),position=pd,size=2.5)+
  geom_point(aes(x="Cichliformes",y=mean(Cichliformes_FSpe_Null[,1])),position=pd,size=2.5)+
  geom_point(aes(x="Characiformes",y=mean(Characiformes_FSpe_Null[,1])),position=pd,size=2.5)+
  geom_point(aes(x="Siluriformes",y=mean(Siluriformes_FSpe_Null[,1])),position=pd,size=2.5)+
  geom_point(aes(x="Gymnotiformes",y=mean(Gymnotiformes_FSpe_Null[,1])),position=pd,size=2.5)+
  xlab("Lineages")+
  ylab("FSpe")+
  theme(panel.background = element_rect(fill = "white", colour = "black", size = 0.5), # opcoes graficas
        panel.grid.major = element_line(colour = NA),
        panel.grid.minor = element_line(colour = NA),
        axis.text = element_text(colour = "black", size = 10),
        axis.title = element_text(colour = "black", size = 15, face = "bold"),
        legend.title = element_blank(),
        legend.background = element_rect(fill = "white"),
        legend.text = element_text(face = "bold", colour = "black", size = 12))
Figure4b

tiff(filename="figures/Figure4.tiff",units="in",width=8,height=7,res=600)
multiplot(Figure4a,Figure4b)
dev.off()
