# Loading packages # 
library(vegan)
library(ggplot2)
library(gridExtra)

####################################
### Exploring the Ecomorphospace ###
####################################
PCA_traits<-princomp(decostand(traits,method="standardize"))
summary(PCA_traits) # overall descriptors of the ecomorphospace
bstick(PCA_traits)
eigenvals(PCA_traits) # Broken-stick criterion: first 2 axes selected
loadings(PCA_traits) # Loadings
write.table(PCA_traits$scores[,1:2],file="results/species_scores.txt") # exporting scores
species_scores<-as.data.frame(PCA_traits$scores)

####################################
######## Designing Figure 3 ########
####################################
coordinates<-data.frame(species_scores)[,c(1:2)]

Characiformes_coordinates<-coordinates[which(orders$x=="Characiformes"),]
outer_Cha<-chull(Characiformes_coordinates)
FigA<-ggplot()+
  geom_point(mapping=aes(x=species_scores$Comp.1,y=species_scores$Comp.2)
             ,colour="grey",size=2.5)+
  geom_polygon(mapping = aes(x=Characiformes_coordinates[outer_Cha,1],
                             y=Characiformes_coordinates[outer_Cha,2]),
               fill="black",colour="black",size=1.5,alpha=0.1)+
  geom_point(mapping = aes(x=Characiformes_coordinates[outer_Cha,1],
                           y=Characiformes_coordinates[outer_Cha,2]),
             colour="black",size=3)+
  scale_colour_manual(values=vals)+
  scale_shape_manual(values=vals2)+
  xlab("PC1 (28.32%)")+  ylab("PC2 (20.49%)")+
  theme(panel.background = element_rect(fill = "white", colour = "black", size = 0.5), # opcoes graficas
        panel.grid.major = element_line(colour = NA),
        panel.grid.minor = element_line(colour = NA),
        axis.text = element_text(colour = "black", size = 10),
        axis.title = element_text(colour = "black", size = 12, face = "bold"),
        legend.title = element_blank(),
        legend.background = element_rect(fill = "white"),
        legend.text = element_text(face = "bold", colour = "black", size = 12))



### Siluriformes ###
Siluriformes_coordinates<-coordinates[which(orders$x=="Siluriformes"),]
outer_Sil<-chull(Siluriformes_coordinates)

FigB<-ggplot()+
  geom_point(mapping=aes(x=species_scores$Comp.1,y=species_scores$Comp.2)
             ,colour="grey",size=2.5)+
  geom_polygon(mapping = aes(x=Siluriformes_coordinates[outer_Sil,1],
                             y=Siluriformes_coordinates[outer_Sil,2]),
               fill="black",colour="black",size=1.5,alpha=0.1)+
  geom_point(mapping = aes(x=Siluriformes_coordinates[outer_Sil,1],
                             y=Siluriformes_coordinates[outer_Sil,2]),
               colour="black",size=3)+
  scale_colour_manual(values=vals)+
  scale_shape_manual(values=vals2)+
  xlab("PC1 (28.32%)")+  ylab("PC2 (20.49%)")+
  theme(panel.background = element_rect(fill = "white", colour = "black", size = 0.5), # opcoes graficas
        panel.grid.major = element_line(colour = NA),
        panel.grid.minor = element_line(colour = NA),
        axis.text = element_text(colour = "black", size = 10),
        axis.title = element_text(colour = "black", size = 12, face = "bold"),
        legend.title = element_blank(),
        legend.background = element_rect(fill = "white"),
        legend.text = element_text(face = "bold", colour = "black", size = 12))

Gymnotiformes_coordinates<-coordinates[which(orders$x=="Gymnotiformes"),]
outer_Gym<-chull(Gymnotiformes_coordinates)
FigC<-ggplot()+
  geom_point(mapping=aes(x=species_scores$Comp.1,y=species_scores$Comp.2)
             ,colour="grey",size=2.5)+
  geom_polygon(mapping = aes(x=Gymnotiformes_coordinates[outer_Gym,1],
                             y=Gymnotiformes_coordinates[outer_Gym,2]),
               fill="black",colour="black",size=1.5,alpha=0.1)+
  geom_point(mapping = aes(x=Gymnotiformes_coordinates[outer_Gym,1],
                           y=Gymnotiformes_coordinates[outer_Gym,2]),
             colour="black",size=3)+
  scale_colour_manual(values=vals)+
  scale_shape_manual(values=vals2)+
  xlab("PC1 (28.32%)")+  ylab("PC2 (20.49%)")+
  theme(panel.background = element_rect(fill = "white", colour = "black", size = 0.5), # opcoes graficas
        panel.grid.major = element_line(colour = NA),
        panel.grid.minor = element_line(colour = NA),
        axis.text = element_text(colour = "black", size = 10),
        axis.title = element_text(colour = "black", size = 12, face = "bold"),
        legend.title = element_blank(),
        legend.background = element_rect(fill = "white"),
        legend.text = element_text(face = "bold", colour = "black", size = 12))

Cichliformes_coordinates<-coordinates[which(orders$x=="Cichliformes"),]
outer_Cich<-chull(Cichliformes_coordinates)
FigD<-ggplot()+
  geom_point(mapping=aes(x=species_scores$Comp.1,y=species_scores$Comp.2)
             ,colour="grey",size=2.5)+
  geom_polygon(mapping = aes(x=Cichliformes_coordinates[outer_Cich,1],
                             y=Cichliformes_coordinates[outer_Cich,2]),
               fill="black",colour="black",size=1.5,alpha=0.1)+
  geom_point(mapping = aes(x=Cichliformes_coordinates[outer_Cich,1],
                           y=Cichliformes_coordinates[outer_Cich,2]),
             colour="black",size=3)+
  scale_colour_manual(values=vals)+
  scale_shape_manual(values=vals2)+
  xlab("PC1 (28.32%)")+  ylab("PC2 (20.49%)")+
  theme(panel.background = element_rect(fill = "white", colour = "black", size = 0.5), # opcoes graficas
        panel.grid.major = element_line(colour = NA),
        panel.grid.minor = element_line(colour = NA),
        axis.text = element_text(colour = "black", size = 10),
        axis.title = element_text(colour = "black", size = 12, face = "bold"),
        legend.title = element_blank(),
        legend.background = element_rect(fill = "white"),
        legend.text = element_text(face = "bold", colour = "black", size = 12))

Cyprinodontiformes_coordinates<-coordinates[which(orders$x=="Cyprinodontiformes"),]
outer_Cyp<-chull(Cyprinodontiformes_coordinates)
FigE<-ggplot()+
  geom_point(mapping=aes(x=species_scores$Comp.1,y=species_scores$Comp.2)
             ,colour="grey",size=2.5)+
  geom_polygon(mapping = aes(x=Cyprinodontiformes_coordinates[outer_Cyp,1],
                             y=Cyprinodontiformes_coordinates[outer_Cyp,2]),
               fill="black",colour="black",size=1.5,alpha=0.1)+
  geom_point(mapping = aes(x=Cyprinodontiformes_coordinates[outer_Cyp,1],
                           y=Cyprinodontiformes_coordinates[outer_Cyp,2]),
             colour="black",size=3)+
  scale_colour_manual(values=vals)+
  scale_shape_manual(values=vals2)+
  xlab("PC1 (28.32%)")+  ylab("PC2 (20.49%)")+
  theme(panel.background = element_rect(fill = "white", colour = "black", size = 0.5), # opcoes graficas
        panel.grid.major = element_line(colour = NA),
        panel.grid.minor = element_line(colour = NA),
        axis.text = element_text(colour = "black", size = 10),
        axis.title = element_text(colour = "black", size = 12, face = "bold"),
        legend.title = element_blank(),
        legend.background = element_rect(fill = "white"),
        legend.text = element_text(face = "bold", colour = "black", size = 12))


Synbranchiformes_coordinates<-coordinates[which(orders$x=="Synbranchiformes"),]
outer_Syn<-chull(Synbranchiformes_coordinates)
FigF<-ggplot()+
  geom_point(mapping=aes(x=species_scores$Comp.1,y=species_scores$Comp.2)
             ,colour="grey",size=2.5)+
  geom_point(mapping = aes(x=Synbranchiformes_coordinates[outer_Syn,1],
                           y=Synbranchiformes_coordinates[outer_Syn,2]),
             colour="black",size=3)+
  scale_colour_manual(values=vals)+
  scale_shape_manual(values=vals2)+
  xlab("PC1 (28.32%)")+  ylab("PC2 (20.49%)")+
  theme(panel.background = element_rect(fill = "white", colour = "black", size = 0.5), # opcoes graficas
        panel.grid.major = element_line(colour = NA),
        panel.grid.minor = element_line(colour = NA),
        axis.text = element_text(colour = "black", size = 10),
        axis.title = element_text(colour = "black", size = 12, face = "bold"),
        legend.title = element_blank(),
        legend.background = element_rect(fill = "white"),
        legend.text = element_text(face = "bold", colour = "black", size = 12))

Gobiiformes_coordinates<-coordinates[which(orders$x=="Gobiiformes"),]
outer_Gob<-chull(Gobiiformes_coordinates)
FigG<-ggplot()+
  geom_point(mapping=aes(x=species_scores$Comp.1,y=species_scores$Comp.2)
             ,colour="grey",size=2.5)+
  geom_point(mapping = aes(x=Gobiiformes_coordinates[outer_Gob,1],
                           y=Gobiiformes_coordinates[outer_Gob,2]),
             colour="black",size=3)+
  scale_colour_manual(values=vals)+
  scale_shape_manual(values=vals2)+
  xlab("PC1 (28.32%)")+  ylab("PC2 (20.49%)")+
  theme(panel.background = element_rect(fill = "white", colour = "black", size = 0.5), # opcoes graficas
        panel.grid.major = element_line(colour = NA),
        panel.grid.minor = element_line(colour = NA),
        axis.text = element_text(colour = "black", size = 10),
        axis.title = element_text(colour = "black", size = 12, face = "bold"),
        legend.title = element_blank(),
        legend.background = element_rect(fill = "white"),
        legend.text = element_text(face = "bold", colour = "black", size = 12))

Perciformes_coordinates<-coordinates[which(orders$x=="Perciformes"),]
outer_Perc<-chull(Perciformes_coordinates)
FigH<-ggplot()+
  geom_point(mapping=aes(x=species_scores$Comp.1,y=species_scores$Comp.2)
             ,colour="grey",size=2.5)+
  geom_point(mapping = aes(x=Perciformes_coordinates[outer_Perc,1],
                           y=Perciformes_coordinates[outer_Perc,2]),
             colour="black",size=3)+
  scale_colour_manual(values=vals)+
  scale_shape_manual(values=vals2)+
  xlab("PC1 (28.32%)")+  ylab("PC2 (20.49%)")+
  theme(panel.background = element_rect(fill = "white", colour = "black", size = 0.5), # opcoes graficas
        panel.grid.major = element_line(colour = NA),
        panel.grid.minor = element_line(colour = NA),
        axis.text = element_text(colour = "black", size = 10),
        axis.title = element_text(colour = "black", size = 12, face = "bold"),
        legend.title = element_blank(),
        legend.background = element_rect(fill = "white"),
        legend.text = element_text(face = "bold", colour = "black", size = 12))

Beloniformes_coordinates<-coordinates[which(orders$x=="Beloniformes"),]
outer_Bel<-chull(Beloniformes_coordinates)
FigI<-ggplot()+
  geom_point(mapping=aes(x=species_scores$Comp.1,y=species_scores$Comp.2)
             ,colour="grey",size=2.5)+
  geom_point(mapping = aes(x=Beloniformes_coordinates[outer_Bel,1],
                           y=Beloniformes_coordinates[outer_Bel,2]),
             colour="black",size=3)+
  scale_colour_manual(values=vals)+
  scale_shape_manual(values=vals2)+
  xlab("PC1 (28.32%)")+  ylab("PC2 (20.49%)")+
  theme(panel.background = element_rect(fill = "white", colour = "black", size = 0.5), # opcoes graficas
        panel.grid.major = element_line(colour = NA),
        panel.grid.minor = element_line(colour = NA),
        axis.text = element_text(colour = "black", size = 10),
        axis.title = element_text(colour = "black", size = 12, face = "bold"),
        legend.title = element_blank(),
        legend.background = element_rect(fill = "white"),
        legend.text = element_text(face = "bold", colour = "black", size = 12))

png("figures/Figure3.png",width=15,height=15,units="cm",res=1200)
grid.arrange(FigA,FigB,FigC,FigD,FigE,FigF,FigG,FigH,FigI,ncol=3)
dev.off()
