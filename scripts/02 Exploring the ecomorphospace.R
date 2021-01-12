# Loading packages # 
library(vegan)
library(ggplot2)

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

### Designing the ecomorphospace ###
vals<-c("Siluriformes" = "#999999","Cichliformes"="#000000",
        "Beloniformes"="#E69F00","Cyprinodontiformes"="#56B4E9",
        "Gymnotiformes"="#009E73","Characiformes"="#0072B2",
        "Synbranchiformes"="#F0E442","Perciformes"="#D55E00"
        ,"Gobiiformes"="#CC79A7")
vals2<-c("Siluriformes" = 1,"Cichliformes"=2,
         "Beloniformes"=0,"Cyprinodontiformes"=7,
         "Gymnotiformes"=6,"Characiformes"=5,
         "Synbranchiformes"=9,"Perciformes"=10
         ,"Gobiiformes"=12)

Figure2<-ggplot()+
  geom_point(mapping=aes(x=species_scores$Comp.1,y=species_scores$Comp.2,
                         shape=factor(orders$x),colour=factor(orders$x)),size=3)+
  scale_colour_manual(values=vals)+
  scale_shape_manual(values=vals2)+
  xlab("PC1 (28.32%)")+
  ylab("PC2 (20.49%)")+
  theme(panel.background = element_rect(fill = "white", colour = "black", size = 0.5), # opcoes graficas
        panel.grid.major = element_line(colour = NA),
        panel.grid.minor = element_line(colour = NA),
        axis.text = element_text(colour = "black", size = 10),
        axis.title = element_text(colour = "black", size = 12, face = "bold"),
        legend.title = element_blank(),
        legend.background = element_rect(fill = "white"),
        legend.text = element_text(face = "bold", colour = "black", size = 12))

ggsave(filename="figures/Figure2.png",units="in",width=10,height=7,dpi=600)