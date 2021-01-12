library(vegan)
library(ggplot2)
library(scater)
library(geometry)
library(rcdd)
source("utils2.R")
library(ape)

### Qual a riqueza ecomorfológica dos clados no dataset A? ###
# Beloniformes, Gobiiformes e Perciformes não podem ser calculado
# Modelos não feitos para Synbranchiformes, que só tinham 5 espécies
FD_dataA<-c(convhulln(data.matrix(split(dataA_scores,dataA_orders$x)$Characiformes)[,c(1:4)],option="FA")$vol,
            convhulln(data.matrix(split(dataA_scores,dataA_orders$x)$Cichliformes)[,c(1:4)],option="FA")$vol,
            convhulln(data.matrix(split(dataA_scores,dataA_orders$x)$Cyprinodontiformes)[,c(1:4)],option="FA")$vol,
            convhulln(data.matrix(split(dataA_scores,dataA_orders$x)$Gymnotiformes)[,c(1:4)],option="FA")$vol,
            convhulln(data.matrix(split(dataA_scores,dataA_orders$x)$Siluriformes)[,c(1:4)],option="FA")$vol,
            convhulln(data.matrix(split(dataA_scores,dataA_orders$x)$Synbranchiformes)[,c(1:4)],option="FA")$vol)
FD_dataA
S_dataA<-c(nrow(split(dataA_scores,dataA_orders$x)$Characiformes),
           nrow(split(dataA_scores,dataA_orders$x)$Cichliformes),
           nrow(split(dataA_scores,dataA_orders$x)$Cyprinodontiformes),
           nrow(split(dataA_scores,dataA_orders$x)$Gymnotiformes),
           nrow(split(dataA_scores,dataA_orders$x)$Siluriformes),
           nrow(split(dataA_scores,dataA_orders$x)$Synbranchiformes))
S_dataA
plot(log(FD_dataA)~S_dataA)
plot(FD_dataA~S_dataA)

Characiformes_dataA<-data.matrix(split(dataA_scores,dataA_orders$x)$Characiformes)
Characiformes_dataA_resampling<-data.frame()
for(i in 1:1000){
  axes<-Characiformes_dataA[sample(nrow(Characiformes_dataA),10),]
  volume<-convhulln(axes[,c(1:4)],option="FA")$vol
  Characiformes_dataA_resampling<-rbind(Characiformes_dataA_resampling,volume)
}

Siluriformes_dataA<-data.matrix(split(dataA_scores,dataA_orders$x)$Siluriformes)
Siluriformes_dataA_resampling<-data.frame()
for(i in 1:1000){
  axes<-Siluriformes_dataA[sample(nrow(Siluriformes_dataA),10),]
  volume<-convhulln(axes[,c(1:4)],option="FA")$vol
  Siluriformes_dataA_resampling<-rbind(Siluriformes_dataA_resampling,volume)
}

Gymnotiformes_dataA<-data.matrix(split(dataA_scores,dataA_orders$x)$Gymnotiformes)
Gymnotiformes_dataA_resampling<-data.frame()
for(i in 1:1000){
  axes<-Gymnotiformes_dataA[sample(nrow(Gymnotiformes_dataA),10),]
  volume<-convhulln(axes[,c(1:4)],option="FA")$vol
  Gymnotiformes_dataA_resampling<-rbind(Gymnotiformes_dataA_resampling,volume)
}

Cichliformes_dataA<-data.matrix(split(dataA_scores,dataA_orders$x)$Cichliformes)
Cichliformes_dataA_resampling<-data.frame()
for(i in 1:1000){
  axes<-Cichliformes_dataA[sample(nrow(Cichliformes_dataA),10),]
  volume<-convhulln(axes[,c(1:4)],option="FA")$vol
  Cichliformes_dataA_resampling<-rbind(Cichliformes_dataA_resampling,volume)
}

Cyprinodontiformes_dataA<-data.matrix(split(dataA_scores,dataA_orders$x)$Cyprinodontiformes)
Cyprinodontiformes_dataA_resampling<-data.frame()
for(i in 1:1000){
  axes<-Cyprinodontiformes_dataA[sample(nrow(Cyprinodontiformes_dataA),10),]
  volume<-convhulln(axes[,c(1:4)],option="FA")$vol
  Cyprinodontiformes_dataA_resampling<-rbind(Cyprinodontiformes_dataA_resampling,volume)
}

error_Characiformes_dataA<-(qnorm(0.0975)*sd(Characiformes_dataA_resampling[,1]))/sqrt(mean(Characiformes_dataA_resampling[,1]))
left_Characiformes_dataA<-mean(Characiformes_dataA_resampling[,1])+error_Characiformes_dataA
right_Characiformes_dataA<-mean(Characiformes_dataA_resampling[,1])-error_Characiformes_dataA

error_Siluriformes_dataA<-(qnorm(0.0975)*sd(Siluriformes_dataA_resampling[,1]))/sqrt(mean(Siluriformes_dataA_resampling[,1]))
left_Siluriformes_dataA<-mean(Siluriformes_dataA_resampling[,1])+error_Siluriformes_dataA
right_Siluriformes_dataA<-mean(Siluriformes_dataA_resampling[,1])-error_Siluriformes_dataA

error_Gymnotiformes_dataA<-(qnorm(0.0975)*sd(Gymnotiformes_dataA_resampling[,1]))/sqrt(mean(Gymnotiformes_dataA_resampling[,1]))
left_Gymnotiformes_dataA<-mean(Gymnotiformes_dataA_resampling[,1])+error_Gymnotiformes_dataA
right_Gymnotiformes_dataA<-mean(Gymnotiformes_dataA_resampling[,1])-error_Gymnotiformes_dataA

error_Cichliformes_dataA<-(qnorm(0.0975)*sd(Cichliformes_dataA_resampling[,1]))/sqrt(mean(Cichliformes_dataA_resampling[,1]))
left_Cichliformes_dataA<-mean(Cichliformes_dataA_resampling[,1])+error_Cichliformes_dataA
right_Cichliformes_dataA<-mean(Cichliformes_dataA_resampling[,1])-error_Cichliformes_dataA

error_Cyprinodontiformes_dataA<-(qnorm(0.0975)*sd(Cyprinodontiformes_dataA_resampling[,1]))/sqrt(mean(Cyprinodontiformes_dataA_resampling[,1]))
left_Cyprinodontiformes_dataA<-mean(Cyprinodontiformes_dataA_resampling[,1])+error_Cyprinodontiformes_dataA
right_Cyprinodontiformes_dataA<-mean(Cyprinodontiformes_dataA_resampling[,1])-error_Cyprinodontiformes_dataA

pd <- position_dodge(0.1)
FRic_A<-ggplot()+
  geom_errorbar(aes(x="Cyprinodontiformes",min=left_Cyprinodontiformes_dataA,max=right_Cyprinodontiformes_dataA),width=0.5,position=pd,colour="orange",size=1.2)+
  geom_errorbar(aes(x="Cichliformes",min=left_Cichliformes_dataA,max=right_Cichliformes_dataA),width=0.5,position=pd,colour="blue",size=1.2)+
  geom_errorbar(aes(x="Characiformes",min=left_Characiformes_dataA,max=right_Characiformes_dataA),width=.5,position=pd,colour="red",size=1.2)+
  geom_errorbar(aes(x="Siluriformes",min=left_Siluriformes_dataA,max=right_Siluriformes_dataA),width=.5,position=pd,colour="pink",size=1.2)+
  geom_errorbar(aes(x="Gymnotiformes",min=left_Gymnotiformes_dataA,max=right_Gymnotiformes_dataA),width=.5,position=pd,colour="green",size=1.2)+
  geom_point(aes(x="Cyprinodontiformes",y=mean(Cyprinodontiformes_dataA_resampling[,1])),position=pd,colour="orange",size=2.5)+
  geom_point(aes(x="Cichliformes",y=mean(Cichliformes_dataA_resampling[,1])),position=pd,colour="blue",size=2.5)+
  geom_point(aes(x="Characiformes",y=mean(Characiformes_dataA_resampling[,1])),position=pd,colour="red",size=2.5)+
  geom_point(aes(x="Siluriformes",y=mean(Siluriformes_dataA_resampling[,1])),position=pd,colour="pink",size=2.5)+
  geom_point(aes(x="Gymnotiformes",y=mean(Gymnotiformes_dataA_resampling[,1])),position=pd,colour="green",size=2.5)+
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

FRic_A


### Qual a especialização funcional no dataset A ###
dataA_axes<-dataA_scores[,1:4]
centroid<-apply(dataA_axes,2,mean)
Total_speS<-apply(dataA_axes,1,function(x){(sum((x-centroid)^2))^0.5})
Total_speS_rel<-Total_speS/max(Total_speS)
mean(Total_speS_rel)
centroid

speS<-apply(data.matrix(split(dataA_scores,dataA_orders$x)$Characiformes)[,c(1:4)],1,function(x){(sum((x-centroid)^2))^0.5})
speS_rel<-speS/max(Total_speS)
mean(speS_rel)

speS<-apply(data.matrix(split(dataA_scores,dataA_orders$x)$Cichliformes)[,c(1:4)],1,function(x){(sum((x-centroid)^2))^0.5})
speS_rel<-speS/max(Total_speS)
mean(speS_rel)

speS<-apply(data.matrix(split(dataA_scores,dataA_orders$x)$Cyprinodontiformes)[,c(1:4)],1,function(x){(sum((x-centroid)^2))^0.5})
speS_rel<-speS/max(Total_speS)
mean(speS_rel)

speS<-apply(data.matrix(split(dataA_scores,dataA_orders$x)$Gymnotiformes)[,c(1:4)],1,function(x){(sum((x-centroid)^2))^0.5})
speS_rel<-speS/max(Total_speS)
mean(speS_rel)

speS<-apply(data.matrix(split(dataA_scores,dataA_orders$x)$Siluriformes)[,c(1:4)],1,function(x){(sum((x-centroid)^2))^0.5})
speS_rel<-speS/max(Total_speS)
mean(speS_rel)

speS<-apply(data.matrix(split(dataA_scores,dataA_orders$x)$Synbranchiformes)[,c(1:4)],1,function(x){(sum((x-centroid)^2))^0.5})
speS_rel<-speS/max(Total_speS)
mean(speS_rel)

speS<-apply(data.matrix(split(dataA_scores,dataA_orders$x)$Gobiiformes)[,c(1:4)],1,function(x){(sum((x-centroid)^2))^0.5})
speS_rel<-speS/max(Total_speS)
mean(speS_rel)

Characiformes_dataA<-data.matrix(split(dataA_scores,dataA_orders$x)$Characiformes)
Characiformes_dataA_FSpe<-data.frame()
for(i in 1:1000){
  axes<-Characiformes_dataA[sample(nrow(Characiformes_dataA),10),]
  axes<-axes[,1:4]
  speS<-apply(axes,1,function(x){(sum((x-centroid)^2))^0.5})
  speS_rel<-mean(speS/max(Total_speS))
  Characiformes_dataA_FSpe<-rbind(Characiformes_dataA_FSpe,speS_rel)
}

Siluriformes_dataA<-data.matrix(split(dataA_scores,dataA_orders$x)$Siluriformes)
Siluriformes_dataA_FSpe<-data.frame()
for(i in 1:1000){
  axes<-Siluriformes_dataA[sample(nrow(Siluriformes_dataA),10),]
  axes<-axes[,1:4]
  speS<-apply(axes,1,function(x){(sum((x-centroid)^2))^0.5})
  speS_rel<-mean(speS/max(Total_speS))
  Siluriformes_dataA_FSpe<-rbind(Siluriformes_dataA_FSpe,speS_rel)
}

Gymnotiformes_dataA<-data.matrix(split(dataA_scores,dataA_orders$x)$Gymnotiformes)
Gymnotiformes_dataA_FSpe<-data.frame()
for(i in 1:1000){
  axes<-Gymnotiformes_dataA[sample(nrow(Gymnotiformes_dataA),10),]
  axes<-axes[,1:4]
  speS<-apply(axes,1,function(x){(sum((x-centroid)^2))^0.5})
  speS_rel<-mean(speS/max(Total_speS))
  Gymnotiformes_dataA_FSpe<-rbind(Gymnotiformes_dataA_FSpe,speS_rel)
}

Cichliformes_dataA<-data.matrix(split(dataA_scores,dataA_orders$x)$Cichliformes)
Cichliformes_dataA_FSpe<-data.frame()
for(i in 1:1000){
  axes<-Cichliformes_dataA[sample(nrow(Cichliformes_dataA),10),]
  axes<-axes[,1:4]
  speS<-apply(axes,1,function(x){(sum((x-centroid)^2))^0.5})
  speS_rel<-mean(speS/max(Total_speS))
  Cichliformes_dataA_FSpe<-rbind(Cichliformes_dataA_FSpe,speS_rel)
}


Cyprinodontiformes_dataA<-data.matrix(split(dataA_scores,dataA_orders$x)$Cyprinodontiformes)
Cyprinodontiformes_dataA_FSpe<-data.frame()
for(i in 1:1000){
  axes<-Cyprinodontiformes_dataA[sample(nrow(Cyprinodontiformes_dataA),10),]
  axes<-axes[,1:4]
  speS<-apply(axes,1,function(x){(sum((x-centroid)^2))^0.5})
  speS_rel<-mean(speS/max(Total_speS))
  Cyprinodontiformes_dataA_FSpe<-rbind(Cyprinodontiformes_dataA_FSpe,speS_rel)
}

error_Characiformes_dataAF<-(qnorm(0.0975)*sd(Characiformes_dataA_FSpe[,1]))/sqrt(mean(Characiformes_dataA_FSpe[,1]))
left_Characiformes_dataAF<-mean(Characiformes_dataA_FSpe[,1])+error_Characiformes_dataA
right_Characiformes_dataAF<-mean(Characiformes_dataA_FSpe[,1])-error_Characiformes_dataA

error_Siluriformes_dataAF<-(qnorm(0.0975)*sd(Siluriformes_dataA_FSpe[,1]))/sqrt(mean(Siluriformes_dataA_FSpe[,1]))
left_Siluriformes_dataAF<-mean(Siluriformes_dataA_FSpe[,1])+error_Siluriformes_dataA
right_Siluriformes_dataAF<-mean(Siluriformes_dataA_FSpe[,1])-error_Siluriformes_dataA

error_Gymnotiformes_dataAF<-(qnorm(0.0975)*sd(Gymnotiformes_dataA_FSpe[,1]))/sqrt(mean(Gymnotiformes_dataA_FSpe[,1]))
left_Gymnotiformes_dataAF<-mean(Gymnotiformes_dataA_FSpe[,1])+error_Gymnotiformes_dataA
right_Gymnotiformes_dataAF<-mean(Gymnotiformes_dataA_FSpe[,1])-error_Gymnotiformes_dataA

error_Cichliformes_dataAF<-(qnorm(0.0975)*sd(Cichliformes_dataA_FSpe[,1]))/sqrt(mean(Cichliformes_dataA_FSpe[,1]))
left_Cichliformes_dataAF<-mean(Cichliformes_dataA_FSpe[,1])+error_Cichliformes_dataA
right_Cichliformes_dataAF<-mean(Cichliformes_dataA_FSpe[,1])-error_Cichliformes_dataA

error_Cyprinodontiformes_dataAF<-(qnorm(0.0975)*sd(Cyprinodontiformes_dataA_FSpe[,1]))/sqrt(mean(Cyprinodontiformes_dataA_FSpe[,1]))
left_Cyprinodontiformes_dataAF<-mean(Cyprinodontiformes_dataA_FSpe[,1])+error_Cyprinodontiformes_dataA
right_Cyprinodontiformes_dataAF<-mean(Cyprinodontiformes_dataA_FSpe[,1])-error_Cyprinodontiformes_dataA

pd <- position_dodge(0.1)
FSpe_A<-ggplot()+
  geom_errorbar(aes(x="Cichliformes",min=left_Cichliformes_dataAF,max=right_Cichliformes_dataAF),width=0.5,position=pd,colour="blue",size=1.2)+
  geom_errorbar(aes(x="Cyprinodontiformes",min=left_Cyprinodontiformes_dataAF,max=right_Cyprinodontiformes_dataAF),width=0.5,position=pd,colour="orange",size=1.2)+
  geom_errorbar(aes(x="Characiformes",min=left_Characiformes_dataAF,max=right_Characiformes_dataAF),width=.5,position=pd,colour="red",size=1.2)+
  geom_errorbar(aes(x="Siluriformes",min=left_Siluriformes_dataAF,max=right_Siluriformes_dataAF),width=.5,position=pd,colour="pink",size=1.2)+
  geom_errorbar(aes(x="Gymnotiformes",min=left_Gymnotiformes_dataAF,max=right_Gymnotiformes_dataAF),width=.5,position=pd,colour="green",size=1.2)+
  geom_point(aes(x="Cyprinodontiformes",y=mean(Cyprinodontiformes_dataA_FSpe[,1])),position=pd,colour="orange",size=2.5)+
  geom_point(aes(x="Cichliformes",y=mean(Cichliformes_dataA_FSpe[,1])),position=pd,colour="blue",size=2.5)+
  geom_point(aes(x="Characiformes",y=mean(Characiformes_dataA_FSpe[,1])),position=pd,colour="red",size=2.5)+
  geom_point(aes(x="Siluriformes",y=mean(Siluriformes_dataA_FSpe[,1])),position=pd,colour="pink",size=2.5)+
  geom_point(aes(x="Gymnotiformes",y=mean(Gymnotiformes_dataA_FSpe[,1])),position=pd,colour="green",size=2.5)+
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
FSpe_A

tiff(filename="Figure5.tiff",units="in",width=8,height=7,res=300)
multiplot(FRic_A,FSpe_A)
dev.off()




### overlap measurements dataA ###
#Carregar função inter no script anterior de hypervolume
dataA_ChaSil_overlap<-data.frame()
dataA_ChaGym_overlap<-data.frame()
dataA_ChaCyp_overlap<-data.frame()
dataA_ChaCic_overlap<-data.frame()
dataA_SilGym_overlap<-data.frame()
dataA_SilCyp_overlap<-data.frame()
dataA_SilCic_overlap<-data.frame()
dataA_GymCyp_overlap<-data.frame()
dataA_GymCic_overlap<-data.frame()
dataA_CypCic_overlap<-data.frame()
for(i in 1:1000){
  data1<-Characiformes_dataA[sample(nrow(Characiformes_dataA),10),]
  data2<-Siluriformes_dataA[sample(nrow(Siluriformes_dataA),10),]
  data3<-Gymnotiformes_dataA[sample(nrow(Gymnotiformes_dataA),10),]
  data4<-Cyprinodontiformes_dataA[sample(nrow(Cyprinodontiformes_dataA),10),]
  data5<-Cichliformes_dataA[sample(nrow(Cichliformes_dataA),10),]
  dataA_ChaSil_overlap<-rbind(dataA_ChaSil_overlap,inter(data1[,1:4],data2[,1:4])$vol_inter)
  dataA_ChaGym_overlap<-rbind(dataA_ChaGym_overlap,inter(data1[,1:4],data3[,1:4])$vol_inter)
  dataA_ChaCyp_overlap<-rbind(dataA_ChaCyp_overlap,inter(data1[,1:4],data4[,1:4])$vol_inter)
  dataA_ChaCic_overlap<-rbind(dataA_ChaCic_overlap,inter(data1[,1:4],data5[,1:4])$vol_inter)
  dataA_SilGym_overlap<-rbind(dataA_SilGym_overlap,inter(data2[,1:4],data3[,1:4])$vol_inter)
  dataA_SilCyp_overlap<-rbind(dataA_SilCyp_overlap,inter(data2[,1:4],data4[,1:4])$vol_inter)
  dataA_SilCic_overlap<-rbind(dataA_SilCic_overlap,inter(data2[,1:4],data5[,1:4])$vol_inter)
  dataA_GymCyp_overlap<-rbind(dataA_GymCyp_overlap,inter(data3[,1:4],data4[,1:4])$vol_inter)
  dataA_GymCic_overlap<-rbind(dataA_GymCic_overlap,inter(data3[,1:4],data5[,1:4])$vol_inter)
  dataA_CypCic_overlap<-rbind(dataA_CypCic_overlap,inter(data4[,1:4],data5[,1:4])$vol_inter)
}

dataA_ChaSil_overlap
median(dataA_ChaSil_overlap[,1])
range(dataA_ChaSil_overlap) ###
error_dataA_ChaSil<-(qnorm(0.0975)*sd(dataA_ChaSil_overlap[,1]))/sqrt(mean(dataA_ChaSil_overlap[,1]))
left_ChaSil_dataA<-mean(dataA_ChaSil_overlap[,1])+error_dataA_ChaSil
right_ChaSil_dataA<-mean(dataA_ChaSil_overlap[,1])-error_dataA_ChaSil
left_ChaSil_dataA
right_ChaSil_dataA

range(dataA_ChaGym_overlap)
median(dataA_ChaGym_overlap[,1])
error_dataA_ChaGym<-(qnorm(0.0975)*sd(dataA_ChaGym_overlap[,1]))/sqrt(mean(dataA_ChaGym_overlap[,1]))
left_ChaGym_dataA<-mean(dataA_ChaGym_overlap[,1])+error_dataA_ChaGym
right_ChaGym_dataA<-mean(dataA_ChaGym_overlap[,1])-error_dataA_ChaGym
left_ChaGym_dataA
right_ChaGym_dataA

range(dataA_ChaCyp_overlap)
median(dataA_ChaCyp_overlap[,1])
error_dataA_ChaCyp<-(qnorm(0.0975)*sd(dataA_ChaCyp_overlap[,1]))/sqrt(mean(dataA_ChaCyp_overlap[,1]))
left_ChaCyp_dataA<-mean(dataA_ChaCyp_overlap[,1])+error_dataA_ChaCyp
right_ChaCyp_dataA<-mean(dataA_ChaCyp_overlap[,1])-error_dataA_ChaCyp
left_ChaCyp_dataA
right_ChaCyp_dataA

range(dataA_ChaCic_overlap)
median(dataA_ChaCic_overlap[,1])
error_dataA_ChaCic<-(qnorm(0.0975)*sd(dataA_ChaCic_overlap[,1]))/sqrt(mean(dataA_ChaCic_overlap[,1]))
left_ChaCic_dataA<-mean(dataA_ChaCic_overlap[,1])+error_dataA_ChaCic
right_ChaCic_dataA<-mean(dataA_ChaCic_overlap[,1])-error_dataA_ChaCic
left_ChaCic_dataA
right_ChaCic_dataA

range(dataA_SilGym_overlap)
median(dataA_SilGym_overlap[,1])
error_dataA_SilGym<-(qnorm(0.0975)*sd(dataA_SilGym_overlap[,1]))/sqrt(mean(dataA_SilGym_overlap[,1]))
left_SilGym_dataA<-mean(dataA_SilGym_overlap[,1])+error_dataA_SilGym
right_SilGym_dataA<-mean(dataA_SilGym_overlap[,1])-error_dataA_SilGym
left_SilGym_dataA
right_SilGym_dataA

range(dataA_SilCyp_overlap)
median(dataA_SilCyp_overlap[,1])
error_dataA_SilCyp<-(qnorm(0.0975)*sd(dataA_SilCyp_overlap[,1]))/sqrt(mean(dataA_SilCyp_overlap[,1]))
left_SilCyp_dataA<-mean(dataA_SilCyp_overlap[,1])+error_dataA_SilCyp
right_SilCyp_dataA<-mean(dataA_SilCyp_overlap[,1])-error_dataA_SilCyp
left_SilCyp_dataA
right_SilCyp_dataA

range(dataA_SilCic_overlap)
median(dataA_SilCic_overlap[,1])
error_dataA_SilCic<-(qnorm(0.0975)*sd(dataA_SilCic_overlap[,1]))/sqrt(mean(dataA_SilCic_overlap[,1]))
left_SilCic_dataA<-mean(dataA_SilCic_overlap[,1])+error_dataA_SilCic
right_SilCic_dataA<-mean(dataA_SilCic_overlap[,1])-error_dataA_SilCic
left_SilCic_dataA
right_SilCic_dataA

range(dataA_GymCyp_overlap)
median(dataA_GymCyp_overlap[,1])
error_dataA_GymCyp<-(qnorm(0.0975)*sd(dataA_GymCyp_overlap[,1]))/sqrt(mean(dataA_GymCyp_overlap[,1]))
left_GymCyp_dataA<-mean(dataA_GymCyp_overlap[,1])+error_dataA_GymCyp
right_GymCyp_dataA<-mean(dataA_GymCyp_overlap[,1])-error_dataA_GymCyp
left_GymCyp_dataA
right_GymCyp_dataA

range(dataA_GymCic_overlap)
median(dataA_GymCic_overlap[,1])
error_dataA_GymCic<-(qnorm(0.0975)*sd(dataA_GymCic_overlap[,1]))/sqrt(mean(dataA_GymCic_overlap[,1]))
left_GymCic_dataA<-mean(dataA_GymCic_overlap[,1])+error_dataA_GymCic
right_GymCic_dataA<-mean(dataA_GymCic_overlap[,1])-error_dataA_GymCic
left_GymCic_dataA
right_GymCic_dataA

range(dataA_CypCic_overlap)
median(dataA_CypCic_overlap[,1])
error_dataA_CypCic<-(qnorm(0.0975)*sd(dataA_CypCic_overlap[,1]))/sqrt(mean(dataA_CypCic_overlap[,1]))
left_CypCic_dataA<-mean(dataA_CypCic_overlap[,1])+error_dataA_CypCic
right_CypCic_dataA<-mean(dataA_CypCic_overlap[,1])-error_dataA_CypCic
left_CypCic_dataA
right_CypCic_dataA

























### overlap measurements Rafa ###
S_Rafa
Rafa_ChaSil_overlap<-data.frame()
Rafa_ChaGym_overlap<-data.frame()
Rafa_SilGym_overlap<-data.frame()
Rafa_ChaEut_overlap<-data.frame()
Rafa_SilEut_overlap<-data.frame()
Rafa_GymEut_overlap<-data.frame()
for(i in 1:100){
  data1<-Characiformes_Rafa[sample(nrow(Characiformes_Rafa),15),]
  data2<-Siluriformes_Rafa[sample(nrow(Siluriformes_Rafa),15),]
  data3<-Gymnotiformes_Rafa[sample(nrow(Gymnotiformes_Rafa),15),]
  data4<-Euteleostei_Rafa[sample(nrow(Euteleostei_Rafa),15),]
  Rafa_ChaSil_overlap<-rbind(Rafa_ChaSil_overlap,inter(data1[,1:4],data2[,1:4])$vol_inter)
  Rafa_ChaGym_overlap<-rbind(Rafa_ChaGym_overlap,inter(data1[,1:4],data3[,1:4])$vol_inter)
  Rafa_ChaEut_overlap<-rbind(Rafa_ChaEut_overlap,inter(data1[,1:4],data4[,1:4])$vol_inter)
  Rafa_SilGym_overlap<-rbind(Rafa_SilGym_overlap,inter(data2[,1:4],data3[,1:4])$vol_inter)
  Rafa_SilEut_overlap<-rbind(Rafa_SilEut_overlap,inter(data2[,1:4],data4[,1:4])$vol_inter)
  Rafa_GymEut_overlap<-rbind(Rafa_GymEut_overlap,inter(data3[,1:4],data4[,1:4])$vol_inter)
}

Rafa_ChaSil_overlap
median(Rafa_ChaSil_overlap[,1])
error_Rafa_ChaSil<-(qnorm(0.0975)*sd(Rafa_ChaSil_overlap[,1]))/sqrt(mean(Rafa_ChaSil_overlap[,1]))
left_ChaSil_Rafa<-mean(Rafa_ChaSil_overlap[,1])+error_Rafa_ChaSil
right_ChaSil_Rafa<-mean(Rafa_ChaSil_overlap[,1])-error_Rafa_ChaSil
left_ChaSil_Rafa
right_ChaSil_Rafa

Rafa_ChaGym_overlap
median(Rafa_ChaGym_overlap[,1])
error_Rafa_ChaGym<-(qnorm(0.0975)*sd(Rafa_ChaGym_overlap[,1]))/sqrt(mean(Rafa_ChaGym_overlap[,1]))
left_ChaGym_Rafa<-mean(Rafa_ChaGym_overlap[,1])+error_Rafa_ChaGym
right_ChaGym_Rafa<-mean(Rafa_ChaGym_overlap[,1])-error_Rafa_ChaGym
left_ChaGym_Rafa
right_ChaGym_Rafa

Rafa_SilGym_overlap
median(Rafa_SilGym_overlap[,1])
error_Rafa_SilGym<-(qnorm(0.0975)*sd(Rafa_SilGym_overlap[,1]))/sqrt(mean(Rafa_SilGym_overlap[,1]))
left_SilGym_Rafa<-mean(Rafa_SilGym_overlap[,1])+error_Rafa_SilGym
right_SilGym_Rafa<-mean(Rafa_SilGym_overlap[,1])-error_Rafa_SilGym
left_SilGym_Rafa
right_SilGym_Rafa

Rafa_ChaEut_overlap
median(Rafa_ChaEut_overlap[,1])
error_Rafa_ChaEut<-(qnorm(0.0975)*sd(Rafa_ChaEut_overlap[,1]))/sqrt(mean(Rafa_ChaEut_overlap[,1]))
left_ChaEut_Rafa<-mean(Rafa_ChaEut_overlap[,1])+error_Rafa_ChaEut
right_ChaEut_Rafa<-mean(Rafa_ChaEut_overlap[,1])-error_Rafa_ChaEut
left_ChaEut_Rafa
right_ChaEut_Rafa

Rafa_SilEut_overlap
median(Rafa_SilEut_overlap[,1])
error_Rafa_SilEut<-(qnorm(0.0975)*sd(Rafa_SilEut_overlap[,1]))/sqrt(mean(Rafa_SilEut_overlap[,1]))
left_SilEut_Rafa<-mean(Rafa_SilEut_overlap[,1])+error_Rafa_SilEut
right_SilEut_Rafa<-mean(Rafa_SilEut_overlap[,1])-error_Rafa_SilEut
left_SilEut_Rafa
right_SilEut_Rafa

Rafa_GymEut_overlap
median(Rafa_GymEut_overlap[,1])
error_Rafa_GymEut<-(qnorm(0.0975)*sd(Rafa_GymEut_overlap[,1]))/sqrt(mean(Rafa_GymEut_overlap[,1]))
left_GymEut_Rafa<-mean(Rafa_GymEut_overlap[,1])+error_Rafa_GymEut
right_GymEut_Rafa<-mean(Rafa_GymEut_overlap[,1])-error_Rafa_GymEut
left_GymEut_Rafa
right_GymEut_Rafa