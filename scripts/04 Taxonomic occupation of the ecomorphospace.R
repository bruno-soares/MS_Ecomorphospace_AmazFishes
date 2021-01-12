# Data #
species_scores
orders
families

# Loading packages #
library(vegan)
library(RVAideMemoire)

### PERMANOVA for differences in the ecomorphospace among fish clades ###
permanova1<-adonis(species_scores[,1:2]~orders$x,method="euclidean",permutations=999)
permanova2<-adonis(species_scores[,1:2]~families$x,method="euclidean",permutations=999)
permanova1 # Are orders occupying different portions of the ecomorphospace?
permanova2 # Are families occupying different portions of the ecomorphospace?

# Pairwide function for adonis() #
pairwise.adonis <- function(x,factors, sim.function = 'vegdist', sim.method = 'euclidean', p.adjust.m ='bonferroni')
{
  library(vegan)
  
  co = combn(unique(as.character(factors)),2)
  pairs = c()
  F.Model =c()
  R2 = c()
  p.value = c()
  
  
  for(elem in 1:ncol(co)){
    if(sim.function == 'daisy'){
      library(cluster); x1 = daisy(x[factors %in% c(co[1,elem],co[2,elem]),],metric=sim.method)
    } else{x1 = vegdist(x[factors %in% c(co[1,elem],co[2,elem]),],method=sim.method)}
    
    ad = adonis(x1 ~ factors[factors %in% c(co[1,elem],co[2,elem])] );
    pairs = c(pairs,paste(co[1,elem],'vs',co[2,elem]));
    F.Model =c(F.Model,ad$aov.tab[1,4]);
    R2 = c(R2,ad$aov.tab[1,5]);
    p.value = c(p.value,ad$aov.tab[1,6])
  }
  p.adjusted = p.adjust(p.value,method=p.adjust.m)
  sig = c(rep('',length(p.adjusted)))
  sig[p.adjusted <= 0.05] <-'.'
  sig[p.adjusted <= 0.01] <-'*'
  sig[p.adjusted <= 0.001] <-'**'
  sig[p.adjusted <= 0.0001] <-'***'
  
  pairw.res = data.frame(pairs,F.Model,R2,p.value,p.adjusted,sig)
  print("Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1")
  return(pairw.res)
  
} 

### Pairwise PERMANOVA for differences in the ecomorphospace among fish clades ###
pairwise_orders<-pairwise.adonis(species_scores[,1:2],as.factor(orders$x))
pairwise_families<-pairwise.adonis(species_scores[,1:2],as.factor(families$x))
write.table(pairwise_orders,"results/pairwise_orderlevel.txt")
write.table(pairwise_families,"results/pairwise_familylevel.txt")
