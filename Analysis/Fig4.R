animal_omicron_type <- read.csv('/Dell/Dell4/weics/Omicron/Animal_overlap/mutation_site_animal_distribution.txt',sep = '\t')
animal_omicron_type[7,3] <- 2
animal_omicron_type$species <- as.character(animal_omicron_type$species)
patient_S_nonS <- read.table('patient_s_enrich.txt')
colnames(patient_S_nonS) <- colnames(animal_omicron_type)
animal_omicron_type <- rbind(animal_omicron_type,patient_S_nonS)
species_list <- as.list(animal_omicron_type$species)
animal_omicron_type_stat <- animal_omicron_type
colnames(animal_omicron_type_stat) <- c('species','p_value','odds_ratio')
mouse_WNY <- read.table('mouse_WNY_site.txt',sep = '\t',header = T)
colnames(mouse_WNY) <- colnames(animal_omicron_type)
animal_omicron_type <- rbind(animal_omicron_type,mouse_WNY)
animal_omicron_type$species <- as.character(animal_omicron_type$species)
animal_omicron_type_stat$p_value <- as.numeric(animal_omicron_type_stat$p_value)
animal_omicron_type_stat$odds_ratio <- as.numeric(animal_omicron_type_stat$odds_ratio)
for (i in 1:nrow(animal_omicron_type)) {
  p_value <- fisher.test(matrix(c(animal_omicron_type[i,2],animal_omicron_type[i,3],sum(animal_omicron_type[-i,2]),sum(animal_omicron_type[-i,3])),nrow = 2))$p.value
  odd_ration <- fisher.test(matrix(c(animal_omicron_type[i,2],animal_omicron_type[i,3],sum(animal_omicron_type[-i,2]),sum(animal_omicron_type[-i,3])),nrow = 2))$estimate
  animal_omicron_type_stat[i,1] <- animal_omicron_type[i,1]
  animal_omicron_type_stat[i,2] <- p_value
  animal_omicron_type_stat[i,3] <- odd_ration
}
plot(animal_omicron_type_stat$odds_ratio,animal_omicron_type_stat$p_value)
library(ggplot2)
ggplot()+
  geom_point(data=animal_omicron_type_stat, mapping=aes(x=odds_ratio,y=-log10(p_value)))+
  ggrepel::geom_text_repel(data=animal_omicron_type_stat, mapping=aes(x=odds_ratio,y=-log10(p_value),label=species))


for (i in 1:nrow(animal_omicron_type)) {
  p_value <- fisher.test(matrix(c(animal_omicron_type[i,2],animal_omicron_type[i,3],(25-animal_omicron_type[i,2]),(1273-25-animal_omicron_type[i,3])),nrow = 2))$p.value
  odd_ration <- fisher.test(matrix(c(animal_omicron_type[i,2],animal_omicron_type[i,3],(25-animal_omicron_type[i,2]),(1273-25-animal_omicron_type[i,3])),nrow = 2))$estimate
  animal_omicron_type_stat[i,1] <- animal_omicron_type[i,1]
  animal_omicron_type_stat[i,2] <- p_value
  animal_omicron_type_stat[i,3] <- odd_ration
}
plot(animal_omicron_type_stat$odds_ratio,animal_omicron_type_stat$p_value)
library(ggplot2)
g<- ggplot()+
  geom_point(data=animal_omicron_type_stat, mapping=aes(x=odds_ratio,y=-log10(p_value)))+
  ggrepel::geom_text_repel(data=animal_omicron_type_stat, mapping=aes(x=odds_ratio,y=-log10(p_value),label=species))+
  my_theme2
g

pdf("fig5_pos.pdf",useDingbats = F,width = 5.4,height = 3.6)  
grid.newpage()
pushViewport(viewport(layout=grid.layout(1,1)))
vplayout <- function(x,y)
  viewport(layout.pos.row = x,layout.pos.col = y)
print(g,vp=vplayout(1,1))
dev.off()
