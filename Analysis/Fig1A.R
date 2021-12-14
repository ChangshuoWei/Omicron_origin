library('tidyverse')
library('data.table')

##orf####
orfsubg <- fread('./MN908947.3_region.txt', header = T, sep = '\t',
                  stringsAsFactors = F) %>% as.data.frame()
orfsubg <- orfsubg[!grepl('(nsp)|(ORF10)|(ORF7b)', orfsubg$region),]
orfsubg$type <- orfsubg$region



pos_info <- fread('./Omicron_mutation_fastml_pos.txt', header = F, sep = '\t',
                 stringsAsFactors = F) %>% as.data.frame()
colnames(pos_info) <- c('start','refer','alter')

orfsubg$region <-'genome'
orfsubg$g <- 10
omicron_pos <- ggplot(orfsubg)+
  geom_segment(aes(x = 0, y = g, xend = 29903, yend = g), colour = 'grey')+
  geom_rect(aes(ymin=g-0.5, ymax=g+0.5, xmin=start, xmax=end, fill=type), color='black')+
  geom_vline(xintercept = pos_info$start,color = "red")+
  scale_fill_manual(values = c('grey', 'grey', '#6e86c2', '#ef7b21', '#f4cc43', '#45bdcd', '#5c53a1', '#af7ab3', '#d64879', '#7dbe2c', '#c1d5e5', '#e5cfc2'))+
  theme_classic()+
  scale_x_continuous(breaks=c(seq(0, 29903, 5000),29903))+
  theme(text=element_text(size=8),
        axis.line.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title=element_blank(),
        axis.text.y=element_blank())+
  # scale_fill_brewer(palette='Set1')+
  scale_color_manual(values=c('black', 'red'))

pos_info<-after_break_pos_data
pos_info$pos <- as.integer(as.numeric(as.character(pos_info$pos)))
orfsubg$region <-'genome'
orfsubg$g <- 10
#orfsubg$type <- factor(orfsubg$type,levels = c('3pUTR','5pUTR',"ORF1ab","S","ORF3a","E","M","ORF6","ORF7a","ORF8","N"))
after_break_pos <-ggplot(orfsubg)+
  geom_segment(aes(x = 0, y = g, xend = 29903, yend = g), colour = 'grey')+
  geom_rect(aes(ymin=g-0.5, ymax=g+0.5, xmin=start, xmax=end, fill=type), color='black')+
  geom_vline(xintercept = pos_info$pos,color = "red")+
  scale_fill_manual(values = c('grey', 'grey', '#6e86c2', '#ef7b21', '#f4cc43', '#45bdcd', '#5c53a1', '#af7ab3', '#d64879', '#7dbe2c', '#c1d5e5', '#e5cfc2'))+
  theme_classic()+
  scale_x_continuous(breaks=c(seq(0, 29903, 5000),29903))+
  theme(text=element_text(size=8),
        axis.line.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title=element_blank(),
        axis.text.y=element_blank())
omicron_pos 
after_break_pos



pos_info <- fread('./Omicron_mutation_fastml_pos.txt', header = F, sep = '\t',
                  stringsAsFactors = F) %>% as.data.frame()
colnames(pos_info) <- c('start','refer','alter')
freq1<-ggplot(data = pos_info ,aes(start))+
  geom_freqpoly(binwidth =500)+
  coord_cartesian(ylim = c(0,15))+
  my_theme2
pos_info<-after_break_pos_data
pos_info$pos <- as.integer(as.numeric(as.character(pos_info$pos)))
freq2<-ggplot(data = pos_info ,aes(pos))+
  geom_freqpoly(binwidth =500)+
  coord_cartesian(ylim = c(0,15))+
  my_theme2
freq2
pdf("/Dell/Dell12/Sequencing/Omicron/20211209/fig1_raw/fig1a_v2.pdf",useDingbats = F,width = 9.6,height = 7.2)  
grid.newpage()
pushViewport(viewport(layout=grid.layout(4,1)))
vplayout <- function(x,y)
  viewport(layout.pos.row = x,layout.pos.col = y)
print(omicron_pos,vp=vplayout(1,1))
print(after_break_pos,vp=vplayout(2,1))
print(freq1,vp=vplayout(3,1))
print(freq2,vp=vplayout(4,1))
dev.off()
