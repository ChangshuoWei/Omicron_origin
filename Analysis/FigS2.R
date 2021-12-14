pos_info <- fread('/Dell/Dell4/weics/Omicron/recomb/USA_var.txt', header = F, sep = '\t',
                  stringsAsFactors = F) %>% as.data.frame()
colnames(pos_info) <- c("name",'start','refer','alter')

orfsubg$region <-'genome'
orfsubg$g <- 10
USA_pos <- ggplot(orfsubg)+
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
USA_pos

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
omicron_pos




pos_info <- fread('/Dell/Dell4/weics/Omicron/recomb/USA_var.txt', header = F, sep = '\t',
                  stringsAsFactors = F) %>% as.data.frame()
colnames(pos_info) <- c("name",'start','refer','alter')

orfsubg$region <-'genome'
orfsubg$g <- 10
USA_pos_zoom <- ggplot(orfsubg)+
  geom_segment(aes(x = 22500, y = g, xend = 23600, yend = g), colour = 'grey')+
  geom_rect(aes(ymin=g-0.5, ymax=g+0.5, xmin=22500, xmax=23600, fill=type), color='black')+
  geom_vline(xintercept = pos_info[pos_info$start > 22500 & pos_info$start < 23600,]$start,color = "red")+
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
USA_pos_zoom

pos_info <- fread('./Omicron_mutation_fastml_pos.txt', header = F, sep = '\t',
                  stringsAsFactors = F) %>% as.data.frame()
colnames(pos_info) <- c('start','refer','alter')
orfsubg$region <-'genome'
orfsubg$g <- 10
omicron_pos_zoom <- ggplot(orfsubg)+
  geom_segment(aes(x = 22500, y = g, xend = 23600, yend = g), colour = 'grey')+
  geom_rect(aes(ymin=g-0.5, ymax=g+0.5, xmin=22500, xmax=23600, fill=type), color='black')+
  geom_vline(xintercept = pos_info[pos_info$start > 22500 & pos_info$start < 23600,]$start,color = "red")+
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
omicron_pos_zoom

#/Dell/Dell12/Sequencing/Omicron/20211209/fig1_raw/
pdf("figS3.pdf",useDingbats = F,width = 9.6,height = 7.2)  
grid.newpage()
pushViewport(viewport(layout=grid.layout(4,1)))
vplayout <- function(x,y)
  viewport(layout.pos.row = x,layout.pos.col = y)
print(omicron_pos,vp=vplayout(1,1))
print(USA_pos,vp=vplayout(2,1))
print(omicron_pos_zoom,vp=vplayout(3,1))
print(USA_pos_zoom,vp=vplayout(4,1))
dev.off()
