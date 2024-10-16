#library ------ 
library("ggpubr")
library("ggplot2")
library("ggprism")
library("rstatix")
library("gtools")
library("tidyverse")
library("multcompView")
library("dplyr") 
library("wesanderson")
library("reshape2")
library("ggseqlogo")
library("ggradar")

# correlate with contig's info ----

contigStat <- read.table("./6CAMI.dataset.all.sample.contig.stat.txt",header=T, sep = "\t", check.name = F,row.names=1)
contigStat$ID <- rownames(contigStat)

group_list = unlist(strsplit("HumanGut:MouseGut:Plant:Marine:AirSkin:Strain", ":"))
color_var = c("#7570b3", "#a6761d", "#1b9e77","#1f78b4", "#e7298a", "#BDC367")

map <- read.table("./group.txt",header=T, sep = "\t", check.name = F,row.names=1)
map$ID <- rownames(map)

contigDBset <- merge(map,contigStat,by="ID",all=TRUE)
data <- melt(contigDBset)

data$Group <- factor(data$Group,levels=group_list)


Fbox1 <- facet(ggboxplot(data, x = "Group", y = "value", fill = "Group",facet.by = "variable",alpha=0.7,outlier.size = 0.7,size = 0.2,
                         ggtheme = theme_bw() +
                           theme(axis.text.x = element_text(color="black",size=8,angle=-45,hjust= 0.1 ,vjust = 0 ,face="bold"),
                                 axis.text.y = element_text(color="black",size=8,face="bold"),
                                 axis.title.y=element_text(color="black",size=8),
                                 axis.line = element_line(color="black"),
                                 axis.ticks = element_line(color="black"),
                                 strip.text.x = element_text(size = 8, colour = "black", face="bold"), 
                                 strip.background  = element_blank(),
                                 legend.position = "right",
                                 legend.text = element_text(size = 6, colour = "black"),
                                 legend.title = element_text(size = 6),
                                 legend.key.width = unit(0.3, 'cm'),
                                 legend.key.size = unit(0, 'lines'),
                                 panel.grid = element_blank(),
                                 panel.background = element_blank()),
                         legend = "right",title = "",xlab = 'Group', ylab = 'Value',width = 0.7)+ 
                scale_y_continuous(expand = expansion(mult = c(0.01, 0.1)))+
                scale_fill_manual(values=color_var,labels=group_list,limits = group_list, breaks = group_list),
                facet.by = "variable", ncol =2,short.panel.labs = TRUE,strip.position = "top",scales = "free_y", 
                 panel.labs.font = list(size = 8, angle = 0),panel.grid = element_blank(), panel.labs.background = list(fill = NA, color = NA) )


pdf("6CAMI.dataset.info.pdf",width = 5 , height = 4)
Fbox1
dev.off()