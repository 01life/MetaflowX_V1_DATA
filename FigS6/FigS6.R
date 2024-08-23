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
library("fmsb")

#binnigTools.stat ------ 

group_list = unlist(strsplit("HumanGut:MouseGut:Plant:Marine:AirSkin:Strain", ":"))
# improve_list = unlist(strsplit("ExtendImprove:MDMcleanerImprove:DeepurifyImprove", ":"))
color_var = c("#7570b3", "#a6761d", "#1b9e77","#1f78b4", "#e7298a", "#BDC367")
biner_list <- c("MetaBAT2","CONCOCT","SemiBin2","MaxBin2","MetaBinner","COMEBin","binny")


pdf("./FigS6.legend.pdf",width = 3,height = 3)
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("topleft", legend =group_list, pch=16, pt.cex=0.7, cex=0.7, bty='n',
       col = color_var) 
mtext("CAMI dataset", at=0.05, cex=0.7)
dev.off()


profile <- read.table("./6CAMI.dataset.7binner.running.info.txt",header=T, sep = "\t",check.name = F)

profile$ID <- profile$sampleID

map <- read.table("../group.txt",header=T, sep = "\t", check.name = F,row.names=1)
map$ID <- rownames(map)

map <- subset(map,Group %in% group_list)
needID <- intersect(map$ID, profile$ID)
map <- subset(map,ID %in% needID)
profile <- subset(profile,ID %in% needID)
plot <- merge(profile,map,by="ID",all=TRUE)

plot$task <- factor(plot$task,levels=biner_list)
plot$Group <- factor(plot$Group,levels=group_list)

cpu <- ggboxplot(plot, x = "task", y = "cpu",alpha=0.7,fill = "Group",outlier.size = 0.7,
                  ggtheme = theme_bw() +
                    theme(axis.text.x = element_text(color="black",size=8,angle=0,hjust= 0.5 ,vjust = 0 ,face="bold"),
                          axis.text.y = element_text(color="black",size=8,face="bold"),
                          axis.title.y=element_text(color="black",size=8,face="bold"),
                          axis.line = element_line(color="black"),
                          axis.ticks = element_line(color="black"),
                          strip.text.x = element_text(size = 8, colour = "black", face="bold"), 
                          strip.background  = element_blank(),
                          legend.position = "none",
                          legend.text = element_text(size=8, colour = "black"),
                          legend.title = element_text(size=8),
                          legend.key.width = unit(0.3, 'cm'),
                          legend.key.size = unit(0, 'lines'),
                          panel.grid = element_blank(),
                          panel.background = element_blank()
                          ),
                  legend = "none",title = "",xlab = '', ylab = 'CPU Utilization (%)',width = 0.7)+
  scale_y_continuous(expand = expansion(mult = c(0.01, 0.1)))+
  scale_fill_manual(values=color_var,labels=group_list,limits = group_list, breaks = group_list)

pdf("FigS6.a.pdf",width = 8,height = 3)
cpu
dev.off()

#"realtime_M"       
realtime <- ggboxplot(plot, x = "task", y = "realtime_M",alpha=0.7,fill = "Group",outlier.size = 0.7,
                 #palette = "jco",
                 ggtheme = theme_bw() +
                   theme(axis.text.x = element_text(color="black",size=8,angle=0,hjust= 0.5 ,vjust = 0 ,face="bold"),
                         axis.text.y = element_text(color="black",size=8,face="bold"),
                         axis.title.y=element_text(color="black",size=8,face="bold"),
                         axis.line = element_line(color="black"),
                         axis.ticks = element_line(color="black"),
                         strip.text.x = element_text(size = 8, colour = "black", face="bold"), 
                         strip.background  = element_blank(),
                         legend.position = "none",
                         legend.text = element_text(size=8, colour = "black"),
                         legend.title = element_text(size=8),
                         legend.key.width = unit(0.3, 'cm'),
                         legend.key.size = unit(0, 'lines'),
                         panel.grid = element_blank(),
                         panel.background = element_blank()),
                 legend = "none",title = "",xlab = '', ylab = 'Real Time (M)',width = 0.7)+
  scale_y_continuous(expand = expansion(mult = c(0.01, 0.1)))+
  scale_fill_manual(values=color_var,labels=group_list,limits = group_list, breaks = group_list)

pdf("FigS6.b.pdf",width = 8,height = 3)
realtime
dev.off()

#"peak_rss_G"



peak <- ggboxplot(plot, y = "peak_rss_G", x = "task",alpha=0.7,fill = "Group",outlier.size = 0.7,
                      #palette = "jco",
                      ggtheme = theme_bw() +
                        theme(axis.text.x = element_text(color="black",size=8,angle=0,hjust= 0.5 ,vjust = 0 ,face="bold"),
                              axis.text.y = element_text(color="black",size=8,face="bold"),
                              axis.title.y=element_text(color="black",size=8,face="bold"),
                              axis.line = element_line(color="black"),
                              axis.ticks = element_line(color="black"),
                              strip.text.x = element_text(size = 8, colour = "black", face="bold"), 
                              strip.background  = element_blank(),
                              legend.position = "none",
                              legend.text = element_text(size=8, colour = "black"),
                              legend.title = element_text(size=8),
                              legend.key.width = unit(0.3, 'cm'),
                              legend.key.size = unit(0, 'lines'),
                              panel.grid = element_blank(),
                              panel.background = element_blank()),
                      legend ="none" ,title = "",xlab = '', ylab = 'Peak Resident Set Size (G)',width = 0.7)+
  scale_y_continuous(expand = expansion(mult = c(0.01, 0.1)))+
  scale_fill_manual(values=color_var,labels=group_list,limits = group_list, breaks = group_list)

pdf("FigS6.c.pdf",width = 8,height = 3)
peak
dev.off()



