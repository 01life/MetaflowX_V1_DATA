#!/usr/bin/env Rscript

#library ------ 
library("ggpubr")
library("ggplot2")
library("ggprism")
library("rstatix")
library("gtools")
library("tidyverse")
library("dplyr") 
library("reshape2")
library("aplot")
library("eulerr")
library("ggvenn")


group <- c("MetaWRAP","MetaflowX")
group_col <- c("#9c89b8","#84a59d")
Completeness_order <- c("<50",">=50",">60",">70",">80",">90")
Contamination_order <- c(">=10","<10","<5","<1","<0.1" )


#Veen ------

#BinID   GTDB_taxonomy   fastani_reference       Completeness    Contamination   Group
pd <- read.table("metawrap_metaflowX.qs.txt",header=T, sep = "\t", check.name = F)

mr <- subset(pd,Group %in% "MetaWRAP")
mf <- subset(pd,Group %in% "MetaflowX")

listInput <- list(`MetaWRAP` = mr$GTDB_taxonomy,
          `MetaflowX` = mf$GTDB_taxonomy)

plot_venn <- ggvenn(
  listInput, 
  fill_color = group_col,
  stroke_size = 0.4, set_name_size = 4,text_size=4,
  )


pdf("FigS15a.pdf",width = 3.5 , height = 3.5)
plot_venn
dev.off()



#boxplot ------#
pd_pair <- read.table("pair_metawrap_metaflowX.qs.txt",header=T, sep = "\t", check.name = F)


my_comparisons <- list( c("MetaWRAP", "MetaflowX") )
index_list <- c("Completeness","Contamination")

pd_pair$Group <- factor(pd_pair$Group,levels=group)

pd_pair$BinID <- factor(pd_pair$BinID, levels = unique(pd_pair$BinID))

for (i in  index_list ){
  itemViolin <- ggviolin(pd_pair, x = "Group", y = i, color = "Group",alpha=0.7,outlier.size = 0.7,size = 0.2,
                        add = c("jitter", "mean_sd"),add.params = list(binwidth=0.5,size = 0.15),
                        ggtheme = theme_bw() +
                                  theme(
                                    axis.text.x = element_text(color="black",size=10,angle=-0,hjust= 0.5 ,vjust = 0 ,face="bold"),
                                    axis.text.y = element_text(color="black",size=10,face="bold"),
                                    axis.title.y=element_text(color="black",size=10),
                                    axis.line = element_line(color="black"),
                                    axis.ticks = element_line(color="black"),
                                    strip.text.x = element_text(size = 8, colour = "black", face="bold"), 
                                    strip.background  = element_blank(),
                                    legend.position = "none",
                                    legend.text = element_text(size = 6, colour = "black"),
                                    legend.title = element_text(size = 6),
                                    legend.key.width = unit(0.3, 'cm'),
                                    legend.key.size = unit(0, 'lines'),
                                    panel.grid = element_blank(),
                                    panel.background = element_blank()
                                    ),
                         legend = "none",title = "",xlab = "", ylab = i,width = 0.7
                      )+ 
                stat_compare_means(comparisons = my_comparisons , method = "wilcox.test",size=2,paired = TRUE)+
                scale_color_manual(values=group_col,labels=group,limits = group, breaks = group)
                 
      pdf(file=paste("FigS15b",i,"_improvement_violin.pdf",sep=""),width = 3 , height = 3)
      print(itemViolin)
      dev.off()


}


#---- table

      # 使用 across() 函数计算多个列的统计值
      summary_stats <- pd_pair %>%
      group_by(Group) %>%
            summarise(
                  across(
                  c(Completeness, Contamination),
                  list(
                  Mean = ~ mean(.x, na.rm = TRUE),
                  Median = ~ median(.x, na.rm = TRUE),
                  SD = ~ sd(.x, na.rm = TRUE),
                  Min = ~ min(.x, na.rm = TRUE),
                  Max = ~ max(.x, na.rm = TRUE)
                  ),
                  .names = "{col}_{fn}"
                  )
            )

      # 保存统计总结到文件

      write.table(summary_stats,file = "pair_metawrap_metaflowx_summary_stats.txt" , sep = "\t"    , row.names = T, col.names = NA, quote = F)
