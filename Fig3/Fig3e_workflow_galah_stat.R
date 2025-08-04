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
library("ggbeeswarm")
library("gghalves")
library("purrr")
library("readr")

#Fig 2 remove host ------ 

galah_result <- read.table("3_workflow_galah-Non-redundant.txt",header=T, sep = "\t", check.name = F)

workflows <- c("MetaflowX", "nf_mag", "MetaWRAP" )
workflows_colors <- c("#24B064", "#118ab2",  "#f07167") # "#ffd166",


# 转换为 long 格式
df_long <- galah_result %>%
  pivot_longer(
    cols = c(MetaflowX, nf_mag, MetaWRAP),
    names_to = "workflow",
    values_to = "counts"
  )

# 查看转换后的数据
print(df_long)

df_long$workflow <- factor(df_long$workflow,levels=workflows)



bar <- ggbarplot(df_long, x= "level", y="counts",alpha=0.8,
  fill = "workflow", color = "workflow", position = position_dodge(0.9),
  label = TRUE,lab.size = 2)+ #, lab.col = "white", lab.pos = "in"
      labs(y="#Representative Taxa",x="")+
    # scale_y_continuous(expand = c(0,0))+
    # scale_x_discrete(expand=c(0.4,0)) +
    scale_fill_manual(values = workflows_colors, labels = workflows, limits = workflows, breaks = workflows)+
    scale_color_manual(values = workflows_colors, labels = workflows, limits = workflows, breaks = workflows)+
    theme_classic() +
      theme(axis.text.x = element_text(color = "black", size = 8, angle = 0, hjust = 0.5, vjust =1, face = "bold"),
            axis.text.y = element_text(color = "black", size = 8, face = "bold"),
            axis.title.y = element_text(color = "black", size = 8,face = "bold"),
            axis.line = element_line(color = "black"),
            axis.ticks = element_line(color = "black"),
            legend.text = element_text(size = 8, colour = "black"),
            legend.title = element_text(size = 8),
            legend.key.size = unit(0.5, 'lines'),
            legend.position = "none",
            panel.grid = element_blank(),
            panel.background = element_blank())

pdf("Fig3e_Unique_Representative_Taxo.pdf", width = 2 , height = 3 )
bar
dev.off()

