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


#Fig 2 remove host ------ 

gtdb_test_time_pd <- read.table("gtdb_run_time.txt",header=T, sep = "\t", check.name = F)
gtdb_tool <- c("MetaflowX-chunked","gtdbtk-single")
gtdb_tool_col <- c("#24B064","#636363")
gtdb_test_time_pd$method <- factor(gtdb_test_time_pd$method,levels=gtdb_tool)

gtdb_test_time_pd$group <- as.factor(gtdb_test_time_pd$group)

bar <- ggbarplot(gtdb_test_time_pd, x= "group", y="time",alpha=0.8,width = 0.8,
  fill = "method", color = "method", position = position_dodge(0.9),
  label = TRUE,lab.size = 2)+ #, lab.col = "white", lab.pos = "in"
      labs(y="GTDB-Tk Runtime (min)",x="Bin Count")+
    scale_fill_manual(values = gtdb_tool_col, labels = gtdb_tool, limits = gtdb_tool, breaks = gtdb_tool)+
    scale_color_manual(values = gtdb_tool_col, labels = gtdb_tool, limits = gtdb_tool, breaks = gtdb_tool)+
    theme_classic() +
      theme(axis.text.x = element_text(color = "black", size = 8, angle = 0, hjust = 0.5, vjust =1, face = "bold"),
            axis.text.y = element_text(color = "black", size = 8, face = "bold"),
            axis.title.y = element_text(color = "black", size = 8,face = "bold"),
            axis.line = element_line(color = "black"),
            axis.ticks = element_line(color = "black"),
            legend.text = element_text(size = 8, colour = "black"),
            legend.title = element_text(size = 8),
            legend.key.size = unit(0, 'lines'),
            legend.position = "none",
            panel.grid = element_blank(),
            panel.background = element_blank())

pdf("Fig2c_GTDB_Runtime.pdf", width = 2.5 , height = 3 )
bar
dev.off()

