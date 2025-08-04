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

remove_host_test_time_pd <- read.table("remove_host_test_time.txt",header=T, sep = "\t", check.name = F)
remove_host_tool <- c("MetaflowX","KneadData")
remove_host_tool_col <- c("#24B064","#636363")
remove_host_test_time_pd$tool <- factor(remove_host_test_time_pd$tool,levels=remove_host_tool)


p2a <- ggplot(remove_host_test_time_pd, aes(x = tool, y = time, fill = tool)) +
  geom_bar(stat = "summary", fun = mean, width = 0.5, color = "black", alpha = 0.8) +  # 平均值柱子
  stat_summary(fun.data = mean_se, geom = "errorbar",width = 0.3, linewidth = 0.5) + # 控制误差线长度和粗细  
  geom_jitter(width = 0.15, size = 0.8, alpha = 1, shape = 21, stroke = 0.2, color = "black") +  # 控制点大小、透明度等
  scale_fill_manual(values=remove_host_tool_col,labels=remove_host_tool,limits = remove_host_tool, breaks = remove_host_tool)+
  theme_classic() +
  theme(
    axis.text.x = element_text(color="black",size=8,angle=0,hjust= 0.5 ,vjust = 0 ,face="bold"),
    axis.text.y = element_text(color = "black", size = 8, face = "bold"),
    axis.title.y = element_text(color = "black", size = 8, face = "bold"),
    axis.title.x = element_text(color = "black", size = 8, face = "bold"),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    strip.text.x = element_text(size = 8, colour = "black", face = "bold"),
    strip.background = element_blank(),
    legend.position = "none",
    panel.grid = element_blank(),
    panel.background = element_blank(),
    plot.margin = margin(l = 5, r = 10,t=10)
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(x = "", y = "Remove Host Time (second)")


ggsave("Fig2b-metaflowX_VS_KneadData.pdf", p2a, width = 2, height = 3)