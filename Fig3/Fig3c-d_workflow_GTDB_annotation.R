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

gtdb_result <- read.table("3_workflow_merged_GTDB_annotion.tsv",header=T, sep = "\t", check.name = F)

workflows <- c("MetaflowX", "nf-mag", "MetaWRAP" )
workflows_colors <- c("#24B064", "#118ab2",  "#f07167") # "#ffd166",

classification_col <- c('#24B064','#636363')
classification_list <- c("classified","unclassified")


gtdb_result$workflow <- factor(gtdb_result$workflow,levels=workflows)
gtdb_result$classification_status <- factor(gtdb_result$classification_status, levels = classification_list)
gtdb_result$level <- factor(gtdb_result$level, levels = c("phylum", "class", "order", "family", "genus", "species"))




#注释率

classification_ratio_df <- gtdb_result %>%
  filter(level == "species") %>%
  group_by(workflow, level,classification_status) %>%
  summarise(bin_count = sum(count), .groups = "drop") 

print(classification_ratio_df)


classification_ratio_df <- classification_ratio_df %>%
  mutate(
    workflow_str = as.character(workflow),
    fill_group = ifelse(classification_status == "classified", workflow_str, "unclassified"))

fill_colors <- c(
  setNames(workflows_colors, workflows),  # 对应 workflow 的颜色
  "unclassified" = "#636363"              # 加上 unclassified 的灰色
)

print(classification_ratio_df)


classification_ratio  <- ggbarplot(classification_ratio_df, x= "workflow", y="bin_count",alpha=0.8,
  fill = "fill_group", color = "fill_group", position = position_dodge(0.8),
  label = TRUE,lab.size = 2)+ #, lab.col = "white", lab.pos = "in"
      labs(y="#Bins",x="")+
    # scale_y_continuous(expand = c(0,0))+
    # scale_x_discrete(expand=c(0.4,0)) +
    scale_fill_manual(values = fill_colors)+
    scale_color_manual(values = fill_colors)+
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

pdf("Fig3d_workflow_bin_taxonomy_classification_ratio.pdf", width = 2.5 , height = 3 )
classification_ratio
dev.off()


summary_stats <- gtdb_result %>%
  group_by(workflow, level) %>%
  summarise(
    level_count = n_distinct(name),
    .groups = "drop"
  )

print(summary_stats)

bar <- ggbarplot(summary_stats, x= "level", y="level_count",alpha=0.8,
  fill = "workflow", color = "workflow", position = position_dodge(0.9),
  label = TRUE,lab.size = 2)+ #, lab.col = "white", lab.pos = "in"
      labs(y="counts",x="")+
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

pdf("Fig3c_workflow_bin_taxonomy_count.pdf", width = 3.5 , height = 3 )
bar
dev.off()


sample_species_stats <- gtdb_result %>%
  filter(level =="species") %>%
  group_by(workflow, level, sample_id) %>%
  summarise(
    level_count = n_distinct(name),
    .groups = "drop"
  )

sample_genus_stats <- gtdb_result %>%
  filter(level =="genus") %>%
  group_by(workflow, level, sample_id) %>%
  summarise(
    level_count = n_distinct(name),
    .groups = "drop"
  )

write.table(sample_genus_stats, file = "genus.tsv", sep = "\t", quote = FALSE, row.names = FALSE, col.names = TRUE)
write.table(sample_species_stats, file = "species.tsv", sep = "\t", quote = FALSE, row.names = FALSE, col.names = TRUE)
