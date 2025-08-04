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

checkm2_result <- read.table("3_workflow_merged_checkm2_with_group.tsv",header=T, sep = "\t", check.name = F)

workflows <- c("MetaflowX", "nf-mag", "MetaWRAP" )
workflows_colors <- c("#24B064", "#118ab2",  "#f07167") # "#ffd166",

qs_col <- c('#344E41','#588157','#DAD7CD')
qs_list <- c("HQ", "MQ", "LQ")



# 定义 bin_quality：HQ / MQ / LQ
checkm2_result$bin_quality <- with(checkm2_result, ifelse(
  Completeness >= 90 & Contamination <= 5, "HQ",
  ifelse(Completeness >= 50 & Contamination <= 10, "MQ", "LQ")
))

# 可选：将 bin_quality 转换为因子，设定顺序 HQ > MQ > LQ
checkm2_result$bin_quality <- factor(checkm2_result$bin_quality, levels = qs_list)

checkm2_result$QS <- checkm2_result$Completeness -  checkm2_result$Contamination * 5 



checkm2_result$workflow <- factor(checkm2_result$workflow, levels = workflows)

HM_result <- subset(checkm2_result, bin_quality %in% c("HQ","MQ")) 

index_list <- c("Completeness","Contamination","Genome_Size","QS")


for (i in index_list) {
  itemBox <- ggboxplot(HM_result,
                        x = "group", y = i,alpha=0.1,
                        fill = "workflow", color = "workflow", outlier.shape = NA,
                        outlier.size = 0.5, size = 0.4, width = 0.7,
                        ggtheme = theme_bw() +
                          theme(
                            axis.text.x = element_text(color = "black", size = 10, angle = 0, hjust = 0.5, vjust = -0.5, face = "bold"),
                            axis.text.y = element_text(color = "black", size = 10, face = "bold"),
                            axis.title.y = element_text(color = "black", size = 10),
                            axis.line = element_line(color = "black"),
                            axis.ticks = element_line(color = "black"),
                            strip.text.x = element_text(size = 8, colour = "black", face = "bold"),
                            strip.background = element_blank(),
                            legend.position = "none",
                            legend.text = element_text(size = 6, colour = "black"),
                            legend.title = element_text(size = 6),
                            legend.key.width = unit(0.3, 'cm'),
                            legend.key.size = unit(0, 'lines'),
                            panel.grid = element_blank(),
                            panel.background = element_blank()
                          ),
                        legend = "none", title = "", xlab = "", ylab = i
                      ) +
                scale_y_continuous(expand = expansion(mult = c(0.03, 0.03))) +
                scale_fill_manual(values = workflows_colors, labels = workflows, limits = workflows, breaks = workflows)+
                scale_color_manual(values = workflows_colors, labels = workflows, limits = workflows, breaks = workflows)

  fp <- facet(itemBox ,
        facet.by = "bin_quality",
        ncol =1,
        short.panel.labs = TRUE,
        scales = "free_y",
        strip.position = "top",
        panel.labs.font = list(size = 8,angle = 0),panel.grid = element_blank(),
        panel.labs.background = list(fill = NA, color = NA)
        )
  pdf(file = paste0("Fig3b_workflow_bin_", i, "_boxplot.pdf"), width = 3, height = 3)
  print(fp)
  dev.off()
}





# 参数定义
bin_level <- c("HQ", "MQ")
group_list <- c("HMP", "marine", "MG", "Plant")
index_list <- c("Completeness", "Contamination", "Genome_Size", "QS")

# 所有组合
combinations <- expand.grid(
  bin = bin_level,
  group = group_list,
  index = index_list,
  stringsAsFactors = FALSE
)

# 定义一个函数：输入组合，输出检验结果
do_test <- function(bin, group, index) {
  df_sub <- checkm2_result %>%
    filter(bin_quality == bin, group == group)

  # 如果分组太少，跳过
  if (length(unique(df_sub$workflow)) < 2) {
    return(NULL)
  }

  # 如果变量全 NA 或全常数，也跳过
  if (all(is.na(df_sub[[index]])) || length(unique(na.omit(df_sub[[index]]))) == 1) {
    return(NULL)
  }

  # 做检验
  result <- tryCatch({
    df_sub %>%
      pairwise_wilcox_test(
        formula = as.formula(paste(index, "~ workflow")),
        p.adjust.method = "BH"
      ) %>%
      mutate(bin = bin, group = group, index = index)
  }, error = function(e) {
    return(NULL)
  })

  return(result)
}

# 遍历所有组合并收集结果
all_results <- pmap_dfr(
  combinations,
  ~ do_test(..1, ..2, ..3)  # ..1=bin, ..2=group, ..3=index
)

# 输出结果
print(head(all_results))

write.table(all_results, file = "wilcoxon_all_results.csv", sep = "\t", quote = FALSE, row.names = FALSE, col.names = TRUE)


summary_stats <- checkm2_result %>%
group_by(workflow,bin_quality,group) %>%
    summarise(
            across(
            c(Completeness, Contamination,QS),
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
write.table(summary_stats, file = "checkm2_results_summmary.tsv", sep = "\t", quote = FALSE, row.names = FALSE, col.names = TRUE)


#----- count bin level
bin_level_count <- checkm2_result %>%
  count(workflow, bin_quality)

print(bin_level_count)
bar <- ggbarplot(bin_level_count, x= "bin_quality", y="n",alpha=0.8,
  fill = "workflow", color = "workflow", position = position_dodge(0.9),
  label = TRUE,lab.size = 2)+ #, lab.col = "white", lab.pos = "in"
      labs(y="Count of Bins",x="")+
    # scale_y_continuous(expand = c(0,0))+
    # scale_x_discrete(expand=c(0.4,0)) +
    scale_fill_manual(values = workflows_colors, labels = workflows, limits = workflows, breaks = workflows)+
    scale_color_manual(values = workflows_colors, labels = workflows, limits = workflows, breaks = workflows)+
    theme_classic() +
      theme(axis.text.x = element_text(color = "black", size = 10, angle = 0, hjust = 0.5, vjust =1, face = "bold"),
            axis.text.y = element_text(color = "black", size = 10, face = "bold"),
            axis.title.y = element_text(color = "black", size = 10,face = "bold"),
            axis.line = element_line(color = "black"),
            axis.ticks = element_line(color = "black"),
            legend.text = element_text(size = 8, colour = "black"),
            legend.title = element_text(size = 8),
            legend.key.size = unit(0.8, 'lines'),
            legend.position = "none",
            panel.grid = element_blank(),
            panel.background = element_blank())

pdf("Fig3a_workflow_bin_level_count.pdf", width = 3 , height = 3 )
bar
dev.off()



gs <-   ggscatter(checkm2_result, x = "Completeness", y = "Contamination", color = "workflow", size=1.2, alpha=0.3)+
    scale_fill_manual(values = workflows_colors, labels = workflows, limits = workflows, breaks = workflows)+
    scale_color_manual(values = workflows_colors, labels = workflows, limits = workflows, breaks = workflows)+
    theme_bw() +
      theme(axis.text.x = element_text(color = "black", size = 10, angle = 0, hjust = 0.5, vjust =1, face = "bold"),
            axis.text.y = element_text(color = "black", size = 10, face = "bold"),
            axis.title.y = element_text(color = "black", size = 10,face = "bold"),
            axis.title.x = element_text(color = "black", size = 10,face = "bold"),
            axis.line = element_line(color = "black"),
            axis.ticks = element_line(color = "black"),
            legend.text = element_text(size = 8, colour = "black"),
            legend.title = element_text(size = 8),
            legend.key.size = unit(0, 'lines'),
            legend.position = "right",
            panel.grid = element_blank(),
            panel.background = element_blank())

pdf("Fig3Sa_workflow_bin_scatter.pdf", width = 4 , height = 3 )
gs 
dev.off()



pdf("Fig3_lenged.pdf",width = 4,height = 1)

  par(mar=c(1,1,1,1),mgp=c(0,0,0))

  plot(NULL, xlim = c(0, 1), ylim = c(0, 1),
      xlab = "", ylab = "", xaxt = "n", yaxt = "n", bty = "n")

  # 添加 bar 样式图例
  legend(
    x = 0.05, y = 1,  # 左上角位置
    legend = workflows,
    pch = 15,  # 方形色块
    col = workflows_colors,
    pt.cex = 1.8,    # 色块大小
    cex = 0.9,       # 字体大小
    ncol = 3,        # 分成三列
    bty = "n",       # 无边框
    x.intersp = 0.8, # 色块与文字之间间距
    y.intersp = 1.4  # 行距
  )
dev.off()

