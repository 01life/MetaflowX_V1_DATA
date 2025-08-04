# 加载必要的包
library(ggpubr)
library(ggplot2)
library(ggrepel)
library(ggdist)
library(dplyr)


# 创建示例数据
set.seed(123)

#---- singal
      df <- read.table("./single_Bins_Reassembly_Optimization_evaluation.point.txt",header=T, sep = "\t", check.name = F,quote="")

      color_var <- c('#EF476F',  '#FFD166',  '#06D6A0',  '#118AB2')
      group_list <- c("Init","ReAss","ReBin","ReFine")


      df$Stage <- factor(df$Stage, levels = group_list)

      p <- ggplot(df, aes(x = Stage, y = Completeness)) +
      
      geom_line(aes(group=BinID), color="#d9d9d9", linewidth = 0.2, alpha = 0.5) + # 配对线
      geom_point(aes(color = Stage),  shape = 20, size = 1) + # 离散点
      geom_boxplot(aes(fill = Stage), alpha = 0.5, outliers=FALSE, color = "black") + # 箱线图和离群点
      
      geom_hline(yintercept = 50, linewidth = 0.5, linetype = "dashed", color = "gray") + # 灰色虚线
      scale_color_manual(values = color_var, labels = group_list) +
      scale_fill_manual(values = color_var, labels = group_list) +
      labs(title = "single assembly", x = "", y = "Completeness") +
      theme_minimal() +
      theme(axis.text.x = element_text(color = "black", size = 10, angle = -45, hjust = 0, vjust = 0, face = "bold"),
            axis.text.y = element_text(color = "black", size = 10, face = "bold"),
            axis.title.y = element_text(color = "black", size = 10),
            axis.line = element_line(color = "black"),
            axis.ticks = element_line(color = "black"),
            legend.text = element_text(size = 6, colour = "black"),
            legend.title = element_text(size = 8),
            legend.key.size = unit(0, 'lines'),
            legend.position = "none",
            panel.grid = element_blank(),
            panel.background = element_blank())


      pdf("single_reass_bin_Completeness_paired.pdf", width = 3, height = 4)
      print(p)
      dev.off()


      p <- ggplot(df, aes(x = Stage, y = Contamination)) +
      
      geom_line(aes(group=BinID), color="#d9d9d9", linewidth = 0.2, alpha = 0.5) + # 配对线
      geom_point(aes(color = Stage),  shape = 20, size = 1) + # 离散点
      geom_boxplot(aes(fill = Stage), alpha = 0.5, outliers=FALSE, color = "black") + # 箱线图和离群点
      
      geom_hline(yintercept = 10, linewidth = 0.5, linetype = "dashed", color = "gray") + # 灰色虚线
      scale_color_manual(values = color_var, labels = group_list) +
      scale_fill_manual(values = color_var, labels = group_list) +
      labs(title = "single assembly", x = "", y = "Contamination") +
      theme_minimal() +
      theme(axis.text.x = element_text(color = "black", size = 10, angle = -45, hjust = 0, vjust = 0, face = "bold"),
            axis.text.y = element_text(color = "black", size = 10, face = "bold"),
            axis.title.y = element_text(color = "black", size = 10),
            axis.line = element_line(color = "black"),
            axis.ticks = element_line(color = "black"),
            legend.text = element_text(size = 6, colour = "black"),
            legend.title = element_text(size = 8),
            legend.key.size = unit(0, 'lines'),
            legend.position = "none",
            panel.grid = element_blank(),
            panel.background = element_blank())


      pdf("single_reass_bin_Contamination_paired.pdf", width = 3, height = 4)
      print(p)
      dev.off()

#---- co-assembly
      df_co <- read.table("./co-ass-Bins_Reassembly_Optimization_evaluation.point.txt",header=T, sep = "\t", check.name = F,quote="")

      color_var <- c('#EF476F',  '#FFD166',  '#06D6A0',  '#118AB2')
      group_list <- c("Init","ReAss","ReBin","ReFine")


      df_co$Stage <- factor(df_co$Stage, levels = group_list)

      p <- ggplot(df_co, aes(x = Stage, y = Completeness)) +
      
      geom_line(aes(group=BinID), color="#d9d9d9", linewidth = 0.2, alpha = 0.5) + # 配对线
      geom_point(aes(color = Stage),  shape = 20, size = 1) + # 离散点
      geom_boxplot(aes(fill = Stage), alpha = 0.5, outliers=FALSE, color = "black") + # 箱线图和离群点
      
      geom_hline(yintercept = 50, linewidth = 0.5, linetype = "dashed", color = "gray") + # 灰色虚线
      scale_color_manual(values = color_var, labels = group_list) +
      scale_fill_manual(values = color_var, labels = group_list) +
      labs(title = "co-assembly", x = "", y = "Completeness") +
      theme_minimal() +
      theme(axis.text.x = element_text(color = "black", size = 10, angle = -45, hjust = 0, vjust = 0, face = "bold"),
            axis.text.y = element_text(color = "black", size = 10, face = "bold"),
            axis.title.y = element_text(color = "black", size = 10),
            axis.line = element_line(color = "black"),
            axis.ticks = element_line(color = "black"),
            legend.text = element_text(size = 6, colour = "black"),
            legend.title = element_text(size = 8),
            legend.key.size = unit(0, 'lines'),
            legend.position = "none",
            panel.grid = element_blank(),
            panel.background = element_blank())


      pdf("co-ass_reass_bin_Completeness_paired.pdf", width = 3, height = 4)
      print(p)
      dev.off()


      p <- ggplot(df_co, aes(x = Stage, y = Contamination)) +
      
      geom_line(aes(group=BinID), color="#d9d9d9", linewidth = 0.2, alpha = 0.5) + # 配对线
      geom_point(aes(color = Stage),  shape = 20, size = 1) + # 离散点
      geom_boxplot(aes(fill = Stage), alpha = 0.5, outliers=FALSE, color = "black") + # 箱线图和离群点
      
      geom_hline(yintercept = 10, linewidth = 0.5, linetype = "dashed", color = "gray") + # 灰色虚线
      scale_color_manual(values = color_var, labels = group_list) +
      scale_fill_manual(values = color_var, labels = group_list) +
      labs(title = "co-assembly", x = "", y = "Contamination") +
      theme_minimal() +
      theme(axis.text.x = element_text(color = "black", size = 10, angle = -45, hjust = 0, vjust = 0, face = "bold"),
            axis.text.y = element_text(color = "black", size = 10, face = "bold"),
            axis.title.y = element_text(color = "black", size = 10),
            axis.line = element_line(color = "black"),
            axis.ticks = element_line(color = "black"),
            legend.text = element_text(size = 6, colour = "black"),
            legend.title = element_text(size = 8),
            legend.key.size = unit(0, 'lines'),
            legend.position = "none",
            panel.grid = element_blank(),
            panel.background = element_blank())


      pdf("co-ass_reass_bin_Contamination_paired.pdf", width = 3, height = 4)
      print(p)
      dev.off()


      df_co$Method <- rep('Co_Ass', length(df_co$BinID))
      df_co$mergeID <- paste(df_co$BinID,df_co$Stage,sep="-")

      df$Method <- rep('Single', length(df$BinID))
      df$mergeID <- paste(df$BinID,df$Stage,sep="-")


      merge_df <- rbind(df, df_co)

      sub_df <- subset(merge_df,Stage %in% c("ReAss"))

      method_color <- c('#018571', '#a6611a')
      method_list <- c("Single","Co_Ass")

      sub_df$Method <- factor(sub_df$Method, levels = method_list)

      sub_df  <-  sub_df %>%
                  group_by(BinID) %>%
                  filter(n() == 2) %>% # 过滤掉不完整的配对数据
                  ungroup()

      sub_df$BinID <- factor(sub_df$BinID, levels = unique(df$BinID))







 p <- ggplot(sub_df, aes(x = Method, y = Contamination)) +
      geom_line(aes(group = BinID), color = "#d9d9d9", linewidth = 0.2, alpha = 0.5) + # 配对线
      stat_slab(aes(thickness = after_stat(pdf * n), color = Method, fill = Method), scale = 0.5, slab_linewidth = 0.5, alpha = 0.4) + # 线条图
      stat_dotsinterval(aes(color = Method, fill = Method), side = "bottom", scale = 0.5, slab_linewidth = NA, alpha = 0.6) + # 离散点
      geom_boxplot(width = 0.1, alpha = 0.5, outliers = FALSE, fill =NA, color = "#525252") + # 仅箱线图
      geom_hline(yintercept = 10, linewidth = 0.5, linetype = "dashed", color = "gray") + # 灰色虚线
      stat_compare_means(method = "wilcox.test", paired = TRUE, label = "p.format", label.y = 100,size = 2) + # 添加 Wilcoxon 检验结果


      scale_color_manual(values = method_color, labels = method_list) +
      scale_fill_manual(values = method_color, labels = method_list) +

      labs(title = "", x = "", y = "Contamination") +
      theme_minimal() +
      theme(
      axis.text.x = element_text(color = "black", size = 10, angle = 0, hjust = 0.5, vjust = 0, face = "bold"),
      axis.text.y = element_text(color = "black", size = 10, face = "bold"),
      axis.title.y = element_text(color = "black", size = 10),
      axis.line = element_line(color = "black"),
      axis.ticks = element_line(color = "black"),
      legend.text = element_text(size = 6, colour = "black"),
      legend.title = element_text(size = 8),
      legend.key.size = unit(0, 'lines'),
      legend.position = "none",
      panel.grid = element_blank(),
      panel.background = element_blank()
      )

      pdf("co-ass_vs_single_Contamination_paired.pdf", width = 3 , height = 2.5)
      print(p)
      dev.off()
      
 p <- ggplot(sub_df, aes(x = Method, y = Completeness)) +
      geom_line(aes(group = BinID), color = "#d9d9d9", linewidth = 0.2, alpha = 0.5) + # 配对线
      stat_slab(aes(thickness = after_stat(pdf * n), color = Method, fill = Method), scale = 0.5, slab_linewidth = 0.5, alpha = 0.4) + # 线条图
      stat_dotsinterval(aes(color = Method, fill = Method), side = "bottom", scale = 0.5, slab_linewidth = NA, alpha = 0.6) + # 离散点
      geom_boxplot(width = 0.1, alpha = 0.5, outliers = FALSE, fill =NA, color = "#525252") + # 仅箱线图
      geom_hline(yintercept = 50, linewidth = 0.5, linetype = "dashed", color = "gray") + # 灰色虚线
      stat_compare_means(method = "wilcox.test", paired = TRUE, label = "p.format", label.y = 100, size = 2) + # 添加 Wilcoxon 检验结果
  
      scale_color_manual(values = method_color, labels = method_list) +
      scale_fill_manual(values = method_color, labels = method_list) +

      labs(title = "", x = "", y = "Completeness") +
      theme_minimal() +
      theme(
      axis.text.x = element_text(color = "black", size = 10, angle = 0, hjust = 0.5, vjust = 0, face = "bold"),
      axis.text.y = element_text(color = "black", size = 10, face = "bold"),
      axis.title.y = element_text(color = "black", size = 10),
      axis.line = element_line(color = "black"),
      axis.ticks = element_line(color = "black"),
      legend.text = element_text(size = 6, colour = "black"),
      legend.title = element_text(size = 8),
      legend.key.size = unit(0, 'lines'),
      legend.position = "none",
      panel.grid = element_blank(),
      panel.background = element_blank()
      )

      pdf("co-ass_vs_single_Completeness_paired.pdf", width = 3 , height = 2.5)
      print(p)
      dev.off()


#---- table

      # 使用 across() 函数计算多个列的统计值
      summary_stats <- merge_df %>%
      group_by(Method,Stage) %>%
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

      write.table(summary_stats,file = "co-ass_vs_single_summary_stats.txt" , sep = "\t"    , row.names = T, col.names = NA, quote = F)
