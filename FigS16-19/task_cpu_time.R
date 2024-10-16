#library ------ 
library("ggpubr")
library("ggplot2")
library("ggprism")
library("rstatix")
library("gtools")
library("tidyverse")
library("multcompView")
library("dplyr") 
library("reshape2")

#input data ------ 
set.seed(4321)

# color_var <- c("#E76F51","#06D6A0","#0081A7") 

#"#A82203" "#208CC0" "#F1AF3A" "#CF5E4E" "#637B31"

# color_var <-c( "#DD5129" ,"#0F7BA2" ,"#43B284", "#FAB255")
color_var <-c( "#CF5E4E","#F1AF3A" ,"#208CC0", "#637B31")
group_list <- c("HG","iHMP","Ocean","Lou_HG")

reads_stat <- read.table("All_reads_stat.xls",header=T, sep = "\t",check.name = F)
cpu_stat <- read.table("All_CPU_time.txt.S",header=T, sep = "\t",check.name = F)



# 定义任务和对应的乘数值的字典
multiplier_dict <- list(
  FASTP = 16,
  METAPHLAN = 32,
  MEGAHIT = 16,
  METASPADES = 32,
  HUMANN = 32,
  PRODIGAL = 16,
  BOWTIE2GENE = 16,
  CONCOCT = 16,
  METABAT2 = 16,
  SEMIBIN2 = 16,
  DASTOOL = 2,
  CHECKM2 = 16,
  BOWTIE2BIN = 16
)

#reads size ------ 

  # 定义要绘制的任务列表
  tasks <- c("FASTP","METAPHLAN","HUMANN","PRODIGAL","BOWTIE2GENE","CONCOCT","METABAT2","SEMIBIN2","DASTOOL","CHECKM2","BOWTIE2BIN","MEGAHIT","METASPADES")



  # 循环处理每个任务
  for (task in tasks) {
    # 筛选相应任务的数据
    cpu_stat_filtered <- subset(cpu_stat, ID %in% reads_stat$ID & simple_task %in% c(task))
    
    # 合并数据
    plot <- merge(cpu_stat_filtered, reads_stat, by = "ID", all = TRUE)
    plot$cpu_hour <- plot$realtime_h * multiplier_dict[[task]]  # 根据任务名称获取对应的乘数值


    plot$Group <- factor(plot$Group, levels = group_list)

    print(table(plot$Group[!is.na(plot$realtime_h)]))

    # 拟合模型
    model <- lm(cpu_hour ~ clean_bases, data = plot)
    # 提取系数
    coefficients <- model$coefficients
    # 将系数粘贴成公式
    equation <- paste(task, paste(" y = ", round(coefficients[1], 4), "+", round(coefficients[2], 4), "x", sep = ""), sep = "\t")
    # 将公式保存为文件
    write(equation, paste0("./", task, "_reads_equation.txt"))


  summary_plot <- plot %>%
    group_by(simple_task) %>%
    summarize(
      clean_bases_mean = round(mean(clean_bases), 3), 
      clean_bases_sd = round(sd(clean_bases), 3),
      cpu_hour_mean = round(mean(cpu_hour), 3),
      cpu_hour_sd = round(sd(cpu_hour), 3),
  )



    write.table(plot, paste0("./", task, "_task_cpuhour.txt") , sep = "\t"    , row.names = T, col.names = NA, quote = F)
    write.table(summary_plot, paste0("./", task, "_task_cpuhour_summary.txt") , sep = "\t"    , row.names = T, col.names = NA, quote = F)

    # 绘图
    # plot_title <- paste("QC ::", task)
    p <- ggplot(plot, aes(clean_bases, cpu_hour, color = Group)) +
      geom_point(size = 1) +
      stat_smooth(aes(fill = Group, color = Group), method = "lm") +
      theme_bw() +
      theme(axis.text.x = element_text(color = "black", size = 10, angle = 0, hjust = 0.5, vjust = 0, face = "bold"),
            axis.text.y = element_text(color = "black", size = 10, face = "bold"),
            axis.title = element_text(color = "black", size = 10, face = "bold"),
            axis.line = element_line(color = "black"),
            axis.ticks = element_line(color = "black"),
            strip.text.x = element_text(size = 8, colour = "black", face = "bold"),
            strip.background = element_blank(),
            legend.position = "none",
            legend.text = element_text(size = 10, colour = "black"),
            legend.title = element_text(size = 10),
            legend.key.width = unit(0.3, 'cm'),
            legend.key.size = unit(0, 'lines'),
            panel.grid = element_blank(),
            panel.background = element_blank()
      ) +
      labs(xlab = 'Clean Dases (G)', ylab = 'CPU Hour (H)', )
    
    # 保存图形
    pdf(file = paste0("./", task, "_reads.pdf"), width = 2.5, height = 2.5)
    print(
    
    ggpar(p, legend = "none", title = task, xlab = 'Clean Dases (G)', ylab = 'CPU Hour (H)')+
      scale_fill_manual(values=color_var,labels=group_list,limits = group_list, breaks = group_list) + 
      scale_color_manual(values=color_var,labels=group_list,limits = group_list, breaks = group_list) 

    
    )
    dev.off()
  }
print("~~~~~~~~~~~~~~~~~~~~~~reads size~~~~~~~~~~~~~~~~~~~~~~~")

#realtime_h

    for (task in tasks) {
    # 筛选相应任务的数据
    cpu_stat_filtered <- subset(cpu_stat, ID %in% reads_stat$ID & simple_task %in% c(task))

    # 合并数据
    plot <- merge(cpu_stat_filtered, reads_stat, by = "ID", all = TRUE)
    # plot$cpu_hour <- plot$realtime_h * multiplier_dict[[task]]  # 根据任务名称获取对应的乘数值


    plot$Group <- factor(plot$Group, levels = group_list)
    print(table(plot$Group[!is.na(plot$realtime_h)]))


    # 拟合模型
    model <- lm(realtime_h ~ clean_bases, data = plot)
    # 提取系数
    coefficients <- model$coefficients
    # 将系数粘贴成公式
    equation <- paste(task, paste(" y = ", round(coefficients[1], 4), "+", round(coefficients[2], 4), "x", sep = ""), sep = "\t")
    # 将公式保存为文件
    write(equation, paste0("./", task, "_realtime_equation.txt"))

    # 绘图
    # plot_title <- paste("QC ::", task)
    p <- ggplot(plot, aes(clean_bases, realtime_h, color = Group)) +
      geom_point(size = 1) +
      stat_smooth(aes(fill = Group, color = Group), method = "lm") +
      theme_bw() +
      theme(axis.text.x = element_text(color = "black", size = 10, angle = 0, hjust = 0.5, vjust = 0, face = "bold"),
            axis.text.y = element_text(color = "black", size = 10, face = "bold"),
            axis.title = element_text(color = "black", size = 10, face = "bold"),
            axis.line = element_line(color = "black"),
            axis.ticks = element_line(color = "black"),
            strip.text.x = element_text(size = 8, colour = "black", face = "bold"),
            strip.background = element_blank(),
            legend.position = "none",
            legend.text = element_text(size = 10, colour = "black"),
            legend.title = element_text(size = 10),
            legend.key.width = unit(0.3, 'cm'),
            legend.key.size = unit(0, 'lines'),
            panel.grid = element_blank(),
            panel.background = element_blank()
      ) +
      labs(xlab = 'Clean Dases (G)', ylab = 'Real Run Time (H)', )

    # 保存图形
    pdf(file = paste0("./", task, "_realtime.pdf"), width = 2.5, height = 2.5)
    print(

    ggpar(p, legend = "none", title = task, xlab = 'Clean Dases (G)', ylab = 'Real Run Time (H)')+
      scale_fill_manual(values=color_var,labels=group_list,limits = group_list, breaks = group_list) + 
      scale_color_manual(values=color_var,labels=group_list,limits = group_list, breaks = group_list) 


    )
    dev.off()
    }

print("~~~~~~~~~~~~~~~~~~~~~~realtime_h~~~~~~~~~~~~~~~~~~~~~~~")


#contig size ------ 

  contig_stat <- read.table("./All_contig_stat.xls",header=T, sep = "\t",check.name = F)

  summary_contig_stat <- contig_stat %>%
    group_by(ID,Group) %>%
    summarize(
      Total_length = sum(Length),
  )


  # 定义要绘制的任务列表
  tasks <- c("PRODIGAL","BOWTIE2GENE","CONCOCT","METABAT2","SEMIBIN2","DASTOOL")

  # 循环处理每个任务
  for (task in tasks) {
    # 筛选相应任务的数据
    cpu_stat_filtered <- subset(cpu_stat, ID %in% summary_contig_stat$ID & simple_task %in% c(task))
    
    # 合并数据
    plot <- merge(cpu_stat_filtered, summary_contig_stat, by = "ID", all = TRUE)
    plot$cpu_hour <- plot$realtime_h * multiplier_dict[[task]]  # 根据任务名称获取对应的乘数值
    plot$Group <- factor(plot$Group, levels = group_list)

    print(table(plot$Group[!is.na(plot$realtime_h)]))


    # 拟合模型
    model <- lm(cpu_hour ~ Total_length, data = plot)
    # 提取系数
    coefficients <- model$coefficients
    # 将系数粘贴成公式
    equation <- paste(task, paste(" y = ", round(coefficients[1], 4), "+", round(coefficients[2], 4), "x", sep = ""), sep = "\t")
    # 将公式保存为文件
    write(equation, paste0("./", task, "_contig_Total_length_equation.txt"))
    
    # 绘图
    # plot_title <- paste("QC ::", task)
    pp <- ggplot(plot, aes(Total_length, cpu_hour, color = Group)) +
      geom_point(size = 1) +
      stat_smooth(aes(fill = Group, color = Group), method = "lm") +
      theme_bw() +
      theme(axis.text.x = element_text(color = "black", size = 10, angle = 60, hjust = 1, vjust = 1, face = "bold"),
            axis.text.y = element_text(color = "black", size = 10, face = "bold"),
            axis.title = element_text(color = "black", size = 10, face = "bold"),
            axis.line = element_line(color = "black"),
            axis.ticks = element_line(color = "black"),
            strip.text.x = element_text(size = 8, colour = "black", face = "bold"),
            strip.background = element_blank(),
            legend.position = "none",
            legend.text = element_text(size = 10, colour = "black"),
            legend.title = element_text(size = 10),
            legend.key.width = unit(0.3, 'cm'),
            legend.key.size = unit(0, 'lines'),
            panel.grid = element_blank(),
            panel.background = element_blank()
      ) 
    
    # 保存图形
    pdf(file = paste0("./", task, "_contig_Total_length.pdf"), width = 2.5, height = 2.5)
    print(
    
      ggpar(pp, legend = "none", title = task, xlab = 'Total Assembled Length (bp)', ylab = 'CPU-Hour (H)') + 
      scale_fill_manual(values=color_var,labels=group_list,limits = group_list, breaks = group_list) + 
      scale_color_manual(values=color_var,labels=group_list,limits = group_list, breaks = group_list) 
    
    )
    dev.off()
  }
print("~~~~~~~~~~~~~~~~~~~~~~contig size~~~~~~~~~~~~~~~~~~~~~~~")



#METASPADES ------ 

  task <- "METASPADES"
  cpu_stat_filtered <- subset(cpu_stat, ID %in% reads_stat$ID & simple_task %in% c(task))
  
  # 合并数据
  plot <- merge(cpu_stat_filtered, reads_stat, by = "ID", all = TRUE)
  plot <- subset(plot, Group %in% c("HG","iHMP"))
  plot$Group <- factor(plot$Group, levels = group_list)

  print(table(plot$Group[!is.na(plot$realtime_h)]))


  plot$cpu_hour <- plot$realtime_h * multiplier_dict[[task]] 
  model <- lm(cpu_hour ~ clean_bases, data = plot)
  coefficients <- model$coefficients
  equation <- paste(task, paste(" y = ", round(coefficients[1], 4), "+", round(coefficients[2], 4), "x", sep = ""), sep = "\t")
  write(equation, paste0("./", task, "_reads_equation.txt"))

  plot <- ggplot(plot, aes(clean_bases, cpu_hour, color = Group)) +
    geom_point(size = 1) +
    stat_smooth(aes(fill = Group, color = Group), method = "lm") +
    theme_bw() +
    theme(axis.text.x = element_text(color = "black", size = 10, angle = 0, hjust = 0.5, vjust = 0, face = "bold"),
          axis.text.y = element_text(color = "black", size = 10, face = "bold"),
          axis.title = element_text(color = "black", size = 10, face = "bold"),
          axis.line = element_line(color = "black"),
          axis.ticks = element_line(color = "black"),
          strip.text.x = element_text(size = 8, colour = "black", face = "bold"),
          strip.background = element_blank(),
          legend.position = "none",
          legend.text = element_text(size = 10, colour = "black"),
          legend.title = element_text(size = 10),
          legend.key.width = unit(0.3, 'cm'),
          legend.key.size = unit(0, 'lines'),
          panel.grid = element_blank(),
          panel.background = element_blank()
    ) +
    labs(xlab = 'Clean Dases (G)', ylab = 'CPU Hour (H)', )
  
  pdf(file = paste0("./", task, "_reads.pdf"), width = 2.5, height = 2.5)

  ggpar(plot, legend = "none", title = task, xlab = 'Clean Dases (G)', ylab = 'CPU Hour (H)')+
    scale_fill_manual(values=color_var,labels=group_list,limits = group_list, breaks = group_list) + 
    scale_color_manual(values=color_var,labels=group_list,limits = group_list, breaks = group_list) 
  dev.off()


print("~~~~~~~~~~~~~~~~~~~~~~METASPADES~~~~~~~~~~~~~~~~~~~~~~~")



#MEGAHIT ------ 
  task <- "MEGAHIT"
  cpu_stat_filtered <- subset(cpu_stat, ID %in% reads_stat$ID & simple_task %in% c(task))
  # 合并数据
  plot <- merge(cpu_stat_filtered, reads_stat, by = "ID", all = TRUE)
  plot <- subset(plot, Group %in% c("Ocean","Lou_HG"))
  plot$Group <- factor(plot$Group, levels = group_list)

  plot$cpu_hour <- plot$realtime_h * multiplier_dict[[task]] 

  print(table(plot$Group[!is.na(plot$realtime_h)]))

  
  model <- lm(cpu_hour ~ clean_bases, data = plot)
  coefficients <- model$coefficients
  equation <- paste(task, paste(" y = ", round(coefficients[1], 4), "+", round(coefficients[2], 4), "x", sep = ""), sep = "\t")
  write(equation, paste0("./", task, "_reads_equation.txt"))

  p3 <- ggplot(plot, aes(clean_bases, cpu_hour, color = Group)) +
    geom_point(size = 1) +
    stat_smooth(aes(fill = Group, color = Group), method = "lm") +
    theme_bw() +
    theme(axis.text.x = element_text(color = "black", size = 10, angle = 0, hjust = 0.5, vjust = 0, face = "bold"),
          axis.text.y = element_text(color = "black", size = 10, face = "bold"),
          axis.title = element_text(color = "black", size = 10, face = "bold"),
          axis.line = element_line(color = "black"),
          axis.ticks = element_line(color = "black"),
          strip.text.x = element_text(size = 8, colour = "black", face = "bold"),
          strip.background = element_blank(),
          legend.position = "none",
          legend.text = element_text(size = 10, colour = "black"),
          legend.title = element_text(size = 10),
          legend.key.width = unit(0.3, 'cm'),
          legend.key.size = unit(0, 'lines'),
          panel.grid = element_blank(),
          panel.background = element_blank()
    ) +
    labs(xlab = 'Clean Dases (G)', ylab = 'CPU Hour (H)', )
  
  pdf(file = paste0("./", task, "_reads.pdf"), width = 2.5, height = 2.5)

  ggpar(p3, legend = "none", title = task, xlab = 'Clean Dases (G)', ylab = 'CPU Hour (H)')+
    scale_fill_manual(values=color_var,labels=group_list,limits = group_list, breaks = group_list) + 
    scale_color_manual(values=color_var,labels=group_list,limits = group_list, breaks = group_list) 
  dev.off()

print("~~~~~~~~~~~~~~~~~~~~~~MEGAHIT~~~~~~~~~~~~~~~~~~~~~~~")



#read & contig ------ 


  ##Total_length
  summary_contig_stat <- contig_stat %>%
    group_by(ID) %>%
    summarize(
      Contig_Count = n(),
      Total_length = sum(Length),
  )

    
    # 合并数据
    plot <- merge(summary_contig_stat, reads_stat, by = "ID", all = TRUE)
    plot$Group <- factor(plot$Group, levels = group_list)

    print(table(plot$Group[!is.na(plot$Total_length)]))


    model <- lm(Total_length ~ clean_bases, data = plot)
    coefficients <- model$coefficients
    equation <- paste("Total_length ~ clean_bases", paste(" y = ", round(coefficients[1], 4), "+", round(coefficients[2], 4), "x", sep = ""), sep = "\t")
    write(equation, paste0("./reads_contig_length_equation.txt"))

    p4 <- ggplot(plot, aes(clean_bases, Total_length, color = Group)) +
      geom_point(size = 1) +
      stat_smooth(aes(fill = Group, color = Group), method = "lm") +
      theme_bw() +
      theme(axis.text.x = element_text(color = "black", size = 10, angle = 0, hjust = 0.5, vjust = 0, face = "bold"),
            axis.text.y = element_text(color = "black", size = 10, face = "bold"),
            axis.title = element_text(color = "black", size = 10, face = "bold"),
            axis.line = element_line(color = "black"),
            axis.ticks = element_line(color = "black"),
            strip.text.x = element_text(size = 8, colour = "black", face = "bold"),
            strip.background = element_blank(),
            legend.position = "none",
            legend.text = element_text(size = 10, colour = "black"),
            legend.title = element_text(size = 10),
            legend.key.width = unit(0.3, 'cm'),
            legend.key.size = unit(0, 'lines'),
            panel.grid = element_blank(),
            panel.background = element_blank()
      ) +
      labs(xlab = 'Clean Dases (G)', ylab = 'Total Assembled Length (bp)', )
    
    pdf(file = paste0("./reads_contig_length.pdf"), width = 3.5, height = 3)

    ggpar(p4, legend = "none", title = "", xlab = 'Clean Dases (G)', ylab = 'Total Assembled Length (bp)')+
      scale_fill_manual(values=color_var,labels=group_list,limits = group_list, breaks = group_list) + 
      scale_color_manual(values=color_var,labels=group_list,limits = group_list, breaks = group_list) 
    dev.off()

print("~~~~~~~~~~~~~~~~~~~~~~read & contig~~~~~~~~~~~~~~~~~~~~~~~")


## Contig_Count
  model <- lm(Contig_Count ~ clean_bases, data = plot)
  coefficients <- model$coefficients
  equation <- paste("Contig_Count ~ clean_bases", paste(" y = ", round(coefficients[1], 4), "+", round(coefficients[2], 4), "x", sep = ""), sep = "\t")
  write(equation, paste0("./reads_contig_count_equation.txt"))

  p5 <- ggplot(plot, aes(clean_bases, Contig_Count, color = Group)) +
    geom_point(size = 1) +
    stat_smooth(aes(fill = Group, color = Group), method = "lm") +
    theme_bw() +
    theme(axis.text.x = element_text(color = "black", size = 10, angle = 0, hjust = 0.5, vjust = 0, face = "bold"),
          axis.text.y = element_text(color = "black", size = 10, face = "bold"),
          axis.title = element_text(color = "black", size = 10, face = "bold"),
          axis.line = element_line(color = "black"),
          axis.ticks = element_line(color = "black"),
          strip.text.x = element_text(size = 8, colour = "black", face = "bold"),
          strip.background = element_blank(),
          legend.position = "none",
          legend.text = element_text(size = 10, colour = "black"),
          legend.title = element_text(size = 10),
          legend.key.width = unit(0.3, 'cm'),
          legend.key.size = unit(0, 'lines'),
          panel.grid = element_blank(),
          panel.background = element_blank()
    ) +
    labs(xlab = 'Clean Dases (G)', ylab = 'Total Assembled Contig Numbers', )
  
  pdf(file = paste0("./reads_contig_count.pdf"), width = 3.5, height = 3)

  ggpar(p5, legend = "none", title = "", xlab = 'Clean Dases (G)', ylab = 'Total Assembled Contig Numbers')+
    scale_fill_manual(values=color_var,labels=group_list,limits = group_list, breaks = group_list) + 
    scale_color_manual(values=color_var,labels=group_list,limits = group_list, breaks = group_list) 
  dev.off()

print("~~~~~~~~~~~~~~~~~~~~~~Contig_Count~~~~~~~~~~~~~~~~~~~~~~~")


## Contig_Count & bin_count
  bin_stat <- read.table("./All_binCount.xls",header=T, sep = "\t",check.name = F)
  
  plot <- merge(summary_contig_stat, bin_stat, by = "ID", all = TRUE)
  plot$Group <- factor(plot$Group, levels = group_list)
  print(table(plot$Group[!is.na(plot$Contig_Count)]))


  model <- lm(Bin_Count ~ Contig_Count, data = plot)
  coefficients <- model$coefficients
  equation <- paste("Bin_Count ~ Contig_Count", paste(" y = ", round(coefficients[1], 4), "+", round(coefficients[2], 4), "x", sep = ""), sep = "\t")
  write(equation, paste0("./reads_contig_count_equation.txt"))

  p5 <- ggplot(plot, aes(Contig_Count, Bin_Count, color = Group)) +
    geom_point(size = 1) +
    stat_smooth(aes(fill = Group, color = Group), method = "lm") +
    theme_bw() +
    theme(axis.text.x = element_text(color = "black", size = 10, angle = 0, hjust = 0.5, vjust = 0, face = "bold"),
          axis.text.y = element_text(color = "black", size = 10, face = "bold"),
          axis.title = element_text(color = "black", size = 10, face = "bold"),
          axis.line = element_line(color = "black"),
          axis.ticks = element_line(color = "black"),
          strip.text.x = element_text(size = 8, colour = "black", face = "bold"),
          strip.background = element_blank(),
          legend.position = "none",
          legend.text = element_text(size = 10, colour = "black"),
          legend.title = element_text(size = 10),
          legend.key.width = unit(0.3, 'cm'),
          legend.key.size = unit(0, 'lines'),
          panel.grid = element_blank(),
          panel.background = element_blank()
    ) +
    labs(xlab = 'Total Assembled Contig Numbers', ylab = 'Total Bin Numbers', )
  
  pdf(file = paste0("./contig_bin_count.pdf"), width = 3.5, height = 3)

  ggpar(p5, legend = "none", title = "", xlab = 'Total Assembled Contig Numbers', ylab = 'Total Bin Numbers')+
    scale_fill_manual(values=color_var,labels=group_list,limits = group_list, breaks = group_list) + 
    scale_color_manual(values=color_var,labels=group_list,limits = group_list, breaks = group_list) 
  dev.off()

print("~~~~~~~~~~~~~~~~~~~~~~Contig_Count & bin_count~~~~~~~~~~~~~~~~~~~~~~~")


#binner & contig count  ------ 
  # 定义要绘制的任务列表
  tasks <- c("CONCOCT","METABAT2","SEMIBIN2")

  summary_contig_stat <- contig_stat %>%
    group_by(ID,Group) %>%
    summarize(
      Contig_Count = n(),
      Total_length = sum(Length),
  )


  write.table(summary_contig_stat, "./summary_contig_stat.txt" , sep = "\t"    , row.names = T, col.names = NA, quote = F)


  # 循环处理每个任务
  for (task in tasks) {
    # 筛选相应任务的数据
    cpu_stat_filtered <- subset(cpu_stat, ID %in% summary_contig_stat$ID & simple_task %in% c(task))
    plot <- merge(cpu_stat_filtered, summary_contig_stat, by = "ID", all = TRUE)
    plot$cpu_hour <- plot$realtime_h * multiplier_dict[[task]]  # 根据任务名称获取对应的乘数值


    plot$Group <- factor(plot$Group, levels = group_list)

    print(table(plot$Group[!is.na(plot$cpu_hour)]))

    
    # 拟合模型
    model <- lm(cpu_hour ~ Contig_Count, data = plot)
    # 提取系数
    coefficients <- model$coefficients
    # 将系数粘贴成公式
    equation <- paste(task, paste(" y = ", round(coefficients[1], 4), "+", round(coefficients[2], 4), "x", sep = ""), sep = "\t")
    # 将公式保存为文件
    write(equation, paste0("./", task, "_contigCount_equation.txt"))
    
    # 绘图
    # plot_title <- paste("QC ::", task)
    p <- ggplot(plot, aes(Contig_Count, cpu_hour, color = Group)) +
      geom_point(size = 1) +
      stat_smooth(aes(fill = Group, color = Group), method = "lm") +
      theme_bw() +
      theme(axis.text.x = element_text(color = "black", size = 10, angle = 0, hjust = 0.5, vjust = 0, face = "bold"),
            axis.text.y = element_text(color = "black", size = 10, face = "bold"),
            axis.title = element_text(color = "black", size = 10, face = "bold"),
            axis.line = element_line(color = "black"),
            axis.ticks = element_line(color = "black"),
            strip.text.x = element_text(size = 8, colour = "black", face = "bold"),
            strip.background = element_blank(),
            legend.position = "none",
            legend.text = element_text(size = 10, colour = "black"),
            legend.title = element_text(size = 10),
            legend.key.width = unit(0.3, 'cm'),
            legend.key.size = unit(0, 'lines'),
            panel.grid = element_blank(),
            panel.background = element_blank()
      ) +
      labs(xlab = 'Total Assembled Contig Numbers', ylab = 'CPU Hour (H)', )
    
    # 保存图形
    pdf(file = paste0("./", task, "_contigCount.pdf"), width = 2.5, height = 2.5)
    print(
    
    ggpar(p, legend = "none", title = task, xlab = 'Total Assembled Contig Numbers', ylab = 'CPU Hour (H)')+
      scale_fill_manual(values=color_var,labels=group_list,limits = group_list, breaks = group_list) + 
      scale_color_manual(values=color_var,labels=group_list,limits = group_list, breaks = group_list) 

    
    )
    dev.off()
  }

print("~~~~~~~~~~~~~~~~~~~~~~binner & contig count~~~~~~~~~~~~~~~~~~~~~~~")



## Contig_Count & DASTOOL 

  summary_contig_stat <- contig_stat %>%
    group_by(ID,Group) %>%
    summarize(
      Contig_Count = n(),
      Total_length = sum(Length),
  )

  task <- "DASTOOL"

  cpu_stat_filtered <- subset(cpu_stat, ID %in% summary_contig_stat$ID & simple_task %in% c(task))

  plot <- merge(summary_contig_stat, cpu_stat_filtered, by = "ID", all = TRUE)
  plot$cpu_hour <- plot$realtime_h * multiplier_dict[[task]]  # 根据任务名称获取对应的乘数
print(plot)



  model <- lm( cpu_hour ~ Contig_Count, data = plot)
  coefficients <- model$coefficients
  equation <- paste("Contig_Count ~ cpu_hour", paste(" y = ", round(coefficients[1], 4), "+", round(coefficients[2], 4), "x", sep = ""), sep = "\t")
  write(equation, paste0("./cpu_hour_contig_count_equation.txt"))

  p5 <- ggplot(plot, aes(Contig_Count,cpu_hour, color = Group)) +
    geom_point(size = 1) +
    stat_smooth(aes(fill = Group, color = Group), method = "lm") +
    theme_bw() +
    theme(axis.text.x = element_text(color = "black", size = 10, angle = 0, hjust = 0.5, vjust = 0, face = "bold"),
          axis.text.y = element_text(color = "black", size = 10, face = "bold"),
          axis.title = element_text(color = "black", size = 10, face = "bold"),
          axis.line = element_line(color = "black"),
          axis.ticks = element_line(color = "black"),
          strip.text.x = element_text(size = 8, colour = "black", face = "bold"),
          strip.background = element_blank(),
          legend.position = "none",
          legend.text = element_text(size = 10, colour = "black"),
          legend.title = element_text(size = 10),
          legend.key.width = unit(0.3, 'cm'),
          legend.key.size = unit(0, 'lines'),
          panel.grid = element_blank(),
          panel.background = element_blank()
    ) +
    labs(xlab = 'Total Assembled Contig Numbers', ylab = 'CPU Hour (H)', )
  
  pdf(file = paste0("./DASTOOL_contig_count.pdf"), width = 2.5, height = 2.5)

  ggpar(p5, legend = "none", title = "DASTOOL", xlab = 'Total Assembled Contig Numbers', ylab = 'CPU Hour (H)')+
    scale_fill_manual(values=color_var,labels=group_list,limits = group_list, breaks = group_list) + 
    scale_color_manual(values=color_var,labels=group_list,limits = group_list, breaks = group_list) 
  dev.off()

print("~~~~~~~~~~~~~~~~~~~~~~Contig_Count & DASTOOL ~~~~~~~~~~~~~~~~~~~~~~~")



