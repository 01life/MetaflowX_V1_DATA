library("ggplot2")
library("ggpubr")
library("dplyr")

library("ggpubr")
library("ggplot2")
library("reshape")
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


set.seed(123)

raw_df <- read.table("./BinReassembly_final.txt",header=T, sep = "\t", check.name = F,quote="")

all_df  <- subset(raw_df,Level %in% c("HQ","MQ"))

color_var <- c('#DAD7CD',  '#588157',  '#344E41')
group_list <- c("LQ","MQ","HQ")


method_color <- c('#018571', '#a6611a')
method_list <- c("Single","Co_Ass")


df <- subset(all_df,Method %in% method_list)


plot <- df %>%
  group_by(Level,Method) %>%
  summarise(count = n())

plot$Method <- factor(plot$Method, levels = method_list)
plot$Level <- factor(plot$Level, levels = group_list)



bar <- ggbarplot(plot, "Method", "count",
  fill = "Level", color = "Level", 
  label = TRUE, lab.col = "white", lab.pos = "in")+
      labs(y="Count of Bins",x="")+
    scale_y_continuous(expand = c(0,0))+
    scale_x_discrete(expand=c(0.4,0)) +
    scale_color_manual(values = color_var, labels = group_list) +
    scale_fill_manual(values = color_var, labels = group_list) +
    theme_minimal() +
      theme(axis.text.x = element_text(color = "black", size = 10, angle = 0, hjust = 0.5, vjust =1, face = "bold"),
            axis.text.y = element_text(color = "black", size = 10, face = "bold"),
            axis.title.y = element_text(color = "black", size = 10,face = "bold"),
            axis.line = element_line(color = "black"),
            axis.ticks = element_line(color = "black"),
            legend.text = element_text(size = 10, colour = "black"),
            legend.title = element_text(size = 10),
            legend.key.size = unit(0, 'lines'),
            legend.position = "right",
            panel.grid = element_blank(),
            panel.background = element_blank())

pdf("co-ass_vs_single_QS_count.pdf", width = 2.5 , height = 3 )
bar
dev.off()





 



index_list <- c("Completeness","Contamination")

method_color <- c("#EF476F",'#018571', '#a6611a')
method_list <- c("Init","Single","Co_Ass")
all_df$Method <- factor(all_df$Method, levels = method_list)

my_comparisons <- list( c("Init", "Single"), c("Single", "Co_Ass"), c("Init", "Co_Ass") )


order_df  <-  all_df %>%
            group_by(BinID) %>%
            filter(n() == 3) %>% # 过滤掉不完整的配对数据
            ungroup()

order_df$BinID <- factor(order_df$BinID, levels = unique(all_df$BinID))


for (i in  index_list ){

itemViolin <- ggviolin(order_df, x = "Method", y = i, color = "Method",alpha=0.7,outlier.size = 0.7,size = 0.2,
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
                stat_compare_means(comparisons = my_comparisons , method = "wilcox.test",paired = TRUE,size=2)+
                # stat_compare_means(aes(label = after_stat(p.signif)), method = "wilcox.test", ref.group = "Init")+
                scale_y_continuous(expand = expansion(mult = c(0.01, 0.2)))+
                scale_color_manual(values=method_color,labels=method_list,limits = method_list, breaks = method_list)
                 



    pdf(file=paste("reass_refine_",i,"_improvement_violin.pdf",sep=""),width = 3 , height = 3)
    print(itemViolin)
    dev.off()
}


#---- table

      # 使用 across() 函数计算多个列的统计值
      summary_stats <- all_df %>%
      group_by(Method) %>%
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

      write.table(summary_stats,file = "BinReassembly_final_summary_stats.txt" , sep = "\t"    , row.names = T, col.names = NA, quote = F)
