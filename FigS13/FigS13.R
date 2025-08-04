library(ggpubr)
library(ggplot2)
library(ggrepel)
library(ggdist)
library(dplyr)
library(tidyr)
library(reshape2)

group_list = unlist(strsplit("HumanGut:MouseGut:Plant:Marine:AirSkin:Strain", ":"))
biner_list <- c("MetaBAT2","CONCOCT","SemiBin2","MaxBin2","MetaBinner","COMEBin","binny","Top","MetaflowX")
color_var = c("#66c2a5", "#fc8d62", "#7570b3","#e78ac3", "#a6d854", "#80b1d3","#bc4749","#ffc300","#023047")

single_biner <- read.table("6dataset_7binner_single_binner_amber.txt",header=T, sep = "\t", check.name = F)
single_biner$ID <- single_biner$Sample

multi_biner <- read.table("6dataset_combine_7binner_amber.txt",header=T, sep = "\t", check.name = F)
multi_biner$ID <- multi_biner$Sample

map <- read.table("./group.txt",header=T, sep = "\t", check.name = F,row.names=1)
map$ID <- rownames(map)
map <- subset(map,Group %in% group_list)
needID1 <- intersect(map$ID, single_biner$ID)
needID <- intersect(needID1, multi_biner$ID)
map <- subset(map,ID %in% needID)
single_biner <- subset(single_biner,ID %in% needID)
multi_biner <- subset(multi_biner,ID %in% needID)

multi_binner_top_list <- list(
 "AirSkin" = "metabat_semibin2_comebin_concoct_binny_maxbin" , 
 "HumanGut" = "metabat_semibin2_concoct_binny_metabinner_maxbin_comebin" ,
 "Marine" = "concoct_metabat_maxbin_binny_comebin_semibin2" ,
 "MouseGut" = "concoct_semibin2_binny_comebin" ,
 "Plant" = "metabinner_semibin2_metabat_comebin_binny_concoct" ,
 "Strain" =  "binny_concoct"
)


figTitle <- list(
    accuracy_bp='Accuracy', 
    precision_weighted_bp= 'Purity', 
    recall_weighted_bp= 'Completeness', 
    f1_score_per_bp= 'F1 score for sample', 
    rand_index_bp= 'Rand index', 
    percentage_of_assigned_bps= 'Percentage of binned'
)

for (dataset in group_list){
    single_biner_plot <- merge(single_biner,map,by="ID",all=TRUE)
    multi_biner_plot <- merge(multi_biner,map,by="ID",all=TRUE)

    single_biner_plot <- subset(single_biner_plot,Group %in% c(dataset))
    
    multi_biner_plot <- subset(multi_biner_plot,Group %in% c(dataset))
    multi_biner_plot <- subset(multi_biner_plot,Tool %in% c(multi_binner_top_list[[dataset]] , "concoct_metabat_metabinner_semibin2"))

    sub_single_plot <-  single_biner_plot[, c( "Sample","Tool" ,"accuracy_bp", "precision_weighted_bp", "recall_weighted_bp", "f1_score_per_bp", "rand_index_bp", "percentage_of_assigned_bps")]
    
    sub_multi_plot <-  multi_biner_plot[, c( "Sample","Tool" ,"accuracy_bp", "precision_weighted_bp", "recall_weighted_bp", "f1_score_per_bp", "rand_index_bp", "percentage_of_assigned_bps")]
    # sub_multi_plot <- subset(sub_multi_plot,Tool %in% c("metabat_semibin2_concoct_binny_metabinner_maxbin_comebin","concoct_metabat_metabinner_semibin2"))

    sub_multi_plot <- sub_multi_plot %>%
    mutate(label = case_when(
        Tool == multi_binner_top_list[[dataset]] ~ "Top",
        Tool == "concoct_metabat_metabinner_semibin2" ~ "MetaflowX",
        TRUE ~ NA_character_  # 处理其他情况
    ))

   

    sub_single_plot$label <- sub_single_plot$Tool
  
    merge_plot <- rbind(sub_single_plot,sub_multi_plot)
      

    merge_plot <- subset(merge_plot,label %in% biner_list)
    
    merge_plot  <-  merge_plot %>%
                group_by(Sample) %>%
                filter(n() == 9) %>% # 过滤掉不完整的配对数据
                ungroup()

    merge_plot$Sample <- factor(merge_plot$Sample,levels=needID)
    merge_plot$label <- factor(merge_plot$label,levels=biner_list)

    print(dim(merge_plot))


    long_plot <- melt(merge_plot, id.vars = c("label","Sample"),value.var = c("accuracy_bp","precision_weighted_bp","recall_weighted_bp","f1_score_per_bp","rand_index_bp","percentage_of_assigned_bps"))

    long_plot <- subset(long_plot,variable %in% c("accuracy_bp","precision_weighted_bp","recall_weighted_bp","f1_score_per_bp","rand_index_bp","percentage_of_assigned_bps"))

    long_plot$value <- as.numeric( long_plot$value)

        onefig <- facet(ggboxplot(long_plot, x = "label", y = "value",alpha=0.7,fill = "label",outlier.size = 0.01,size=0.1,facet.by = "variable",
                    ggtheme = theme_bw() +
                    theme(axis.text.x = element_text(color="black",size=8,angle=-90,hjust= 0 ,vjust = 0 ,face="bold"),
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
                    legend = "none",title = "",xlab = dataset, ylab = "",width = 0.7)+
        scale_y_continuous(expand = expansion(mult = c(0.01, 0.1)))+
        scale_fill_manual(values=color_var,labels=biner_list,limits = biner_list, breaks = biner_list),
                facet.by = "variable", ncol =1,short.panel.labs = TRUE,strip.position = "top",scales = "free_y", 
                 panel.labs.font = list(size = 5, angle = 0),panel.grid = element_blank(), panel.labs.background = list(fill = NA, color = NA) )


        pdf(file=paste("FigS15.",dataset,".pdf",sep=""),width = 1.5 , height = 7)
        print(onefig)
        dev.off()

        # 使用 compare_means() 输出统计检验结果
        results <- compare_means(
        accuracy_bp ~ Tool,     # 公式，表示比较 Tool 对 accuracy_bp 的影响
        data = merge_plot,      # 数据框
        method = "wilcox.test", # 使用 Wilcoxon 符号秩检验
        paired = TRUE           # 指定配对检验
        )

        write.table(results,file = paste(dataset,"_accuracy_bp_Wilcoxon_paired.xls",sep="") , sep = "\t"    , row.names = T, col.names = NA, quote = F)


        results <- compare_means(
        precision_weighted_bp ~ Tool,     # 公式，表示比较 Tool 对 precision_weighted_bp 的影响
        data = merge_plot,      # 数据框
        method = "wilcox.test", # 使用 Wilcoxon 符号秩检验
        paired = TRUE           # 指定配对检验
        )

        write.table(results,file = paste(dataset,"_precision_weighted_bp_Wilcoxon_paired.xls",sep="") , sep = "\t"    , row.names = T, col.names = NA, quote = F)


        results <- compare_means(
        recall_weighted_bp ~ Tool,     # 公式，表示比较 Tool 对 recall_weighted_bp 的影响
        data = merge_plot,      # 数据框
        method = "wilcox.test", # 使用 Wilcoxon 符号秩检验
        paired = TRUE           # 指定配对检验
        )

        write.table(results,file = paste(dataset,"_recall_weighted_bp_Wilcoxon_paired.xls",sep="") , sep = "\t"    , row.names = T, col.names = NA, quote = F)




    #---- table

    # 使用 across() 函数计算多个列的统计值
    summary_stats <- merge_plot %>%
    group_by(Tool) %>%
        summarise(
                across(
                c( "accuracy_bp", "precision_weighted_bp", "recall_weighted_bp", "f1_score_per_bp", "rand_index_bp", "percentage_of_assigned_bps"),
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

    write.table(summary_stats,file = paste(dataset,"_amber_summary_stats.txt",sep="") , sep = "\t"    , row.names = T, col.names = NA, quote = F)



}
