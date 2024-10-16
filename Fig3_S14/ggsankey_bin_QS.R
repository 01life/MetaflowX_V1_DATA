#!/usr/bin/env Rscript

library(ggplot2)
library(dplyr)
library(ggsankey)
library(viridis)

# 指定颜色
node_colors <- c("HQ" = "#344E41", "LQ" = "#DAD7CD", "MQ" = "#588157")


#------ singal
      df <- read.table("./single_Bins_Reassembly_Optimization_evaluation.sanket.txt",header=T, sep = "\t", check.name = F,quote="")


      df_sum <- df %>%
      group_by(Init, ReAss, ReBin, ReFine, Final) %>%
      summarise(value = n())


      init_sumresult <- df_sum %>%
            group_by(Init) %>%
            summarize(total_value = sum(value))%>%
            mutate(label = paste("Init", Init, sep = "-"))%>%
            select(label, total_value)

      ass_sumresult <- df_sum %>%
            group_by(ReAss) %>%
            summarize(total_value = sum(value))%>%
            mutate(label = paste("ReAss", ReAss, sep = "-"))%>%
            select(label, total_value)

      bin_sumresult <- df_sum %>%
            group_by(ReBin) %>%
            summarize(total_value = sum(value))%>%
            mutate(label = paste("ReBin", ReBin, sep = "-"))%>%
            select(label, total_value)

      refine_sumresult <- df_sum %>%
            group_by(ReFine) %>%
            summarize(total_value = sum(value))%>%
            mutate(label = paste("ReFine", ReFine, sep = "-"))%>%
            select(label, total_value)

      final_sumresult <- df_sum %>%
            group_by(Final) %>%
            summarize(total_value = sum(value))%>%
            mutate(label = paste("Final", Final, sep = "-"))%>%
            select(label, total_value)


      combined_result <- bind_rows(init_sumresult, ass_sumresult, bin_sumresult, refine_sumresult, final_sumresult)

      df_sankey <- make_long(df,  Init, ReAss, ReBin, ReFine, Final)


      # Add the total_value column from result to new_df and create the label column
      df_sankey_label <- df_sankey %>%
      mutate(label = paste(x, node, sep = "-"))

      match_index <- match(df_sankey_label$label,combined_result$label) 

      df_sankey_label$num <- combined_result$total_value[match_index]
      df_sankey_label$show_label <- paste(df_sankey_label$node, df_sankey_label$num, sep = ":")

      # 指定节点顺序
      node_order <- c("LQ", "MQ" ,"HQ")
      df_sankey_label$node <- factor(df_sankey_label$node, levels = node_order)
      df_sankey_label$next_node <- factor(df_sankey_label$next_node, levels = node_order)




      # color_var <- c('#DAD7CD',  '#588157',  '#344E41')
      # group_list <- c("LQ","MQ","HQ")


      # 绘制桑基图

      p <- ggplot(df_sankey_label, aes(x = x, 
                              next_x = next_x, 
                              node = node, 
                              next_node = next_node,
                              fill = factor(node), 
                              label = show_label)) +
      geom_sankey(flow.alpha = 0.5, node.color = 1, width = 0.08) +
      geom_sankey_label(size = 2, color = 1, fill = "white") +
      scale_fill_manual(values = node_colors) +
      scale_x_discrete(limits = c("Init", "ReAss", "ReBin","ReFine","Final")) +
      theme_sankey(base_size = 10) +
      labs(title = "single assembly",
            x = NULL) +
      theme(legend.position = "bottom")+
      guides(fill = guide_legend(title = "Bin Quality"))+
      theme( axis.text.x = element_text(color="black",size=10 ,face="bold") )



      pdf("single_reass_bin_evaluation.pdf",width = 4, height = 3)
      p
      dev.off()


#------ co assembly

      df <- read.table("./co-ass-Bins_Reassembly_Optimization_evaluation.sanket.txt",header=T, sep = "\t", check.name = F,quote="")


      df_sum <- df %>%
      group_by(Init, ReAss, ReBin, ReFine, Final) %>%
      summarise(value = n())


      init_sumresult <- df_sum %>%
            group_by(Init) %>%
            summarize(total_value = sum(value))%>%
            mutate(label = paste("Init", Init, sep = "-"))%>%
            select(label, total_value)

      ass_sumresult <- df_sum %>%
            group_by(ReAss) %>%
            summarize(total_value = sum(value))%>%
            mutate(label = paste("ReAss", ReAss, sep = "-"))%>%
            select(label, total_value)

      bin_sumresult <- df_sum %>%
            group_by(ReBin) %>%
            summarize(total_value = sum(value))%>%
            mutate(label = paste("ReBin", ReBin, sep = "-"))%>%
            select(label, total_value)

      refine_sumresult <- df_sum %>%
            group_by(ReFine) %>%
            summarize(total_value = sum(value))%>%
            mutate(label = paste("ReFine", ReFine, sep = "-"))%>%
            select(label, total_value)

      final_sumresult <- df_sum %>%
            group_by(Final) %>%
            summarize(total_value = sum(value))%>%
            mutate(label = paste("Final", Final, sep = "-"))%>%
            select(label, total_value)


      combined_result <- bind_rows(init_sumresult, ass_sumresult, bin_sumresult, refine_sumresult, final_sumresult)

      df_sankey <- make_long(df,  Init, ReAss, ReBin, ReFine, Final)


      # Add the total_value column from result to new_df and create the label column
      df_sankey_label <- df_sankey %>%
      mutate(label = paste(x, node, sep = "-"))

      match_index <- match(df_sankey_label$label,combined_result$label) 

      df_sankey_label$num <- combined_result$total_value[match_index]
      df_sankey_label$show_label <- paste(df_sankey_label$node, df_sankey_label$num, sep = ":")

      # 指定节点顺序
      node_order <- c("LQ", "MQ" ,"HQ")
      df_sankey_label$node <- factor(df_sankey_label$node, levels = node_order)
      df_sankey_label$next_node <- factor(df_sankey_label$next_node, levels = node_order)




      # 绘制桑基图

      p <- ggplot(df_sankey_label, aes(x = x, 
                              next_x = next_x, 
                              node = node, 
                              next_node = next_node,
                              fill = factor(node), 
                              label = show_label)) +
      geom_sankey(flow.alpha = 0.5, node.color = 1, width = 0.08) +
      geom_sankey_label(size = 2, color = 1, fill = "white") +
      scale_fill_manual(values = node_colors) +
      scale_x_discrete(limits = c("Init", "ReAss", "ReBin","ReFine","Final")) +
      theme_sankey(base_size = 10) +
      labs(title = "co-assembly",
            x = NULL) +
      theme(legend.position = "bottom")+
      guides(fill = guide_legend(title = "Bin Quality"))+
      theme( axis.text.x = element_text(color="black",size=10 ,face="bold") )



      pdf("co-ass_reass_bin_evaluation.pdf",width = 4, height = 3)
      p
      dev.off()