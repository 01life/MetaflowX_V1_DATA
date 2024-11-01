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

binner_abb <- list(
"binny" = "Bn",
"maxbin" ="Mx",
"metabat" ="Mt",
"comebin" ="Cb",
"concoct" = "Cc",
"semibin2" = "Sb",
"metabinner" = "Mb"
)

profile <- read.table("6dataset_combine_7binner_preSample_avgBinScore_counts.txt",header=T, sep = "\t", check.name = F)
profile$BinScore <- as.character(profile$BinScore)
profile$binner_num <- sapply(strsplit(profile$binner, "_"), length)
profile$binner_num <- as.character(profile$binner_num)


profile$binner_order <- sapply(strsplit(profile$binner, "_"), function(x) {
  sorted_elements <- sort(x)          # Sort the elements
  paste(sorted_elements, collapse = "_")  # Join with "_"
})


profile$binner_list <- sapply(strsplit(profile$binner_order, "_"), function(x) {
  add_names <- lapply(x, function(element) {
    if (element %in% names(binner_abb)) {
      return(binner_abb[[element]])
    } else {
      return(NA)  # or handle it in another way if the element is not found
    }
  })
  paste(add_names, collapse = "_")  # Join with "_"
})


df <- profile %>%
  group_by(binner_list, BinScore) %>%
  summarise(mean_counts = mean(avg_counts), .groups = 'drop') %>%
  group_by(binner_list)%>%
  reframe(
    binner_list = binner_list,
    mean_counts = mean_counts,
    BinScore = BinScore,
    Percentage = (mean_counts / sum(mean_counts)) * 100
  )


# type_col <- rev(c("#f1eef6","#bdc9e1","#74a9cf","#2b8cbe","#045a8d","#062439"))

type_col <- c("#05668d","#028090","#00a896","#02c39a","#f0f3bd","#eaeaea") # e9ecef f4f1de  ebebeb eaeaea
F1_list <- c(">0.9",">0.8",">0.7",">0.6",'>=0.5',"<0.5")

df$BinScore <- factor(df$BinScore,levels=F1_list)

# mean count------ 

df_sorted <- df %>%
  filter(BinScore %in% c( ">0.9",">0.8",">0.7",">0.6",'>=0.5')) %>%
  group_by(binner_list) %>%
  summarise(counts = sum(mean_counts))%>%
  arrange(desc(counts))

df$binner_list <- factor(df$binner_list,levels=df_sorted$binner_list)


dd <- ggbarplot(df, x = "binner_list", y = "mean_counts", fill = "BinScore", color = "BinScore",size = 0.1,
            ggtheme = theme_bw() +
              theme(axis.text.x = element_text(color="black",size=0,angle=-90,hjust= 0 ,vjust = 0.5 ,face="bold"),
                    axis.text.y = element_text(color="black",size=8,face="bold"),
                    axis.title.y=element_text(color="black",size=8,face="bold"),
                    axis.title.x=element_text(color="black",size=8,face="bold"),
                    axis.line = element_line(color="black"),
                    axis.ticks = element_line(color="black"),
                    strip.text.x = element_text(size = 8, colour = "black", face="bold"), 
                    strip.background  = element_blank(),
                    legend.position = "top",
                    legend.text = element_text(size = 8, colour = "black"),
                    legend.title = element_text(size = 8),
                    legend.key.width = unit(0.3, 'cm'),
                    legend.key.size = unit(0, 'lines'),
                    panel.grid = element_blank(),
                    plot.margin = margin(l =5 , r = 20),
                    panel.background = element_blank()),
            legend = "top",title = "",xlab = '', ylab = 'Avg (#Bins)',width =0.7)+ 
    scale_y_continuous(expand = expansion(mult = c(0, 0.01)))+
    scale_color_manual(values=type_col,labels=F1_list,limits = F1_list, breaks = F1_list)+
    scale_fill_manual(values=type_col,labels=F1_list,limits = F1_list, breaks = F1_list)

# drawwidth <-  length(levels(df$binner_list))*0.2 + 1
# drawheight <- max(nchar(levels(df$binner_list)))*0.1 + 2.5




# pdf("Fig2b.pdf",width = 14,height = drawheight)
# dd
# dev.off()

# Percentage------ 

# df_sorted <- df %>%
#   filter(BinScore %in% c( ">0.9",">0.8",">0.7",">0.6",'>=0.5')) %>%
#   group_by(binner_list) %>%
#   summarise(counts = sum(Percentage))%>%
#   arrange(desc(counts))

# df$binner_list <- factor(df$binner_list,levels=df_sorted$binner_list)

dper <- ggbarplot(df, x = "binner_list", y = "Percentage", fill = "BinScore", color = "BinScore" ,size = 0.1,
            ggtheme = theme_bw() +
              theme(axis.text.x = element_text(color="black",size=6,angle=-90,hjust= 0 ,vjust = 0.5 ),
                    axis.text.y = element_text(color="black",size=8,face="bold"),
                    axis.title.y=element_text(color="black",size=8,face="bold"),
                    axis.title.x=element_text(color="black",size=8,face="bold"),
                    axis.line = element_line(color="black"),
                    axis.ticks = element_line(color="black"),
                    strip.text.x = element_text(size = 8, colour = "black", face="bold"), 
                    strip.background  = element_blank(),
                    legend.position = "top",
                    legend.text = element_text(size = 8, colour = "black"),
                    legend.title = element_text(size = 8),
                    legend.key.width = unit(0.3, 'cm'),
                    legend.key.size = unit(0, 'lines'),
                    panel.grid = element_blank(),
                    plot.margin = margin(l =5 , r = 20),
                    panel.background = element_blank()),
            legend = "none",title = "",xlab = 'Binners Combination', ylab = 'Percentage',width =0.7)+ 
    scale_y_continuous(expand = expansion(mult = c(0, 0)))+
    scale_color_manual(values=type_col,labels=F1_list,limits = F1_list, breaks = F1_list)+
    scale_fill_manual(values=type_col,labels=F1_list,limits = F1_list, breaks = F1_list)

# drawwidth <-  length(levels(df$binner_list))*0.2 + 1
# drawheight <- max(nchar(levels(df$binner_list)))*0.1 + 2.5

mulitPlot <- dd %>% insert_bottom(dper,height = 0.2)

pdf("Fig2c.pdf",width = 14,height = 7)
mulitPlot
dev.off()

# split by Percentage------ 

df <- profile %>%
  group_by(binner_list, BinScore, dataset) %>%
  summarise(mean_counts = mean(avg_counts), .groups = 'drop') %>%
  group_by(binner_list,dataset)%>%
  reframe(
    binner_list = binner_list,
    mean_counts = mean_counts,
    BinScore = BinScore,
    dataset = dataset,
    Percentage = (mean_counts / sum(mean_counts)) * 100
  )

df$BinScore <- factor(df$BinScore,levels=F1_list)

dataset_list <- c( "Human_gut","Mouse_gut","airskin","marine","plant_associated","strain" )

for ( i in dataset_list ) {

  sub_df <- subset(df,dataset %in% c(i))

  sub_df_sorted <- sub_df %>%
    filter(BinScore %in% c( ">0.9",">0.8",">0.7",">0.6",'>=0.5')) %>%
    group_by(binner_list) %>%
    summarise(counts = sum(Percentage))%>%
    arrange(desc(counts))

  sub_df$binner_list <- factor(sub_df$binner_list,levels=sub_df_sorted$binner_list)

  sub_dper <- ggbarplot(sub_df, x = "binner_list", y = "Percentage", fill = "BinScore",color = "BinScore",size = 0.1,
              ggtheme = theme_bw() +
                theme(axis.text.x = element_text(color="black",size=8,angle=-90,hjust= 0 ,vjust = 0.5 ,face="bold"),
                      axis.text.y = element_text(color="black",size=8,face="bold"),
                      axis.title.y=element_text(color="black",size=8,face="bold"),
                      axis.title.x=element_text(color="black",size=8,face="bold"),
                      axis.line = element_line(color="black"),
                      axis.ticks = element_line(color="black"),
                      strip.text.x = element_text(size = 8, colour = "black", face="bold"), 
                      strip.background  = element_blank(),
                      legend.position = "none",
                      legend.text = element_text(size = 8, colour = "black"),
                      legend.title = element_text(size = 8),
                      legend.key.width = unit(0.3, 'cm'),
                      legend.key.size = unit(0, 'lines'),
                      panel.grid = element_blank(),
                      plot.margin = margin(l =5 , r = 20),
                      panel.background = element_blank()),
              legend = "none",title = "",xlab = i, ylab = 'Percentage',width =0.7)+ 
      scale_color_manual(values=type_col,labels=F1_list,limits = F1_list, breaks = F1_list)+
      scale_fill_manual(values=type_col,labels=F1_list,limits = F1_list, breaks = F1_list)
  drawwidth <-  length(levels(sub_df$binner_list))*0.2 + 1
  drawheight <- max(nchar(levels(sub_df$binner_list)))*0.1 +2

pdf(paste("FigS10.",i,".pdf",sep=""),width = 14,height = drawheight)
print(sub_dper)
dev.off()


}

