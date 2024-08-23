#library ------ 
library("ggpubr")
library("ggplot2")
library("ggprism")
library("rstatix")
library("gtools")
library("tidyverse")
library("dplyr") 
library("reshape2")
library("ggseqlogo")
library("fmsb")

#'precision_avg_bp': 'Average purity (bp)',
#'recall_avg_bp': 'Average completeness (bp)',
#f1_score_bp': 'F1 score (bp)',
#'f1_score_per_bp': 'F1 score for sample (bp)',
#'accuracy_bp': 'Accuracy (bp)',
#'precision_weighted_bp': 'Purity (bp)',
#'recall_weighted_bp': 'Completeness (bp)',
#'rand_index_bp': 'Rand index (bp)',
#'adjusted_rand_index_bp': 'Adjusted Rand index (bp)',
#'percentage_of_assigned_bps': 'Percentage of binned bp',

#'f1_score_per_seq': 'F1 score for sample (seq)',
#'accuracy_seq': 'Accuracy (seq)',
#'misclassification_bp': 'Misclassification rate (bp)',
#'misclassification_seq': 'Misclassification rate (seq)',
#'precision_weighted_seq': 'Purity (seq)',
#'recall_weighted_seq': 'Completeness (seq)',
#'rand_index_seq': 'Rand index (seq)',
#'adjusted_rand_index_seq': 'Adjusted Rand index (seq)',
#'percentage_of_assigned_seqs': 'Percentage of binned sequences',


create_beautiful_radarchart <- function(data, color = "#00AFBB", 
                                        title_cex = 0.7,
                                        vlabels = colnames(data), vlcex = 0.7,
                                        caxislabels = NULL, title = NULL, ...){
  radarchart(
    data, axistype = 1,
    # Customize the polygon
    pcol = color, 
    #pfcol = scales::alpha(color, 0.5), 
    plwd = 1.5, plty = 1,
    # Customize the grid
    cglcol = "grey", cglty = 1, cglwd = 0.8,
    # Customize the axis
    axislabcol = "black", 
    seg = 5,
    # Variable labels
    vlcex = vlcex, 
    vlabels = vlabels,
    caxislabels = caxislabels, title = title, cex.main = title_cex,...
  )
}

##1 profile
#1_Quality_of_bins_all_bins_have_the_same_weight_metrics_table.txt AMBER.result.txt
#cami1.result.txt  self_similarize.result.txt


biner_list <- c("CONCOCT" ,"MaxBin2","MetaBAT2","binny","MetaBinner","SemiBin2","COMEBin" ,"GS")
biner_cor <- c("#66c2a5", "#fc8d62", "#7570b3","#e78ac3", "#a6d854", "#80b1d3","#bc4749","#ffd60a")

pdf("./Fig2.legend.pdf",width = 3,height = 3)
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("topleft", legend =biner_list, pch=16, pt.cex=0.7, cex=0.7, bty='n',
       col = biner_cor) 
mtext("Binner", at=0.05, cex=0.7)
dev.off()

#ALL ------
profile <- read.table("6dataset_7binner_single_binner_amber.txt",header=T, sep = "\t", check.name = F)
#bp ------ 
bpPD <-  profile[, c( "Sample","Tool" ,"accuracy_bp", "precision_weighted_bp", "recall_weighted_bp", "f1_score_per_bp", "rand_index_bp", "percentage_of_assigned_bps")]

bp_avg <- bpPD %>% 
  dplyr::group_by(Tool) %>% 
  summarize(
    Avg_Accuracy = mean(accuracy_bp),
    Avg_Purity = mean(precision_weighted_bp),
    Avg_Completeness = mean(recall_weighted_bp),
    Avg_F1_score = mean(f1_score_per_bp),
    Avg_Rand_index = mean(rand_index_bp),
    Avg_Percentage_of_binned = mean(percentage_of_assigned_bps),
  )%>% 
  column_to_rownames(var = "Tool")

tt <- as.data.frame(t(bp_avg))
tt$Min <- rep(0,  nrow(tt))
tt$Max <- rep(1,  nrow(tt))
dd <- as.data.frame(t(tt))
df <- dd[c("Max","Min", "CONCOCT" ,"MaxBin2","MetaBAT2","binny","MetaBinner","SemiBin2","COMEBin" ,"GS"), , drop = FALSE]

pdf(file=paste("Fig2a.pdf",sep=""),width = 6 , height = 6)

# png("plot01.base.png",width = 6,height = 6,units='in',res=600)
create_beautiful_radarchart(
  data = df, caxislabels = seq(0, 1, 0.2),
  color = c("#66c2a5", "#fc8d62", "#7570b3","#e78ac3", "#a6d854", "#80b1d3","#bc4749","#ffd60a"),
  vlabels = c("Accuracy","Purity","Completeness"  ,         
              "F1 score" ,"Rand index","Percentage of binned" ),
  vlcex = 0.7,
  title_cex=1,
  title = "Single binner"
)
dev.off()


group_list = unlist(strsplit("HumanGut:MouseGut:Plant:Marine:AirSkin:Strain", ":"))

profile$ID <- profile$Sample

map <- read.table("../group.txt",header=T, sep = "\t", check.name = F,row.names=1)
map$ID <- rownames(map)
map <- subset(map,Group %in% group_list)
needID <- intersect(map$ID, profile$ID)
map <- subset(map,ID %in% needID)
profile <- subset(profile,ID %in% needID)
plot <- merge(profile,map,by="ID",all=TRUE)


for (i in group_list) {

  sub_plot <- subset(plot,Group %in% c(i))

  bpPD <-  sub_plot[, c( "Sample","Tool" ,"accuracy_bp", "precision_weighted_bp", "recall_weighted_bp", "f1_score_per_bp", "rand_index_bp", "percentage_of_assigned_bps")]

  bp_avg <- bpPD %>% 
    dplyr::group_by(Tool) %>% 
    summarize(
      Avg_Accuracy = mean(accuracy_bp),
      Avg_Purity = mean(precision_weighted_bp),
      Avg_Completeness = mean(recall_weighted_bp),
      Avg_F1_score = mean(f1_score_per_bp),
      Avg_Rand_index = mean(rand_index_bp),
      Avg_Percentage_of_binned = mean(percentage_of_assigned_bps),
    )%>% 
    column_to_rownames(var = "Tool")

  tt <- as.data.frame(t(bp_avg))
  tt$Min <- rep(0,  nrow(tt))
  tt$Max <- rep(1,  nrow(tt))
  dd <- as.data.frame(t(tt))
  df <- dd[c("Max","Min", "CONCOCT" ,"MaxBin2","MetaBAT2","binny","MetaBinner","SemiBin2","COMEBin" ,"GS"), , drop = FALSE]
  pdf(file=paste("FigS7a.",i,"_radia.pdf",sep=""),width = 5 , height = 5)
  create_beautiful_radarchart(
    data = df, caxislabels = seq(0, 1, 0.2),
    color = c("#66c2a5", "#fc8d62", "#7570b3","#e78ac3", "#a6d854", "#80b1d3","#bc4749","#ffd60a"),
    vlabels = c("Accuracy","Purity","Completeness"  ,         
                "F1 score" ,"Rand index","Percentage of binned" ),
    vlcex = 0.6,
    title_cex = 1,
    title = i
  )

    dev.off()

}











# #DASTOOL ------
# #CAMI_DASTool_AMBER.txt
# #DASTool_AMBER.txt
# profile <- read.table("/aimigene/lianglifeng/01.Project/09.MetaFlowX/mode5/plotCAMI_DASTool_AMBER.txt",header=T, sep = "\t", check.name = F)
# #bp ------ 
# bpPD <-  profile[, c( "Sample","Tool" ,"accuracy_bp", "precision_weighted_bp", "recall_weighted_bp", "f1_score_per_bp", "rand_index_bp", "percentage_of_assigned_bps")]
# bp_avg <- bpPD %>% 
#   dplyr::group_by(Tool) %>% 
#   summarize(
#     Avg_Accuracy = mean(accuracy_bp),
#     Avg_Purity = mean(precision_weighted_bp),
#     Avg_Completeness = mean(recall_weighted_bp),
#     Avg_F1_score = mean(f1_score_per_bp),
#     Avg_Rand_index = mean(rand_index_bp),
#     Avg_Percentage_of_binned = mean(percentage_of_assigned_bps),
#   )%>% 
#   column_to_rownames(var = "Tool")

# tt <- as.data.frame(t(bp_avg))
# tt$Min <- rep(0,  nrow(tt))
# tt$Max <- rep(1,  nrow(tt))
# dd <- as.data.frame(t(tt))
# df <- dd[c("Max","Min", "combination6" ,"combination7","combination8","combination1","combination3","combination2","combination4","Gold_standard" ), , drop = FALSE]


# png("/aimigene/lianglifeng/01.Project/09.MetaFlowX/mode5/plot09.CAMI.DASTool.base.png",width = 6,height = 6,units='in',res=600)
# create_beautiful_radarchart(
#   data = df, caxislabels = c(0, 0.25, 0.5, 0.75, 1),
#   color = c("#046C9A","#f4cae4","#7570b3" ,"#9A8822", "#5BBCD6","#FA796C","#66a61e","#ffd92f"),
#   vlabels = c("Accuracy","Purity","Completeness"  ,         
#               "F1 score" ,"Rand index","Percentage of binned" ),
#   vlcex = 1,
#   title = "DASTool base"
# )
# dev.off()

# #seq ------ 
# seqPD <-  profile[, c( "Sample","Tool" ,"accuracy_seq", "precision_weighted_seq", "recall_weighted_seq", "f1_score_per_seq", "rand_index_seq", "percentage_of_assigned_seqs")]
# seq_avg <- seqPD %>% 
#   dplyr::group_by(Tool) %>% 
#   summarize(
#     Avg_Accuracy = mean(accuracy_seq),
#     Avg_Purity = mean(precision_weighted_seq),
#     Avg_Completeness = mean(recall_weighted_seq),
#     Avg_F1_score = mean(f1_score_per_seq),
#     Avg_Rand_index = mean(rand_index_seq),
#     Avg_Percentage_of_binned = mean(percentage_of_assigned_seqs),
#   )%>% 
#   column_to_rownames(var = "Tool")

# tt <- as.data.frame(t(seq_avg))
# tt$Min <- rep(0,  nrow(tt))
# tt$Max <- rep(1,  nrow(tt))
# dd <- as.data.frame(t(tt))
# df <- dd[c("Max","Min", "combination6" ,"combination7","combination8","combination1","combination3","combination2","combination4","Gold_standard" ), , drop = FALSE]

# png("/aimigene/lianglifeng/01.Project/09.MetaFlowX/mode5/plot10.CAMI.DASTool.sequence.png",width = 6,height = 6,units='in',res=600)
# seqRadar <- create_beautiful_radarchart(
#   data = df, caxislabels = c(0, 0.25, 0.5, 0.75, 1),
#   color = c("#046C9A","#f4cae4","#7570b3" ,"#9A8822", "#5BBCD6","#FA796C","#66a61e","#ffd92f"),
#   vlabels = c("Accuracy","Purity","Completeness"  ,         
#               "F1 score" ,"Rand index","Percentage of binned" ),
#   vlcex = 1,
#   title = "DASTool sequence"
# )
# dev.off()


# #legend ------ 
# # Reduce plot margin using par()
# #"#7294D4"
# #F98400 
# #"combination6" ,"combination7","combination8","combination1","combination3","combination2","combination4","Gold_standard"
# png("/aimigene/lianglifeng/01.Project/09.MetaFlowX/mode5/plotbinner.legend.png",width = 7,height = 3.5,units='in',res=600)
# plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
# legend("topleft", legend =c("MetaBinner+concoct",
#                             "MetaBinner+metabat2",
#                             "concoct+metabat2",
#                             "maxbin2+metabat2+concoct" ,
#                             "MetaBinner+maxbin2+metabat2+concoct",
#                             "MetaBinner+metabat2+concoct",
#                             "binny+MetaBinner+SemiBin+maxbin2+metabat2+concoct",
#                             "Gold_standard" ), pch=16, pt.cex=1, cex=1, bty='n',
#        col = c("#046C9A","#f4cae4","#7570b3" ,"#9A8822", "#5BBCD6","#FA796C","#66a61e","#ffd92f"))
# mtext("binners DASTool combination", at=0.2, cex=1.2)
# dev.off()


