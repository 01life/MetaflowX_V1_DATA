#library ------ 
library("ggpubr")
library("ggplot2")
library("ggprism")
library("rstatix")
library("gtools")
library("tidyverse")
library("multcomp")
library("nparcomp")
library("multcompView")
library("dplyr") 


library("reshape2")
library("ggseqlogo")
library("ggradar")
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
    # Variable labels
    vlcex = vlcex, 
    vlabels = vlabels,
    caxislabels = caxislabels, title = title, ...
  )
}

##1 profile
#1_Quality_of_bins_all_bins_have_the_same_weight_metrics_table.txt AMBER.result.txt
#cami1.result.txt  self_similarize.result.txt


# biner_list <- c("MetaBAT2","CONCOCT","SemiBin2","MaxBin2","MetaBinner","COMEBin","binny")
# biner_cor <- c("#e41a1c","#377eb8","#4daf4a","#984ea3","#ff7f00","#a65628","#f781bf")


# #binnigTools.stat ------ 
# pdf("binner.legend.pdf",width = 2.5 , height = 3.1)
# plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
# legend("topleft", legend =group_list, pch=16, pt.cex=1, cex=1, bty='n',
#        col = color_var)
# mtext("Binner Software", at=0.05, cex=1.2)
# dev.off()

#ALL ------
profile <- read.table("/aimigene/lianglifeng/01.Project/09.MetaFlowX/mode5/plotAMBER.result.txt",header=T, sep = "\t", check.name = F)
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
df <- dd[c("Max","Min", "concoct" ,"maxbin","metabat","binny","metabinner","semibin","Gold_standard" ), , drop = FALSE]

png("/aimigene/lianglifeng/01.Project/09.MetaFlowX/mode5/plot01.base.png",width = 6,height = 6,units='in',res=600)
create_beautiful_radarchart(
  data = df, caxislabels = c(0, 0.25, 0.5, 0.75, 1),
  color = c("#66c2a5", "#fc8d62", "#7570b3","#e78ac3", "#a6d854", "#80b1d3","#ffd92f"),
  vlabels = c("Accuracy","Purity","Completeness"  ,         
              "F1 score" ,"Rand index","Percentage of binned" ),
  vlcex = 1,
  title = "base"
)
dev.off()

#seq ------ 
seqPD <-  profile[, c( "Sample","Tool" ,"accuracy_seq", "precision_weighted_seq", "recall_weighted_seq", "f1_score_per_seq", "rand_index_seq", "percentage_of_assigned_seqs")]
seq_avg <- seqPD %>% 
  dplyr::group_by(Tool) %>% 
  summarize(
    Avg_Accuracy = mean(accuracy_seq),
    Avg_Purity = mean(precision_weighted_seq),
    Avg_Completeness = mean(recall_weighted_seq),
    Avg_F1_score = mean(f1_score_per_seq),
    Avg_Rand_index = mean(rand_index_seq),
    Avg_Percentage_of_binned = mean(percentage_of_assigned_seqs),
  )%>% 
  column_to_rownames(var = "Tool")

tt <- as.data.frame(t(seq_avg))
tt$Min <- rep(0,  nrow(tt))
tt$Max <- rep(1,  nrow(tt))
dd <- as.data.frame(t(tt))
df <- dd[c("Max","Min", "concoct" ,"maxbin","metabat","binny","metabinner","semibin","Gold_standard" ), , drop = FALSE]

png("/aimigene/lianglifeng/01.Project/09.MetaFlowX/mode5/plot02.sequence.png",width = 6,height = 6,units='in',res=600)
seqRadar <- create_beautiful_radarchart(
  data = df, caxislabels = c(0, 0.25, 0.5, 0.75, 1),
  color = c("#66c2a5", "#fc8d62", "#7570b3","#e78ac3", "#a6d854", "#377eb8","#ffd92f"),
  vlabels = c("Accuracy","Purity","Completeness"  ,         
              "F1 score" ,"Rand index","Percentage of binned" ),
  vlcex = 1,
  title = "sequence"
)
dev.off()

#CAMI ------
profile <- read.table("/aimigene/lianglifeng/01.Project/09.MetaFlowX/mode5/plotcami1.result.txt",header=T, sep = "\t", check.name = F)
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
df <- dd[c("Max","Min", "concoct" ,"maxbin","metabat","binny","metabinner","semibin","Gold_standard" ), , drop = FALSE]




png("/aimigene/lianglifeng/01.Project/09.MetaFlowX/mode5/plot03.CAMI.base.png",width = 6,height = 6,units='in',res=600)
create_beautiful_radarchart(
  data = df, caxislabels = c(0, 0.25, 0.5, 0.75, 1),
  color = c("#66c2a5", "#fc8d62", "#7570b3","#e78ac3", "#a6d854", "#80b1d3","#ffd92f"),
  vlabels = c("Accuracy","Purity","Completeness"  ,         
              "F1 score" ,"Rand index","Percentage of binned" ),
  vlcex = 1,
  title = "CAMI base"
)
dev.off()

#seq ------ 
seqPD <-  profile[, c( "Sample","Tool" ,"accuracy_seq", "precision_weighted_seq", "recall_weighted_seq", "f1_score_per_seq", "rand_index_seq", "percentage_of_assigned_seqs")]
seq_avg <- seqPD %>% 
  dplyr::group_by(Tool) %>% 
  summarize(
    Avg_Accuracy = mean(accuracy_seq),
    Avg_Purity = mean(precision_weighted_seq),
    Avg_Completeness = mean(recall_weighted_seq),
    Avg_F1_score = mean(f1_score_per_seq),
    Avg_Rand_index = mean(rand_index_seq),
    Avg_Percentage_of_binned = mean(percentage_of_assigned_seqs),
  )%>% 
  column_to_rownames(var = "Tool")

tt <- as.data.frame(t(seq_avg))
tt$Min <- rep(0,  nrow(tt))
tt$Max <- rep(1,  nrow(tt))
dd <- as.data.frame(t(tt))
df <- dd[c("Max","Min", "concoct" ,"maxbin","metabat","binny","metabinner","semibin","Gold_standard" ), , drop = FALSE]

png("/aimigene/lianglifeng/01.Project/09.MetaFlowX/mode5/plot04.CAMI.sequence.png",width = 6,height = 6,units='in',res=600)
seqRadar <- create_beautiful_radarchart(
  data = df, caxislabels = c(0, 0.25, 0.5, 0.75, 1),
  color = c("#66c2a5", "#fc8d62", "#7570b3","#e78ac3", "#a6d854", "#377eb8","#ffd92f"),
  vlabels = c("Accuracy","Purity","Completeness"  ,         
              "F1 score" ,"Rand index","Percentage of binned" ),
  vlcex = 1,
  title = "CAMI sequence"
)
dev.off()

#SELF ------
profile <- read.table("/aimigene/lianglifeng/01.Project/09.MetaFlowX/mode5/plotself_similarize.result.txt",header=T, sep = "\t", check.name = F)
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
df <- dd[c("Max","Min", "concoct" ,"maxbin","metabat","binny","metabinner","semibin","Gold_standard" ), , drop = FALSE]

png("/aimigene/lianglifeng/01.Project/09.MetaFlowX/mode5/plot05.SELF.base.png",width = 6,height = 6,units='in',res=600)
create_beautiful_radarchart(
  data = df, caxislabels = c(0, 0.25, 0.5, 0.75, 1),
  color = c("#66c2a5", "#fc8d62", "#7570b3","#e78ac3", "#a6d854", "#80b1d3","#ffd92f"),
  vlabels = c("Accuracy","Purity","Completeness"  ,         
              "F1 score" ,"Rand index","Percentage of binned" ),
  vlcex = 1,
  title = "SELF base"
)
dev.off()

#seq ------ 
seqPD <-  profile[, c( "Sample","Tool" ,"accuracy_seq", "precision_weighted_seq", "recall_weighted_seq", "f1_score_per_seq", "rand_index_seq", "percentage_of_assigned_seqs")]
seq_avg <- seqPD %>% 
  dplyr::group_by(Tool) %>% 
  summarize(
    Avg_Accuracy = mean(accuracy_seq),
    Avg_Purity = mean(precision_weighted_seq),
    Avg_Completeness = mean(recall_weighted_seq),
    Avg_F1_score = mean(f1_score_per_seq),
    Avg_Rand_index = mean(rand_index_seq),
    Avg_Percentage_of_binned = mean(percentage_of_assigned_seqs),
  )%>% 
  column_to_rownames(var = "Tool")

tt <- as.data.frame(t(seq_avg))
tt$Min <- rep(0,  nrow(tt))
tt$Max <- rep(1,  nrow(tt))
dd <- as.data.frame(t(tt))
df <- dd[c("Max","Min", "concoct" ,"maxbin","metabat","binny","metabinner","semibin","Gold_standard" ), , drop = FALSE]

png("/aimigene/lianglifeng/01.Project/09.MetaFlowX/mode5/plot06.SELF.sequence.png",width = 6,height = 6,units='in',res=600)
seqRadar <- create_beautiful_radarchart(
  data = df, caxislabels = c(0, 0.25, 0.5, 0.75, 1),
  color = c("#66c2a5", "#fc8d62", "#7570b3","#e78ac3", "#a6d854", "#377eb8","#ffd92f"),
  vlabels = c("Accuracy","Purity","Completeness"  ,         
              "F1 score" ,"Rand index","Percentage of binned" ),
  vlcex = 1,
  title = "SELF sequence"
)
dev.off()


#legend ------ 
# Reduce plot margin using par()
png("/aimigene/lianglifeng/01.Project/09.MetaFlowX/mode5/plotlegend.png",width = 3,height = 3.5,units='in',res=600)
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("topleft", legend =c("concoct" ,"maxbin","metabat","binny","metabinner","semibin","Gold_standard"), pch=16, pt.cex=1, cex=1, bty='n',
       col = c("#66c2a5", "#fc8d62", "#7570b3","#e78ac3", "#a6d854", "#377eb8","#ffd92f"))
mtext("binning Tools", at=0.2, cex=1.2)
dev.off()




#binnigTools.stat ------ 
bstat <- read.table("/aimigene/lianglifeng/01.Project/09.MetaFlowX/mode5/plotbinnigTools.stat.txt",header=T, sep = "\t", check.name = F)

df <- melt(bstat)


Lbar <- ggboxplot(df, x = "task", y = "value",alpha=0.7,fill = "task",
                    #palette = "jco",
                    ggtheme = theme_bw() +
                      theme(axis.text.x = element_text(color="black",size=10,angle=-45,hjust= 0.1 ,vjust = 0 ,face="bold"),
                            axis.text.y = element_text(color="black",size=10,face="bold"),
                            axis.title.y=element_text(color="black",size=10),
                            axis.line = element_line(color="black"),
                            axis.ticks = element_line(color="black"),
                            strip.text.x = element_text(size = 8, colour = "black", face="bold"), 
                            strip.background  = element_blank(),
                            legend.position = "none",
                            legend.text = element_text(size = 10, colour = "black"),
                            legend.title = element_text(size = 10),
                            legend.key.width = unit(0.3, 'cm'),
                            legend.key.size = unit(0, 'lines'),
                            panel.grid = element_blank(),
                            panel.background = element_blank()),
                    legend = "right",title = "",xlab = '', ylab = '',width = 0.7)+
  scale_fill_manual(values=c("#66c2a5", "#fc8d62", "#7570b3","#e78ac3", "#a6d854", "#377eb8"),labels=c("CONCOCT" ,"MAXBIN2","METABAT2","BINNY","METABINNER","SEMIBIN"))+
  scale_y_continuous(expand = expansion(mult = c(0.01, 0.1)))


flbar <- facet(Lbar ,
      facet.by = "variable",
      ncol =3,
      short.panel.labs = TRUE,
      scales = "free_y",
      strip.position = "top",
      panel.labs.font = list(size = 10,angle = 0),panel.grid = element_blank(),
      panel.labs.background = list(fill = NA, color = NA)
)
png("/aimigene/lianglifeng/01.Project/09.MetaFlowX/mode5/plot07.bining.tools.stat.png",width = 8,height = 4,units='in',res=600)
flbar
dev.off()


#DASTOOL ------
#CAMI_DASTool_AMBER.txt
#DASTool_AMBER.txt
profile <- read.table("/aimigene/lianglifeng/01.Project/09.MetaFlowX/mode5/plotCAMI_DASTool_AMBER.txt",header=T, sep = "\t", check.name = F)
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
df <- dd[c("Max","Min", "combination6" ,"combination7","combination8","combination1","combination3","combination2","combination4","Gold_standard" ), , drop = FALSE]


png("/aimigene/lianglifeng/01.Project/09.MetaFlowX/mode5/plot09.CAMI.DASTool.base.png",width = 6,height = 6,units='in',res=600)
create_beautiful_radarchart(
  data = df, caxislabels = c(0, 0.25, 0.5, 0.75, 1),
  color = c("#046C9A","#f4cae4","#7570b3" ,"#9A8822", "#5BBCD6","#FA796C","#66a61e","#ffd92f"),
  vlabels = c("Accuracy","Purity","Completeness"  ,         
              "F1 score" ,"Rand index","Percentage of binned" ),
  vlcex = 1,
  title = "DASTool base"
)
dev.off()

#seq ------ 
seqPD <-  profile[, c( "Sample","Tool" ,"accuracy_seq", "precision_weighted_seq", "recall_weighted_seq", "f1_score_per_seq", "rand_index_seq", "percentage_of_assigned_seqs")]
seq_avg <- seqPD %>% 
  dplyr::group_by(Tool) %>% 
  summarize(
    Avg_Accuracy = mean(accuracy_seq),
    Avg_Purity = mean(precision_weighted_seq),
    Avg_Completeness = mean(recall_weighted_seq),
    Avg_F1_score = mean(f1_score_per_seq),
    Avg_Rand_index = mean(rand_index_seq),
    Avg_Percentage_of_binned = mean(percentage_of_assigned_seqs),
  )%>% 
  column_to_rownames(var = "Tool")

tt <- as.data.frame(t(seq_avg))
tt$Min <- rep(0,  nrow(tt))
tt$Max <- rep(1,  nrow(tt))
dd <- as.data.frame(t(tt))
df <- dd[c("Max","Min", "combination6" ,"combination7","combination8","combination1","combination3","combination2","combination4","Gold_standard" ), , drop = FALSE]

png("/aimigene/lianglifeng/01.Project/09.MetaFlowX/mode5/plot10.CAMI.DASTool.sequence.png",width = 6,height = 6,units='in',res=600)
seqRadar <- create_beautiful_radarchart(
  data = df, caxislabels = c(0, 0.25, 0.5, 0.75, 1),
  color = c("#046C9A","#f4cae4","#7570b3" ,"#9A8822", "#5BBCD6","#FA796C","#66a61e","#ffd92f"),
  vlabels = c("Accuracy","Purity","Completeness"  ,         
              "F1 score" ,"Rand index","Percentage of binned" ),
  vlcex = 1,
  title = "DASTool sequence"
)
dev.off()


#legend ------ 
# Reduce plot margin using par()
#"#7294D4"
#F98400 
#"combination6" ,"combination7","combination8","combination1","combination3","combination2","combination4","Gold_standard"
png("/aimigene/lianglifeng/01.Project/09.MetaFlowX/mode5/plotbinner.legend.png",width = 7,height = 3.5,units='in',res=600)
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("topleft", legend =c("MetaBinner+concoct",
                            "MetaBinner+metabat2",
                            "concoct+metabat2",
                            "maxbin2+metabat2+concoct" ,
                            "MetaBinner+maxbin2+metabat2+concoct",
                            "MetaBinner+metabat2+concoct",
                            "binny+MetaBinner+SemiBin+maxbin2+metabat2+concoct",
                            "Gold_standard" ), pch=16, pt.cex=1, cex=1, bty='n',
       col = c("#046C9A","#f4cae4","#7570b3" ,"#9A8822", "#5BBCD6","#FA796C","#66a61e","#ffd92f"))
mtext("binners DASTool combination", at=0.2, cex=1.2)
dev.off()


