library(ComplexHeatmap)
library(circlize)
library(Cairo)
library (RColorBrewer)
library(cowplot)
library(dplyr)


raw_pr = read.table("pathogens_bin_RA_order_plot.txt", sep = "\t",header = T, row.names = 1, check.names = FALSE)
metadata = read.table("sample.info.txt", sep = "\t", header = T,row.names = 1)

metadata$ID <- rownames(metadata)

samples_overlap = intersect(colnames(raw_pr), metadata$ID)

sub_metadata = filter(metadata, ID %in% samples_overlap)
sub_pr = raw_pr[,samples_overlap]
x = as.matrix(t(sub_pr))

sub_metadata_order <- as.matrix(sub_metadata[rownames(x),])


width_ht=8.5+ncol(x)*0.09
height_ht=1+ nrow(x)*0.2
width_box=0.5
height_box=0.2
f3 <-c("#ffffff", rev(colorRampPalette(brewer.pal(11, "RdYlBu"))(100)))



annotation_row <- rowAnnotation(Individual=sub_metadata_order[,1],col=list(Individual= c("I3"="#d8e2dc","I4"="#ffe5d9","I5"="#ffcad4","I6"="#f4acb7","I7"="#9d8189","I2"="#d6ce93","I1"="#a3a380")),show_legend=FALSE,annotation_name_gp = gpar(fontsize = 8),gap=unit(0.1,"cm"),annotation_legend_param=list(title_gp = gpar(fontsize = 8),labels_gp = gpar(fontsize = 8)),simple_anno_size = unit(0.2, "cm"))


ht <- Heatmap(x, name = " ", col = f3,
              cluster_rows = FALSE,
              cluster_columns = FALSE,
              show_row_names = FALSE,
              show_column_names = TRUE,
              column_names_gp = gpar(fontsize = 8), 
              rect_gp = gpar(col = "grey", lwd = 0.5),   
              right_annotation = annotation_row,                     
              width = unit(ncol(x)*width_box, "cm"), 
              height = unit(nrow(x)*height_box, "cm"),
              heatmap_legend_param = list(title = "RA",labels_gp=gpar(fontsize=8)),
              )
pdf('bin_RA_heatmap.pdf',height = 6, width = 6)
draw(ht)
dev.off()
