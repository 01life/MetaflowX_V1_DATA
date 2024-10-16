library("ggpubr") 
library("ggprism")

library('ComplexHeatmap')

profile <- read.table("bin_card_count.txt",header=T, sep = "\t", row.names=1,check.name = F)
data <- t(profile)

p = Heatmap(data)

plot <- data[rownames(data)[row_order(p)],colnames(data)[column_order(p)]]

bb <- ggballoonplot(plot, color = "#d8a48f", fill = "#d8a48f",size.range = c(0,8),
            ggtheme = theme_bw() + 
            theme(
                axis.text.x = element_text(color="black",size=8),
                legend.text = element_text(color="black",size=8),
                axis.text.y = element_text(color="black",size=8),
                axis.title = element_blank(),
                axis.line = element_blank(),
                axis.ticks = element_line(color="black")
            )
)

pdf("bin-card.pdf",width = 6,height = 8)
bb
dev.off()