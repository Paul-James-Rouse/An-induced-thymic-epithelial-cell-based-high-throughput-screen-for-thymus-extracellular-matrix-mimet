#### Packages to load in ####
library(ggplot2)
library(RColorBrewer)

#### Hard coded Variables ####
Data.location <- "" # add location for input csv

Graph.Output.location <- "" # add location for output csv


#### Load in csv file ####
input.csv <- read.csv(file = paste0(Data.location, list.files(path = Data.location, pattern = "Supplementary Figure 2")))

#### Data Transformation #### 
# Make Sample a factor
input.csv$Sample <- factor(input.csv$Sample, levels = c("iTEC", "TEC", "MEF"))

# Make BioRep a factor
input.csv$BioRep <- factor(input.csv$BioRep, levels = c("A","B","C"))

# Changes the column names 
colnames(input.csv) <- c("Gene", "Relative Expression", "Sample", "BioRep")

# Make a vector of Genes
Genes <- unique(input.csv$Gene)

# split the input.csv in a a list by Genes
Data <- vector("list")
for (i in 1:length(Genes)) {
  
  Data[[Genes[i]]] <- input.csv[which(input.csv$Gene == Genes[i]),]
  
}

#### Plotting ####
for (i in 1:length(Genes)) {
  
  ggplot(Data[[i]], aes(x = Sample, y = `Relative Expression`)) +
    
    theme(plot.title = element_text(size = 10, face="italic", family="Arial", color="Black",
                                    hjust = 0, lineheight = 1.2),
          plot.subtitle = element_text(size = 10, family="Arial", face="italic", hjust = 0),
          plot.caption = element_text(size = 12, family="Arial"),
          axis.title.x = element_text(vjust = 5, size = 8, family="Arial"),
          axis.title.y = element_text(vjust = 0, size = 8, family="Arial"),
          axis.text.x = element_text(size = 8, angle = 60, vjust = 0.75, family="Arial"),
          axis.text.y = element_text(size = 8, angle = 90, hjust = 0.5, family="Arial"),
          axis.line.x = element_line(color="black", size = 0.5),
          axis.line.y = element_line(color="black", size = 0.5),
          legend.position = "none",
          panel.border = element_blank(),
          panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    
    scale_fill_manual(values = c("#B3CDE3", "#FBB4AE", "#CCEBC5")) +
    scale_color_manual(values = c("#B3CDE3", "#FBB4AE", "#CCEBC5")) +
    geom_boxplot(aes(fill = Sample), lwd=0.1, outlier.size = 0.3) +
    geom_point(aes(fill = Sample), size = 1, shape = 21, stroke = 0.25) +
    labs(title = Genes[i], y = "Relative Expression", x = "Cell Type") 
  
  ggsave(filename = paste0(Genes[i], ".tiff"),
         device = "tiff",
         path = Graph.Output.location,
         width = 75, 
         height = 59.9392,
         units = "mm",
         dpi = 2400)
  
}
