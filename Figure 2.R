#### Packages to load in ####
library(ggplot2)
library(RColorBrewer)

#### Hard coded Variables ####
Data.location <- "" # add location of input csv

Graph.Output.location <- "" # add location of output graphs

#### Load in csv file ####
input.csv <- read.csv(file = paste0(Data.location,"Figure 2.csv"))

#### Data Transformation #### 
colnames(input.csv ) <- c("Polymer","Average_Count","Rep","Cell_type")

input.csv $Polymer <- factor(input.csv $Polymer, 
                             levels = c("369", 
                                        sort(unique(input.csv [,"Polymer"][which(input.csv [,"Polymer"] != "369")]))) )

input.csv $Cell_type <- factor(input.csv $Cell_type, levels = c("iTEC", "MEFs"))

#### Plotting ####
ggplot(input.csv, aes(x = Polymer, y = Average_Count)) +
  
  theme(plot.title = element_text(size = 10, family="Arial", color="Black", lineheight = 1.2),
        plot.subtitle = element_text(size = 12, family="Arial", face="italic"),
        axis.title.x = element_text(size = 10, family="Arial"),
        axis.title.y = element_text(size = 10, family="Arial"), 
        axis.text.x = element_text(size = 8, angle = 60, vjust = 0.75, family="Arial"),
        axis.text.y = element_text(size = 8, angle = 90, hjust = 0.5, family="Arial"),
        axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        legend.position = "none",
        panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  
  scale_fill_manual(values = c("#B3CDE3", "#FBB4AE")) + 
  geom_boxplot(aes(fill = Cell_type), lwd=0.25) +
  labs(
    title = "Cell Counts of iTEC and MEFs on Primary Screen",
    y = "Average Cell Count",
    x = "Polymer") 

ggsave(filename = "Figure 2.tiff",
       device = "tiff", path = Graph.Output.location,
       width = 147.9973, height = 78.994, units = "mm",
       dpi = 2400)



