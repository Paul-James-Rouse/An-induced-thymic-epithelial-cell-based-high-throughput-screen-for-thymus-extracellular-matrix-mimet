#### Packages to load in ####
library(ggplot2)
library(RColorBrewer)

#### Hard Coded Variables ####
Data.location <- "" # add location of input csv files

Graph.Output.location <- "" # add location of output graphs

Levels <- c("Gel","98", "111", "258", "287", "309", "316", "327", "361", "394", "396", "398", 
            "427", "481", "509", "519", "520", "529", "531", "551", "563", "369")

#### Load in csv file ####
Data.NotNormalised <- read.csv(file = paste0(Data.location, "Supplementary Figure 3.csv"))
Data.Normalised <- read.csv(file = paste0(Data.location, "Supplementary Figure 3 Normalised.csv"))

#### Data Transformation #### 
Data.NotNormalised$Polymer <- factor(Data.NotNormalised$Polymer, levels = Levels)
Data.Normalised$Polymer <- factor(Data.Normalised$Polymer, levels = Levels)

#### Plotting ####
# Cell Count Box plot
ggplot(Data.NotNormalised, aes(x = Polymer, y = Count)) +
  
  theme(plot.title = element_text(size = 10, family="Arial", color="Black", hjust = 0, lineheight = 1.2),
        axis.title.x = element_text(vjust = 3, size = 10, family="Arial"),
        axis.title.y = element_text(vjust = 0, size = 10, family="Arial"), 
        axis.text.x = element_text(size = 8, angle = 60, vjust = 0.75, family="Arial"),
        axis.text.y = element_text(size = 8, angle = 90, hjust = 0.5, family="Arial"),
        axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        legend.title = element_text(size = 2, family="Arial"),
        legend.text = element_text(size = 6, family="Arial"),
        legend.position = "top",
        panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  
  scale_fill_manual(values = c("#B3CDE3", "#FBB4AE","#CCEBC5")) + 
  geom_boxplot(aes(fill = Group), lwd=0.25) +
  labs(
    title = "Un-normalised iTEC Focused Array",
    y = "Cell Count",
    x = "Polymer",
    fill = "") 

ggsave(filename = "Supplementary Figure 3 Un-normalised.tiff", device = "tiff", 
       path = Graph.Output.location, width = 147.9973, height = 74.93, units = "mm",
       dpi = 2400)

# Unnormalised Box plot by replicate
ggplot(Data.NotNormalised, aes(x = Replicate, y = Count)) +
  
  theme(plot.title = element_text(size = 10, family="Arial", color="Black", hjust = 0, lineheight = 1.2),
        axis.title.x = element_text(vjust = 3, size = 10, family="Arial"),
        axis.title.y = element_text(vjust = 0, size = 10, family="Arial"),
        axis.text.x = element_text(size = 8, angle = 60, vjust = 0.75, family="Arial"),
        axis.text.y = element_text(size = 8, angle = 90, hjust = 0.5, family="Arial"),
        axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        legend.position = "none",
        panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  
  geom_boxplot(aes(group = Replicate), fill = "#CCCCCC", lwd=0.25) +
  labs(
    title = "Un-normalised Counts",
    y = "Cell Count",
    x = "Replicate",
    fill = "") 

ggsave(filename = "Supplementary Figure 3 Un-normalised by replicate.tiff", device = "tiff", 
       path = Graph.Output.location, width = 147.9973, height = 74.93, units = "mm",
       dpi = 2400)

# Normalised Box plot by replicate
ggplot(Data.Normalised, aes(x = Replicate, y = Normalised.Count)) +
  
  theme(plot.title = element_text(size = 10, family="Arial", color="Black", hjust = 0, lineheight = 1.2),
        axis.title.x = element_text(vjust = 3, size = 10, family="Arial"),
        axis.title.y = element_text(vjust = 0, size = 10, family="Arial"),
        axis.text.x = element_text(size = 8, angle = 60, vjust = 0.75, family="Arial"),
        axis.text.y = element_text(size = 8, angle = 90, hjust = 0.5, family="Arial"),
        axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        legend.title = element_text(size = 2, family="Arial"),
        legend.text = element_text(size = 6, family="Arial"),
        legend.position = "none",
        panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  geom_boxplot(aes(group = Replicate), fill = "#CCCCCC", lwd=0.25) +
  labs(
    title = "Normalised Counts",
    y = "Cell Count",
    x = "Replicate",
    fill = "") 

ggsave(filename = "Supplementary Figure 3 Normalised by replicate.tiff", device = "tiff", 
       path = Graph.Output.location, width = 147.9973, height = 74.93, units = "mm",
       dpi = 2400)
