#### Packages to load in ####
library(ggplot2)
library(RColorBrewer)
library(rstatix)

#### Hard Coded Variables ####
Data.location <- "" # add location of input csv files

Graph.Output.location <- "" # add location of output graphs

Levels <- c("Mg", "Gel", "111", "287", "396", "427", "509", "519", "520", "563", "Glass")

#### Load in csv file ####
Data <- read.csv(file = paste0(Data.location, "Figure 5.csv"))

#### Data Transformation #### 
# Make factors
Data$Polymer <- factor(Data$Polymer, levels = Levels)
Data$Group <- factor(Data$Group, levels = c("Natural", "Synthetic ", "Solvent Control"))
Data$BioRep <- factor(Data$BioRep, levels = c("1", "2", "3"))

# Remove Glass only condition because the reviewers did not like it as a control
Data <- Data[which(Data[,"Group"] != "Solvent Control"),]

#### Plotting ####
Thymocyte_Pops <- colnames(Data)[which(grepl(x = colnames(Data), pattern = "Total"))]

for (i in 1:length(Thymocyte_Pops)) { # open for loop to cycle through thymocyte populations
  
  ggplot(Data, aes(x = Polymer, y = Data[,Thymocyte_Pops[i]])) +
    theme(plot.title = element_text(size = 10, family="Arial", color="Black", hjust = 0,
                                    lineheight = 1.2),
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
    
    scale_fill_manual(values = c("#CCEBC5","#B3CDE3", "#FBB4AE")) +
    scale_y_continuous(trans = 'log10') +
    geom_boxplot(aes(fill = Group), lwd=0.1, outlier.size = 0.3) +
    geom_point(aes(fill = Group), size = 1, shape = 21, stroke = 0.25,) +
    labs(
      title = sub( "_", " ", Thymocyte_Pops[i]),
      x = "Polymer",
      color = "Biological Replicate") +
    ylab(bquote('Cell Count' ~~ (Log^'10'))) 
  
  ggsave(filename = paste0(Thymocyte_Pops[i], ".tiff"),
         device = "tiff",
         path = Graph.Output.location,
         width = 75, 
         height = 59.9392,
         units = "mm",
         dpi = 2400)
  
}

#### ANOVA ####
for (i in 1:length(Thymocyte_Pops)) {
  
  Variance.test <- levene_test(Data[, Thymocyte_Pops[i]] ~ Polymer, data = Data) 
  
  if(Variance.test$p[1] > 0.05){
    
    print(paste0(Thymocyte_Pops[i], " passed Varience tests"))
    
    one.way <- aov(Data[, Thymocyte_Pops[i]] ~ Polymer, data = Data)
    
    print(summary(one.way))
    
    par(mfrow=c(2,2))
    print(plot(one.way))
    par(mfrow=c(1,1))
    
    Normality.test <- shapiro.test(one.way[["residuals"]])
    
    tukey.one.way<-TukeyHSD(one.way)
    print(tukey.one.way)
    write.csv(x = tukey.one.way[["Condition"]], file = paste0(Graph.Output.location, Thymocyte_Pops[i], " tukey post hoc test.csv"))
    
  } 
  
  else{
    
    print(paste0(Thymocyte_Pops[i], " FAILED Varience tests"))
  }
}
