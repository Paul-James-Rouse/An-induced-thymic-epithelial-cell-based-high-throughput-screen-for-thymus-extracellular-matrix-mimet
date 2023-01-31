#### Packages to load in ####
library(ggplot2)
library(RColorBrewer)
library(rstatix)
library(scales)

#### Hard Coded Variables ####
Data.location <- "~/Win7/Desktop/Upload to github/" # add location of input csv files

Graph.Output.location <- "~/Win7/Desktop/Upload to github/" # add location of output graphs

#### Load in csv file ####
# Load in the data
Data <- read.csv(file = paste0(Data.location, "Figure 6.csv"))

#### Data Transformation #### 
# Make factors
Data$Condition <- factor(Data$Condition, levels = c("Mg", "Gel", "427", "Glass", "Time 0", "TEPC", "CreERt2 MEF"))
Data$Group <- factor(Data$Group, levels = c("Natural", "Synthetic", "Solvent Control", "Cellular Control", "Before Co-Culture"))

# Changes the colnames
colnames(Data) <- c("Condition", "Replicate", "Dll4",  "Dll1",  "Endogenous Foxn1", "Foxn1", "AIRE1", "Psmb11", 
                    "Ctsl1", "Il-7", "Kitl", "Flt3l", "Ccl25", "Cxcl12", "H2-Eb1", "Group") 

# Remove the CreERt2 Only MEFS and TEPC as the iTEC - controls comparison is now in Supplementary Figure 2
Data <- Data[which(Data$Condition != "CreERt2 MEF" & Data$Condition != "TEPC"),]

# Add a log10 transformation of `H2-Eb1` 
Data$`Log10 H2-Eb1` <- log10(Data$`H2-Eb1`)

# make vector of Genes for for loop
Genes <- colnames(Data)[which(!grepl(x = colnames(Data), pattern = "Condition|Replicate|Group"))]

# Remove the glass condition as the reviwers did nor like it as a control
Data <- Data[which(Data$Condition != "Glass") , ]

#### Plotting ####
for (i in 1:length(Genes)) {
  
  if(Genes[i] != "AIRE1" & Genes[i] != "Log10 H2-Eb1"){
    
    ggplot(Data, aes(x = Condition, y = Data[,Genes[i]])) +
      
      theme(plot.title = element_text(size = 10, face="italic", family="Arial",
                                      color="Black", hjust = 0, lineheight = 1.2), 
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
      
      scale_fill_manual(values = c("#CCEBC5","#B3CDE3", "#FBB4AE", "#FFCC33")) +
      scale_color_manual(values = c("#CCEBC5","#B3CDE3", "#FBB4AE", "#FFCC33")) +
      geom_boxplot(aes(fill = Group), lwd=0.1, outlier.size = 0.3) +
      geom_point(aes(fill = Group), size = 1, shape = 21, stroke = 0.25, show.legend=FALSE) +
      labs(
        title = Genes[i],
        y = "Relative Expression",
        x = "Condition")
    
    ggsave(filename = paste0(Genes[i], ".tiff"), device = "tiff", path = Graph.Output.location,
           width = 75, height = 59.9392, units = "mm", dpi = 2400)
    
  } 
  
  if(Genes[i] == "AIRE1"){
    
    ggplot(Data, aes(x = Condition, y = Data[,Genes[i]])) +
      
      theme(plot.title = element_text(size = 10, family="Arial", color="Black",
                                      hjust = 0, lineheight = 1.2), 
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
      
      scale_fill_manual(values = c("#CCEBC5","#B3CDE3", "#FBB4AE", "#FFCC33")) +
      scale_color_manual(values = c("#CCEBC5","#B3CDE3", "#FBB4AE", "#FFCC33")) +
      geom_boxplot(aes(fill = Group), lwd=0.1, outlier.size = 0.3) +
      geom_point(aes(fill = Group), size = 1, shape = 21, stroke = 0.25, show.legend=FALSE) + #
      labs(
        title = Genes[i],
        y = "Relative Expression",
        x = "Condition")
    
    ggsave(filename = paste0(Genes[i], ".tiff"), device = "tiff", path = Graph.Output.location,
           width = 75, height = 59.9392, units = "mm", dpi = 2400) 
    
  } 
  
  if(Genes[i] == "H2-Eb1"){
    
    ggplot(Data, aes(x = Condition, y = Data[,Genes[i]])) +
      
      theme(plot.title = element_text(size = 10, face="italic", family="Arial",
                                      color="Black", hjust = 0, lineheight = 1.2),
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
      
      scale_fill_manual(values = c("#CCEBC5","#B3CDE3", "#FBB4AE", "#FFCC33")) +
      scale_color_manual(values = c("#CCEBC5","#B3CDE3", "#FBB4AE", "#FFCC33")) +
      scale_y_continuous(trans = 'log10') +
      geom_boxplot(aes(fill = Group), lwd=0.1, outlier.size = 0.3) +
      geom_point(aes(fill = Group), size = 1, shape = 21, stroke = 0.25, show.legend=FALSE)+ 
      labs(
        title = bquote(italic('H2-Eb1') ~~ (Log^'10')),
        y = bquote('Relative Expression' ~~ (Log^'10')),
        x = "Condition") 
    
    ggsave(filename = paste0("Log10 ",Genes[i], ".tiff"), device = "tiff", path = Graph.Output.location,
           width = 75, height = 59.9392, units = "mm", dpi = 2400)
    
    ggplot(Data, aes(x = Condition, y = Data[,Genes[i]])) +
      
      theme(plot.title = element_text(size = 10, face="italic", family="Arial",
                                      color="Black", hjust = 0, lineheight = 1.2),
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
      
      scale_fill_manual(values = c("#CCEBC5","#B3CDE3", "#FBB4AE", "#FFCC33")) +
      scale_color_manual(values = c("#CCEBC5","#B3CDE3", "#FBB4AE", "#FFCC33")) +
      geom_boxplot(aes(fill = Group), lwd=0.1, outlier.size = 0.3) +
      geom_point(aes(fill = Group), size = 1, shape = 21, stroke = 0.25, show.legend=FALSE)+ 
      labs(
        title = Genes[i],
        y = "Relative Expression",
        x = "Condition") 
    
    ggsave(filename = paste0(Genes[i], ".tiff"), device = "tiff", path = Graph.Output.location,
           width = 75, height = 59.9392, units = "mm", dpi = 2400)
  }
} 

#### ANOVA ####
# Calculating Variances, Normality and doing the one way ANOVA
Genes.of.interest <- Genes[which(!grepl(x = Genes, pattern = "AIRE1|Endogenous"))]

for (i in 1:length(Genes.of.interest)) {
  
  Variance.test <- levene_test(Data[, Genes.of.interest[i]] ~ Condition, data = Data) 
  
  if(Variance.test$p[1] > 0.05){
    
    print(paste0(Genes.of.interest[i], " passed Varience tests"))
    
    one.way <- aov(Data[,Genes.of.interest[i]] ~ Condition, data = Data)
    
    print(summary(one.way))
    
    par(mfrow=c(2,2))
    print(plot(one.way))
    par(mfrow=c(1,1))
    
    Normality.test <- shapiro.test(one.way[["residuals"]])
    
    print(paste0(Genes.of.interest[i], " Normality test = ", Normality.test[["p.value"]], " ( > 0.05 is normal)"))
    
    tukey.one.way<-TukeyHSD(one.way)
    print(tukey.one.way)
    write.csv(x = tukey.one.way[["Condition"]], file = paste0(Graph.Output.location, Genes.of.interest[i], " tukey post hoc test.csv"))
    
  } 
  
  else{
    
    print(paste0(Genes.of.interest[i], "FAILED Varience tests"))
    
  }
  
}