#### Packages to load in ####
library(ggplot2)
library(RColorBrewer)
library(rstatix)

#### Hard coded Variables ####
Data.location <- "~/Win7/Desktop/Upload to github/" # add location of input csv files

Graph.Output.location <- "~/Win7/Desktop/Upload to github/" # add location of output graphs

Samples <- c("iTEC", "Murine TEC", "Human TEC")

Files_Names <- c("Figure 3 iTEC.csv", "Figure 3 Murine TEC.csv", "Figure 3 Human TEC.csv")

Levels <- c("Gel","98", "111", "258", "287", "309", "316", "327", "361", "394", "396",
            "398", "427", "481", "509", "519", "520", "529", "531", "551", "563", "369")

# Load in the data
Data <- vector("list")

#### Load in csv files ####
for (i in 1:length(Files_Names)) { # open for loop
  
  Data[[Samples[i]]] <- read.csv(file = paste0(Data.location, Files_Names[i]))

}

#### Data Transformation #### 
for (i in 1:length(Data)) { # open for loop

  # Add a column with the sample name
  Data[[i]]$Sample <- rep(x = Samples[i], times = nrow(Data[[i]])) 
  
  # Ensure the column names are the same
  colnames(Data[[1]]) <- colnames(Data[[i]])

}

# Bind everything together
Data <- do.call(rbind, Data)

# Order the $Polymer factor
Data$Polymer <- factor(Data$Polymer, levels = Levels)

# Add a new column that uses describes relationship to Figure 2
vector_of_Groups <- unique(Data$Group)
vector_of_Conditions <- c("Selected in Microarray", "Rejected in Microarray", "Gelatin Control") 

for (i in 1:length(vector_of_Groups)) { # open Group loop
  
  Data$Condition[which(Data$Group == vector_of_Groups[i])] <- vector_of_Conditions[i]
  
}

Data$Condition <- factor(Data$Condition, levels = vector_of_Conditions)


# Split up the counts so iTEC and Murine TEC are in separate columns to human TEC
Data$iTEC_and_Murine_TEC_Normalised.Counts <- rep(x = "ERROR", times = nrow(Data))
Data$iTEC_and_Murine_TEC_Normalised.Counts[which(grepl(x = rownames(Data), pattern = "iTEC|Murine"))] <- Data$Normalised.Counts[which(grepl(x = rownames(Data), pattern = "iTEC|Murine"))]
Data$iTEC_and_Murine_TEC_Normalised.Counts[which(!grepl(x = rownames(Data), pattern = "iTEC|Murine"))] <- NA_character_
Data$iTEC_and_Murine_TEC_Normalised.Counts <- as.numeric(Data$iTEC_and_Murine_TEC_Normalised.Counts)

# Split up the counts so Human TEC are in separate columns to iTEC and Murine TEC
Data$Human_Normalised.Counts <- rep(x = "ERROR", times = nrow(Data))
Data$Human_Normalised.Counts[which(grepl(x = rownames(Data), pattern = "Human"))] <- Data$Normalised.Counts[which(grepl(x = rownames(Data), pattern = "Human"))]
Data$Human_Normalised.Counts[which(!grepl(x = rownames(Data), pattern = "Human"))] <- NA_character_
Data$Human_Normalised.Counts <- as.numeric(Data$Human_Normalised.Counts)


#### Plotting ####
ggplot(Data, aes(x = Polymer)) +
  
  theme(plot.title = element_text(size=12, family="Arial", color="Black", lineheight = 1.2), 
        axis.title.x = element_text(vjust=0, size=10, family="Arial"),
        axis.title.y = element_text(vjust=0, size=10, family="Arial"),
        axis.text.x = element_text(size = 8, angle = 60, vjust = 0.75, family="Arial"),
        axis.text.y = element_text(size = 8, angle = 90, hjust = 0.5, family="Arial"),
        axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        legend.position="none",
        panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  
  scale_fill_manual(values=c( "#B3CDE3", "#FBB4AE", "#CCEBC5"),
                    breaks=c("iTEC", "Human TEC", "Murine TEC")) + 
  
  geom_abline(intercept=1, slope=0, lwd=0.25, linetype="dashed") +
  
  geom_boxplot(aes(fill=Sample, y=iTEC_and_Murine_TEC_Normalised.Counts),
               lwd=0.25,show.legend=FALSE, position=position_dodge(w = 0.9),
               width = 0.5) +
  
  geom_point(aes(fill=Sample, y=Human_Normalised.Counts),
             shape=21, width=0.5, stroke=0.25,  size=1.5) +
  
  guides(fill = guide_legend(override.aes = list(size=5, shape=c(22,21,22))
  ) ) +
  
  labs(
    title = "Normalized Cell Counts on Secondary Array",
    y = "Normalized Cell Count") 

ggsave(filename = "Figure 3.tiff",
       device = "tiff",
       path = Graph.Output.location,
       width = 170, 
       height = 75,
       units = "mm",
       dpi = 2400) 

#### ANOVA ####
Cell.types <- unique(Data$Sample)

for (i in 1:length(Cell.types)) { 
  
  subset.Data <- Data[which(Data$Sample == Cell.types[i]) , ]
  
  if(Cell.types[i] != "Human TEC"){ # open if conditions if not human data (N>1)   
    
    Variance.test <- levene_test(subset.Data$Normalised.Counts ~ Polymer, data = subset.Data)
    
    print(paste0(Cell.types[i], " Variance.test = ",Variance.test$p[1]))
    
    if( Variance.test$p[1] > 0.05){ # open if data passes assumptions  
      
      print(paste0(Cell.types[i], " Varience tests"))
      
      one.way <- aov(subset.Data$Normalised.Counts ~ Polymer, data = subset.Data)
      
      print(summary(one.way))
      
      par(mfrow=c(2,2))
      print(plot(one.way))
      par(mfrow=c(1,1))
      
      Normality.test <- shapiro.test(one.way[["residuals"]])
      
      tukey.one.way<-TukeyHSD(one.way)
      print(tukey.one.way)
      write.csv(x = tukey.one.way[["Condition"]], file = paste0(Graph.Output.location, Cell.types[i], " tukey post hoc test.csv"))
      
    } 
  } 
}
