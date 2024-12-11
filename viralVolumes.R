install.packages("ggplot2")
library(ggplot2)

##Importing the data for dsDNA using the data from Cui et al., 2014
virionData2014 <- read.csv("Cui_etal2014.csv") 
View(virionData2014)#Viewing the data in a new tab

#Transforming the data
log_volume <- log(virionData2014$`Virion volume (nm×nm×nm)`)
log_genome <- log(virionData2014$`Genome length (kb)`)

#Our model is that logV = BlogL + loga
#Therefore, we need to create a linear model
lmVirion <- lm(log_volume ~ log_genome, data = virionData2014)
summary(lmVirion)
#This finds that B = 1.5152 and loga = 7.0748

#Plotting the graph
virusGraph <- ggplot(lmVirion, aes(x = log_genome, y = log_volume)) +
  geom_point() +
  theme_minimal() +
  labs(x = "log [Genome length (kb)]", y = "log [Virion volume(nm3)]") +
  geom_smooth(data = lmVirion, method = "lm", fill = "grey") +
  theme(axis.title = element_text(face="bold"))
virusGraph