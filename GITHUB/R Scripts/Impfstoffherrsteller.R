## Libraries

# filters & arrange
library("dplyr")
library("tidyr")

# visualization

library("ggplot2")
library("ggExtra")
library("tidyverse")
library("ggpubr")
library("patchwork")
library("hrbrthemes")

# descriptive statistics

library("car")
library("caret")

# Long data
library("reshape2")

# seven avg
library("zoo")


# Import Data

library(readxl)
Output_Impfstoffe_EU_inkl_Schweiz <- read_excel("Data/Output_Impfstoffe_EU_inkl. Schweiz.xlsx")
View(Output_Impfstoffe_EU_inkl_Schweiz)


a <- filter(Output_Impfstoffe_EU_inkl_Schweiz, Product != "All")


g <- ggplot(data = a, aes(fill = factor(Product, levels = c("Comirnaty","Moderna", "Janssen", "Vaxzevria", "Sputnik", "Beijing CNBG")), x = Total_dose_administered, y =  Country)) + 
  geom_bar(position = "fill", stat = "identity") + 
  theme_ipsum() +
  labs(x = "Verabreichte Impfdosen in %", y = "Länder", title = "Verabreichte Impfdosen in der Europäischen Union und der Schweiz") + 
  scale_fill_discrete(name = "Hersteller", labels = c("Biontech/Pfizer", "Moderna", "Johnson&Johnson", "Astra Zeneca", "Sputnik", "Sinopharm"))
  

plot(g)