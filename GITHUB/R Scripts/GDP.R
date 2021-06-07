## GDP_per_capita

# Data Import local (Already filtered, see above)

library(readr)
gdp_project <- read_csv("Data/alte daten/gdp_project.csv")
View(gdp_project)

gdp_p <- filter(gdp_project, Country_Code == "CHE" | Country_Code == "GBR" | Country_Code == "TUR" |
                  Country_Code == "ISR" | Country_Code == "EUU" | Country_Code == "HUN")


gdp_p <- select(gdp_p, Country_Name, "2019")

gdp_a <- rename(gdp_p, GDP = "2019")


summary(gdp_a)


ggplot(gdp_a, aes(y = GDP)) + geom_boxplot()


ggplot(gdp_a, aes(x = reorder(Country_Name, -GDP), y = GDP, fill = Country_Name)) + geom_col() +
  labs(x = "", y = "BIP pro Kopf (in USD)", title = "GDP pro Kopf  - EU, CH, GBR, ISR, TUR and HUN") + 
  theme(legend.position = "none")
  




# filer gdp_per_capita of the observed countries (already done by export)

gdp_project <- filter(GDP, Country_Code == "CHE" | Country_Code == "DEU" | Country_Code == "FRA" | 
                        Country_Code == "ITA" | Country_Code == "ESP" | Country_Code == "PRT" | 
                        Country_Code == "BEL" | Country_Code == "AUT" | Country_Code == "NOR" |
                        Country_Code == "LUX" | Country_Code == "NLD" | Country_Code == "DNK" |
                        Country_Code == "IRL" | Country_Code == "GRC" | Country_Code == "FIN" |
                        Country_Code == "EST" | Country_Code == "LVA" | Country_Code == "LTU" |
                        Country_Code == "MLT" | Country_Code == "POL" | Country_Code == "SVK" |
                        Country_Code == "SVN" | Country_Code == "CZE" | Country_Code == "HUN" |
                        Country_Code == "CYP" | Country_Code == "BGR" | Country_Code == "ROU" |
                        Country_Code == "HRV" | Country_Code == "GBR" | Country_Code == "TUR" |
                        Country_Code == "ISR" | Country_Code == "EUU")

View(gdp_project)

# export

write.csv(gdp_project, file = "C:/Users/jean-/OneDrive - Hochschule Luzern/Wirtschaftsinformatik/FS21/BINA/Case Study/BIP pro Kopf/gdp_project.csv", row.names = FALSE)