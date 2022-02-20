#Libraries
packages = c('ggstatsplot','ggside','tidyverse','stringr','readxl',
             'parallelPlot', 'afex')

for(p in packages){
  if(!require(p, character.only = T)){
    install.packages(p)
  }
}

setwd("C:/Users/Sean Prajs/Desktop/Visual Analytics/MAAG2/TakeHomeEx/3")

#Ex1
choc <- read.csv("data/chocolate.csv")

head(choc)
str(choc)

choc_nec <- choc[, c(1:3,7,10)]
str(choc_nec)

choc_nec$cocoa_percent <- (str_remove_all(choc_nec$cocoa_percent,"%") %>% as.numeric())/100
str(choc_nec)

choc_nec <- choc_nec %>% filter(cocoa_percent >=0.7)
str(choc_nec)

choc_grouped <- choc_nec[,c(3:5)] %>% 
  group_by(company_location) %>% 
  summarise(
    cocoa_percent = mean(cocoa_percent),
    rating = mean(rating)
  )

print(choc_grouped$company_location)

choc_grouped <- choc_grouped[order(-choc_grouped$rating),]
choc_grouped

ggdotplotstats(data = choc_grouped[1:15,], y = "company_location", x = "rating")
choc_grouped[1:15,]

# Exercise 2
data <- read_xls("data/Superstore-2021.xls")
colnames(data)

necessary <- data[c("State" ,"Profit")]
str(necessary)

detach("package:ggpubr", unload = TRUE)
detach("package::ggpubr", unload = TRUE)

data_grouped <- dplyr::group_by(data, "State") %>% 
  dplyr::summarise(
    Profit = mean(Profit),
    Sales = mean(Sales))
data_grouped

ggwithinstats(
  data = necessary_grouped,
  x = State,
  y = Profit)
)

#Exercise 3
drinks <- read.csv("data/starbucks_drink.csv")
str(drinks)
unique(drinks$Category)

kids <- drinks %>% filter(Category=="kids-drinks-and-other")
kids_nutr <- kids[,c(2:15)]

parallelPlot(kids_nutr)

histoVisibility <- rep(TRUE, ncol(kids_nutr))
parallelPlot(kids_nutr,
             rotateTitle = TRUE,
             histoVisibility = histoVisibility)
