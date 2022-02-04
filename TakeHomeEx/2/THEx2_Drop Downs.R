packages = c('ggplot2', 'ggiraph', 'plotly', 'gganimate', 
             'patchwork', 'DT', 'tidyverse', 'gifski',
             'readxl', 'gapminder', 'plyr')

for(p in packages){
  if(!require(p, character.only = T)){
    install.packages(p)
  }
  library(p, character.only = T)
}

setwd("C:/Users/Sean Prajs/Desktop/Visual Analytics/MAAG2/TakeHomeEx/2")

#importing data
twotwoten <- read.csv("data/respopagesextod2000to2010/respopagesextod2000to2010.csv")
twoeltwotw <- read.csv("data/respopagesextod2011to2020/respopagesextod2011to2020.csv")

datalist <- list(twotwoten, twoeltwotw)

#Exploratory Data Analysis
for (d in datalist){
  print("__ Summary __")
  print(str(d))
  print("__ Head __")
  print(head(d))
}

data <- rbind(twotwoten, twoeltwotw)

#Data Prep
#Which variables do I need?
str(data)

data_graph <- data[,c("AG","Sex","Pop", "Time", "PA")]
#data$Time = as.character(data$Time)
str(data)

data_grouped <- data_graph %>%
  group_by(AG, Sex, Time, PA) %>% 
  dplyr::summarise(Pop = sum(Pop))


y_labels <- abs(seq(-(round_any(max(
  filter(data_grouped, Sex=="Females")$Pop)/100, 100, f = ceiling)),
  round_any(max(
    filter(data_grouped, Sex=="Females")$Pop)/100, 100, f = ceiling),
  50))
y_breaks <- seq(-(round_any(max(
  filter(data_grouped, Sex=="Females")$Pop), 10000, f = ceiling)),
  round_any(max(
    filter(data_grouped, Sex=="Females")$Pop), 10000, f = ceiling),
  5000)
y_breaks
y_labels

plot <- data_grouped %>% ggplot(aes(x=AG, y = Pop, fill = Sex, frame = Time, ids = AG)) +
  geom_col(position = "identity", data = data_grouped %>% filter(Sex=="Females", PA == "Bedok"),
           fill = "pink") + 
  geom_col(position = "identity", data = data_grouped %>% filter(Sex=="Males", PA == "Bedok"), 
           aes(y=Pop*(-1)), fill = "navy") + 
  geom_col(position = "identity", data = data_grouped %>% filter(Sex=="Females", PA == "Tampines"),
           fill = "pink") + 
  geom_col(position = "identity", data = data_grouped %>% filter(Sex=="Males", PA == "Tampines"), 
           aes(y=Pop*(-1)), fill = "navy") + 
  geom_col(position = "identity", data = data_grouped %>% filter(Sex=="Females", PA == "Jurong West"),
           fill = "pink") + 
  geom_col(position = "identity", data = data_grouped %>% filter(Sex=="Males", PA == "Jurong West"), 
           aes(y=Pop*(-1)), fill = "navy") + 
  geom_col(position = "identity", data = data_grouped %>% filter(Sex=="Females", PA == "Woodlands"),
           fill = "pink") + 
  geom_col(position = "identity", data = data_grouped %>% filter(Sex=="Males", PA == "Woodlands"), 
           aes(y=Pop*(-1)), fill = "navy") + 
  geom_col(position = "identity", data = data_grouped %>% filter(Sex=="Females", PA == "Hougang"),
           fill = "pink") + 
  geom_col(position = "identity", data = data_grouped %>% filter(Sex=="Males", PA == "Hougang"), 
           aes(y=Pop*(-1)), fill = "navy") +
  scale_y_continuous(name = "Population (in 00's)", 
                     breaks = y_breaks,
                     labels = y_labels) +
  labs(title = "Age Sex Pyramid Singapore", x = "Age Group") +
  coord_flip() + 
  theme_light() +
  theme(plot.title = element_text(hjust=0.5))

updatemenus <- list(
  list(
    active = 0,
    buttons = list(
      list(
        label = "Bedok",
        method = "update",
        args = list(list(visible = c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE,
                                     FALSE, FALSE, FALSE, FALSE)),
                    list(title = "Age Sex Pyramid Bedok"))),
      list(
        label = "Tampines",
        method = "update",
        args = list(list(visible = c(FALSE, FALSE, TRUE, TRUE, FALSE, FALSE,
                                     FALSE, FALSE, FALSE, FALSE)),
                    list(title = "Age Sex Pyramid Tampines"))),
      list(
        label = "Jurong West",
        method = "update",
        args = list(list(visible = c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE,
                                     FALSE, FALSE, FALSE, FALSE)),
                    list(title = "Age Sex Pyramid Jurong West"))),
      list(
        label = "Woodlands",
        method = "update",
        args = list(list(visible = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
                                     TRUE, TRUE, FALSE, FALSE)),
                    list(title = "Age Sex Pyramid Woodlands"))),
      list(
        label = "Hougang",
        method = "update",
        args = list(list(visible = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
                                     FALSE, FALSE, TRUE, TRUE)),
                    list(title = "Age Sex Pyramid Hougang")))
  )
))

ggplotly(plot) %>% animation_opts(
  1000, easing = "elastic", redraw = TRUE) %>%
  layout(showlegend=FALSE,
         updatemenus=updatemenus)


##########################################

y_labels <- abs(seq(-(round_any(max(
  filter(data_grouped, Sex=="Females")$Pop)/1000, 100, f = ceiling)),
  round_any(max(
    filter(data_grouped, Sex=="Females")$Pop)/1000, 100, f = ceiling),
  50))
y_breaks <- seq(-(round_any(max(
  filter(data_grouped, Sex=="Females")$Pop), 100000, f = ceiling)),
  round_any(max(
    filter(data_grouped, Sex=="Females")$Pop), 100000, f = ceiling),
  50000)
y_breaks
y_labels



plot <- data_grouped %>% ggplot(aes(x=AG, y = Pop, fill = Sex, frame = Time, ids = AG)) +
  geom_col(position = "identity", data = data_grouped %>% filter(Sex=="Females"),
           fill = "pink") + 
  geom_col(position = "identity", data = data_grouped %>% filter(Sex=="Males"), 
           aes(y=Pop*(-1)), fill = "navy") + 
  scale_y_continuous(name = "Population (in 000's)", 
                     breaks = y_breaks,
                     labels = y_labels) +
  labs(title = "Age Sex Pyramid Singapore",
       x = "Age Group") +
  coord_flip() + 
  theme_light() +
  theme(plot.title = element_text(hjust=0.5))

ggplotly(plot) %>% animation_opts(
  1000, easing = "elastic", redraw = TRUE
)

#(references frame: https://community.plotly.com/t/issue-adding-colour-aesthetic-with-ggplotly-when-added-all-points-appear-on-all-frames/40613
# https://github.com/plotly/plotly.R/issues/1544)
#(reference animation_opts: https://plotly.com/r/animations/)
#(reference drop down: https://stackoverflow.com/questions/40024029/plotly-updating-data-with-dropdown-selection)
