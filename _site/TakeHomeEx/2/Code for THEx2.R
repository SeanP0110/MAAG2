packages = c('ggiraph', 'plotly', 'gganimate', 
             'patchwork', 'DT', 'tidyverse', 'gifski')

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

data_graph <- data %>% select(c("AG","Sex","Pop", "Time"))
#data$Time = as.character(data$Time)
str(data_graph)

data_grouped <- data_graph %>%
  group_by(AG, Sex, Time) %>% 
  dplyr::summarise(Pop = sum(Pop))

#(reference: https://statisticsglobe.com/group-data-frame-by-multiple-columns-in-r)
str(data_grouped)

data_grouped

print(data_grouped, n=40)

#First Graph Structure

y_labels <- abs(seq(-(round_any(max(
  filter(data_grouped, Sex=="Females", Time == 2000)$Pop)/1000, 100, f = ceiling)),
  round_any(max(
    filter(data_grouped, Sex=="Females", Time == 2000)$Pop)/1000, 100, f = ceiling),
  50))
y_breaks <- seq(-(round_any(max(
  filter(data_grouped, Sex=="Females", Time == 2000)$Pop), 100000, f = ceiling)),
  round_any(max(
    filter(data_grouped, Sex=="Females", Time == 2000)$Pop), 100000, f = ceiling),
  50000)
y_breaks
y_labels

plot <- data_grouped %>% ggplot(aes(x=AG, y = Pop, fill = Sex)) +
  geom_col(data = data_grouped %>% filter(Sex=="Females", Time == 2000),
           fill = "pink") + 
  geom_col(data = data_grouped %>% filter(Sex=="Males", Time == 2000), 
           aes(y=Pop*(-1)), fill = "navy") + 
  scale_y_continuous(name = "Population (in 000's)", 
                     breaks = y_breaks,
                     labels = y_labels) +
  labs(title = "Age Sex Pyramid Singapore", x = "Age Group") +
  coord_flip() + 
  theme_light() +
  theme(plot.title = element_text(hjust=0.5))

ggplotly(plot)

#change 5_to_9 to 05_to_09 and 0_to_4 to 00_to_04

data_grouped[data_grouped$AG=="5_to_9", ]$AG <- "05_to_09"
data_grouped[data_grouped$AG=="0_to_4", ]$AG <- "00_to_04"

#(reference coord_flip(): https://www.datanovia.com/en/blog/how-to-create-a-ggplot-horizontal-bar-chart/)
#(reference inverse plot: https://stackoverflow.com/questions/14680075/simpler-population-pyramid-in-ggplot2)
#(reference seq function: https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/seq)
#(reference rounding up: https://stackoverflow.com/questions/6461209/how-to-round-up-to-the-nearest-10-or-100-or-x)
#(reference theme: https://www.datanovia.com/en/blog/ggplot-themes-gallery/#use-themes-in-ggplot2)

#graph animation by year
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

plot <- data_grouped %>% ggplot(aes(x=AG, y = Pop, fill = Sex)) +
  geom_col(data = data_grouped %>% filter(Sex=="Females"),
           fill = "pink") + 
  geom_col(data = data_grouped %>% filter(Sex=="Males"), 
           aes(y=Pop*(-1)), fill = "navy") + 
  scale_y_continuous(name = "Population (in 000's)", 
                     breaks = y_breaks,
                     labels = y_labels) +
  labs(title = "Age Sex Pyramid Singapore",
       subtitle = "Year: {frame_time}",
       x = "Age Group") +
  coord_flip() + 
  theme_light() +
  theme(plot.title = element_text(hjust=0.5)) +
  transition_time(Time) +
  ease_aes('linear', interval = 0.1)


animate(plot, 
        duration = 20)

#(reference animation: https://stackoverflow.com/questions/52899017/slow-down-gganimate-in-r)


#interactive using ggiraph
y_labels <- abs(seq(-(round_any(max(
  filter(data_grouped, Sex=="Females", Time == 2000)$Pop)/1000, 100, f = ceiling)),
  round_any(max(
    filter(data_grouped, Sex=="Females", Time == 2000)$Pop)/1000, 100, f = ceiling),
  50))
y_breaks <- seq(-(round_any(max(
  filter(data_grouped, Sex=="Females", Time == 2000)$Pop), 100000, f = ceiling)),
  round_any(max(
    filter(data_grouped, Sex=="Females", Time == 2000)$Pop), 100000, f = ceiling),
  50000)
y_breaks
y_labels

data_grouped$tooltip <- c(paste0("Age: ", data_grouped$AG, 
                                 "\n Population: ", format(data_grouped$Pop, big.mark=","), 
                                 "\n Gender: ", data_grouped$Sex))

plot <- data_grouped %>% ggplot(aes(x=AG, y = Pop, fill = Sex)) +
  geom_col_interactive(data = data_grouped %>% filter(Sex=="Females", Time == 2000),
           fill = "pink", aes(tooltip=tooltip)) + 
  geom_col_interactive(data = data_grouped %>% filter(Sex=="Males", Time == 2000), 
           aes(y=Pop*(-1), tooltip=tooltip), fill = "navy") + 
  scale_y_continuous(name = "Population (in 000's)", 
                     breaks = y_breaks,
                     labels = y_labels) +
  labs(title = "Age Sex Pyramid Singapore", x = "Age Group") +
  coord_flip() + 
  theme_light() +
  theme(plot.title = element_text(hjust=0.5))



girafe(ggobj = plot,
       width_svg = 6,
       height_svg = 6*0.70)

#(reference tooltip: https://stackoverflow.com/questions/43935048/how-add-more-information-in-tooltip-cloude-in-ggiraph-packages-in-r)

#by year using plotly
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

#division by planning areas
data <- rbind(twotwoten, twoeltwotw)

#Data Prep
#Which variables do I need?
str(data)

unique(data$PA)
big_PA <- data[,c("PA","Pop")]
big_PA_grouped <- big_PA %>%
  group_by(PA) %>% 
  dplyr::summarise(Pop = sum(Pop))

arrange(big_PA_grouped, by = desc(Pop))[1:5,]

top5_PA <- data %>% filter(PA=="Bedok"|PA=="Tampines"|
                             PA=="Jurong West" | PA=="Woodlands"|
                             PA=="Hougang")

top5_PA_graph <- top5_PA[,c("AG","Sex","Pop", "Time", "PA")]

top5_PA_grouped <- top5_PA_graph %>%
  group_by(AG, Sex, Time, PA) %>% 
  dplyr::summarise(Pop = sum(Pop))

top5_PA_grouped[top5_PA_grouped$AG=="5_to_9", ]$AG <- "05_to_09"
top5_PA_grouped[top5_PA_grouped$AG=="0_to_4", ]$AG <- "00_to_04"

y_labels <- abs(seq(-(round_any(max(
  filter(top5_PA_grouped, Sex=="Females")$Pop)/100, 100, f = ceiling)),
  round_any(max(
    filter(top5_PA_grouped, Sex=="Females")$Pop)/100, 100, f = ceiling),
  50))
y_breaks <- seq(-(round_any(max(
  filter(top5_PA_grouped, Sex=="Females")$Pop), 10000, f = ceiling)),
  round_any(max(
    filter(top5_PA_grouped, Sex=="Females")$Pop), 10000, f = ceiling),
  5000)
y_breaks
y_labels



top5_PA_plot <- top5_PA_grouped %>% ggplot(aes(x=AG, 
                                       y = Pop, 
                                       fill = Sex, 
                                       frame = Time, 
                                       ids = AG)) +
  geom_col(position = "identity", 
           data = top5_PA_grouped %>% filter(Sex=="Females"),
           fill = "pink") + 
  geom_col(position = "identity",
           data = top5_PA_grouped %>% filter(Sex=="Males"), 
           aes(y=Pop*(-1)), fill = "navy") + 
  scale_y_continuous(name = "Population (in 00's)", 
                     breaks = y_breaks,
                     labels = y_labels) +
  labs(title = "Age Sex Pyramid Singapore",
       x = "Age Group") +
  coord_flip() + 
  theme_light() +
  theme(plot.title = element_text(hjust=0.5)) +
  facet_wrap(~PA)

ggplotly(top5_PA_plot) %>% animation_opts(
  1000, easing = "elastic", redraw = TRUE
)




###################################################################


x <- data_grouped %>% filter(Sex=="Females", Time==2000)
x %>% filter(Pop == max(x$Pop))

signif(max(x$Pop), 3)

signif(160380, 3)

round_any(160390, 1000, f = ceiling)

xi <- as.Date(paste(date$Time, 1, 1, sep = "-"))
#lapply(data$Time, function(x) as.Date(as.character(x), "%Y"))
xii <- data
xii$Time <- xi

str(xii)

y <- data$Time
lapply(y, strptime(as.character(y), format = "%Y"))


female <- data %>% filter(Sex=="Females")
male <- data %>% filter(Sex=="Males")

nrow(data)
nrow(female)
nrow(male)

sum(data$Pop)
sum(female$Pop)
sum(male$Pop)


