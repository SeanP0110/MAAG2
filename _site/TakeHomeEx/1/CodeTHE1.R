packages = c('tidyverse', 'readxl', 'knitr')

for(p in packages){
  if(!require(p, character.only = T)){
    install.packages(p)
  }
  library(p, character.only = T)
}

orders <- read_xls("data/Superstore-2021.xls", sheet = "Orders")
people <- read_xls("data/Superstore-2021.xls", sheet = "People")
returns <- read_xls("data/Superstore-2021.xls", sheet = "Returns")

str(orders)

join <- left_join(returns, orders, by = c("Order ID" = "Order ID"))

freq_returned <- join %>% 
  count(`Sub-Category`) %>%
  rename(Returns = n)

freq_sort <- freq_returned %>% arrange(desc(Returns))

freq_cum <- freq_sort %>% mutate(CumReturns = cumsum(Returns))

freq_cum

#making the bar chart
bar <- freq_cum_prop %>% ggplot(aes(reorder(`Sub-Category`, -Returns), Returns)) + 
  geom_col(color = "blue", fill = "blue") +
  labs(x = "Sub-Catgory") +
  scale_y_continuous(limits= c(0,sum(freq_cum_prop$Returns),100)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

bar

#making the line chart
freq_cum_prop <- freq_cum %>% mutate(
  PropCumReturns = CumReturns/sum(Returns))
freq_cum_prop

line <- freq_cum_prop %>% ggplot(aes(
  reorder(`Sub-Category`, PropCumReturns), PropCumReturns)) +
  geom_point(shape = 19, size = 1.5) + 
  geom_path(aes(reorder(`Sub-Category`, PropCumReturns), group = 1)) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x = "Sub-Category")
line

#combining the graphs
tot_returns = sum(freq_cum_prop$Returns)

pareto <-
  freq_cum_prop %>% ggplot(aes(reorder(`Sub-Category`, -Returns), Returns)) + 
  geom_col(color = "blue", fill = "blue") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_point(aes(reorder(`Sub-Category`, PropCumReturns), PropCumReturns*tot_returns), 
             shape = 19, size = 1.5) + 
  geom_path(aes(reorder(`Sub-Category`, PropCumReturns), PropCumReturns*tot_returns, group = 1)) +
  scale_y_continuous("Returns", sec.axis = 
                       sec_axis(~.*1/tot_returns, name = "Percentage",
                                labels = scales::percent_format()),
                     limits= c(0,sum(freq_cum_prop$Returns),100)) +
  labs(x = "Sub-Catgory")
pareto

###############################################################################
# Population Pyramid
data <- read.csv("data/respopagesextod2021.csv")
str(data)

agesex <- data[ , c("AG", "Sex")]
agesex

agesex <- agesex %>% mutate(Male = ifelse(agesex$Sex == 'Males', 1, 0))
agesex <- agesex %>% mutate(Female = ifelse(agesex$Sex == 'Females', 1, 0))

agesex

agesex_grouped <- agesex %>% group_by(AG) %>% summarise(
  "Total Male" = sum(Male), "Total Female" = sum(Female)) %>%
  ungroup()

agesex_grouped

plot <- agesex_grouped %>% ggplot(aes(x = AG, y = Male)) +
  geom_bar(aes(x = AG, y = agesex_grouped$Male, colour="Blue")) +
  geom_bar(aes(x = AG, y = agesex_grouped$Female, colour="Red")) +
  scale_y_continuous(breaks = seq(-3000, 3000, 500)) + 
  coord_flip()

plot

aeg <- data.frame(table(agesex$AG))
aeg

x <- agesex %>% subset(AG=="0_to_4")
length(x)

plot <- agesex %>% ggplot(aes(AG, fill=Sex)) + 
  geom_bar(aes(subset(agesex,Sex=="Females"))) +
  geom_bar(aes(agesex %>% subset(Sex=="Males"))) +
  coord_flip()
plot

plot <- agesex %>% subset(Sex="Females") %>% ggplot(aes(AG)) + 
  geom_bar() + coord_flip()
plot

ggplot(data=test,aes(x=as.factor(v),fill=g)) + 
  geom_bar(data=subset(test,g=="F")) + 
  geom_bar(data=subset(test,g=="M"),aes(y=..count..*(-1))) + 
  scale_y_continuous(breaks=seq(-40,40,10),labels=abs(seq(-40,40,10))) + 
  coord_flip()
