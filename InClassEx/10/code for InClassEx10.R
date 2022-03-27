packages = c('tidyverse', 'lubridate', 
             'ggthemes')
for (p in packages){
  if(!require(p, character.only = T)){
    install.packages(p)
  }
  library(p,character.only = T)
}

coffeechain <- read_rds("data/CoffeeChain.rds")

product <- coffeechain %>%
  group_by(`Product`) %>%
  summarise(`target` = sum(`Budget Sales`),
            `current` = sum(`Sales`)) %>%
  ungroup()

ggplot(product, aes(Product, current)) + 
  geom_col(aes(Product, max(target) * 1.01),
           fill="grey85", width=0.85) +
  geom_col(aes(Product, target * 0.75),
           fill="grey60", width=0.85) +
  geom_col(aes(Product, target * 0.5),
           fill="grey50", width=0.85) +
  geom_col(aes(Product, current), 
           width=0.35,
           fill = "black") + 
  geom_errorbar(aes(y = target,
                    x = Product, 
                    ymin = target,
                    ymax= target), 
                width = .4,
                colour = "red",
                size = 1) +
  coord_flip()

sales_report <- coffeechain %>%
  filter(Date >= "2013-01-01") %>%
  mutate(Month = month(Date)) %>%
  group_by(Month, Product) %>%
  summarise(Sales = sum(Sales)) %>%
  ungroup() %>%
  select(Month, Product, Sales)

mins <- group_by(sales_report, Product) %>% 
  slice(which.min(Sales))
maxs <- group_by(sales_report, Product) %>% 
  slice(which.max(Sales))
ends <- group_by(sales_report, Product) %>% 
  filter(Month == max(Month))

quarts <- sales_report %>%
  group_by(Product) %>%
  summarise(quart1 = quantile(Sales, 
                              0.25),
            quart2 = quantile(Sales, 
                              0.75)) %>%
  right_join(sales_report)

ggplot(sales_report, aes(x=Month, y=Sales)) + 
  facet_grid(Product ~ ., scales = "free_y") + 
  geom_ribbon(data = quarts, aes(ymin = quart1, max = quart2), 
              fill = 'grey90') +
  geom_line(size=0.3) +
  geom_point(data = mins, col = 'red') +
  geom_point(data = maxs, col = 'blue') +
  geom_text(data = mins, aes(label = Sales), vjust = -1) +
  geom_text(data = maxs, aes(label = Sales), vjust = 2.5) +
  geom_text(data = ends, aes(label = Sales), hjust = 0, nudge_x = 0.5) +
  geom_text(data = ends, aes(label = Product), hjust = 0, nudge_x = 1) +
  expand_limits(x = max(sales_report$Month) + 
                  (0.25 * (max(sales_report$Month) - min(sales_report$Month)))) +
  scale_x_continuous(breaks = seq(1, 12, 1)) +
  scale_y_continuous(expand = c(0.1, 0)) +
  theme_tufte(base_size = 3, base_family = "Helvetica") +
  theme(axis.title=element_blank(), axis.text.y = element_blank(), 
        axis.ticks = element_blank(), strip.text = element_blank())
