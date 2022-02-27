packages = c('tidyquant','tidyverse','data.table','ggHoriPlot','data.table')
for (p in packages) {
  if(!require(p,character.only = T)){
    install.packages(p)
  }
  library(p,character.only = T)
}

from_date = "2020-01-01"
to_date = "2021-12-31"
period_type = "days"  # "days"/ "weeks"/ "months"/ "years"
stock_selected = "SE"

stock_data_daily = tq_get(stock_selected,
                          get = "stock.prices",
                          from = from_date,
                          to = to_date) %>%tq_transmute(select     = NULL, 
                                                        mutate_fun = to.period, 
                                                        period  = period_type)

stocks <- c("SE","DBSDF","O39.SI","U11.SI","SNGNF","GRAB","WLMIF","SINGF",
            "C38U.SI","A17U.SI","SJX.F","BN4.SI","FLEX","SPXCF","G07.SI",
            "G13.SI","C07.SI","2588.HK","M44U.SI","ME8U.SI","C09.SI","O32.SI",
            "N2IU.SI","U14.SI","BUOU.SI","T82U.SI","U96.SI","S58.SI","KLIC",
            "K71U.SI","KEN","RW0U.SI","CJLU.SI","U06.SI","C52.SI","TDCX",
            "HCTPF","Z25.SI","KARO","TRIT")


stock_price <- function(from,to,pt,stock){
  s <- tq_get(stock_selected,
         get = "stock.prices",
         from = from_date,
         to = to_date) %>%tq_transmute(select     = NULL, 
                                       mutate_fun = to.period, 
                                       period  = period_type)
  stock <- paged_table(s)
  return(data.frame(stock))
}


x <- stock_price(from_date,to_date,period_type,stock_selected)

all_stocks <- stock_price(from_date,to_date,period_type,stocks[1])
all_stocks$CompanyName <- stocks[1]


for (stock in stocks[2:40]){
  df <- stock_price(from_date,to_date,period_type,stock)
  df$CompanyName <- stock
  all_stocks = rbind(all_stocks,df)
}



unique(all_stocks$CompanyName)


cutpoints <- function(stock){
  stock  %>% 
  mutate(
    outlier = between(
      close, 
      quantile(close, 0.25, na.rm=T)-
        1.5*IQR(close, na.rm=T),
      quantile(close, 0.75, na.rm=T)+
        1.5*IQR(close, na.rm=T))) %>% 
  filter(outlier)
}

cutpoints(x)

ori <- sum(range(cutpoints(x)$close))/2
sca <- seq(range(cutpoints(x)$close)[1], 
           range(cutpoints(x)$close)[2], 
           length.out = 7)[-4]

round(ori, 2)
round(sca, 2) 

all_stocks %>% ggplot() +
  geom_horizon(aes(date, 
                   close,
                   fill = ..Cutpoints..), 
               origin = ori, horizonscale = sca) +
  scale_fill_hcl(palette = 'RdBu', reverse = T) +
  facet_grid(CompanyName~.) +
  theme_few() +
  theme(
    panel.spacing.y=unit(0, "lines"),
    strip.text.y = element_text(size = 7, angle = 0, hjust = 0),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.border = element_blank()
  ) +
  scale_x_date(expand=c(0,0), 
               date_breaks = "1 month", 
               date_labels = "%b") +
  xlab('Date') +
  ggtitle('SG Main Stock Prices 2020-2022')
