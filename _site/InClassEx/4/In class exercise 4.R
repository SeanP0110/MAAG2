packages = c('ggstatsplot', 'ggside', 'tidyverse')
#install.packages('PMCMRplus') #for pairwise comparison
#install devtools

for(p in packages){
  if(!require(p, character.only = T)){
    install.packages(p)
  }
  library(p, character.only = T)
}

#ggside: used to create side by side plots like 
#scatterplot with side boxplot

exam <- read.csv("InClassEx/4/data/Exam_data.csv")


#One-sample test
set.seed(1234)

gghistostats(
  data = exam,
  x = ENGLISH,
  type = "bayes",
  test.value = 60,
  xlab ="English scores"
)


#two-sample mean test
ggbetweenstats(
  data = exam,
  x = GENDER,
  y = MATHS,
  type = "np",
  messages = FALSE
)

ggbetweenstats(
  data = exam,
  x = RACE,
  y = ENGLISH,
  type ="p",
  mean.ci =TRUE,
  pairwise.comparisons =TRUE,
  pairwise.display ="s",
  p.adjust.method ="fdr",
  messages = FALSE
)

ggscatterstats(
  data = exam,
  x = MATHS,
  y = ENGLISH,
  marginal = TRUE,
)


exam1 <- exam %>%
  mutate(MATHS_bins =
           cut(MATHS,
               breaks = c(0,60,75,85,100))
  )

ggbarstats(exam1,
           x = MATHS_bins,
           y = GENDER)
