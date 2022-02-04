packages = c('ggiraph', 'plotly', 'gganimate', 
             'patchwork', 'DT', 'tidyverse', 'gifski',
             'readxl', 'gapminder')

for(p in packages){
  if(!require(p, character.only = T)){
    install.packages(p)
  }
  library(p, character.only = T)
}

#ggiraph (pronounced giraffe) is meant for us to create
#interactive graphs in R. When you use ggiraph, you get
#the output is in scalable vector graphics (SVG).
#when you use plotly the output is in HTML to enable
#interactive visualisations.
#DT is an interactive package focused on tables
#gifski helps us wrap the images created by gganimate
#into a movie. It can group a series of sequentiel png
#and gif files into a movie.
#gapminder is used for its colour scheme and country names

exam_data <- read_csv("data/Exam_data.csv")

p <- ggplot(data=exam_data, aes(x = MATHS)) +
  geom_dotplot_interactive(aes(tooltip = ID,
                               data_id = CLASS),
                           stackgroups = TRUE,
                           binwidth = 1,
                           method = "histodot") +
  scale_y_continuous(NULL, breaks = NULL)

girafe(ggobj = p, width_svg = 6,
       height_svg = 6*0.618)

#you can change the tooltip ID into data_id = CLASS.
#All the students belonging to a class will be highlighted

#slide 9, you can also use the options method to edit
#the effects shown (or not shown or faded) during hovering

#if you want to link dots in the pot then you can use onclick
#(slide 10), but the hyperlink should be provided first separately
#in the dataframe


plot_ly(data = exam_data,
        x = ~MATHS,
        y = ~ENGLISH)

plot_ly(data = exam_data,
        x = ~ENGLISH,
        y = ~MATHS,
        color = ~RACE,
        colors = "Set1")

pal <- c("red", "yellow", "blue", "black")
plot_ly(data = exam_data,
        x = ~ENGLISH,
        y = ~MATHS,
        color = ~RACE,
        colors = pal)

plot_ly(data = exam_data,
        x = ~ENGLISH,
        y = ~MATHS,
        text = ~paste("Student ID:", ID,
                      "<br>Class:", CLASS),
        color = ~RACE,
        colors =pal)

plot_ly(data = exam_data,
        x = ~ENGLISH,
        y = ~MATHS,
        text = ~paste("Student ID:", ID,
                      "<br>Class:", CLASS),
        color = ~RACE,
        colors =pal) %>%
        layout(title ='English Score versus Maths Score',
               xaxis = list(range = c(0,100)),
               yaxis = list(range = c(0,100)))

#create a plotly with ggplot
p <- ggplot(data=exam_data,
            aes(x = MATHS,
                y = ENGLISH)) +
  geom_point(dotsize =1) +
  coord_cartesian(xlim=c(0,100),
                  ylim=c(0,100))

ggplotly(p)


#connected graphs
d <- highlight_key(exam_data)
p1 <- ggplot(data=d,
             aes(x = MATHS,
                 y = ENGLISH)) +
  geom_point(size=1) +
  coord_cartesian(xlim=c(0,100),
                  ylim=c(0,100))
p2 <- ggplot(data=d,
             aes(x = MATHS,
                 y = SCIENCE)) +
  geom_point(size=1) +
  coord_cartesian(xlim=c(0,100),
                  ylim=c(0,100))
subplot(ggplotly(p1),
        ggplotly(p2))

#crosstalk package
