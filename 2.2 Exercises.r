###   2.2 Exercises: Tree Maps, Area Charts, Stacked Area Charts, & Step Charts


### Step 1: Importing Data and installing treemap package in R

## Import the datafile in R and view the data sample

# read the unemployment.rate and store in data frame 

unemployement_rate <- read.csv("C:/Users/Soukhna/Desktop/DSC640_06012020/DSC 640_Exercises/2.2 Excercises/unemployement-rate-1948-2010.csv")

#head(unemployement_rate)

data <- read.csv("C:/Users/Soukhna/Desktop/DSC640_06012020/DSC 640_Exercises/2.2 Excercises/unemployement-rate-1948-2010.csv")


head(data)                 # show the first five rows (variables)
str(data)                  # shows the structure of the data frame
attach(data)               #attach the data frame to the environment
#names(data)               # look at our variable names
#summary(data)             # provides summary statistics on the columns of the data frame
#View(data)                 # shows a spreadsheet-like display of the entire data frame
nrow(data)                 # showing number of the rows
ncol(data)                 # showing  the number of columns
dim(data)                  # shows the dimensions of the data frame by row and column
colnames(data)             # shows the name of each column in the data frame
head(data)                 # shows the first 6 rows of the data frame ## look at the first several rows of the data
tail(data)                 # shows the last 6 rows of the data frame
rownames(data)
colnames(data)

## Explore the data

library(dplyr)
glimpse(data) 


########## unemployement-rate Treemap

## Installing the package and calling the package in R

#install.packages("treemap")
library(treemap)

data <- read.csv("C:/Users/Soukhna/Desktop/DSC640_06012020/DSC 640_Exercises/2.2 Excercises/unemployement-rate-1948-2010.csv")

## Creating the most basic treemap
treemap(data,index = c("Period"),vSize ="Value")


## Titles and font size of the labels
treemap(data,index = c("Period"),vSize ="Value", title = 'Period by Value', 
        palette = 'RdYlGn', fontsize.labels = c(15,10))



############# Basic Treemap

library(plotly)

fig <- plot_ly(
  type="treemap",
  labels=c("Eve", "Cain", "Seth", "Enos", "Noam", "Abel", "Awan", "Enoch", "Azura"),
  parents=c("", "Eve", "Eve", "Seth", "Seth", "Eve", "Eve", "Awan", "Eve")
)
fig


############# Customize Treemap Attributes

library(plotly)

labels = c("Eve", "Cain", "Seth", "Enos", "Noam", "Abel", "Awan", "Enoch", "Azura")
parents = c("", "Eve", "Eve", "Seth", "Seth", "Eve", "Eve", "Awan", "Eve")

fig <- plot_ly(
  type='treemap',
  labels=labels,
  parents=parents,
  values= c(10, 14, 12, 10, 2, 6, 6, 1, 4),
  textinfo="label+value+percent parent+percent entry+percent root",
  domain=list(column=0))

fig <- fig %>% add_trace(
  type='treemap',
  branchvalues="total",
  labels=labels,
  parents=parents,
  values=c(65, 14, 12, 10, 2, 6, 6, 1, 4),
  textinfo="label+value+percent parent+percent entry",
  outsidetextfont=list(size=20, color= "darkblue"),
  marker=list(line= list(width=2)),
  pathbar=list(visible= FALSE),
  domain=list(column=1))

fig <- fig %>% layout(
  grid=list(columns=2, rows=1),
  margin=list(l=0, r=0, b=0, t=0))

fig


####################  Set Color of Treemap Sectors

# There are three different attributes you can use to change the color of the sectors of treemaps you have created with Plotly for R:
  
#  marker.colors
#marker.colorscale
#colorway,

#The following examples show how to use each attribute. To use marker.colors, pass a list of valid CSS colors or hexadecimal color codes.


library(plotly)

labels = c("A1", "A2", "A3", "A4", "A5", "B1", "B2")
parents = c("", "A1", "A2", "A3", "A4", "", "B1")

fig <- plot_ly(
  type="treemap",
  labels=labels,
  parents=parents,
  marker=list(colors=c("#000", "royalblue", "lightgray", "purple", "#FFF", "lightgray", "lightblue")))
fig


###############    unemployement-rate Area Chart 

data <- read.csv("C:/Users/Soukhna/Desktop/DSC640_06012020/DSC 640_Exercises/2.2 Excercises/unemployement-rate-1948-2010.csv")

ggplot2::ggplot(data, ggplot2::aes(x=Year , y=`Value`)) +
  ggplot2::geom_area( fill='Darkblue', alpha=.2) +
  ggplot2::geom_line() +
  ggplot2::ggtitle('Value by Year')


##################   Area Chart

# Libraries
library(tidyverse)
library(hrbrthemes)
library(plotly)
library(patchwork)
library(babynames)
library(viridis)

# Load dataset from github
data <- read.table("https://raw.githubusercontent.com/holtzy/data_to_viz/master/Example_dataset/3_TwoNumOrdered.csv", header=T)
data$date <- as.Date(data$date)

# plot
data %>%
  ggplot( aes(x=date, y=value)) +
  geom_area(fill="#69b3a2", alpha=0.5) +
  geom_line(color="#69b3a2") +
  ggtitle("Evolution of Bitcoin price") +
  ylab("bitcoin price ($)") +
  theme_ipsum()


###################    Area Plot without Lines


library(plotly)

diamonds1 <- diamonds[which(diamonds$cut == "Fair"),]
density1 <- density(diamonds1$carat)

diamonds2 <- diamonds[which(diamonds$cut == "Ideal"),]
density2 <- density(diamonds2$carat)

fig <- plot_ly(x = ~density1$x, y = ~density1$y, type = 'scatter', mode = 'none', name = 'Fair cut', fill = 'tozeroy',
               fillcolor = 'rgba(168, 216, 234, 0.5)')
fig <- fig %>% add_trace(x = ~density2$x, y = ~density2$y, name = 'Ideal cut', fill = 'tozeroy',
                         fillcolor = 'rgba(255, 212, 96, 0.5)')
fig <- fig %>% layout(xaxis = list(title = 'Carat'),
                      yaxis = list(title = 'Density'))

fig



#######     Interior Filling for Area Chart

library(plotly)

month <- c('January', 'February', 'March', 'April', 'May', 'June', 'July',
           'August', 'September', 'October', 'November', 'December')
high_2014 <- c(28.8, 28.5, 37.0, 56.8, 69.7, 79.7, 78.5, 77.8, 74.1, 62.6, 45.3, 39.9)
low_2014 <- c(12.7, 14.3, 18.6, 35.5, 49.9, 58.0, 60.0, 58.6, 51.7, 45.2, 32.2, 29.1)
data <- data.frame(month, high_2014, low_2014)
data$average_2014 <- rowMeans(data[,c("high_2014", "low_2014")])

#The default order will be alphabetized unless specified as below:
data$month <- factor(data$month, levels = data[["month"]])

fig <- plot_ly(data, x = ~month, y = ~high_2014, type = 'scatter', mode = 'lines',
               line = list(color = 'rgba(0,100,80,1)'),
               showlegend = FALSE, name = 'High 2014')
fig <- fig %>% add_trace(y = ~low_2014, type = 'scatter', mode = 'lines',
                         fill = 'tonexty', fillcolor='rgba(0,100,80,0.2)', line = list(color = 'rgba(0,100,80,1)'),
                         showlegend = FALSE, name = 'Low 2014')
fig <- fig %>% layout(title = "High and Low Temperatures in New York",
                      paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(229,229,229)',
                      xaxis = list(title = "Months",
                                   gridcolor = 'rgb(255,255,255)',
                                   showgrid = TRUE,
                                   showline = FALSE,
                                   showticklabels = TRUE,
                                   tickcolor = 'rgb(127,127,127)',
                                   ticks = 'outside',
                                   zeroline = FALSE),
                      yaxis = list(title = "Temperature (degrees F)",
                                   gridcolor = 'rgb(255,255,255)',
                                   showgrid = TRUE,
                                   showline = FALSE,
                                   showticklabels = TRUE,
                                   tickcolor = 'rgb(127,127,127)',
                                   ticks = 'outside',
                                   zeroline = FALSE))

fig



##############  Stacked Area Chart


# Packages
library(ggplot2)
library(dplyr)

unemployement_rate <- read.csv("C:/Users/Soukhna/Desktop/DSC640_06012020/DSC 640_Exercises/2.2 Excercises/unemployement-rate-1948-2010.csv")

#head(unemployement_rate)

plot <- ggplot(unemployement_rate, aes(x=unemployement_rate$Year, y=unemployement_rate$Value, fill=unemployement_rate$Type))

plotWithGeom <- plot + geom_area(aes(colour=unemployement_rate$Type, fill=data$Type))

plotWithGeom  



#############     stacked area chart

# Packages
library(ggplot2)
library(dplyr)

# create data
time <- as.numeric(rep(seq(1,7),each=7))  # x Axis
value <- runif(49, 10, 100)               # y Axis
group <- rep(LETTERS[1:7],times=7)        # group, one shape per group
data <- data.frame(time, value, group)

ggplot(data, aes(x=time, y=value, fill=group)) + 
  geom_area()


# Give a specific order:
data$group <- factor(data$group , levels=c("B", "A", "D", "E", "G", "F", "C") )

# Plot again
ggplot(data, aes(x=time, y=value, fill=group)) + 
  geom_area()



# Note: you can also sort levels alphabetically:
myLevels <- levels(data$group)
data$group <- factor(data$group , levels=sort(myLevels) )

# Note: sort followinig values at time = 5
myLevels <- data %>%
  filter(time==6) %>%
  arrange(value)
data$group <- factor(data$group , levels=myLevels$group )



#############     stacked area chart


# Compute percentages with dplyr
library(dplyr)
data <- data  %>%
  group_by(time, group) %>%
  summarise(n = sum(value)) %>%
  mutate(percentage = n / sum(n))

# Plot
ggplot(data, aes(x=time, y=percentage, fill=group)) + 
  geom_area(alpha=0.6 , size=1, colour="black")

# Note: compute percentages without dplyr:
my_fun <- function(vec){ 
  as.numeric(vec[2]) / sum(data$value[data$time==vec[1]]) *100 
}
data$percentage <- apply(data , 1 , my_fun)

 

################ 3 step charts 

#load the data
unemployement.rate <- read.csv("C:/Users/Soukhna/Desktop/DSC640_06012020/DSC 640_Exercises/2.2 Excercises/unemployement-rate-1948-2010.csv")

library(ggplot2)
#head(unemployement.rate )


# creating a line chart 
#ggplot(unemployement.rate, aes(x=Year,y=Value)) + geom_line(colour ="red", size=1.1)


options(repr.plot.width = 4, repr.plot.height = 3)
ggplot2::ggplot(data=unemployement.rate, ggplot2::aes(x=Year, y=Value)) +
  ggplot2::geom_line(linetype='solid', color='blue', size=1.1)


#######    Unemployment.rate step chart

#plot(unemployement.rate$Year, unemployement.rate$Valye, type = "s", main="Value By Year", xlab= "Year", ylab = "Value")

ggplot2::ggplot(data=unemployement.rate, ggplot2::aes(x=Year, y=Value)) +
  ggplot2::geom_step(linetype='solid', color='blue', size=1.2)



#######    file step chart

file <- read.csv("C:/Users/Soukhna/Desktop/DSC640_06012020/DSC 640_Exercises/2.2 Excercises/file.csv")
#head(file)

ggplot2::ggplot(data=file, ggplot2::aes(x=Period, y=Value)) +
  ggplot2::geom_step(linetype='solid', color='blue', size=1.2)



#######    score step chart

scores <- read.csv("C:/Users/Soukhna/Desktop/DSC640_06012020/DSC 640_Exercises/2.2 Excercises/scores.csv")
head(scores)

ggplot2::ggplot(data=scores, ggplot2::aes(x=Count, y=Score)) +
  ggplot2::geom_step(linetype='solid', color='green', size=1.2)


