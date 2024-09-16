data(iris) # load data

#plot based on hist (default function)

petal_length_mean <- mean (iris$Petal.Length) # calculate the mean value of Petal.Length
hist(iris$Petal.Length) #create a histogram
hist(iris$Petal.Length, breaks = 50, ylim = c(0, 20)) # set 50 breaks for the histogram and set the range of y
abline(v = mean(iris$Petal.Length), col='red', lwd = 3) # add the average on the graph
lines(density(iris$Petal.Length), col = 'green', lwd = 3) # add the density on the graph
hist(iris$Petal.Length, breaks = 50, xlab = 'Length of Petal (cm)', ylab = 'Frequency', main = 'Distribution of Petal Length', col = 'gray', border = "white") #add the legends and title on the graph

#install packages fo "ggplot2", "readr", and "dplyr", only need to do once
install.packages("ggplot2")
install.packages("readr")
install.packages("dplyr")
# load those packages, load by "library" before we use it in one project
library(ggplot2)
library(readr)
library(dplyr)

# plot the histogram based on ggplot
# basic graph
ggplot(iris, aes(x = Petal.Length)) + geom_histogram()
length_stats <- iris |>
  summarise(mean_length = mean(Petal.Length))
length_stats

# add average line by using geom_vline()
ggplot(data = iris, aes(x = Petal.Length, y = after_stat(density))) +
  geom_histogram() +
  geom_vline(aes(xintercept = mean_length), length_stats, color = "red", linewidth = 1)

# add the density by using geom_density()
ggplot(data = iris, aes(x = Petal.Length, y = after_stat(density))) +
  geom_histogram() +
  geom_vline(aes(xintercept = mean_length), length_stats, color = "red", linewidth = 1) +
  geom_density(color = "green", linewidth = 1)

# set 50 breaks for the x extent
ggplot(data = iris, aes(x = Petal.Length, y = after_stat(density))) +
  geom_histogram(bins = 50) +
  geom_vline(aes(xintercept = mean_length), length_stats, color = "red", linewidth = 1) +
  geom_density(color = "green", linewidth = 1)

# change number of bins by changing the width of it
ggplot(data = iris, aes(x = Petal.Length, y = after_stat(density))) +
  geom_histogram(binwidth = 0.1) +
  geom_vline(aes(xintercept = mean_length), length_stats, color = "red", linewidth = 1) +
  geom_density(color = "green", linewidth = 1)

# change number of bins by changing the width of it
ggplot(data = iris, aes(x = Petal.Length, y = after_stat(density))) +
  geom_histogram(boundary = 0.5) +
  geom_vline(aes(xintercept = mean_length), length_stats, color = "red", linewidth = 1) +
  geom_density(color = "green", linewidth = 1)

#change the colors of the background and the bins
ggplot(data = iris, aes(x = Petal.Length, y = after_stat(density))) +
  geom_histogram(bins = 50, color = "white", fill = "blue") +
  geom_vline(aes(xintercept = mean_length), length_stats, color = "red", linewidth = 1) +
  geom_density(color = "green", linewidth = 1)

# import the factor "Species" from data_iris
data_iris <- iris |>
  mutate(Species = factor(Species))

# group it by factor "species"
ggplot(data = data_iris, aes(x = Petal.Length, y = after_stat(density), fill = Species)) +
  geom_histogram(bins = 50, color = "white", fill = "blue") +
  geom_vline(aes(xintercept = mean_length), length_stats, color = "red", linewidth = 1) +
  geom_density(color = "gray", linewidth = 1)+
  labs(x ='Length of Petal (cm)', y='Count', title = 'Petal Length Distributions')

# Add the range of x using xlin()
ggplot(data = data_iris, aes(x = Petal.Length, y = after_stat(density), fill = Species)) +
  geom_histogram(bins = 50, color = "white", fill = "blue") +
  geom_vline(aes(xintercept = mean_length), length_stats, color = "red", linewidth = 1) +
  geom_density(color = "gray", linewidth = 1)+
  labs(x ='Length of Petal (cm)', y='Count', title = 'Petal Length Distributions') +
  xlim(0, 8)

# adjust the position of the legend and separate the three species into three graphs

PetalPlot <- ggplot(data = data_iris, aes(x = Petal.Length, y = after_stat(density), fill = Species)) +
  geom_histogram(bins = 50, color = "white", fill = "blue") + facet_grid(vars(Species)) +
  geom_vline(aes(xintercept = mean_length), length_stats, color = "red", linewidth = 1) +
  geom_density(color = "gray", linewidth = 1, alpha = 0.5)+
  labs(x ='Length of Petal (cm)', y='Count', title = 'Petal length distributions of three species', caption = "Data Resource: iris (https://rpubs.com/moeransm/intro-iris)") +
  xlim(0, 8) +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))

# save plot result
ggsave("Petal_Length_Distributions.png", plot = PetalPlot, width = 8, height = 6, dpi = 300)
