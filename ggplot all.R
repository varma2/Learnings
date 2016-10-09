rm(list= ls())
movies<- read.csv("H:/Movies.csv")

# Load ggplot2 package
library(ggplot2)

# Create a frequency bar chart for rating
ggplot(
  data = movies, 
  aes(x = Rating)) + 
  geom_bar() +
  ggtitle("Count of Movies by Rating")

# Create a horizontal frequency bar chart for genre
ggplot(
  data = movies, 
  aes(x = Rating)) +
  geom_bar() +
  coord_flip() +
  ggtitle("Count of Movies by Rating")

# Create cleveland dot plot
ggplot(
  data = movies, 
  aes(x = Rating)) + 
  geom_point(stat = "count") +
  coord_flip() +
  ggtitle("Count of Movies by Rating")

# Create a pie chart
ggplot(
  data = movies, 
  aes(x = "", fill = Rating)) +
  geom_bar() +
  coord_polar(theta = "y") +
  ggtitle("Count of Movies by Rating") +
  ylab("")

# Create a pie chart of awards
ggplot(
  data = movies, 
  aes(x = "", fill = Awards)) +
  geom_bar() +
  coord_polar(theta = "y") +
  ggtitle("Proportion of Movies that Won Awards") +
  ylab("")


#################################################################

# Univariate visualizations of
# a quantitiative variable

# Create dot plot
ggplot(
  data = movies, 
  aes(x = Runtime, stat = "count")) +
  geom_dotplot(binwidth = 1) +
  ggtitle("Distribution of Movie Runtimes") +
  xlab("Runtime (minutes)")

# Create violin-style dot plot
ggplot(
  data = movies, 
  aes(x = Runtime, stat = "count")) +
  geom_dotplot(
    binwidth = 1, 
    stackdir = "center") +
  ggtitle("Distribution of Movie Runtimes") +
  xlab("Runtime (minutes)")

# Create box plot of runtime
ggplot(
  data = movies, 
  aes(x = Runtime, y = Runtime)) +
  geom_boxplot() +
  coord_flip() +
  ggtitle("Distribution of Movie Runtimes") +
  xlab("") +
  ylab("Runtime (minutes)") +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank())

# Create histogram with ggplot
ggplot(
  data = movies, 
  aes(x = Runtime)) +
  geom_histogram(binwidth = 10) +
  ggtitle("Distribution of Movie Runtimes") +
  xlab("Runtime (minutes)")

# Create density plot with ggplot
ggplot(
  data = movies, 
  aes(x = Runtime)) +
  geom_density() +
  ggtitle("Distribution of Movie Runtimes") +
  xlab("Runtime (minutes)")

# Plot small multiples of all four
dev.off()

library(grid)

viewport <- viewport(
  layout = grid.layout(4, 1))

pushViewport(viewport)

dot <-ggplot(
  data = movies, 
  aes(x = Runtime, ..count..)) +
  geom_dotplot(
    binwidth = 0.25) +
  scale_x_continuous(
    limits = c(0, 250)) +
  ggtitle("Distribution of Movie Runtimes") +
  xlab("")

print(
  x = dot, 
  vp = viewport(
    layout.pos.row = 1, 
    layout.pos.col = 1))

box <- ggplot(
  data = movies, 
  aes(x = Runtime, y = Runtime)) +
  geom_boxplot() +
  coord_flip() +
  scale_y_continuous(
    limits = c(0, 250)) +
  xlab("")

print(
  x = box, 
  vp = viewport(
    layout.pos.row = 2, 
    layout.pos.col = 1))

hist <- ggplot(
  data = movies, 
  aes(x = Runtime)) +
  geom_histogram(binwidth = 10) +
  scale_x_continuous(
    limits = c(0, 250)) +
  xlab("")

print(
  x = hist, 
  vp = viewport(
    layout.pos.row = 3, 
    layout.pos.col = 1))

density <- ggplot(
  data = movies, 
  aes(x = Runtime)) +
  geom_density() +
  scale_x_continuous(
    limits = c(0, 250)) +
  xlab("Runtime (minutes)")

print(
  x = density, 
  vp = viewport(
    layout.pos.row = 4, 
    layout.pos.col = 1))

