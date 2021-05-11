install.packages("datasets")
install.packages("ggplot2")
install.packages("DAAG")
install.packages("MASS")
install.packages("ggcorrplot")
install.packages("GGally")
install.packages("moments")

library(ggplot2)
library(MASS)
library(DAAG)
library(ggcorrplot)
library(GGally)
require(datasets)
library(moments)


# Part A

data(trees)
head(trees)
summary(trees)

cor(trees)

plot(
  x = trees$Gir,
  y = trees$Vol,
  xlab = "Girth of trees",
  ylab = "Volume of trees",
  main = "Girth and Volume of trees",
  col = "red"
) 

abline(lm(trees$Vol ~ trees$Gir), col = "black")

hist(
  trees$Gir,
  main = "Histogram for the girth of the trees",
  xlab = "Girth",
  ylab = "Freq of Girth",
  col = "blue"
)

hist(
  trees$Height,
  main = "Histogram for the Height of the trees",
  xlab = "Height",
  ylab = "Freq of Height.",
  col = "yellow",
) 

hist(
  trees$Vol,
  main = "Histogram for the Volume of the trees",
  xlab = "Volume",
  ylab = "Freq of Vol.",
  col = "green"
) 

plot(
  density(trees$Gir),
  main = "Density Plot for the Girth of the trees",
  xlab = "Girth",
  ylab = "Prob of Girth",
  col = "red"
)

plot(
  density(trees$Height),
  main = "Density Plot for the Height of the trees",
  xlab = "Height",
  ylab = "Prob of Height",
  col = "pink"
) 

plot(
  density(trees$Vol),
  main = "Density Plot for the Volume of the trees",
  xlab = "Volume",
  ylab = "Prob of Volume",
  col = "blue"
)

boxplot(
  trees$Gir,
  main = "Box Plot for Girth of trees",
  col = "orange"
)

boxplot(
  trees$Height,
  main = "Box Plot for Height of trees",
  col = "green"
)

boxplot(
  trees$Vol,
  main = "Box Plot for Volume of trees",
  col = "red"
)

qqnorm(trees$Gir,
       main = "Normal plot for Girth of the trees",
       col = "purple"
)

qqnorm(trees$Height,
       main = "Normal plot for the Height of the trees",
       col = "purple"
)

qqnorm(trees$Vol,
       main = "Normal plot for the Volume of the trees",
       col = "purple"
)

skewness(trees$Gir)
kurtosis(trees$Gir)

skewness(trees$Height)
kurtosis(trees$Height)

skewness(trees$Vol)
kurtosis(trees$Vol)



# -----------------------------

# Part B


data(Rubber)
head(Rubber)
summary(Rubber)

data(oddbooks)
head(oddbooks)
summary(oddbooks)

regrRubber <- lm(loss ~ hard + tens, data = Rubber)
regrRubber

regrOddbooks <- lm(weight ~ thick + height + breadth, data = oddbooks)
regrOddbooks

corrRubber <- cor(Rubber)
corrRubber

corrOddbooks <- cor(oddbooks)
corrOddbooks

ggcorrplot(
  correlationRubber,
  colors = c("red", "blue", "yellow"),
  method = "square"
)

ggcorrplot(
  corrOddbooks,
  colors = c("pink", "blue", "orange"),
  method = "square"
)

ggpairs(
  Rubber,
  diag = list(continuous = "densityDiag"),
  columns = 1:ncol(Rubber),
  upper = list(continuous = "cor"),
  axisLabels = c("show", "internal", "none"),
  lower = list(continuous = "points")
)

ggpairs(
  oddbooks,
  diag = list(continuous = "densityDiag"),
  columns = 1:ncol(Rubber),
  upper = list(continuous = "cor"),
  axisLabels = c("show", "internal", "none"),
  lower = list(continuous = "points")
)

