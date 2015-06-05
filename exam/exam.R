library(ggplot2)
library(GGally)
library(data.table)

dt <- read.csv("./Baculum.csv")
dt <- data.table(dt)

str(dt)

dt$Age.group <- as.factor(dt$Age.group)

table(dt$Age.group)

names(dt)

p <- ggpairs(dt[Age.group == '2'], columns = c(5:13), axisLabels = "show", columnLabels = c("wght", "lgth", ))

p + theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, color = "black"))

age1 <- dt[Age.group == '1', 5:13, with = FALSE]
age2 <- dt[Age.group == '2', 5:13, with = FALSE]
age3 <- dt[Age.group == '3', 5:13, with = FALSE]

dim(age1)
dim(age2)
dim(age3)

pairs(age1)
pairs(age2)
pairs(age3)


p <- ggpairs(dt[Age.group == '2'], columns = c(5:13), axisLabels = "show", columnLabels = c("wght", "lgth", ))

p + theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, color = "black"))
