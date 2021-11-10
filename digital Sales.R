# q1
library(readxl)
library(ggplot2)
toyData <- read_excel("C:/Users/MI/Desktop/Kinesso/test/toy_sales_data.xlsx",sheet="data")
View(toyData)
summary(toyData)

# q2
ggplot(data = toyData, mapping = aes(x = month)) + geom_line(mapping = aes(y = sales/1000000,color = "sales")) + geom_line(mapping = aes(y = tv_spend/1000000,color = "tv_spend")) + geom_line(mapping = aes(y = digital_spend/1000000, color = "digital_spend"))+ xlab("Month")+ylab("Spend in Millions Dollar")+ ggtitle("Time Series on Toy Spending from 2016 to 2017")

# q3
cor(toyData[, 2:4], method = "pearson")


# q4
lin_reg <- lm(sales~., data = toyData)
summary(lin_reg)


# q5
contribution_percentage_tv <- (sum(toyData$sales) - sum(toyData$tv_spend))/sum(toyData$sales)*100
contribution_percentage_tv
contribution_abs_tv <- abs(sum(toyData$sales) - sum(toyData$tv_spend))
contribution_abs_tv
sum(toyData$tv_spend)


# q6
roi <- (sum(toyData[, 2]) - sum(toyData[, 3])) / sum(toyData[, 3]) * 100
roi


# q7
the_next_year_data <- read_excel("C:/Users/MI/Desktop/Kinesso/test/toy_sales_data.xlsx",sheet="planned_spend")
View(the_next_year_data)
summary(the_next_year_data)
the_next_year_data$trend <- c(25,26,27)
the_next_year_data$xmas <- c(0,0,0)
pred_the_next_year <- predict(lin_reg, newdata = the_next_year_data)
pred_the_next_year



