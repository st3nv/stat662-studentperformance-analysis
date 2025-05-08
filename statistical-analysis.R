library(MASS)
library(ggplot2)
library(dplyr)
library(tidyr)
library(randomForest)
library(caret)

data <- read.csv("data/student-merge-processed.csv", header = TRUE)
data_nodrop <- data %>%
  filter(G3_math != 0 &
           G3_por != 0 &
           G1_math != 0 & G1_por != 0 & G2_math != 0 & G2_por != 0)

# MLM Model - full
mlm1 <- lm(
  cbind(G3_math, G3_por)
  ~ famsize + Pstatus + Medu + Fedu + Mjob + Fjob + famrel + 
    famsup + guardian + internet +school + sex + age + address + 
    traveltime + freetime + nursery + schoolsup +
    activities + higher + romantic + freetime + goout,
  data = data_nodrop
)

summary(mlm1)

Y_hat <- fitted.values(mlm1)
Y <- model.response(model.frame(mlm1))
residuals_mlm <- residuals(mlm1)
RSS <- sum(residuals_mlm^2)
Y_mean <- matrix(colMeans(Y),
                 nrow = nrow(Y),
                 ncol = ncol(Y),
                 byrow = TRUE)
TSS <- sum((Y - Y_mean)^2)
R_squared <- 1 - (RSS / TSS)
R_squared

# Subject differences
data_nodrop$G3_math_norm <- (data_nodrop$G3_math - 
                               mean(data_nodrop$G3_math)) / sd(data_nodrop$G3_math)
data_nodrop$G3_por_norm <- (data_nodrop$G3_por - 
                              mean(data_nodrop$G3_por)) / sd(data_nodrop$G3_por)
data_nodrop$sub_diff <- data_nodrop$G3_math_norm - data_nodrop$G3_por_norm

hist(data_nodrop$sub_diff)

lm1 <- lm(
  sub_diff ~ famsize + Pstatus + Medu + Fedu + Mjob + Fjob + 
    famrel + famsup + guardian + internet +
    school + sex + age + address + traveltime + freetime + nursery + schoolsup +
    activities + higher + romantic + freetime + goout,
  data = data_nodrop
)
summary(lm1)

# Random forest
rf <- randomForest(
  sub_diff ~ famsize + Pstatus + Medu + Fedu + Mjob + Fjob + 
    famrel + famsup + guardian + internet +
    school + sex + age + address + traveltime + freetime + 
    nursery + schoolsup +activities + higher + 
    romantic + freetime + goout,
  data = data_nodrop
  ,
  ntree = 1000,
  mtry = 3,
  importance = TRUE
)

# Variable importance
importance_values <- randomForest::importance(rf, type = 2)
importance_df <- data.frame(Variable = rownames(importance_values),
                            Importance = importance_values[, 1])
importance_df <- importance_df[order(-importance_df$Importance), ]
library(ggplot2)

ggplot(importance_df, aes(x = reorder(Variable, Importance), 
                          y = Importance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Variable Importance from Random Forest", 
       x = "Variables", y = "Importance") +
  theme_minimal()
