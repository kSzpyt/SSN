library(ggplot2)
library(neuralnet)
library(nnet)

abalone <- read.table("abalone.txt", sep = ",")
names(abalone) <- c("label",
                    "Length",
                    "Diameter",
                    "Height",
                    "Whole_weight",
                    "Shucked_weight",
                    "Viscera_weight",
                    "Shell_weight",
                    "Rings")


train <- cbind(abalone[, 2:9], class.ind(as.factor(abalone$label)))
# Set labels name
names(train) <- c(names(abalone)[2:9],"F","I","M")

# Scale data
scl <- function(x){ (x - min(x))/(max(x) - min(x)) }
train[, 1:8] <- data.frame(lapply(train[, 1:8], scl))
head(train)

train <- train[, -c(4, 5, 6, 7)]
# Set up formula
n <- names(train)
f <- as.formula(paste("F + I + M ~", paste(n[!n %in% c("F","I","M")], collapse = " + ")))
f

#nn <- neuralnet(f, data = train, hidden = c(2, 1), act.fct = "logistic", linear.output = FALSE, lifesign = "minimal", stepmax = 10^7)

#plot(nn)







# Set seed for reproducibility purposes
set.seed(500)
# 10 fold cross validation
k <- 5
# Results from cv
outs <- NULL
# Train test split proportions
proportion <- 0.95 # Set to 0.995 for LOOCV
#train <- train[-c(4, 5, 6, 8)]
# Crossvalidate, go!
train <- train[1:100,]
for(i in 1:k)
{
  index <- sample(1:nrow(train), round(proportion*nrow(train)))
  train_cv <- train[index, ]
  test_cv <- train[-index, ]
  nn_cv <- neuralnet(f,
                     data = train_cv,
                     hidden = c(2, 1),
                     act.fct = "logistic",
                     linear.output = FALSE,
                     stepmax = 10^7)
  
  # Compute predictions
  pr.nn <- compute(nn_cv, test_cv[, 1:4])
  # Extract results
  pr.nn_ <- pr.nn$net.result
  # Accuracy (test set)
  original_values <- max.col(test_cv[, 5:7])
  pr.nn_2 <- max.col(pr.nn_)
  outs[i] <- mean(pr.nn_2 == original_values)
}

mean(outs)
