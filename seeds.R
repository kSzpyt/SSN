library(ggplot2)
library(neuralnet)
library(nnet)
data <- read.table("seeds.txt", sep = "\t")
#data


#plot(data$V2 ~ data$V7, col = data$V8)

names(data) <- c("area_A", 
                 "perimeter_P", 
                 "compactness_C", 
                 "length_of_kernel", 
                 "width_of_kernel", 
                 "asymmetry_coefficient", 
                 "length_of_kernel_groove", 
                 "label"
)

train <- cbind(data[, 1:7], class.ind(as.factor(data$label)))

names(train) <- c(names(data)[1:7],"K", "R", "C")


scl <- function(x){ (x - min(x))/(max(x) - min(x)) }
train[, 1:8] <- data.frame(lapply(train[, 1:8], scl))
head(train)

n <- names(train)
f <- as.formula(paste("K + R + C ~", paste(n[!n %in% c("K", "R", "C")], collapse = " + ")))
f


set.seed(500)
k <- 5
outs <- NULL
proportion <- 0.75 


for(i in 1:k)
{
  index <- sample(1:nrow(train), round(proportion*nrow(train)))
  train_cv <- train[index, ]
  test_cv <- train[-index, ]
  nn_cv <- neuralnet(f,
                     data = train_cv,
                     hidden = c(3, 2),
                     act.fct = "logistic",
                     linear.output = FALSE,
                     stepmax = 10^7)
  
  
  pr.nn <- compute(nn_cv, test_cv[, 1:7])
  
  pr.nn_ <- pr.nn$net.result
  
  original_values <- max.col(test_cv[, 8:10])
  pr.nn_2 <- max.col(pr.nn_)
  outs[i] <- mean(pr.nn_2 == original_values)
}

mean(outs)