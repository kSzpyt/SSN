library(ggplot2)
library(neuralnet)
library(nnet)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
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
maximum<-apply(train[,1:7], 2, max)
minimum<-apply(train[,1:7], 2, min)
train2 <- as.data.frame(scale(train[, 1:7], center = minimum,scale=maximum-minimum))
train <- cbind(train2, train[,8:10])
train <- train[sample(1:210, 210),]
#scl <- function(x){ (x - min(x))/(max(x) - min(x)) }
#train[, 1:8] <- data.frame(lapply(train[, 1:8], scl))
apply(train, 2, range)
n <- names(train)
f <- as.formula(paste("K + R + C ~", paste(n[!n %in% c("K", "R", "C")], collapse = " + ")))
f

#set.seed(500)
k <- 10
outs <- NULL
proportion <- 0.75 
outs <- NULL


fun <- function(x, fct = "logistic")
{
  
  for(i in 1:6)
  {
    
    index <- (((i-1) * round((1/6)*nrow(train))) + 1):((i*round((1/6) * nrow(train))))
    train_cv <- train[index, ]
    test_cv <- train[-index, ]
    nn_cv <- neuralnet(f,
                       data = train_cv,
                       hidden = x,
                       act.fct = fct,
                       linear.output = FALSE)
    
    
    pr.nn <- compute(nn_cv, test_cv[, 1:7])
    pr.nn_ <- pr.nn$net.result
    
    original_values <- max.col(test_cv[, 8:10])
    pr.nn_2 <- max.col(pr.nn_)
    outs[i] <- mean(pr.nn_2 == original_values)
  }
  a <- mean(outs)
  
  return(a)
}

fun(c(4, 3, 2), "logistic")

# d <- NULL
# for (i in 1:10) {
#   d[i] <- fun(i, "logistic")
# }
# d
# fun(5)
# plot(nn_cv)
# 
# foo <- function(i){
#   index <- (((i-1) * round((1/6)*nrow(train))) + 1):((i*round((1/6) * nrow(train))))
#   index2 <- sample(1:nrow(train), round((1/6)*nrow(train)))
#   return(c(index, index2))
# }
# t <- foo(1)
# length(t)


k <- NULL
for (x in 1:100) {
  k[x] <- fun(x)
}
k
plot(k, type = "l")
which(k == max(k))

k <- NULL
kk <- expand.grid(1:5, 1:5)
for (x in 1:100) {
  k[x] <- fun(as.numeric(kk[x,]))
}
k


tree <- rpart(label ~ ., data, method = "class")
fancyRpartPlot(tree)

