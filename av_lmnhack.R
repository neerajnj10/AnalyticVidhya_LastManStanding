### The solution scored 0.84716548212 for accuracy on LeaderBoard. (rank 83/372)



library(h2o)
h2o.init()

x <- read.csv("~/Data_Science/aAV/Train_Fyxd0t8.csv")
y <- read.csv("~/Data_Science/aAV/Test_C1XBIYq.csv")

library(data.table)
library(dplyr)


# data munging activities

sapply(x, function(y) sum(is.na(y)))

x$Number_Weeks_Used[is.na(x$Number_Weeks_Used)] <- mean(x$Number_Weeks_Used, na.rm=TRUE)

#x <- x[complete.cases(x),]
nms <- c("ID", "Crop_Type","Soil_Type", "Pesticide_Use_Category","Season", "Crop_Damage") 
x[nms] <- lapply(x[nms], as.factor) 
#x$totdose <- x$Number_Doses_Week * x$Number_Weeks_Used
#x$totdquit <- x$Number_Doses_Week * x$Number_Weeks_Quit
x$totdaysused <- x$Number_Weeks_Used * 7
#x$totdaysquit <- x$Number_Weeks_Quit * 7
x$totdaysdose <- x$Number_Doses_Week * x$Number_Weeks_Used * 7
#x$totdaysquit <- x$Number_Doses_Week * x$Number_Weeks_Quit * 7

####
train <- as.data.frame(x)

train$cropsoil <- with(train, interaction(Crop_Type,  Soil_Type))
train$cropsoil <- factor(as.numeric(train$cropsoil) - 1)
train$pestseason <- with(train, interaction(Pesticide_Use_Category,  Season))
train$pestseason <- factor(as.numeric(train$pestseason) - 1)
train$Estimated_Insects_Count <- log(train$Estimated_Insects_Count)
train$Number_Doses_Week <- log(train$Number_Doses_Week)
train$Number_Weeks_Used <- log(train$Number_Weeks_Used)
train$Number_Weeks_Quit <- log(train$Number_Weeks_Quit)

####

nm <- c("ID", "Crop_Type","Soil_Type", "Pesticide_Use_Category","Season") 
y[nm] <- lapply(y[nm], as.factor)

#y$totdose <- y$Number_Doses_Week * y$Number_Weeks_Used
#y$totdquit <-y$Number_Doses_Week * y$Number_Weeks_Quit
y$totdaysused <- y$Number_Weeks_Used * 7
#y$totdaysquit <- y$Number_Weeks_Quit * 7
y$totdaysdose <- y$Number_Doses_Week * y$Number_Weeks_Used * 7
#y$totdaysquit <- y$Number_Doses_Week * y$Number_Weeks_Quit * 7

test <- as.data.frame(y)
test$cropsoil <- with(test, interaction(Crop_Type,  Soil_Type))
test$cropsoil <- factor(as.numeric(test$cropsoil) - 1)
test$pestseason <- with(test, interaction(Pesticide_Use_Category,  Season))
test$pestseason <- factor(as.numeric(test$pestseason) - 1)
test$Estimated_Insects_Count <- log(test$Estimated_Insects_Count)
test$Number_Doses_Week <- log(test$Number_Doses_Week)
test$Number_Weeks_Used <- log(test$Number_Weeks_Used)
test$Number_Weeks_Quit <- log(test$Number_Weeks_Quit)

#test <- y[, -c(6,10,11)]

sapply(y, function(z) sum(is.na(z)))
#y <- y[complete.cases(y),]

# Converting to H2o Data frame & splitting
train.hex <- as.h2o(train)
test.hex <- as.h2o(test)


gbm <- h2o.gbm(
  training_frame = train.hex,     ##
  #validation_frame = valid,   ##
  x=c(2,3,4,5,6,7,11,13),                     ##
  y=10,
  ntrees = 300, 
  distribution = "multinomial",## add a few trees (from 20, though default is 50)
  learn_rate = 0.03,           ## increase the learning rate even further
  max_depth = 20,             ## 
  sample_rate = 0.70,          ## use a random 70% of the rows to fit each tree
  col_sample_rate = 0.70,       ## use 70% of the columns to fit each tree
  stopping_rounds = 0,        ## 
  stopping_tolerance = 0.01,  ##
  score_each_iteration = T,   ##
  model_id = "gbm_covType3",  ##
  seed = 2000000)  

p = as.data.frame(h2o.predict(gbm, newdata = test.hex) )
names <- colnames(y)
my_solution <- data.frame(Id = test$ID, p$predict)
colnames(my_solution) <- c('ID', "Crop_Damage")
write.csv(my_solution, file= "submit.csv", row.names = FALSE)

