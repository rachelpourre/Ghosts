library(tidyverse)
library(vroom)
library(caret)
library(DataExplorer)

### Predictions from 7 different models


Ghost.train <- read_csv("~/Downloads/GhostTrain.csv")
Ghost.test <- read_csv("~/Downloads/GhostTest.csv")
Ghost <- bind_rows(train=Ghost.train, test=Ghost.test, .id = "tt")


ClassTree <- read_csv("~/Downloads/ClassTree.csv")
MultiLayerPerceptron <- read_csv("~/Downloads/Multilayerperceptron.csv")
GBM <- read_csv("~/Downloads/GBM.csv")
SVM <- read_csv("~/Downloads/SVM.csv")
KNN <- read_csv("~/Downloads/KNN.csv")
XgbTree <- read_csv("~/Downloads/XgbTree.csv")
ACC <- read_csv("~/Downloads/ACC.csv")
LogReg <- read_csv("~/Downloads/LogRegPreds.csv")


colnames(MultiLayerPerceptron)[1] <- "ID"
colnames(LogReg)[1] <- "ID"
ACC <- ACC[,c(4,3,2,1)]
colnames(ACC)[1] <- "ID"
colnames(Ghost)[2] <- "ID"
Ghost <- Ghost[,c(2,1,8)]

# ClassTree, MultiLayerPerceptron, GBM, SVM, KNN, XgbTree, ACC

all_models <- left_join(ClassTree, MultiLayerPerceptron, by = "ID") %>%
  left_join(., GBM, by = "ID") %>%
  left_join(., SVM) %>%
  left_join(., KNN) %>%
  left_join(., XgbTree) %>%
  left_join(., ACC) %>%
  left_join(., LogReg) %>%
  left_join(., Ghost)

tmp <- all_models[,-c(26,27)]

preds <- predict(preProcess(x = all_models, method = c("pca")), tmp)

#trainset <- all_models %>% filter(!is.na(all_models$type))
trainset <- na.omit(all_models)
trainset <- trainset[,-c(26)]

tc <- trainControl(method = 'repeatedcv',
                   number = 15,
                   repeats = 13)



treebag.stack.model <- train(
  type ~ .,
  data = trainset,
  method = 'bayesglm',
  metric = 'Accuracy',
  #tuneLength = Grid,
  #tuneLength = 500,
  trControl = tc
)

### Same results for different methods

preds <- predict(treebag.model, all_models %>% filter (tt == "test"))

my_sub <- data_frame('id' = Ghost.test$id, 'type' = preds)

write.table(my_sub, file = "Sub2.csv", sep = ",", row.names = FALSE, col.names = c("id", "type"))


### 0.70699
