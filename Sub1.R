#### GHOST COMPETITION

## FIRST

library(tidyverse)
library(vroom)
library(caret)
library(DataExplorer)

Ghost.train <- read_csv("~/Downloads/GhostTrain.csv")
Ghost.test <- read_csv("~/Downloads/GhostTest.csv")
Ghost <- bind_rows(train=Ghost.train, test=Ghost.test)

tc <- trainControl(method = 'repeatedcv',
                   number = 5,
                   repeats = 3)

Grid <- expand.grid(k=1:3)

#Ghost.train <- Ghost.train[!is.na(Ghost.train)]
Ghost.train$color <- as.factor(Ghost.train$color)
Ghost.train$type <- as.factor(Ghost.train$type)

Ghost.train <- Ghost.train %>% select(-id)

# All: 

knn.model <- train(
  type ~ bone_length + rotting_flesh + hair_length + has_soul + color,
  data = Ghost.train,
  method = 'knn',
  metric = 'Accuracy',
  #tuneLength = Grid,
  tuneLength = 100,
  trControl = tc
)

treebag.model <- train(
  type ~ bone_length + rotting_flesh + hair_length + has_soul + color,
  data = Ghost.train,
  method = 'treebag',
  metric = 'Accuracy',
  #tuneLength = Grid,
  #tuneLength = 500,
  trControl = tc
)

preds <- predict(knn.model, Ghost.test)

probs <- predict(knn.model, Ghost, type='prob')
probs2 <- predict(knn.model, Ghost.test)
probs3 <- predict(treebag.model, Ghost.test)

my_probs <- data_frame('id' = Ghost$id, 'prob' = probs)
my_probs2 <- data_frame('id' = Ghost.test$id, 'prob' = probs2)
my_probs3 <- data_frame('id' = Ghost.test$id, 'prob' = probs3)

write.table(my_probs, file = "Probs_KNN.csv", sep = ",", row.names = FALSE, col.names = c("ID", "PrGhost_Knn", "PrGhoul_Knn", "PrGoblin_Knn"))
write.table(my_probs2, file = "Probs_2.csv", sep = ",", row.names = FALSE, col.names = c("id", "type"))
write.table(my_probs3, file = "Probs_3.csv", sep = ",", row.names = FALSE, col.names = c("id", "type"))



### 0.66918

### Treebag: 0.7183





