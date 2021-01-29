library(caret)

Test_Set = read.csv(file.choose())


sub <- createDataPartition(Test_Set$Dep, p=0.80, list=FALSE)

Train  = Test_Set[sub,]

Test = Test_Set[-sub,]

Model = glm(Dep ~., data = Train)

Predictions = predict(Model, Test[, 1:3])

Cutoff(Predictions, Test, 50, 30)