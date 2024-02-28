
#visualisations
#Converting Death Event Variable to Factor
HeartFailure$DEATH_EVENT <- as.factor(HeartFailure$DEATH_EVENT)

# Using ifelse to create a new variable
# HeartFailure_test$DEATH_EVENT <- ifelse(HeartFailure_test$DEATH_EVENT == 1, "Dead", "Live")

#Viewing the structure of the dataset
str(HeartFailure)

library(ggplot2)

#Visualizing therelationship of Age and Platelets with Death Event
ggplot(HeartFailure, aes(x = age, y = platelets)) +
  geom_point(aes(colour = DEATH_EVENT, shape= DEATH_EVENT), size = 3) +
  xlab("Age") +
  ylab("Platelets") +
  ggtitle("Death Event vs Age and Platelets") +
  theme(plot.title = element_text(hjust = 0.5))

#Visualizing therelationship of Age and Time(Follow-up) with Death Event
ggplot(HeartFailure, aes(x = time, y = age)) +
  geom_point(aes(colour = DEATH_EVENT, shape= DEATH_EVENT), size = 3) +
  xlab("Time(Follow-up)") +
  ylab("Age") +
  ggtitle("Death Event vs Age and Time(Follow-up)") +
  theme(plot.title = element_text(hjust = 0.5))


# SVM models

# Q.3A
#Creating First SVM Model with time, ejection_fraction as x variables
svm_model1 <- svm(DEATH_EVENT ~ time + ejection_fraction , data = HeartFailure,type = "C-classification")

#Printing the model and it's summary
print(svm_model1)
summary(svm_model1)

#Plot the model

plot(svm_model1,HeartFailure_train, time ~ ejection_fraction)


#Predict the model
heartpredict1 <- predict(svm_model1,HeartFailure_test)
predicttable1 <- table(heartpredict1, HeartFailure_test$DEATH_EVENT)
predicttable1

#Accuracy prediction
Accuracy1 <- sum(diag(predicttable1))/sum(predicttable1)
accuracy_formatted1 <- sprintf("Accuracy of the SVM model: %.2f%%", Accuracy1 * 100)
print(accuracy_formatted1)

#Creating Second SVM Model with as platelets and creatinine_phosphokinase as x variables
svm_model2 <- svm(DEATH_EVENT ~ platelets + creatinine_phosphokinase, data = HeartFailure_train,type = "C-classification")

#Printing the model and it's summary
print(svm_model2)
summary(svm_model2)

#Plot the model
plot(svm_model2,HeartFailure_train, platelets ~ creatinine_phosphokinase)

#Predict the model
heartpredict2 <- predict(svm_model2,HeartFailure_test)
predicttable2 <- table(heartpredict2, HeartFailure_test$DEATH_EVENT)
predicttable2

#Accuracy for model 2
Accuracy2 <- sum(diag(predicttable2))/sum(predicttable2)
accuracy_formatted2 <- sprintf("Accuracy of the SVM model: %.2f%%", Accuracy2 * 100)
print(accuracy_formatted2)

#Creating Third SVM Model with age, anaemia as x variables
svm_model3 <- svm(DEATH_EVENT ~ age + anaemia, data = HeartFailure,type = "C-classification")

#Printing the model and it's summary
print(svm_model3)
summary(svm_model3)

#Plot the model

plot(svm_model3, HeartFailure_1_,age ~ anaemia)


#Predict the model
heartpredict3 <- predict(svm_model3,HeartFailure_test)
predicttable3 <- table(heartpredict3, HeartFailure_test$DEATH_EVENT)
predicttable3

#Accuracy for model 3
Accuracy3 <- sum(diag(predicttable3))/sum(predicttable3)

# Format the accuracy as a percentage with two decimal places
accuracy_formatted3 <- sprintf("Accuracy of the SVM model: %.2f%%", Accuracy3 * 100)

# Displaying the accuracy of svm model1
#cat(formatted_accuracy, "\n")
print(accuracy_formatted3)


# Q.3B

#Creating first SVM Model with serum_sodium, time, sex, diabetes as x variables
svm_model4 <- svm(DEATH_EVENT ~ serum_sodium + time + sex + diabetes, data = HeartFailure_train,type = "C-classification")

#Printing the model and it's summary
print(svm_model4)
summary(svm_model4)

#Plot the model
plot(svm_model4, HeartFailure_train, serum_sodium ~time ,slice=list(sex=1,diabetes=0))

#Predict the model
heartpredict4 <- predict(svm_model4,HeartFailure_test)
predicttable4 <- table(heartpredict4, HeartFailure_test$DEATH_EVENT)
predicttable4

#Accuracy for model 4
Accuracy4 <- sum(diag(predicttable4))/sum(predicttable4)
accuracy_formatted4 <- sprintf("Accuracy of the SVM model: %.2f%%", Accuracy4 * 100)
print(accuracy_formatted4)


#Creating second SVM Model with age, anaemia,creatinine_phosphokinase, high_blood_pressure, serum_creatinine as x variables
svm_model5 <- svm(DEATH_EVENT ~ age + anaemia + creatinine_phosphokinase + high_blood_pressure + serum_creatinine, data = HeartFailure_train,type="C-classification")

#Printing the model and it's summary
print(svm_model5)
summary(svm_model5)

#Plot the model
plot(svm_model5, HeartFailure_train, creatinine_phosphokinase ~ serum_creatinine , slice = list(age=50, high_blood_pressure=1,anaemia=1))


#Predict the model
heartpredict5 <- predict(svm_model5,HeartFailure_test)
predicttable5 <- table(heartpredict5, HeartFailure_test$DEATH_EVENT)
predicttable5

#Accuracy for model 5
Accuracy5 <- sum(diag(predicttable6))/sum(predicttable6)
accuracy_formatted5 <- sprintf("Accuracy of the SVM model: %.2f%%", Accuracy6 * 100)
print(accuracy_formatted5)
