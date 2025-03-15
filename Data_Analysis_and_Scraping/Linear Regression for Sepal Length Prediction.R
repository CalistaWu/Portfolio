#Q1
Training_dt <- rbind( iris[1:40, ], iris[51:90, ], iris[101:140, ])
Testing_dt <- rbind( iris[41:50, ], iris[91:100, ], iris[141:150, ])

#Q2
model <- lm(formula = Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, data = Training_dt)
summary(model)

prediction <- c()
ground_truth <- c()

#Prediction
for(i in 1:30){
  result <- predict( model, data.frame(Sepal.Width = Testing_dt$Sepal.Width[i],
                                       Petal.Length = Testing_dt$Petal.Length[i],
                                       Petal.Width = Testing_dt$Petal.Width[i]))
  cat("[",i,"]","Actual length of sepal = ",Testing_dt$Sepal.Length[i],
      ", ", "Predicted length of sepal = ", result, "\n", sep = "")
  prediction <- append( prediction, result)
  ground_truth <- append( ground_truth,Testing_dt$Sepal.Length[i])
}

#Q3 rms
x <- prediction - ground_truth 
rms <- sqrt( sum(x^2)/length(x) )
print(rms)
