install.packages("recosystem")
library(recosystem)

#Data prepration
edx_factor=edx%>%select(movieId,userId,rating)
edx_factor=as.matrix(edx_factor)
write.table(edx_factor,file = "trainset.txt",sep = " ",row.names = FALSE,col.names = FALSE)
trainset=data_file("trainset.txt")



# Recosystem object creation
r=Reco()

#Training
r$train(trainset, opts = list(dim = 30,nthread = 1,niter=20))
stored_prediction = tempfile()


validation_factor=validation%>%select(movieId,userId)
validation_factor=as.matrix(validation_factor)
write.table(validation_factor,file = "testset.txt",sep = " ",row.names = FALSE,col.names = FALSE)
testset=data_file("testset.txt")
r$predict(testset,out_file(stored_prediction))
prediction=scan(stored_prediction)

RMSE(test$rating,prediction)
