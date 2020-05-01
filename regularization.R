lamdas=seq(0,25,1)
error=sapply(lamdas,function(i){
  m=mean(edx$rating)
  
  movie=edx%>%group_by(movieId)%>%
    summarise(m_i=sum(rating-m)/(n()+i))
  
  
  user=edx%>%
    left_join(movie,by="movieId")%>%
    group_by(userId)%>%
    summarise(u_i=sum(rating-m-m_i)/(n()+i))
  
  
  year=edx%>%
    left_join(movie, by="movieId")%>%
    left_join(user,by="userId")%>%
    group_by(releaseyear)%>%
    summarise(y_i=sum(rating-m-m_i-u_i)/(n()))
  
  
  genre=edx%>%
    left_join(movie, by="movieId")%>%
    left_join(user,by="userId")%>%
    left_join(year,by="releaseyear")%>%
    group_by(genres)%>%
    summarise(g_i=sum(rating-m-m_i-u_i-y_i)/(n()))
  
  
  yearsafter=edx%>%
    left_join(movie, by="movieId")%>%
    left_join(user,by="userId")%>%
    left_join(year,by="releaseyear")%>%
    left_join(genre,by="genres")%>%
    group_by(ratedafter)%>%
    summarise(ya_i=sum(rating-m-m_i-u_i-y_i-g_i)/(n()))
  pred_regularize=validation%>%
    left_join(movie,by="movieId")%>%
    left_join(user, by="userId")%>%
    left_join(year,by="releaseyear")%>%
    left_join(genre,by="genres")%>%
    left_join(yearsafter, by="ratedafter")%>%
    mutate(prediction=m+m_i+u_i+y_i+g_i+ya_i)
  return(RMSE(test$rating,pred_regularize$prediction))
  
})

qplot(lamdas,error)
l=lamdas[which.min(error)]

movie=edx%>%group_by(movieId)%>%
  summarise(m_i=sum(rating-m)/(n()+l),n_m=n())
user=edx%>%
  left_join(movie,by="movieId")%>%
  group_by(userId)%>%
  summarise(u_i=sum(rating-m-m_i)/(n()+l),n_u=n())


year=edx%>%
  left_join(movie, by="movieId")%>%
  left_join(user,by="userId")%>%
  group_by(releaseyear)%>%
  summarise(y_i=sum(rating-m-m_i-u_i)/(n()+l),n_y=n())


genre=edx%>%
  left_join(movie, by="movieId")%>%
  left_join(user,by="userId")%>%
  left_join(year,by="releaseyear")%>%
  group_by(genres)%>%
  summarise(g_i=sum(rating-m-m_i-u_i-y_i)/(n()+l),n_g=n())

yearsafter=edx%>%
  left_join(movie, by="movieId")%>%
  left_join(user,by="userId")%>%
  left_join(year,by="releaseyear")%>%
  left_join(genre,by="genres")%>%
  group_by(ratedafter)%>%
  summarise(ya_i=sum(rating-m-m_i-u_i-y_i-g_i)/(n()+l),n_ya=n())

pred_regularize=validation%>%
  left_join(movie,by="movieId")%>%
  left_join(user, by="userId")%>%
  left_join(year,by="releaseyear")%>%
  left_join(genre,by="genres")%>%
  left_join(yearsafter, by="ratedafter")%>%
  mutate(prediction=m+m_i+u_i+y_i+g_i+ya_i)%>%
  .$prediction
model_6=RMSE(test$rating,pred_regularize)







