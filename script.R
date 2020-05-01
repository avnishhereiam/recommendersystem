if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
library(wordcloud)

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

releaseyear <- stringi::stri_extract(edx$title, regex = "(\\d{4})", comments = TRUE) %>% 
  as.numeric()

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")


set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

# Rating Distribution
edx %>% group_by(rating) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

edx$timestamp=as.POSIXct(edx$timestamp, origin="1970-01-01")
edx$timestamp=format(as.Date(edx$timestamp, format="%d/%m/%Y"),"%Y")

release = str_sub(edx$title,start = -5,end = -2)
edx=edx%>%mutate(releaseyear=release)
rated=as.numeric(edx$timestamp)-as.numeric(edx$releaseyear)
edx=edx%>%mutate(ratedafter=rated)

# No.of users and movies
edx%>%summarize(users=length(unique(userId)),movies=length(unique(movieId)))

# Distribution of Rating
edx%>%ggplot(aes(rating))+geom_bar()

# Distribution of users
edx%>%count(userId)%>%
 ggplot(aes(n))+geom_histogram(bins  = 30,color="red")+
  scale_x_log10()+ggtitle("Distribution of rating providing users")

# Distribution of no of times movie has been rated
edx%>%count(movieId,title)%>%
  ggplot(aes(n))+geom_histogram(bins=30,color="red")+
  scale_x_log10()+ggtitle("Distribution of movie views")

# Top 25 viewd movies
head(edx%>%count(movieId,title)%>%arrange(desc(n))%>%select(title),25)

# how average rating of movies varies for different movies
edx%>%group_by(movieId)%>%summarise(mean=mean(rating))%>%ggplot(aes(mean))+geom_histogram(bins=20,color="red")

# how average rating of movies varies for different users
edx%>%group_by(userId)%>%summarise(mean=mean(rating))%>%
  ggplot(aes(mean))+geom_histogram(bins=20,color="red")

# how average rating of movies varies for different genres
edx%>%group_by(genres)%>%summarise(mean=mean(rating))%>%
  arrange(desc(mean))%>%
  ggplot(aes(mean))+geom_histogram(bins=20,color="red")

# Top 25 rated genres
head(edx%>%group_by(genres)%>%summarise(mean=mean(rating))%>%
       arrange(desc(mean)),25)


# Relation between released year and average rating
edx%>%group_by(releaseyear)%>%summarise(mean=mean(rating))%>%
  ggplot(aes(releaseyear,mean))+geom_point()+geom_smooth()+theme(axis.text.x = element_text(angle =90))

# Relation between rated years after and average rating
edx%>%group_by(ratedafter)%>%summarise(mean=mean(rating))%>%
  ggplot(aes(ratedafter,mean))+geom_point()+geom_smooth()

#correlation factor 'year release' 
edx%>%group_by(releaseyear)%>%summarise(mean=mean(rating))%>%
  summarize(cor(mean,as.numeric(releaseyear)))

#correlation factor 'rated year after'
edx%>%group_by(ratedafter)%>%summarise(mean=mean(rating))%>%
  summarize(cor(mean,ratedafter))

#Validation data set
validation$timestamp=as.POSIXct(validation$timestamp, origin="1970-01-01")
validation$timestamp=format(as.Date(validation$timestamp, format="%d/%m/%Y"),"%Y")

releasev = str_sub(validation$title,start = -5,end = -2)
validation=validation%>%mutate(releaseyear=releasev)
ratedv=as.numeric(validation$timestamp)-as.numeric(validation$releaseyear)
validation=validation%>%mutate(ratedafter=ratedv)

test=validation
validation=validation%>%select(-rating)

RMSE=function(actual,prediction){
  sqrt(mean((actual-prediction)^2,na.rm = T))
}


Result=data_frame()

# Simple Model predicting mean 
m=mean(edx$rating)
avg_model=RMSE(test$rating,m)

#Accounting movie effect : 
movie=edx%>%group_by(movieId)%>%
  summarise(m_i=mean(rating-m))

#Accouting user effect :
user=edx%>%
  left_join(movie,by="movieId")%>%
  group_by(userId)%>%
  summarise(u_i=mean(rating-m-m_i))

#Accounting release year effect :
year=edx%>%
  left_join(movie, by="movieId")%>%
  left_join(user,by="userId")%>%
  group_by(releaseyear)%>%
  summarise(y_i=mean(rating-m-m_i-u_i))

#Accounting genre effect :
genre=edx%>%
  left_join(movie, by="movieId")%>%
  left_join(user,by="userId")%>%
  left_join(year,by="releaseyear")%>%
  group_by(genres)%>%
  summarise(g_i=mean(rating-m-m_i-u_i-y_i))

#Accounting years after rated effect :
yearsafter=edx%>%
  left_join(movie, by="movieId")%>%
  left_join(user,by="userId")%>%
  left_join(year,by="releaseyear")%>%
  left_join(genre,by="genres")%>%
  group_by(ratedafter)%>%
  summarise(ya_i=mean(rating-m-m_i-u_i-y_i-g_i))

# Model 1: Movie effect
pred_by_m=validation%>%
  left_join(movie,by="movieId")%>%
  mutate(prediction=m+m_i)
model_1=RMSE(test$rating,pred_by_m$prediction)

# Model2: Movie & User effect
pred_by_m_u=validation%>%
  left_join(movie,by="movieId")%>%
  left_join(user, by="userId")%>%
  mutate(prediction=m+m_i+u_i)
model_2=RMSE(test$rating,pred_by_m_u$prediction)

# Model3: Movie, User and year effect
pred_by_m_u_y=validation%>%
  left_join(movie,by="movieId")%>%
  left_join(user, by="userId")%>%
  left_join(year,by="releaseyear")%>%
  mutate(prediction=m+m_i+u_i+y_i)
model_3=RMSE(test$rating,pred_by_m_u_y$prediction)

# Model4: Movie, User, year and genre effect 
pred_by_m_u_y_g=validation%>%
  left_join(movie,by="movieId")%>%
  left_join(user, by="userId")%>%
  left_join(year,by="releaseyear")%>%
  left_join(genre,by="genres")%>%
  mutate(prediction=m+m_i+u_i+y_i+g_i)
model_4=RMSE(test$rating,pred_by_m_u_y_g$prediction)

# Model5: Movie, User, year, genre and year after rated effect 
pred_by_m_u_y_g_ya=validation%>%
  left_join(movie,by="movieId")%>%
  left_join(user, by="userId")%>%
  left_join(year,by="releaseyear")%>%
  left_join(genre,by="genres")%>%
  left_join(yearsafter, by="ratedafter")%>%
  mutate(prediction=m+m_i+u_i+y_i+g_i+ya_i)
model_5=RMSE(test$rating,pred_by_m_u_y_g_ya$prediction)