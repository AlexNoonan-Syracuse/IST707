library(ROSE)
library(dplyr)
library(rpart)
library(rpart.plot)
library(reshape2)
library(ggplot2)
library(scales)
library(caret)

df <- read.csv("C://Users//Alex//Desktop//College//Fall 2021//Data Mining//IST707//music_genre.csv")
columns <- colnames(df)
# preprocessing for decision tree

#random imputation

df$music_genre <- replace(df$music_genre,df$music_genre =='', NA)
df$key <- replace(df$key,df$key =='', NA)
df$mode <- replace(df$mode,df$mode =='', NA)

df$tempo <- replace(df$tempo,df$tempo =='?', NA)
df$tempo <- as.numeric(df$tempo)
df$music_genre <-factor(df$music_genre)
df$key <- factor(df$key)
df$mode <- factor(df$mode)

randomImputation <- function (df)
{ 
  
  df_fact <-df %>% select_if(is.factor)
  for(a in colnames(df_fact)){
    missing <- is.na(df_fact[a])
    n.missing <- sum(missing)
    a.obs <- df_fact[a][!missing]
    imputed <- df_fact[a]
    imputed[missing] <- sample(a.obs, n.missing, replace=TRUE)
    df_fact[a]<- imputed
  }
  df_num <- df %>% select_if(is.numeric)
  for(b in colnames(df_num)){
    missing <- is.na(df_num[b])
    n.missing <- nrow(df_num) - length(df_num[b][!missing])
    b.obs <- df_num[b][!missing]
    imputed <- df_num[b]
    min(b.obs)
    max(b.obs)
    c(min(b.obs):max(b.obs))
    imputed[missing] <- sample(x = min(b.obs):max(b.obs), size = n.missing,replace = TRUE)
    df_num[b]<- imputed
    
  }
  df <- cbind(df_fact,df_num)
  return (df)
}

df <- randomImputation(df)

sum(is.na(df))




# accuracy increased from 44 to 53 when splitting to deciles
# splitting loudness, popularity, and speechiness into percentiles

# bucketing continous variables
df$tempo <- ntile(df$tempo, 100)

df$loudness <- ntile(df$loudness, 100)

df$liveness <- ntile(df$liveness, 100)

df$instrumentalness <- ntile(df$instrumentalness, 100)

df$speechiness <- ntile(df$speechiness, 100)

df$valence <- ntile(df$valence, 100)

df$energy <- ntile(df$energy, 100)

df$duration_ms <- ntile(df$duration_ms, 100)

df$acousticness <- ntile(df$acousticness ,100)

df$danceability <- ntile(df$danceability, 100) 


df1 <-na.omit(df)


df1 <- subset(df1, select = -c(instance_id))

df2 <- df1

df1$popularity <- df1$popularity/ min(df1$popularity)
df1$popularity <- log(df1$popularity)
df1$popularity <- ntile(df$popularity, 100)

splitpoint <- sample(nrow(df1), nrow(df1) * 0.8)
train <- df1[splitpoint, ]
test <- df1[-splitpoint, ]
#str(df1)


target = music_genre ~ loudness + popularity + speechiness + instrumentalness + acousticness 
# genre
tree1 <- rpart(music_genre~.
             , data = train
             , method = 'class'
             ,control= rpart.control(minsplit=10
                                     ,minbucket = 5, maxdepth=10
                                     ))
rpart.plot(tree1,extra= 106)

Predictions <- predict(tree1,newdata =  test,type="class")

confusionMatrix(Predictions, test$music_genre)

# heatmap plot 

x1 <-table(test$music_genre,Predictions)

c2= melt(x1)
c2 <- rename(c2, Actual= Var1)


c3 <- ggplot(c2, aes(Actual,Predictions)) + geom_tile(aes(fill=value))
c3 <- c3 + scale_fill_gradient2() +geom_text(aes(fill=value, label=value))

c3

# it appears rock is a difficult genre for the model to classify
# most likely due to the similiarity in rock music to the other genres
# and the tendency for musicians to borrow elements from each other
# model struggles to get over 55% accuracy

# decision tree for popularity

#again using the deciles but 10 for popularity

#https://stats.stackexchange.com/questions/385231/why-log-transform-to-normal-distribution-for-decision-trees
minpop <-min(df2$popularity)
df2$popularity <- df2$popularity/minpop
df2$popularity <- log(df2$popularity)
df2$popularity <- ntile(df2$popularity,10)
df2$popularity <- as.factor(df2$popularity)


splitpoint <- sample(nrow(df2), nrow(df2) * 0.8)
train2 <- df2[splitpoint, ]
test2 <- df2[-splitpoint, ]


#str(df1)



# popularity decision tree 
tree2 <- rpart(popularity~.
               , data = train2
               , method = 'class'
               ,control= rpart.control(minsplit=4
                                       ,minbucket = 5, maxdepth=10
               ))
rpart.plot(tree2,extra= 106)

Predictions <- predict(tree2,newdata =  test2,type="class")

confusionMatrix(Predictions2, test2$popularity)

# heatmap plot 


x2 <-table(test2$popularity,Predictions2)

c4= melt(x2)
c4 <- rename(c4, Actual= Var1, Predicted = Predictions2)


c5 <- ggplot(c4, aes(Actual,Predicted)) + geom_tile(aes(fill=value))
c5 <- c5 + scale_fill_gradient2() +geom_text(aes(fill=value, label=value))

c5
# super accurate > 90%
# super accurate > 90%
