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

df$music_genre <- replace(df$music_genre,df$music_genre =='', NA)
df$key <- replace(df$key,df$key =='', NA)
df$mode <- replace(df$mode,df$mode =='', NA)

df$tempo <- as.numeric(df$tempo)
df$music_genre <-factor(df$music_genre)
df$key <- factor(df$key)
df$mode <- factor(df$mode)

df <-
  df %>%
  mutate_if(is.numeric, ~replace(., . == -1, NA))
df <-
  df %>%
  mutate_if(is.numeric, ~replace(., . == 0, NA))
df <-
  df %>%
  mutate_if(is.factor, ~replace(., . == "empty_field", NA))
df <-
  df %>%
  mutate_if(is.factor, ~replace(., . == "", NA))


# replace ? in tempo

df$tempo <- replace(df$tempo,df$tempo =='?', NA)



# bucketing continous variables
# equal interval vs equal frequency
df2 <- df

df1 <-na.omit(df)
df1 <- subset(df1, select = -c(artist_name, track_name,obtained_date, instance_id))
splitpoint <- sample(nrow(df1), nrow(df1) * 0.8)
train <- df1[splitpoint, ]
test <- df1[-splitpoint, ]
#str(df1)


target = music_genre ~ loudness + popularity + speechiness + instrumentalness + acousticness 
# genre
tree1 <- rpart(target
             , data = train
             , method = 'class'
             ,control= rpart.control(minsplit=4
                                     ,minbucket=7, maxdepth=10,
                                     usesurrogate=2))
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