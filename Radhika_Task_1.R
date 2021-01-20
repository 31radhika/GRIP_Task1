

####### remote file load 
remote_file =  "https://raw.githubusercontent.com/AdiPersonalWorks/Random/master/student_scores%20-%20student_scores.csv"

####### remote file read #######
r_remote_file = read.csv(remote_file)
r_remote_file


print (r_remote_file)
View(r_remote_file)
summary(r_remote_file)

str(r_remote_file)

###### installing packages and libraries #######
install.packages("GGally")
install.packages("ggplot2")
install.packages("tidyverse")
install.packages("reprex")
library("ggplot2")
library("caret")
library("GGally")

 
###### Fitting the model #######
fit_data_model <- lm(Scores ~ Hours, data = r_remote_file)
summary(fit_data_model)


####### Getting the predicted and residual values #######
r_remote_file$predicted <- predict(fit_data_model)   # Save the predicted values
r_remote_file$residuals <- residuals(fit_data_model)

install.packages("dplyr")
library(dplyr)
r_remote_file %>% select(Scores, Hours, predicted, residuals) %>% head()


####### Plotting actual data #######
library(ggplot2)
ggplot(r_remote_file, aes(x = r_remote_file$Scores, y = r_remote_file$Hours)) +  
  geom_point()  # Plotting the actual points


####### Plotting the predicted data #######
ggplot(r_remote_file, aes(x =  r_remote_file$Scores, y =  r_remote_file$Hours)) +
  geom_point() +
  geom_point(aes(y = predicted), shape = 1)  # Adding the predicted values


####### Connecting the actual data to the predicted data #######
ggplot(r_remote_file, aes(x = Scores, y = Hours)) +
  geom_segment(aes(xend = Scores, yend = Hours)) +
  geom_point() +
  geom_point(aes(y = predicted), shape = 1)


####### Plot graph for model residuals #######
ggplot(data=r_remote_file, aes(x = fit_data_model$residual)) +
  geom_histogram(binwidth = 4, color = "blue", fill = "yellow") 


####### Putting the actual data in the linear model #######
plot( r_remote_file$Scores, r_remote_file$Hours, main="Graph between Hours and Scores",xlab=" Scores Variable",ylab="Hours Variable",pch=20)
abline(lm(r_remote_file$Scores ~ r_remote_file$Hours), col="pink")
lines(lowess(r_remote_file$Scores,r_remote_file$Hours), col="red")


####### Calculating the prediction #######
predict(fit_data_model, data.frame(Hours = 9.25))


####### BY #######
####### RADHIKA JOSHI - 31.radhikajoshi@gmail.com #######
####### Data Science & Business Analytics Tasks #######
####### Task 1 #######
