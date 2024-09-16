library(readr)

data <- read_csv("appendix.csv")

# Remove rows where the Institution column is 'MITx'
filtered_data <- data[data$Institution != "MITx", ]

filtered_data$`% Played Video` <- as.numeric(gsub("[^0-9\\.]", "", filtered_data$`% Played Video`))

cleaned_data <- na.omit(filtered_data)

# Write the filtered data to a new CSV file
write_csv(cleaned_data, "Harvard.csv")


data1<-read_csv("Harvard.csv")
harvard<-data1[,-c(1,2,3,4,5,6,7,8)]

set.seed(123)
sample<-sample(c(TRUE,FALSE),nrow(harvard),replace=T,prob=c(0.8,0.2))
train<-harvard[sample,]
test<-harvard[!sample,]
head(train)

#set model
model1 <- lm(`Participants (Course Content Accessed)` ~ ., data = train)
summary(model1)


##question1
model<-lm(`Participants (Course Content Accessed)` ~`Audited (> 50% Course Content Accessed)`,data=train)
summary(model)

par(mfrow=c(2,2)) 
plot(fitted(model), resid(model), axes=TRUE, frame.plot=TRUE, xlab='fitted values', ylab='residue')
plot(train$`Participants (Course Content Accessed)`, resid(model), axes=TRUE, frame.plot=TRUE, xlab='Participants (Course Content Accessed)', ylab='residue')
plot(train$`Audited (> 50% Course Content Accessed)`, resid(model), axes=TRUE, frame.plot=TRUE, xlab='Audited (> 50% Course Content Accessed)', ylab='residue')
hist(resid(model))

#oulier test
outlierTest(model)
#influential test
cd <- cooks.distance(model)
plot(model, which = 4)

cutoff = 4/nrow(train)
abline(h=cutoff,lty=2)
train[cd > cutoff, ]


#multiple linear regression model
model2<- lm(`Participants (Course Content Accessed)`~ `Audited (> 50% Course Content Accessed)`
            +Certified+`% Audited`+`Total Course Hours (Thousands)`,data=train)
summary(model2)

#oulier test
residuals <- resid(model2)

z_scores <- scale(residuals)

threshold <- 0.5

outliers <- which(abs(z_scores) > threshold)

data2 <- train[-outliers,]

##multiple linear regression model without outliers
model3<-lm(`Participants (Course Content Accessed)`~ `Audited (> 50% Course Content Accessed)`
           +Certified+`% Audited`+`Total Course Hours (Thousands)`,data=data2)
summary(model3)

#influential test
cd <- cooks.distance(model3)
plot(model3, which = 4)

cutoff = 4/nrow(data2)
abline(h=cutoff,lty=2)
data2[cd > cutoff, ]

##question 2
qf(0.95,df1=4, df2=97)

#plot
par(mfrow=c(2,2)) 
qqplot(data2$`Audited (> 50% Course Content Accessed)`,data2$`Audited (> 50% Course Content Accessed)`)
qqplot(data2$`Audited (> 50% Course Content Accessed)`,data2$Certified)
qqplot(data2$`Audited (> 50% Course Content Accessed)`,data2$`% Audited`)
qqplot(data2$`Audited (> 50% Course Content Accessed)`,data2$`Total Course Hours (Thousands)`)


#question3 
anova(model3)
confint(model3, level=0.95)
