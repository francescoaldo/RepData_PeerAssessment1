setwd("/Users/francescoaldo/Desktop/COURSES MATERIAL/R DATA SCIENCE - Coursera/c5_w2_ProgAss/RepData_PeerAssessment1") 
library(dplyr) 
library(ggplot2) 

rawData <- read.csv("activity.csv", sep = ",") 


total_day <- stepsData %>% 
             group_by(date) %>% 
             summarise(total_steps = sum(steps, na.rm = TRUE), 
                       na = mean(is.na(steps))) %>% 
             print 


barplot(height = total_day$total_steps,names.arg=total_day$date,cex.names=0.68,las=3,col="orange")
abline(h=median(total_day$total_steps), lty=2,lwd=3, col="black")
abline(h=mean(total_day$total_steps), lty=2,lwd=3, col="red")
text(x = 0,y=median(total_day$total_steps),pos=3,labels = "median")
text(x = 0,y=mean(total_day$total_steps),pos=1,labels = "mean",col="red")


barplot(height = dailyTotal$dailySteps, names.arg = dailyTotal$date, 
        cex.names = 0.5, las = 3, col = "yellow") 
abline(h = median(dailyTotal$dailySteps), lty = 1, lwd = 2.75, col = "darkred") 
abline(h = mean(dailyTotal$dailySteps), lty = 2, lwd = 3.2, col = "darkgreen") 
text(x = 1.5, y = median(dailyTotal$dailySteps), 
     pos = 3, labels = "median", col = "darkred") 
text(x = 1, y = mean(dailyTotal$dailySteps), 
     pos = 1, labels = "mean", col = "darkgreen") 

total_day <- filter(total_day, na < 1) 
hist(total_day$total_steps,col="orange",breaks=20,main="Total steps per day",xlab="Steps per day")
abline(v=median(total_day$total_steps),lty=3, lwd=2, col="black")
legend(legend="median","topright",lty=3,lwd=2,bty = "n") 


hist(dailyTotal$dailySteps, col = "lightblue", breaks = 20, 
     main = "Total steps per day", xlab = "Steps per day") 
abline(v = median(dailyTotal$dailySteps, na.rm = TRUE), 
       lty = 1, lwd = 2.5, col = "darkorange2") 
legend(legend = "median", "topright", lty = 1, lwd = 2.25, 
       bty = "n", col = "darkorange2") 

dailyTotal <- filter(dailyTotal, na != 1) 

kdDailySteps <- kdensity(dailyTotal$dailySteps, start = "gumbel", 
                         kernel = "gaussian", na.rm = TRUE) 
xSeq <- seq(0, max(dailyTotal$dailySteps), length = 20) 
NormalPDF <- dnorm(xSeq, mean = mean(dailyTotal$dailySteps, na.rm = TRUE), 
                   sd = sd(dailyTotal$dailySteps, na.rm = TRUE)) 

hist(dailyTotal$dailySteps, col = "lightblue", breaks = 20, freq = FALSE, 
     main = "Total steps per day", xlab = "Steps per day") 
abline(v = mean(dailyTotal$dailySteps, na.rm = TRUE), 
       lty = 1, lwd = 2.5, col = "darkorange1") 
legend(legend = "median", "topright", lty = 1, lwd = 2.25, 
       bty = "n", col = "darkorange2") 
lines(xSeq, NormalPDF, lty = 2, col = "red") 
lines(kdDailySteps, lty= 1, col = "darkgreen") 

paste0("The mean of the total number of daily steps is ", 
       round(mean(dailyTotal$dailySteps), digits = 2)) 
paste0("The median of the total number of daily steps is ", 
       round(median(dailyTotal$dailySteps), digits = 2)) 

library(dplyr,quietly = TRUE) 
daily_patterns <- rawData %>% 
                  group_by(interval) %>% 
                  summarise(average=mean(steps,na.rm=TRUE)) 
plot(x = 1:nrow(daily_patterns),y = daily_patterns$average,type = "l", 
     col = "red", xaxt = "n",xlab="Intervals", 
     ylab = "Average for given interval across all days") 
axis(1, labels=daily_patterns$interval[seq(1,288,12)], 
     at = seq_along(daily_patterns$interval)[seq(1,288,12)]) 

stepsTS <- rawData %>% 
    group_by(interval) %>% 
    summarise(average = mean(steps, na.rm = TRUE)) 

stepsTSplot <- ggplot(stepsTS, aes(x = interval, y = average)) 
stepsTSplot <- stepsTSplot  + 
               geom_line(color = "red") + 
               xlab("Interval") + 
               ylab("Average number of steps") + 
               ggtitle("Average steps for given interval across all days") + 
               theme_light() + 
               theme(plot.title = element_text(size = 12, hjust = 0.5)) 
stepsTSplot 


paste0("The 5-minute interval with the highest n. of steps on average is the ", 
       stepsTS[stepsTS$average == max(stepsTS$average), 1], "th interval.") 

missingN <- sum(is.na(rawData$steps)) ; missingN 
missingPct <- paste0(round(mean(is.na(rawData$steps)), 
                           digits = 4)*100, "%") 
missingPct 

stepsData2 <- stepsData 
stepsData2 <- mutate(stepsData2, weekday = wday(stepsData2$date, label = TRUE), 
                     IntWdMean = )

naSteps <- rawData[is.na(rawData$steps), ] 


rawData2 <- rawData %>% 
            mutate(weekday = wday(rawData$date, label = TRUE)) %>% 
            group_by(interval, weekday) 

rawData3 <- rawData %>% 
            mutate(weekday = wday(rawData$date, label = TRUE)) %>% 
            group_by(interval, weekday) %>% 
            mutate(steps = ifelse(is.na(steps), mean(steps, na.rm = TRUE), steps), 
                   IntWdMean = mean(steps, na.rm = TRUE)) 

stepsTS <- stepsData %>% 
    group_by(interval) %>% 
    summarise(average = mean(steps, na.rm = TRUE)) 

dayCatTS <- naImputing %>% 
            mutate(TypeDay = ifelse(weekday %in% c("Sab", "Dom"), 
                                    "Weekend", "Weekday")) %>% 
            mutate(TypeDay = as.factor(TypeDay)) %>% 
            group_by(interval, TypeDay) %>% 
            summarise(average = mean(steps, na.rm = TRUE)) 

library(lattice) 
xyplot(average ~ interval | TypeDay, data = dayCatTS, 
       type = "l", col = "red",  layout = c(1,2), 
       main = "Average steps per 5-minute interval, weekdays vs weekend", 
       ylab = "Average number of steps", xlab = "Interval") 








