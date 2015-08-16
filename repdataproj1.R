require("dplyr")
require("ggplot2")
require("scales")


## Open activity file and store in table
activ <- read.csv(".//activity.csv")

## Determine total steps per day calculation

aggSum <- activ %>% group_by(date) %>% summarize(totSteps = sum(steps, na.rm = FALSE))

## Create hisogram of total steps per day

ggplot(data=aggSum, aes(totSteps)) + geom_histogram()

## Calculate and report the mean and median number of steps per day

meanSteps <- mean(aggSum$totSteps, na.rm = TRUE)
medianSteps <- median(aggSum$totSteps, na.rm = TRUE)


## Plot time series graph of the 5-minute intervals and the average number of steps taken

intAvg <- activ %>% group_by(interval) %>% summarize(meanSteps = mean(steps, na.rm = TRUE))

ggplot(intAvg, aes(interval, meanSteps)) + geom_line() +
    xlab("5-Second Time Interval") + 
    ylab("Average Steps Taken") + 
    ggtitle("Time Series - Steps Taken")

filter(intAvg, meanSteps==max(meanSteps))

## Calculate the total number of rows where there is an NA in the row

enAyRows <- sum(is.na(activ))

## Fill in NA values with average of daily number of steps

activImp <- inner_join(activ, intAvg, by="interval") %>% 
    mutate(steps=ifelse(is.na(steps),meanSteps,steps)) %>%
    select(date,interval,steps)

aggSumImp <- activImp %>% group_by(date) %>% summarize(totStepsImp = sum(steps, na.rm = FALSE))

ggplot(data=aggSumImp, aes(totStepsImp)) + geom_histogram()

meanStepsImp <- mean(aggSumImp$totStepsImp, na.rm = TRUE)
medianStepsImp <- median(aggSumImp$totStepsImp, na.rm = TRUE)

activImp <- activImp %>%
    mutate(weekend.indicator =
               as.factor(ifelse(weekdays(as.Date(date)) %in% c("Saturday","Sunday"),
                                "weekend","weekday")))

intAvgImpWk <- activImp %>% group_by(interval) %>% summarize(meanStepsWeekend = mean(steps, na.rm = TRUE))

wk_df <- aggregate(steps ~ weekend.indicator+interval, data=activImp, FUN=mean)

library(lattice)
xyplot(steps ~ interval | factor(weekend.indicator),
       layout = c(1, 2),
       xlab="Interval",
       ylab="Number of steps",
       type="l",
       lty=1,
       data=wk_df)

