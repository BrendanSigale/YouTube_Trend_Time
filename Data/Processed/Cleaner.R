library(dplyr)

df <- read.csv("USvideos_working.csv")

names(df) <- c("video_id", "trending_date", "publish_date", "title", "channel_title", "category_id", "views", "likes", "dislikes", "comment_count")

df$video_id <- NULL
df$trending_date <- as.Date(df$trending_date)
df$publish_date <- as.Date(df$publish_date)
df$category_id <- as.factor(df$category_id)

dftmp <- df
dftmp <- group_by(dftmp, title, channel_title, category_id, publish_date)
dftmp <- summarize(dftmp, 
                   total_trend_time = max(trending_date) - min(trending_date) + 1,
                   time_to_trend = min(min(trending_date) - min(publish_date)),
                   days_spent_trending = n(),
                   views_fd = min(views),
                   likes_fd = min(likes),
                   dislikes_fd = min(dislikes),
                   comment_count_fd = min(comment_count))

##dftmp$day_of_week <- as.factor(weekdays(dftmp$publish_date))

weekdays1 <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
#Use `%in%` and `weekdays` to create a logical vector
#convert to `factor` and specify the `levels/labels`
dftmp$day_of_week <- factor((weekdays(dftmp$publish_date) %in% weekdays1), 
                   levels=c(FALSE, TRUE), labels=c('weekend', 'weekday'))

write.csv(dftmp, "Cleaned.csv")
