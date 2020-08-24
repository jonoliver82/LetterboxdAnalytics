# Perform analytics on Letterboxd profile data obtained via Settings - Export
rm(list = ls())

library(dplyr)
library(lubridate)

watched <- read.csv("C:/Users/jonol/Documents/letterboxd/watched.csv")
print(paste0("Number of movies watched: ", nrow(watched)))

diary <- read.csv("C:/Users/jonol/Documents/letterboxd/diary.csv")
diary$Watched.Date <- as.Date(diary$Watched.Date)

# 1 Diary Entries in Current Year
current_year <- format(Sys.time(), "%Y")
year <- diary %>% dplyr::filter(substr(Watched.Date,1,4) == current_year)
print(paste0("Number of movies watched this year: ", nrow(year)))

# 2 Column Chart of films watched by year/month/week
yearly <- diary %>% dplyr::group_by(year=floor_date(Watched.Date, "year"))
yearly_sum <- summarise(yearly, count=n())
yearly_sum$year <- format(yearly_sum$year, "%Y")
barplot(yearly_sum$count, names=yearly_sum$year, main="Watches per Year")

monthly <- year %>% dplyr::group_by(month=floor_date(Watched.Date, "month"))
monthly_sum <- summarise(monthly, count=n())
monthly_sum$month <- format(monthly_sum$month, "%b")
barplot(monthly_sum$count, names=monthly_sum$month, main="Watches per Month")

weekly <- year %>% dplyr::group_by(week=floor_date(Watched.Date, "week"))
weekly_sum <- summarise(weekly, count=n())
barplot(weekly_sum$count, names=weekly_sum$week, main="Watches per Week", xaxt='n')

# 3 Average Films Watched per month and per week
print(paste0("Average per month: ", format(mean(monthly_sum$count), digits = 1, nsmall = 0)))
monthmin <- which.min(monthly_sum$count)
print(paste0("Min per month: ", monthly_sum$count[monthmin], " in ", monthly_sum$month[monthmin]))
monthmax <- which.max(monthly_sum$count)
print(paste0("Max per month: ", monthly_sum$count[monthmax], " in ", monthly_sum$month[monthmax]))

print(paste0("Average per week: ", format(mean(weekly_sum$count), digits = 1, nsmall = 0)))
weekmin <- which.min(weekly_sum$count)
print(paste0("Min per week: ", weekly_sum$count[weekmin], " in ", weekly_sum$week[weekmin]))
weekmax <- which.max(weekly_sum$count)
print(paste0("Max per week: ", weekly_sum$count[weekmax], " in ", weekly_sum$week[weekmax]))

# 4 Films watched per day of week
year$dow <- wday(year$Watched.Date, label = TRUE, abbr = TRUE, week_start = 1)
daily <- year %>% dplyr::group_by(dow)
daily_sum <- summarise(daily, count=n())
barplot(daily_sum$count, names=daily_sum$dow, main="Watches per Day of Week")

# 4 Pie Chart of Watches v Re-watches
pie(table(year$Rewatch),labels = c("Watches","Re-watches"), main="Watches and Re-watches")

# 5 Column Chart of ratings spread
ratings <- year %>% dplyr::group_by(year$Rating)
ratings_sum <- summarise(ratings, count=n())
barplot(ratings_sum$count, names=ratings_sum$`year$Rating`, main="Ratings Spread")

# 6 List of most watched films this year (names, counts)
print(paste0("Most watched films in ", current_year))
yeardf <- as.data.frame(table(year$Name)) %>% rename(Name = Var1)
yeardf <- filter(yeardf, yeardf$Freq > 1)
print(head(yeardf[order(yeardf$Freq, decreasing = TRUE),], 5))

# 7 List of most watched films of all time (names, counts)
print("Most watched films")
diarydf <- as.data.frame(table(diary$Name)) %>% rename(Name = Var1)
diarydf <- filter(diarydf, diarydf$Freq > 1)
print(head(diarydf[order(diarydf$Freq, decreasing = TRUE),], 20))