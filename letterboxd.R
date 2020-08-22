# Perform analytics on Letterboxd profile data obtained via Settings - Export
library(dplyr)
library(lubridate)

diary <- read.csv("C:/Users/jonol/Documents/letterboxd/diary.csv")

# 1 Diary Entries in Current Year
current_year <- format(Sys.time(), "%Y")
year <- diary %>% dplyr::filter(substr(Watched.Date,1,4) == current_year)
print(paste0("Number of movies watched this year: ", nrow(year)))

# 2 Column Chart of films watched by month/week
year$Watched.Date <- as.Date(year$Watched.Date)

monthly <- year %>% dplyr::group_by(month=floor_date(Watched.Date, "month"))
monthly_sum <- summarise(monthly, count=n())
barplot(monthly_sum$count, names=monthly_sum$month, main="Watches per Month")

weekly <- year %>% dplyr::group_by(week=floor_date(Watched.Date, "week"))
weekly_sum <- summarise(weekly, count=n())
barplot(weekly_sum$count, names=weekly_sum$week, main="Watches per Week")

# 3 Average Films Watched per month and per week
print(paste0("Average per month: ", mean(summarise(monthly, count=n())$count)))
print(paste0("Average per week: ", mean(summarise(weekly, count=n())$count)))

# 4 Films watched per day of week
year$dow <- wday(year$Watched.Date, label = TRUE, abbr = TRUE)
daily <- year %>% dplyr::group_by(dow)
daily_sum <- summarise(daily, count=n())
barplot(daily_sum$count, names=daily_sum$dow, main="Watches per Day of Week")

# 4 Pie Chart of Watches v Re-watches
pie(table(year$Rewatch),c("Watches","Re-watches"), main="Watches and Re-watches")

# 5 Column Chart of ratings spread
ratings <- year %>% dplyr::group_by(year$Rating)
ratings_sum <- summarise(ratings, count=n())
barplot(ratings_sum$count, names=ratings_sum$`year$Rating`, main="Ratings Spread")

# 6 List of most watched films this year (names, counts)
print(paste0("Most watched filmes in ", current_year))
print(head(sort(table(year$Name), decreasing = TRUE), 5))

# 7 List of most watched films of all time (names, counts)
print("Most watched films")
print(head(sort(table(diary$Name), decreasing = TRUE), 20))