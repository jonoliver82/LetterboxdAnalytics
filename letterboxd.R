# Perform analytics on Letterboxd profile data obtained via Settings - Export
rm(list = ls())

library(dplyr)
library(lubridate)

# Read in the source files
watched <- read.csv("C:/Users/jonol/Documents/letterboxd/watched.csv")
print(paste0("Number of movies watched: ", nrow(watched)))

diary <- read.csv("C:/Users/jonol/Documents/letterboxd/diary.csv")
diary$Watched.Date <- as.Date(diary$Watched.Date)
print(paste0("Number of diary entries: ", nrow(diary)))

# Create csv file of for each tag
sapply(unique(diary$Tags), function(tag)
  if(tag != "") {
    write.csv(diary[diary$Tags==tag,],paste0("C:/Users/jonol/Documents/letterboxd/",tag,".csv"), row.names=FALSE)
  }
)

# simple list of films with ratings
ratings <- read.csv("C:/Users/jonol/Documents/letterboxd/ratings.csv")
print(paste0("Number of movies with ratings: ", nrow(ratings)))
write.csv(ratings[order(ratings$Name),c(2,5)], "C:/Users/jonol/Documents/letterboxd/simple_ratings.csv", row.names = FALSE)

# ratings at 4.5 or 5 only
high_ratings <- ratings %>% dplyr::filter(Rating > 4.0)
print(paste0("Number of movies with rating greater than 4: ", nrow(high_ratings)))
write.csv(high_ratings[order(high_ratings$Name),c(2,5)], "C:/Users/jonol/Documents/letterboxd/high_ratings.csv", row.names = FALSE)

# Obtain a list of films watched but not in the diary
not_watched <- dplyr::anti_join(watched, diary, by="Name")
print(paste0("Number of films watched but not diaried since ", min(diary$Watched.Date), " : ", nrow(not_watched)))
write.csv(not_watched[order(not_watched$Name),c(2:4)], "C:/Users/jonol/Documents/letterboxd/notwatched.csv", row.names = FALSE)

# Diary Entries in Current Year
current_year <- format(Sys.time(), "%Y")
year <- diary %>% dplyr::filter(substr(Watched.Date,1,4) == current_year)
print(paste0("Number of movies watched this year: ", nrow(year)))

# Column Chart of films watched by year/month/week
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

# Average Films Watched per month and per week
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

# Films watched per day of week
year$dow <- wday(year$Watched.Date, label = TRUE, abbr = TRUE, week_start = 1)
daily <- year %>% dplyr::group_by(dow)
daily_sum <- summarise(daily, count=n())
barplot(daily_sum$count, names=daily_sum$dow, main="Watches per Day of Week")

# Pie Chart of Watches v Re-watches
pie(table(year$Rewatch),labels = c("Watches","Re-watches"), main="Watches and Re-watches")

# Column Chart of ratings spread
ratings <- year %>% dplyr::group_by(year$Rating)
ratings_sum <- summarise(ratings, count=n())
barplot(ratings_sum$count, names=ratings_sum$`year$Rating`, main="Ratings Spread")

# List of most watched films this year (names, counts)
print(paste0("Most watched films in ", current_year))
yeardf <- as.data.frame(table(year$Name)) %>% rename(Name = Var1)
yeardf <- filter(yeardf, yeardf$Freq > 1)
print(head(yeardf[order(yeardf$Freq, decreasing = TRUE),], 5))

# List of most watched films of all time (names, counts)
print("Most watched films")
diarydf <- as.data.frame(table(diary$Name)) %>% rename(Name = Var1)
diarydf <- filter(diarydf, diarydf$Freq > 1)
print(head(diarydf[order(diarydf$Freq, decreasing = TRUE),], 20))