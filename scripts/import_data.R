library(data.table)
library(stringr)
library(plotly)
rm(list = ls())


dir <- "e:/datascience/projects/whatsapp/"
setwd(dir)

# Read data
data <- unlist(
    sapply(
        c("./data/private_chat.txt", "./data/work_chat.txt"), 
        readLines, 
        encoding = 'UTF-8')
    )

# Match different parts of the messages
dt <- data.table(
    raw = data,
    timestamp = as.POSIXct(
        str_extract(data, regex("(?<=\\[).*(?=\\])")), "%Y-%m-%d %H:%M:%S", 
        tz = "Europe/Berlin"),
    author = str_extract(data, regex("Fanny|Mathias")),
    text = str_extract(data, regex("(?<=\\:[:space:]).*$"))
    )

# Emoji count
dt[, emoji_count := str_count(text, regex("\\p{So}|\\p{Cn}"))] # this needs fixin

# Remove UTF-8 characters
dt[, text := gsub("\\p{So}|\\p{Cn}", "", text, perl = TRUE)]

# Character count
dt[, char_count := nchar(text)]

# image boolean
dt[, image := grepl("\\<bild utesluten\\>", text)]

# Add date
dt[, date := format(timestamp, format = "%Y-%m-%d")]


# Descriptive statistics: character count
dt[, .(average = mean(char_count),
       min = min(char_count),
       max = max(char_count)), 
   by = .(author)]

# Descriptive statistics: emoji count
dt[, .(average = mean(emoji_count),
       min = min(emoji_count),
       max = max(emoji_count)), 
   by = .(author)]

# Descriptive statistics: emoji count
dt[image == TRUE, .(image_count = .N), by = .(author)]


# A
# Aggregate message count
daily <- dt[, .(N = .N), by = date]

plot_ly(daily, x = ~date, y = ~N)




