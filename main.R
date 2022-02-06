library(ggplot2)
library(dplyr)
library(tidyr)
# install.packages("lubridate")
library(lubridate, warn.conflicts = FALSE)
library(plotly)
args <- commandArgs(T)
data <- read.csv(paste0("./data/", args[1], ".csv"))


sleep_length <- time_length(hm(data[, 3]) - hm(data[, 2]), unit = "hour")
sleep_length[sleep_length < 0] <- sleep_length[sleep_length < 0] + 24
data <- data %>% mutate(sleep_length = sleep_length)

data_melted <- data[, 1:3] %>% gather(type, time, -Date, na.rm = T)


p1 <- ggplot(data_melted, aes(x = Date, y = time, group = type, color = type)) +
    geom_line() +
    geom_point(size = 3) +
    geom_text(aes(x = Date, y = time, label = time), nudge_y = 0.5) +
    ylab("Time") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

p2 <- ggplot(data = data, aes(x = Date, y = sleep_length)) +
    geom_bar(stat = "identity", fill = "#7b8c7c") +
    geom_text(aes(x = Date, y = sleep_length,
        label = sprintf("%0.1f", sleep_length)),
        vjust = 5, color = "#ffffff") +
    ylab("Sleep Length") +  
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

if (!dir.exists("./plot")) {
    dir.create("./plot")
}

ggsave(p1, filename = paste0("./plot/", args[1], "_sleep_time.svg"), 
    width = 12, height = 9, dpi = 300)
ggsave(p2,
    filename = paste0("./plot/", args[1], "_sleep_length.svg"),
        width = 12, height = 9, dpi = 300)

