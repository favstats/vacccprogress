
library(here)

vaccs <- read.csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.csv")


vacc_date <- max(as.Date(unique(vaccs[vaccs$location == "World",]$date)), na.rm = T)

last_date <- readLines(here::here("tmp", "lastdate.txt"))


# check whether the new Datenstand is already in the time series data
if (vacc_date != last_date) {
    # no new data
    print("No new data. Skipping update.")
    cat("no_update", file = here::here("tmp", "update.txt"))
    
    # quit(status = 0, save = "no")
}


# write out lastdate and update for gh actions
cat(last_date, file = here::here("tmp", "lastdate.txt"))
cat("update", file = here::here("tmp", "update.txt"))
