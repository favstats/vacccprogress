
vaccs <- read.csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.csv")

vacc_date <- max(as.Date(unique(vaccs[vaccs$location == "World",]$date)), na.rm = T)

last_date <- readLines("lastdate.txt")


# check whether the new data has already been tweeted
if (vacc_date != last_date) {
    # no new data
    print("No new data. Skipping update.")
    cat("no_update", file = "/tmp/update.txt")
    cat(last_date, file = "/tmp/last_date.txt")
    
    quit(status = 0, save = "no")
}


# write out lastdate and update for gh actions
cat(last_date, file = "lastdate.txt")
cat("update", file = "/tmp/update.txt")
