library(dplyr)


vaccs <- read.csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.csv")

vacc_date <- vaccs %>% 
    filter(location == "World") %>% 
    pull(date) %>%
    as.Date() %>% 
    max(na.rm = T)

last_date <- readLines("lastdate.txt")


# check whether the new Datenstand is already in the time series data
if (vacc_date != last_date) {
    # no new data
    print("No new data. Skipping update.")
    readr::write_lines("no_update", here::here("tmp", "update.txt"))
    # readr::write_lines("no_update", "/tmp/ts_download.txt")
    quit(status = 0, save = "no")
}


# write out ts_download and ts_datenstand for gh actions
cat(last_date, file = here::here("tmp", "lastdate.txt"))

# readr::write_lines(format(max(rki_data$publication_date), "%Y-%m-%dT%H%M%S", tz = "Europe/Berlin"), "/tmp/ts_datenstand.txt")
# readr::write_lines(format(ts_download, "%Y-%m-%dT%H%M%S", tz = "Europe/Berlin"), "/tmp/ts_download.txt")