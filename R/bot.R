

source(here::here("R", "utils.R"))


library(dplyr)
library(emo)
library(countrycode)
library(stringr)
library(twitteR)

flag_emojis <- emo::jis %>%
    filter(group == "Flags") %>% 
    mutate(iso_code = countrycode::countrycode(name, origin = "country.name", destination = "iso3c")) %>% 
    filter(!is.na(iso_code)) %>% 
    select(iso_code, emoji)

consumer_key <- Sys.getenv("consumer_key")

consumer_secret <- Sys.getenv("consumer_secret")

access_token <- Sys.getenv("token")

access_secret <- Sys.getenv("secret")

setup_twitter_oauth(consumer_key,consumer_secret,
                    access_token,access_secret)

vaccs <- read.csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.csv")

last_date <- vaccs %>% 
    filter(date == max(date)) %>% 
    pull(date) %>% 
    unique()


current_date <- readLines(here::here("lastdate.txt"))

if (current_date != last_date){
    ####### Vaccinated by Continent #####
    
    
    continental <- vaccs %>% 
        filter(location %in% c("Europe", "North America", "South America", "Asia", "Africa", "Oceania")) %>% 
        group_by(location) %>% 
        arrange(desc(date)) %>% 
        slice(1) %>% 
        ungroup() %>% 
        arrange(desc(date)) %>% 
        mutate(location = case_when(
            location == "South America" ~ "S. America",
            location == "North America" ~ "N. America",
            T ~ location
        )) 
    
    ####### At least 1 dose #####
    
    c_1dose <- continental %>% 
        rowwise() %>% 
        mutate(full_vacc_label = generate_pbar(people_vaccinated_per_hundred/100),
               full_vacc_label = paste0(location, ":\n", full_vacc_label)) %>% 
        pull(full_vacc_label) %>% 
        paste0(collapse = "\n") %>% 
        paste0("At least 1 dose by continent:\n\n", .) 
    
    
    ############# Fully ###########
    
    c_fully <-continental %>% 
        rowwise() %>% 
        mutate(full_vacc_label = generate_pbar(people_fully_vaccinated_per_hundred/100),
               full_vacc_label = paste0(location, ":\n", full_vacc_label)) %>% 
        pull(full_vacc_label) %>% 
        paste0(collapse = "\n") %>% 
        paste0("Fully vaccinated by continent:\n\n", .) 
    
    ############# World Stats ###########
    
    
    world_stats <- vaccs %>% 
        filter(location %in% c("World")) %>% 
        group_by(location) %>% 
        arrange(desc(date)) %>% 
        slice(1) %>% 
        ungroup()%>% 
        rowwise() %>% 
        mutate(full_vacc_label = generate_pbar(people_fully_vaccinated_per_hundred/100, 20),
               full_vacc_label = paste0("Fully vaccinated:\n", full_vacc_label),
               vacc_label = generate_pbar(people_vaccinated_per_hundred/100, 20),
               vacc_label = paste0("At least 1 dose:\n", vacc_label),
               people_fully_vaccinated = scales::unit_format(scale = 1/1e6, accuracy = 0.01)(people_fully_vaccinated),
               people_vaccinated = scales::unit_format(scale = 1/1e6, accuracy = 0.01)(people_vaccinated),
               daily_vaccinations = scales::unit_format(scale = 1/1e6, accuracy = 0.01)(daily_vaccinations),
               # daily_vaccinations_per_million = scales::label_number()(daily_vaccinations_per_million),
               full_label = glue::glue("🌍World Vaccination Stats:\n\n{vacc_label} ({people_vaccinated})\n{full_vacc_label} ({people_fully_vaccinated})\n\nDaily Vaccinations:\n{daily_vaccinations} doses administered\n{daily_vaccinations_per_million} doses per million people")) %>% pull(full_label)
    
    
    
    ############# Daily Vaccinations Top 7 ###########
    
    
    top_daily <- vaccs %>% 
        filter(date == max(date)) %>% 
        filter(!str_detect(iso_code, "OWID")) %>% 
        arrange(desc(daily_vaccinations)) %>% 
        mutate(daily_vaccinations_per_hundred = sprintf("%.2f", daily_vaccinations_per_million/10000)) %>% 
        left_join(flag_emojis) %>% 
        mutate(dvac_lab = scales::unit_format(scale = 1/1e6, accuracy = 0.001)(daily_vaccinations),
               full_lab = glue::glue("{emoji} {dvac_lab} ({daily_vaccinations_per_hundred}% of pop.)"),
               full_lab = ifelse(row_number() != 1, str_remove_all(full_lab, " of pop."), full_lab)) %>% 
        slice(1:7) %>% 
        pull(full_lab) %>% 
        paste0(collapse = "\n") %>% 
        paste0("Countries with Top Daily Vaccinations:\n\n", .)
    
    
    ############# Tweet it ###########
    
    twitteR::tweet(text = c_1dose, bypassCharLimit = T)
    
    Sys.sleep(5)
    
    twitteR::tweet(text = c_fully, bypassCharLimit = T)
    
    Sys.sleep(5)
    
    twitteR::tweet(text = world_stats, bypassCharLimit = T)
    
    Sys.sleep(5)
    
    twitteR::tweet(text = top_daily, bypassCharLimit = T)
    
}



cat(last_date, file = here::here("lastdate.txt"))
