
source(here::here("R", "utils.R"))


library(dplyr)
library(emo)
library(countrycode)
library(stringr)
library(webshot2)
library(rtweet)

print("authenticate")

# Create a token containing your Twitter keys
rtweet::create_token(
    app = "Automated Bot Content",  # the name of the Twitter app
    consumer_key = Sys.getenv("consumer_key"),
    consumer_secret = Sys.getenv("consumer_secret"),
    access_token = Sys.getenv("token"),
    access_secret = Sys.getenv("secret")
)

flag_emojis <- emo::jis %>%
    filter(group == "Flags") %>% 
    mutate(iso_code = countrycode::countrycode(name, origin = "country.name", destination = "iso3c")) %>% 
    filter(!is.na(iso_code)) %>% 
    select(iso_code, emoji)



vaccs <- read.csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.csv") %>% 
    mutate(date = as.Date(date))

pops <- read.csv("https://raw.githubusercontent.com/datasets/population/master/data/population.csv") %>% 
    as_tibble() %>% 
    filter(Year == max(Year)) %>% 
    select(iso_code = Country.Code, pop = Value)


####### Vaccinated by Continent #####


continental <- vaccs %>% 
    filter(location %in% c("Europe", "North America", "South America", "Asia", "Africa", "Oceania")) %>% 
    group_by(location) %>% 
    arrange(desc(date)) %>% 
    slice(1) %>% 
    ungroup() %>% 
    arrange(desc(date)) %>% 
    mutate(location = case_when(
        location == "South America" ~ "S. Amer.",
        location == "North America" ~ "N. Amer.",
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

c_fully <- continental %>% 
    rowwise() %>% 
    mutate(full_vacc_label = generate_pbar(people_fully_vaccinated_per_hundred/100),
           full_vacc_label = paste0(location, ":\n", full_vacc_label)) %>% 
    pull(full_vacc_label) %>% 
    paste0(collapse = "\n") %>% 
    paste0("Fully vaccinated by continent:\n\n", .) 



####### Vaccinated by Income #####


income_dat <- vaccs %>% 
    filter(str_detect(location, "income")) %>% 
    group_by(location) %>% 
    arrange(desc(date)) %>% 
    slice(1) %>% 
    ungroup() %>% 
    mutate(location = factor(location, levels = c("High income", "Upper middle income", "Lower middle income", "Low income"))) %>% 
    arrange(location)



####### At least 1 dose #####

inc_1dose <- income_dat %>% 
    rowwise() %>% 
    mutate(full_vacc_label = generate_pbar(people_vaccinated_per_hundred/100),
           full_vacc_label = paste0(location, ":\n", full_vacc_label),
           full_vacc_label = str_replace(full_vacc_label, "High income:", "High-income countries:"),
           full_vacc_label = str_replace(full_vacc_label, "Low income:", "Low-income countries:")) %>% 
    pull(full_vacc_label) %>% 
    paste0(collapse = "\n\n") %>% 
    paste0("At least 1 dose by country income group:\n\n", .)


############# Fully ###########

inc_fully <- income_dat %>% 
    rowwise() %>% 
    mutate(full_vacc_label = generate_pbar(people_fully_vaccinated_per_hundred/100),
           full_vacc_label = paste0(location, ":\n", full_vacc_label),
           full_vacc_label = str_replace(full_vacc_label, "High income:", "High-income countries:"),
           full_vacc_label = str_replace(full_vacc_label, "Low income:", "Low-income countries:")) %>% 
    pull(full_vacc_label) %>% 
    paste0(collapse = "\n\n") %>% 
    paste0("Fully vaccinated by country income group:\n\n", .)


############# World Stats ###########


world_stats <- vaccs %>% 
    filter(location %in% c("World")) %>% 
    group_by(location) %>% 
    arrange(desc(date)) %>% 
    slice(1) %>% 
    ungroup()%>% 
    rowwise() %>% 
    mutate(people_fully_vaccinated = scales::unit_format(scale = 1/1e9, accuracy = 0.001, unit = "bn")(people_fully_vaccinated),
           full_vacc_label = generate_pbar(people_fully_vaccinated_per_hundred/100, 17),
           full_vacc_label = glue::glue("Fully vaccinated ({people_fully_vaccinated}):\n {full_vacc_label}"),
           people_vaccinated = scales::unit_format(scale = 1/1e9, accuracy = 0.001, unit = "bn")(people_vaccinated),
           vacc_label = generate_pbar(people_vaccinated_per_hundred/100, 17),
           vacc_label = glue::glue("At least 1 dose ({people_vaccinated}):\n {vacc_label}"),
           daily_vaccinations = scales::unit_format(scale = 1/1e6, accuracy = 0.1)(daily_vaccinations),
           # daily_vaccinations_per_million = scales::label_number()(daily_vaccinations_per_million),
           full_label = glue::glue("🌍World Vaccination Stats:\n\n{vacc_label} \n{full_vacc_label}\n\nDaily Vaccinations:\n{daily_vaccinations} doses administered\n{daily_vaccinations_per_million} doses per million people")) %>% pull(full_label)

############# North America ###########

na_stats <- vaccs %>% 
    filter(location %in% c("North America")) %>% 
    group_by(location) %>% 
    arrange(desc(date)) %>% 
    slice(1) %>% 
    ungroup()%>% 
    rowwise() %>% 
    mutate(full_vacc_label = generate_pbar(people_fully_vaccinated_per_hundred/100, 17),
           full_vacc_label = glue::glue("Fully vaccinated\n {full_vacc_label}"),
          vacc_label = generate_pbar(people_vaccinated_per_hundred/100, 17),
           vacc_label = glue::glue("At least 1 dose:\n {vacc_label}"),
           full_label = glue::glue("{vacc_label} \n{full_vacc_label}")) %>% pull(full_label)


############# North America ###########

sa_stats <- vaccs %>% 
    filter(location %in% c("South America")) %>% 
    group_by(location) %>% 
    arrange(desc(date)) %>% 
    slice(1) %>% 
    ungroup()%>% 
    rowwise() %>% 
    mutate(full_vacc_label = generate_pbar(people_fully_vaccinated_per_hundred/100, 17),
           full_vacc_label = glue::glue("Fully vaccinated\n {full_vacc_label}"),
           vacc_label = generate_pbar(people_vaccinated_per_hundred/100, 17),
           vacc_label = glue::glue("At least 1 dose:\n {vacc_label}"),
           full_label = glue::glue("{vacc_label} \n{full_vacc_label}")) %>% pull(full_label)


############# Europe ###########

eu_stats <- vaccs %>% 
    filter(location %in% c("Europe")) %>% 
    group_by(location) %>% 
    arrange(desc(date)) %>% 
    slice(1) %>% 
    ungroup()%>% 
    rowwise() %>% 
    mutate(full_vacc_label = generate_pbar(people_fully_vaccinated_per_hundred/100, 17),
           full_vacc_label = glue::glue("Fully vaccinated\n {full_vacc_label}"),
           vacc_label = generate_pbar(people_vaccinated_per_hundred/100, 17),
           vacc_label = glue::glue("At least 1 dose:\n {vacc_label}"),
           full_label = glue::glue("{vacc_label} \n{full_vacc_label}")) %>% pull(full_label)



############# Oceania ###########

oceania_stats <- vaccs %>% 
    filter(location %in% c("Oceania")) %>% 
    group_by(location) %>% 
    arrange(desc(date)) %>% 
    slice(1) %>% 
    ungroup()%>% 
    rowwise() %>% 
    mutate(full_vacc_label = generate_pbar(people_fully_vaccinated_per_hundred/100, 17),
           full_vacc_label = glue::glue("Fully vaccinated\n {full_vacc_label}"),
           vacc_label = generate_pbar(people_vaccinated_per_hundred/100, 17),
           vacc_label = glue::glue("At least 1 dose:\n {vacc_label}"),
           full_label = glue::glue("{vacc_label} \n{full_vacc_label}")) %>% pull(full_label)

############# Africa ###########

af_stats <- vaccs %>% 
    filter(location %in% c("Africa")) %>% 
    group_by(location) %>% 
    arrange(desc(date)) %>% 
    slice(1) %>% 
    ungroup()%>% 
    rowwise() %>% 
    mutate(full_vacc_label = generate_pbar(people_fully_vaccinated_per_hundred/100, 17),
           full_vacc_label = glue::glue("Fully vaccinated\n {full_vacc_label}"),
           vacc_label = generate_pbar(people_vaccinated_per_hundred/100, 17),
           vacc_label = glue::glue("At least 1 dose:\n {vacc_label}"),
           full_label = glue::glue("{vacc_label} \n{full_vacc_label}")) %>% pull(full_label)

############# Asia ###########

asia_stats <- vaccs %>% 
    filter(location %in% c("Asia")) %>% 
    group_by(location) %>% 
    arrange(desc(date)) %>% 
    slice(1) %>% 
    ungroup()%>% 
    rowwise() %>% 
    mutate(full_vacc_label = generate_pbar(people_fully_vaccinated_per_hundred/100, 17),
           full_vacc_label = glue::glue("Fully vaccinated\n {full_vacc_label}"),
           vacc_label = generate_pbar(people_vaccinated_per_hundred/100, 17),
           vacc_label = glue::glue("At least 1 dose:\n {vacc_label}"),
           full_label = glue::glue("{vacc_label} \n{full_vacc_label}")) %>% pull(full_label)


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
    tidyr::drop_na(daily_vaccinations_per_million) %>% 
    slice(1:7) %>% 
    pull(full_lab) %>% 
    paste0(collapse = "\n") %>% 
    paste0("Countries with Top Daily Vaccinations:\n\n", .)


############# Daily Vaccination % Top 7 ###########


top_daily_perc <- vaccs %>% 
    filter(date == max(date)) %>%
    filter(!str_detect(iso_code, "OWID")) %>% 
    left_join(pops) %>% 
    ## only show countries that have at least 1 million population
    ## so microstates don't rise to top with few hundred vaccinations
    filter(pop >= 1000000) %>% 
    mutate(daily_vaccinations_per_hundred = sprintf("%.2f", daily_vaccinations_per_million/10000)) %>% 
    arrange(desc(daily_vaccinations_per_hundred)) %>% 
    left_join(flag_emojis) %>% 
    mutate(dvac_lab = scales::unit_format(scale = 1/1e6, accuracy = 0.001)(daily_vaccinations),
           dvac_lab = ifelse(str_detect(dvac_lab, "0.0"),  scales::label_number()(daily_vaccinations), dvac_lab) %>% str_remove("\\.0"),
           full_lab = glue::glue("{emoji} {dvac_lab} ({daily_vaccinations_per_hundred}% of pop.)"),
           full_lab = ifelse(row_number() != 1, str_remove_all(full_lab, " of pop."), full_lab)) %>%
    tidyr::drop_na(daily_vaccinations_per_million) %>% 
    slice(1:7) %>% 
    pull(full_lab) %>% 
    paste0(collapse = "\n") %>% 
    paste0("Countries w/ greatest share of population vaccinated per day:\n\n", .)


############ Images #############


# webshot("https://ourworldindata.org/grapher/share-people-fully-vaccinated-covid?tab=map&time=latest", 
#         here::here("img", "share-people-fully-vaccinated-covid.png"), selector = "figure")
# 
# webshot("https://ourworldindata.org/grapher/share-people-fully-vaccinated-covid?tab=map&time=latest&region=Africa", 
#         here::here("img", "share-people-fully-vaccinated-covid-africa.png"), selector = "figure")
# 
# webshot("https://ourworldindata.org/grapher/share-people-fully-vaccinated-covid?tab=map&time=latest&region=NorthAmerica", 
#         here::here("img", "share-people-fully-vaccinated-covid-na.png"), selector = "figure")
# 
# webshot("https://ourworldindata.org/grapher/share-people-fully-vaccinated-covid?tab=map&time=latest&region=SouthAmerica", 
#         here::here("img", "share-people-fully-vaccinated-covid-sa.png"), selector = "figure")
# 
# webshot("https://ourworldindata.org/grapher/share-people-fully-vaccinated-covid?tab=map&time=latest&region=Asia", 
#         here::here("img", "share-people-fully-vaccinated-covid-asia.png"), selector = "figure")
# 
# webshot("https://ourworldindata.org/grapher/share-people-fully-vaccinated-covid?tab=map&time=latest&region=Europe", 
#         here::here("img", "share-people-fully-vaccinated-covid-europe.png"), selector = "figure")
# 
# webshot("https://ourworldindata.org/grapher/share-people-fully-vaccinated-covid?tab=map&time=latest&region=Oceania", 
#         here::here("img", "share-people-fully-vaccinated-covid-oceania.png"), selector = "figure")

############# Tweet it ###########

print("c_1dose")

rtweet::post_tweet(status = c_1dose)

Sys.sleep(5)
print("c_fully")

rtweet::post_tweet(status = c_fully)

Sys.sleep(5)
print("world_stats")

rtweet::post_tweet(status = world_stats)

Sys.sleep(5)
print("top_daily")

rtweet::post_tweet(status = top_daily)

Sys.sleep(5)
print("top_daily_perc")

rtweet::post_tweet(status = top_daily_perc)

Sys.sleep(5)
print("inc_1dose")

rtweet::post_tweet(status = inc_1dose)

Sys.sleep(5)
print("inc_fully")

rtweet::post_tweet(status = inc_fully)

Sys.sleep(5)
print("share-people-fully-vaccinated-covid")

rtweet::post_tweet(status = "Share of the population vaccinated against #COVID19\n\nhttps://ourworldindata.org/grapher/share-people-fully-vaccinated-covid?tab=map&time=latest")#, 
              # media = here::here("img", "share-people-fully-vaccinated-covid.png"))

Sys.sleep(5)
print("Africa")

rtweet::post_tweet(status = glue::glue("Share of population vaccinated against #COVID19 (Africa)\n\n{af_stats}\n\nhttps://ourworldindata.org/grapher/share-people-fully-vaccinated-covid?tab=map&time=latest"))#,
              # media = here::here("img", "share-people-fully-vaccinated-covid-africa.png"))

Sys.sleep(5)
print("North America")

rtweet::post_tweet(status = glue::glue("Share of population vaccinated against #COVID19 (North America)\n\n{na_stats}\n\nhttps://ourworldindata.org/grapher/share-people-fully-vaccinated-covid?tab=map&time=latest"))#, 
              # media = here::here("img", "share-people-fully-vaccinated-covid-na.png"))

Sys.sleep(5)
print("South America")

rtweet::post_tweet(status = glue::glue("Share of population vaccinated against #COVID19 (South America)\n\n{sa_stats}\n\nhttps://ourworldindata.org/grapher/share-people-fully-vaccinated-covid?tab=map&time=latest"))#,
              # media = here::here("img", "share-people-fully-vaccinated-covid-sa.png"))

Sys.sleep(5)
print("Europe")

rtweet::post_tweet(status = glue::glue("Share of population vaccinated against #COVID19 (Europe)\n\n{eu_stats}\n\nhttps://ourworldindata.org/grapher/share-people-fully-vaccinated-covid?tab=map&time=latest"))#,
              # media = here::here("img", "share-people-fully-vaccinated-covid-europe.png"))

Sys.sleep(5)
print("Asia")

rtweet::post_tweet(status = glue::glue("Share of population vaccinated against #COVID19 (Asia)\n\n{asia_stats}\n\nhttps://ourworldindata.org/grapher/share-people-fully-vaccinated-covid?tab=map&time=latest"))#, 
              # media = here::here("img", "share-people-fully-vaccinated-covid-asia.png"))

Sys.sleep(5)
print("Oceania")

rtweet::post_tweet(status = glue::glue("Share of population vaccinated against #COVID19 (Oceania)\n\n{oceania_stats}\n\nhttps://ourworldindata.org/grapher/share-people-fully-vaccinated-covid?tab=map&time=latest"))#, 
              # media = here::here("img", "share-people-fully-vaccinated-covid-oceania.png"))

