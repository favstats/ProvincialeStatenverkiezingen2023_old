source("utils.R")
# ?get_targeting
# get_targeting("41459763029", timeframe = "LAST_90_DAYS")
# debugonce(get_targeting)

library(httr)
library(tidyverse)

tstamp <- Sys.time()

source("utils.R")

unlink("provincies/7", recursive = T, force = T)
unlink("provincies/30", recursive = T, force = T)

dir.create("provincies/7")
dir.create("provincies/30")

# rawadvertisers <- read_csv("data/advertisers - advertisers.csv")  %>%
#   mutate(party_lab = case_when(
#     str_detect(advertiser_name, "VVD") ~ "VVD",
#     str_detect(advertiser_name, "\\bCDA\\b") ~ "CDA",
#     str_detect(advertiser_name, "PvdA|Jonge Socialisten") ~ "PvdA",
#     str_detect(advertiser_name, "D66|Jonge Democraten") ~ "D66",
#     str_detect(advertiser_name, "GroenLinks") ~ "GroenLinks",
#     str_detect(advertiser_name, "ChristenUnie") ~ "ChristenUnie",
#     str_detect(advertiser_name, "\\bSP\\b") ~ "SP",
#     str_detect(advertiser_name, "FvD|FVD|Forum voor Democratie") ~ "FvD",
#     str_detect(advertiser_name, "50PLUS") ~ "50PLUS",
#     str_detect(advertiser_name, "\\bSGP\\b") ~ "SGP",
#     str_detect(advertiser_name, "PvdD|Partij voor de Dieren") ~ "PvdD",
#     str_detect(advertiser_name, "PVV") ~ "PVV",
#     str_detect(advertiser_name, "DENK") ~ "DENK",
#     str_detect(advertiser_name, "Volt") ~ "Volt Nederland",
#     str_detect(advertiser_name, "BIJ1") ~ "BIJ1",
#     str_detect(advertiser_name, "BVNL") ~ "BVNL",
#     str_detect(advertiser_name, "Ja21") ~ "Ja21",
#     T ~ ""
#   ))



internal_page_ids <- read_csv("data/nl_advertisers.csv") %>%
  mutate(page_id = as.character(page_id))

internal_page_ids <- read_csv("https://raw.githubusercontent.com/favstats/ProvincialeStatenverkiezingen2023/main/data/nl_advertisers.csv") %>%
    mutate(page_id = as.character(page_id))

# internal_page_ids %>%
#     count(party, sort = T) %>% View

wtm_data <- read_csv("data/wtm-advertisers-nl-2023-02-28.csv") %>% #names
    select(page_id = advertisers_platforms.advertiser_platform_ref,
           page_name = name, party = entities.short_name)  %>%
    mutate(page_id = as.character(page_id)) %>%
    # filter(party == "And") %>% #View
    # count(party, sort = T)  %>%
  mutate(party = case_when(
    str_detect(party, "VVD") ~ "VVD",
    str_detect(party, "\\bCDA\\b") ~ "CDA",
    str_detect(party, "PvdA|Jonge Socialisten") ~ "PvdA",
    str_detect(party, "D66|Jonge Democraten") ~ "D66",
    str_detect(party, "GroenLinks|GL") ~ "GroenLinks",
    str_detect(party, "ChristenUnie|CU") ~ "ChristenUnie",
    str_detect(party, "\\bSP\\b") ~ "SP",
    str_detect(party, "FvD|FVD|Forum voor Democratie") ~ "FvD",
    str_detect(party, "50Plus|50PLUS") ~ "50PLUS",
    str_detect(party, "\\bSGP\\b") ~ "SGP",
    str_detect(party, "PvdD|Partij voor de Dieren") ~ "PvdD",
    str_detect(party, "PVV") ~ "PVV",
    str_detect(party, "DENK") ~ "DENK",
    str_detect(party, "Volt|VOLT") ~ "Volt Nederland",
    str_detect(party, "BIJ1|BiJ") ~ "BIJ1",
    str_detect(party, "BVNL") ~ "BVNL",
    str_detect(party, "Ja21") ~ "JA21",
    str_detect(page_name, "Alliantie") ~ "Alliantie",
    T ~ party
  )) #%>% #View
    # count(party, sort = T)

# wtm_data %>% count(party)

rep <- read_csv("data/FacebookAdLibraryReport_2023-03-05_NL_last_30_days_advertisers.csv") %>% janitor::clean_names()  %>%
    mutate(page_id = as.character(page_id)) %>%
    mutate(party1 = case_when(
        str_detect(page_name, "VVD") ~ "VVD",
        str_detect(page_name, "\\bCDA\\b") ~ "CDA",
        str_detect(page_name, "PvdA|Jonge Socialisten") ~ "PvdA",
        str_detect(page_name, "D66|Jonge Democraten") ~ "D66",
        str_detect(page_name, "GroenLinks|GL") ~ "GroenLinks",
        str_detect(page_name, "ChristenUnie|CU") ~ "ChristenUnie",
        str_detect(page_name, "\\bSP\\b") ~ "SP",
        str_detect(page_name, "FvD|FVD|Forum voor Democratie") ~ "FvD",
        str_detect(page_name, "50Plus|50PLUS") ~ "50PLUS",
        str_detect(page_name, "\\bSGP\\b") ~ "SGP",
        str_detect(page_name, "PvdD|Partij voor de Dieren") ~ "PvdD",
        str_detect(page_name, "PVV") ~ "PVV",
        str_detect(page_name, "DENK") ~ "DENK",
        str_detect(page_name, "Volt|VOLT") ~ "Volt Nederland",
        str_detect(page_name, "BIJ1|BiJ") ~ "BIJ1",
        str_detect(page_name, "BVNL") ~ "BVNL",
        str_detect(page_name, "Ja21") ~ "JA21",
        str_detect(page_name, "Alliantie") ~ "Alliantie",
        str_detect(page_name, "BBB") ~ "BBB",
        T ~ NA_character_
    )) %>%
    mutate(party2 = case_when(
        str_detect(disclaimer, "VVD") ~ "VVD",
        str_detect(disclaimer, "\\bCDA\\b") ~ "CDA",
        str_detect(disclaimer, "PvdA|Jonge Socialisten") ~ "PvdA",
        str_detect(disclaimer, "D66|Jonge Democraten") ~ "D66",
        str_detect(disclaimer, "GroenLinks|GL") ~ "GroenLinks",
        str_detect(disclaimer, "ChristenUnie|CU") ~ "ChristenUnie",
        str_detect(disclaimer, "\\bSP\\b") ~ "SP",
        str_detect(disclaimer, "FvD|FVD|Forum voor Democratie") ~ "FvD",
        str_detect(disclaimer, "50Plus|50PLUS") ~ "50PLUS",
        str_detect(disclaimer, "\\bSGP\\b") ~ "SGP",
        str_detect(disclaimer, "PvdD|Partij voor de Dieren") ~ "PvdD",
        str_detect(disclaimer, "PVV") ~ "PVV",
        str_detect(disclaimer, "DENK") ~ "DENK",
        str_detect(disclaimer, "Volt|VOLT") ~ "Volt Nederland",
        str_detect(disclaimer, "BIJ1|BiJ") ~ "BIJ1",
        str_detect(disclaimer, "BVNL") ~ "BVNL",
        str_detect(disclaimer, "Ja21") ~ "JA21",
        str_detect(disclaimer, "BBB") ~ "BBB",
        T ~ NA_character_
    )) %>%
    mutate(party = ifelse(is.na(party1), party2, party1)) %>%
    drop_na(party) %>%
    distinct(page_id, .keep_all = T) %>%
    filter(str_detect(page_name, "Global Space Conference on Climate Change|de Alliantie|PvdA - GroenLinks", negate = T))

338750440106782

all_dat <- #read_csv("nl_advertisers.csv") %>%
    # mutate(page_id = as.character(page_id)) %>%
    bind_rows(internal_page_ids) %>%
    bind_rows(wtm_data) %>%
    bind_rows(rep) %>%
    distinct(page_id, .keep_all = T)

# all_dat %>%
#     count(party, sort  =T)

# all_dat %>%
#     bind_rows(rep %>% select(page_name, page_id, disclaimer, party))  %>%
#     distinct(page_id, .keep_all = T) %>%
#     filter(!(page_id %in% all_dat$page_id)) %>%
#     filter(str_detect(page_name, "Global Space Conference on Climate Change|de Alliantie|PvdA - GroenLinks", negate = T)) %>% View

# all_dat %>% filter(str_detect(page_name, "BBB")) %>% View

write_csv(all_dat, file = "data/nl_advertisers.csv")

# janitor::clean_names() %>%
# arrange(desc(amount_spent_usd)) %>%
# mutate(spend_upper = amount_spent_usd %>% as.numeric()) %>%
# arrange(-spend_upper) %>%
# mutate_all(as.character)C

# internal_page_ids %>% count(party, sort =T) %>% slice(11:17)
#
# internal_page_ids %>%
#   filter(party == "Politiek Op Maat")
#
# rawadvertisers %>%
#   # filter(category == "Political Organization") %>% View
#   # filter(str_detect(category, "Party|Politician|Candidade")) %>%
#   rename(page_id = advertiser_id) %>%
#   select(page_id, page_name = advertiser_name, party = party_lab)
#   left_join(internal_page_ids) %>%
#   # drop_na(party) %>%
#   filter(!is.na(party) | party_lab != "") %>%
#   # filter(party == "PvdA" & party_lab == "")
#   count(party, party_lab, sort = T)  %>% View
#
#
#
#   internal_page_ids %>%
#     bind_rows(
#       rawadvertisers %>%
#         rename(page_id = advertiser_id) %>%
#         select(page_id, page_name = advertiser_name, party = party_lab) %>%
#         filter(party != "") %>%
#         filter(str_starts(page_id, "AR", negate = T)) %>%
#         mutate(source = "yo")
#     ) %>%
#     distinct(page_id, .keep_all = T) %>%
#     write_csv("data/nl_advertisers.csv")


# georgia_wtm <- readr::read_csv("data/wtm-advertisers-us-2022-11-28T14_22_01.338Z.csv") %>%
#   select(page_name = name,
#          page_id = advertisers_platforms.advertiser_platform_ref) %>%
#   mutate(page_id = as.character(page_id))

# options(scipen = 999999)

# georgia_wtm

# internal_page_ids <- georgia_wtm %>%
#   mutate_all(as.character) %>%
#   bind_rows(last90days)  %>%
#   distinct(page_id, .keep_all = T)

# get_targeting(internal_page_ids$page_id[1], timeframe = "LAST_30_DAYS")
# debugonce(get_targeting)
# get_targeting("121264564551002", timeframe = "LAST_30_DAYS")

scraper <- function(.x, time = "7") {

  # print(paste0(.x$page_name,": ", round(which(internal_page_ids$page_id == .x$page_id)/nrow(internal_page_ids)*100, 2)))

  yo <- get_targeting(.x$page_id, timeframe = glue::glue("LAST_{time}_DAYS")) %>%
    mutate(tstamp = tstamp)

  if(nrow(yo)!=0){
    path <- paste0(glue::glue("provincies/{time}/"),.x$page_id, ".rds")
    # if(file.exists(path)){
    #   ol <- read_rds(path)
    #
    #   saveRDS(yo %>% bind_rows(ol), file = path)
    # } else {

    saveRDS(yo, file = path)
    # }
  }

  # print(nrow(yo))
  # })

}

scraper <- possibly(scraper, otherwise = NULL, quiet = F)


# if(F){
#     # dir("provincies/7", full.names
# }
da30 <- readRDS("data/election_dat30.rds")
da7 <- readRDS("data/election_dat7.rds")

### save seperately
yo <- all_dat %>% #count(cntry, sort  =T) %>%
    # filter(!(page_id %in% already_there)) %>%
  filter(!(page_id %in% unique(da7$page_id))) %>%
  # filter(cntry == "GB") %>%
  # slice(1:10) %>%
  split(1:nrow(.)) %>%
  map_dfr_progress(scraper, 7)

yo <- all_dat %>% #count(cntry, sort  =T) %>%
    # filter(!(page_id %in% already_there)) %>%
    filter(!(page_id %in% unique(da30$page_id))) %>%
    # filter(cntry == "GB") %>%
    # slice(1:10) %>%
    split(1:nrow(.)) %>%
    map_dfr_progress(scraper, 30)

# saveRDS(yo, file = )
library(tidyverse)
da30  <- dir("provincies/30", full.names = T) %>%
  map_dfr_progress(readRDS)  %>%
    mutate(total_spend_formatted = parse_number(total_spend_formatted)) %>%
    rename(page_id = internal_id) %>%
    left_join(all_dat)
da30 %>% count(party)
da7  <- dir("provincies/7", full.names = T) %>%
    map_dfr_progress(readRDS) %>%
    mutate(total_spend_formatted = parse_number(total_spend_formatted)) %>%
    rename(page_id = internal_id) %>%
    left_join(all_dat)

saveRDS(da30, "data/election_dat30.rds")
saveRDS(da7, "data/election_dat7.rds")



bbb %>% filter(str_detect(funding_, "Strijker"))

# da7 %>%
#   distinct(internal_id, .keep_all = T) %>%
#   mutate(total_spend = parse_number(total_spend_formatted)) %>%
#   rename(page_id = internal_id) %>%
#   left_join(internal_page_ids) %>%
#   group_by(party) %>%
#   summarize(total_spend = sum(total_spend))
#
#
# amgna <- da7 %>%
#   mutate(total_spend = parse_number(total_spend_formatted)) %>%
#   rename(page_id = internal_id) %>%
#   left_join(internal_page_ids)
#
#
# amgna %>%
#   filter(type == "gender") %>%
#   filter(value == "Women") %>%
#   # mutate(total_spend = total_spend*total_spend_pct) %>%
#   ggplot(aes(party, total_spend_pct)) +
#   geom_boxplot() #+
#   # scale_y_log10()
#
#
#
# amgna %>%
#   filter(type == "detailed")
