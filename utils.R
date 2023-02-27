calc_targeting <- function(only_tags) {

    total_sppppeen <- only_tags %>%
        distinct(internal_id, .keep_all = T)  %>%
        # mutate(total_spend = readr::parse_number(total_spend_formatted)) %>%
        mutate(total_spend = ifelse(total_spend == 100, 1, total_spend)) %>%
        select(internal_id, total_spend) %>%
        arrange(desc(total_spend)) %>%
        summarize(total_spend = sum(total_spend))

    howmuchisinterest <- only_tags %>%
        filter(type == "detailed") %>%
        group_by(internal_id) %>%
        filter(total_spend_pct == max(total_spend_pct)) %>%
        slice(1) %>%
        ungroup() %>%
        # mutate(total_spend = readr::parse_number(total_spend_formatted)) %>%
        mutate(total_spend = ifelse(total_spend == 100, 1, total_spend)) %>%
        mutate(spend_per = total_spend * total_spend_pct) %>%
        select(internal_id, spend_per) %>%
        arrange(desc(spend_per)) %>%
        summarize(spend_per = sum(spend_per)) %>%
        mutate(target = "interest")

    howmuchislocation <- only_tags %>%
        filter(type == "location") %>%
        group_by(internal_id, location_type) %>%
        filter(total_spend_pct == max(total_spend_pct)) %>%
        slice(1) %>%
        ungroup() %>%
        # mutate(total_spend = readr::parse_number(total_spend_formatted)) %>%
        mutate(total_spend = ifelse(total_spend == 100, 1, total_spend)) %>%
        mutate(spend_per = total_spend * total_spend_pct) %>%
        select(internal_id, spend_per, location_type) %>%
        arrange(desc(spend_per)) %>%
        group_by(location_type) %>%
        summarize(spend_per = sum(spend_per)) %>%
        rename(target = location_type)

    howmuchisage <- only_tags %>%
        filter(type == "age") %>%
        filter(total_spend_pct != 0) %>%
        group_by(internal_id) %>%
        mutate(n_ages = n()) %>% #count(n_ages, sort = T)
        ungroup() %>%
        filter(n_ages <= 47) %>%
        group_by(internal_id) %>%
        filter(total_spend_pct == max(total_spend_pct)) %>%
        slice(1) %>%
        ungroup() %>%
        # mutate(total_spend = readr::parse_number(total_spend_formatted)) %>%
        mutate(total_spend = ifelse(total_spend == 100, 1, total_spend)) %>%
        mutate(spend_per = total_spend * total_spend_pct) %>%
        select(internal_id, spend_per) %>%
        summarize(spend_per = sum(spend_per))  %>%
        mutate(target = "age")

    howmuchisgender <- only_tags %>%
        filter(type == "gender") %>%
        filter(total_spend_pct != 0) %>%
        filter(value != "All") %>%
        group_by(internal_id) %>%
        filter(total_spend_pct == max(total_spend_pct)) %>%
        slice(1) %>%
        ungroup() %>%
        # mutate(total_spend = readr::parse_number(total_spend_formatted)) %>%
        mutate(total_spend = ifelse(total_spend == 100, 1, total_spend)) %>%
        mutate(spend_per = total_spend * total_spend_pct) %>%
        select(internal_id, spend_per) %>%
        summarize(spend_per = sum(spend_per))  %>%
        mutate(target = "gender")

    howmuchcustom <- only_tags %>%
        filter(type == "custom_audience") %>%
        filter(total_spend_pct != 0) %>%
        # filter(value != "All") %>%
        group_by(internal_id) %>%
        filter(total_spend_pct == max(total_spend_pct)) %>%
        slice(1) %>%
        ungroup() %>%
        # mutate(total_spend = readr::parse_number(total_spend_formatted)) %>%
        mutate(total_spend = ifelse(total_spend == 100, 1, total_spend)) %>%
        mutate(spend_per = total_spend * total_spend_pct) %>%
        select(internal_id, spend_per) %>%
        summarize(spend_per = sum(spend_per))  %>%
        mutate(target = "custom_audience")


    howmuchlookalike <- only_tags %>%
        filter(type == "lookalike_audience") %>%
        filter(total_spend_pct != 0) %>%
        # filter(value != "All") %>%
        group_by(internal_id) %>%
        filter(total_spend_pct == max(total_spend_pct)) %>%
        slice(1) %>%
        ungroup() %>%
        # mutate(total_spend = readr::parse_number(total_spend_formatted)) %>%
        mutate(total_spend = ifelse(total_spend == 100, 1, total_spend)) %>%
        mutate(spend_per = total_spend * total_spend_pct) %>%
        select(internal_id, spend_per) %>%
        summarize(spend_per = sum(spend_per))  %>%
        mutate(target = "lookalike_audience")

    howmuchlanguage <- only_tags %>%
        filter(type == "language") %>%
        filter(total_spend_pct != 0) %>%
        drop_na(value) %>%
        # filter(value != "All") %>%
        group_by(internal_id) %>%
        filter(total_spend_pct == max(total_spend_pct)) %>%
        slice(1) %>%
        ungroup() %>%
        # mutate(total_spend = readr::parse_number(total_spend_formatted)) %>%
        mutate(total_spend = ifelse(total_spend == 100, 1, total_spend)) %>%
        mutate(spend_per = total_spend * total_spend_pct) %>%
        select(internal_id, spend_per) %>%
        summarize(spend_per = sum(spend_per))  %>%
        mutate(target = "language")

    targeting_on_each <- howmuchisinterest %>%
        bind_rows(howmuchislocation) %>%
        bind_rows(howmuchisage) %>%
        bind_rows(howmuchisgender) %>%
        bind_rows(howmuchcustom) %>%
        bind_rows(howmuchlookalike) %>%
        bind_rows(howmuchlanguage) %>%
        mutate(total = total_sppppeen$total_spend) %>%
        mutate(perc = spend_per/total*100) %>%
        arrange(desc(perc))

    return(targeting_on_each)
}

relationshipstuff <- "Recently moved|Away|[r|R]elationship|Parents|Partner|Separated|Divorced|Single|Complicated|Married|Engaged|Newlywed|Civil Union|Unspecified"


add_ribbons <- function(x, adv, col) {
    x %>%
        tab_options(table.width = pct(100)) %>%
        tab_style(
            style = cell_borders(
                sides = c("left"),
                color = col,
                weight = px(18.5),
                style = "solid"
            ),
            locations = cells_body(
                columns = `Numeber of Advertisers`,
                rows = adv
            ))
}
