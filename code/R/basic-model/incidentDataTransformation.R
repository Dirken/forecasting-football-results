###############################################
#incidentDataTransformation
###############################################
value_from_xpath  <- function(element, xpath, to.int = F, index = 1) {
  xml_find_all(element, xpath) %>%
  {ifelse(length(.), xml_text(.[[index]]), NA)} %>%
  {ifelse(to.int, as.integer(.), .)}
}

node_to_dataframe <- function(n, key) {
  tibble_(list(
    id = ~value_from_xpath(n, './id', to.int = T),
    type = ~value_from_xpath(n, './type'),
    subtype1 = ~value_from_xpath(n, './subtype'),
    subtype2 = ~value_from_xpath(n, paste0('./', key, '_type')),
    player1 = ~value_from_xpath(n, './player1'),
    player2 = ~value_from_xpath(n, './player2'),
    team = ~value_from_xpath(n, './team'),
    lon = ~value_from_xpath(n, './coordinates/value', to.int = T, index = 1),
    lat = ~value_from_xpath(n, './coordinates/value', to.int = T, index = 2),
    elapsed = ~value_from_xpath(n, './elapsed', T),
    elapsed_plus = ~value_from_xpath(n, './elapsed_plus', T)
  ))
}
incidents <- map_df(list('goal', 'card', 'foulcommit', 'shoton', 'shotoff', 'cross', 'corner'), function(key) {
  match %>%
    filter(paste0('!is.na(', key, ')')) %>% 
    select('id', key) %>%
    collect() %>% 
    rename_('value' = key) %>%
    pmap_df(function(id, value) {
      xml <- read_xml(value)
      df  <- xml %>%
        xml_find_all(paste0('/', key, '/value')) %>%
        map_df(node_to_dataframe, key)
      
      # Add the id of the game as 'foreign key' game_id
      if (nrow(df) > 0) {
        if (length(xml_find_all(xml, paste0('/', key, '/value/', key, '_type'))) > 0) {
          df %<>%
            rename(tmp = subtype1) %>%
            rename(subtype1 = subtype2) %>%
            rename(subtype2 = tmp)
        }
        df$game_id  <- id
      }
      return(df)
    })
})

# Turn the type fields into factors
incidents$type <- factor(incidents$type)
incidents$subtype1 <- factor(incidents$subtype1)
incidents$subtype2 <- factor(incidents$subtype2)

# clean up the instances where instead of using elapsed_plus, elapsed is greater than 90
# unfortunately we can't know if there were instances of this at halftime too
overtime <- incidents[incidents$elapsed > 90,]$elapsed - 90
incidents[incidents$elapsed > 90,]$elapsed_plus <- overtime
incidents[incidents$elapsed > 90,]$elapsed <- 90

# a bit of feature engineering. Add a half variable and
# a half_elapsed variable, which goes beyond 45/90
incidents %<>% mutate(half = (elapsed %/% 46) + 1)
incidents %<>% mutate(half_elapsed = ifelse(half == 1, elapsed, elapsed - 45))
incidents %<>% mutate(elapsed_plus = ifelse(is.na(elapsed_plus), 0, elapsed_plus))
incidents %<>% mutate(half_elapsed = elapsed + elapsed_plus)

saveRDS(incidents, "./rds/incidents.rds")