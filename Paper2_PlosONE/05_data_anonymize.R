## Anonymize game data
## Juan Rocha
## 200625

source("00_load_libraries.R")
source("01_read_data.R")

anonymous_level <- function(x) {
  l <- levels(x) %>% length()
  ifelse(
    l <= length(letters), 
    nl <- sample(letters, size = l, FALSE),
    nl <- interaction(as_factor(sample(letters, 26, FALSE)), 
                      as_factor(c(1:ceiling(l/26)))) %>%
      levels() %>%
      sample(size = l)
  )
  return(nl[x])
}


dat <- dat %>% 
  select(-Session, -Date, -Player) %>%
  mutate(Place = anonymous_level(Place),
         ID_player = anonymous_level(ID_player),
         group = anonymous_level(group))

# write_csv(dat, "200626_BEST_rawdata_anonymised.csv")
