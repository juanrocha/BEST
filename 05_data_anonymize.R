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
    nl <- interaction(letters, letters[1:ceiling(l/26)]) %>%
      levels() %>%
      as.character() %>%
      sample(size = l, replace = FALSE) 
  )
  
  return(nl[x])
}


## test the function:
forcats::gss_cat$denom %>% levels()
forcats::gss_cat$denom %>%   
  anonymous_level() %>%
  as.factor() %>% levels() %>% length()

## Select a mini-survey with only the questions used:
### Survey data:

ind_coop <-  ind_coop %>% 
  # coordination_all is now the coordination score for all rounds, while coordination_2 is for second part
  rename(coordination_all = coordination) %>%  
  # step added to avoid using place names
  mutate(
    Place = fct_recode(Place, A = "Buenavista", B = "Las Flores", C = "Taganga", D = "Tasajera"),
    ID_player = as_factor(ID_player))

names(ind_coop) <- str_remove_all(names(ind_coop), pattern = "2" )

mini_surv <- ind_coop %>%
  ungroup() %>%
  # remove tested but unused vars in the analysis
  select(mean_extraction, mean_prop_extr, med_coop, variance, coordination, var_extraction,
         var_prop_extr, Treatment, education_yr,  BD_how_often, fishing_children, fishing_future,
         Risk, group_fishing, sharing_art, Amb, prop_ag, ID_player, Treatment, Place, group ) %>%
  ## anonymize  
  mutate(
    ID_player_anonym = anonymous_level(ID_player),
    group_anonym = anonymous_level(group)
  )


## Merge the new IDs from survey with the game and delete the identifiable vars
dat <- dat %>% 
  select(-Session, -Date, -Player) %>%
  mutate(
    Place = fct_recode(Place, A = "Buenavista", B = "Las Flores", 
                       C = "Taganga", D = "Tasajera")) %>%
  left_join(select(mini_surv, ID_player, ID_player_anonym, group, group_anonym)) %>%
  select(-ID_player, -group)

## Now delete them from mini survey
mini_surv <- mini_surv %>%
  select(-ID_player, -group)


## Write data to csv files:
# write_csv(dat, "200928_BEST_rawdata_anonymised.csv")
# write_csv(mini_surv, "200928_BEST_mini_survey_anonymised.csv")
