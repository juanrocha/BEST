## Analysis paper II
## Juan Rocha
## juan.rocha@su.se
## 
## Regressions for SM


## Game data regressions on: 
## individual extraction
# simple
p1 <- plm(ind_extraction ~ Treatment + part + as.numeric(Round) + StockSizeBegining , 
          data = pdata.frame(dat, index = c('ID_player' ,'Round', "group")),
          model = "random", 
          effect = "individual" )
# clustered:
p2 <- plm(ind_extraction ~ Treatment + part + as.numeric(Round) + StockSizeBegining , 
          data = pdata.frame(dat, index = c('ID_player' ,'Round', "group")),
          model = "random", random.method = "walhus",
          effect = "nested" )

# simple
p3 <- plm(ind_extraction ~ Treatment + part + Treatment * part + as.numeric(Round) + StockSizeBegining , 
          data = pdata.frame(dat, index = c('ID_player' ,'Round', "group")),
          model = "random", 
          effect = "individual" )

# clustered:
p4 <- plm(ind_extraction ~ Treatment + part +Treatment * part + as.numeric(Round) + StockSizeBegining , 
          data = pdata.frame(dat, index = c('ID_player' ,'Round', "group")),
          model = "random", random.method = "walhus",
          effect = "nested" )
# # simple
p5 <- plm(ind_extraction ~ Treatment + as.numeric(Round) + StockSizeBegining , 
          data = pdata.frame(dat %>% filter(part == TRUE), index = c('ID_player' ,'Round', "group")),
          model = "random", 
          effect = "individual" )
# clustered:
p6 <- plm(ind_extraction ~ Treatment + as.numeric(Round) + StockSizeBegining , 
          data = pdata.frame(dat %>% filter(part == TRUE), index = c('ID_player' ,'Round', "group")),
          model = "random", random.method = "walhus",
          effect = "nested" )


## proportion of stock extracted
# simple
q1 <- plm(prop ~ Treatment + part + as.numeric(Round)  , 
          data = pdata.frame(dat, index = c('ID_player' ,'Round', "group")),
          model = "random", 
          effect = "individual" )

# clustered:
q2 <- plm(prop ~ Treatment + part + as.numeric(Round)  , 
          data = pdata.frame(dat, index = c('ID_player' ,'Round', "group")),
          model = "random", random.method = "walhus",
          effect = "nested" )
# simple
q3 <- plm(prop ~ Treatment + part + Treatment * part + as.numeric(Round)  , 
          data = pdata.frame(dat, index = c('ID_player' ,'Round', "group")),
          model = "random", random.method = "walhus",
          effect = "individual" )
# clustered:
q4 <- plm(prop ~ Treatment + part +Treatment * part + as.numeric(Round) , 
          data = pdata.frame(dat, index = c('ID_player' ,'Round', "group")),
          model = "random", random.method = "walhus",
          effect = "nested" )
# simple
q5 <- plm(prop ~ Treatment + as.numeric(Round) , 
          data = pdata.frame(dat %>% filter(part == TRUE), index = c('ID_player' ,'Round', "group")),
          model = "random", 
          effect = "individual" )

# clustered:
q6 <- plm(prop ~ Treatment + as.numeric(Round)  , 
          data = pdata.frame(dat %>% filter(part == TRUE), index = c('ID_player' ,'Round', "group")),
          model = "random", random.method = "walhus",
          effect = "nested" )

## and cooperation
# simple
c1 <- plm(cooperation2 ~ Treatment + part + as.numeric(Round)  , 
          data = pdata.frame(dat, index = c('ID_player' ,'Round', "group")),
          model = "random", 
          effect = "individual" )

# clustered:
c2 <- plm(cooperation2 ~ Treatment + part + as.numeric(Round)  , 
          data = pdata.frame(dat, index = c('ID_player' ,'Round', "group")),
          model = "random", random.method = "walhus",
          effect = "nested" )


# simple
c3 <- plm(cooperation2 ~ Treatment + part + Treatment * part + as.numeric(Round)  , 
          data = pdata.frame(dat, index = c('ID_player' ,'Round', "group")),
          model = "random", random.method = "walhus",
          effect = "individual" )

# clustered:
c4 <- plm(cooperation2 ~ Treatment + part +Treatment * part + as.numeric(Round) , 
          data = pdata.frame(dat, index = c('ID_player' ,'Round', "group")),
          model = "random", random.method = "walhus",
          effect = "nested" )

# simple
c5 <- plm(cooperation2 ~ Treatment + as.numeric(Round) , 
          data = pdata.frame(dat %>% filter(part == TRUE), index = c('ID_player' ,'Round', "group")),
          model = "random", 
          effect = "individual" )
# clustered:
c6 <- plm(cooperation2 ~ Treatment + as.numeric(Round)  , 
          data = pdata.frame(dat %>% filter(part == TRUE), index = c('ID_player' ,'Round', "group")),
          model = "random", random.method = "walhus",
          effect = "nested" )

# stargazer::stargazer(
#   # clustered and robust:
#   coeftest(p4, vcov.=function(x) vcovHC(x, method="white2", type="HC1", cluster = "group")),
#   # clustered and robust:
#   coeftest(p4, vcov.=function(x) vcovHC(x, method="white2", type="HC2", cluster = "group")),
#   # clustered and robust:
#   coeftest(p4, vcov.=function(x) vcovHC(x, method="white2", type="HC3", cluster = "group")),
#   # clustered and robust:
#   coeftest(p4, vcov.=function(x) vcovHC(x, method="white2", type="HC4", cluster = "group")),
#   # last model with NW and HC4, more strict
#   coeftest(p4, vcov.=function(x) vcovNW(x,  type="HC4", cluster = "group")),
#   type = "latex", multicolumn = FALSE, header = FALSE, intercept.bottom = FALSE, 
#   model.names= FALSE, font.size = "small", digits = 2, 
#   float = TRUE, no.space = TRUE, single.row = FALSE, align = TRUE,
#   dep.var.caption = "", dep.var.labels.include = FALSE, df = FALSE,
#   title = "Clustered and robust standard errors estimation with White method and (1) HC1, (2) HC2, (3) HC3, (4) HC4 weighting schemes, and (5) Newey and West method with HC4 scheme. The response variable is individual extraction."
# )


### Regressions on surveys:

ind_coop <-  ind_coop %>% 
  # coordination_all is now the coordination score for all rounds, while coordination_2 is for second part
  rename(coordination_all = coordination) %>%  
  # step added to avoid using place names
  mutate(Place = fct_recode(Place, A = "Buenavista", B = "Las Flores", C = "Taganga", D = "Tasajera"))

names(ind_coop) <- str_remove_all(names(ind_coop), pattern = "2" )

# write_csv(ind_coop, path = "ind_coop.csv") # file for Caroline to play around with regressions
#


y_vars <- c("mean_extraction", "mean_prop_extr", "med_coop", "variance", "coordination", "var_extraction", "var_prop_extr")
x_vars <- c("Treatment + Place + education_yr + BD_how_often + fishing_children  + Risk + Amb  + prop_ag")
out1 <-  map2(x_vars, y_vars,
              ~ lm_robust(as.formula(paste(.y, "~", .x)),
                          data = ind_coop %>% filter(part == T) %>% ungroup(),
                          se_type = 'stata', cluster = group)
)

x_vars <- c( "Treatment + education_yr + BD_how_often + fishing_children  + Risk + Amb  + prop_ag")
out2 <-  map2(x_vars, y_vars,
              ~ lm_robust(as.formula(paste(.y, "~", .x)),
                          data = ind_coop %>% filter(part == T) %>% ungroup(),
                          se_type = 'stata', cluster = group)
)

x_vars <- c( "Treatment + Place")
out3 <-  map2(x_vars, y_vars,
              ~ lm_robust(as.formula(paste(.y, "~", .x)),
                          data = ind_coop %>% filter(part == T) %>% ungroup(),
                          se_type = 'stata', cluster = group)
)

x_vars <- c( "Treatment + education_yr + BD_how_often + fishing_children + fishing_future + Risk + group_fishing + Amb  + prop_ag  ")
out4 <-  map2(x_vars, y_vars,
              ~ lm_robust(as.formula(paste(.y, "~", .x)),
                          data = ind_coop %>% filter(part == T) %>% ungroup(),
                          se_type = 'stata', cluster = group)
)

df_rsqr <- tibble(
  original =   out1 %>% map(., summary) %>%  map(.,"adj.r.squared") %>% unlist(),
  no_place = out2 %>% map(., summary) %>%  map(.,"adj.r.squared") %>% unlist(),
  just_place = out3 %>% map(., summary) %>%  map(.,"adj.r.squared") %>% unlist(),
  extras = out4 %>% map(., summary) %>%  map(.,"adj.r.squared") %>% unlist(),
  vars = y_vars
)

y_vars <- c("mean_extraction", "mean_prop_extr", "med_coop", "variance", "coordination", "var_extraction", "var_prop_extr")
x_vars <- c("Treatment + Place + education_yr + BD_how_often + fishing_children  + Risk + Amb  + prop_ag")
out5 <-  map2(x_vars, y_vars,
              ~ lm(as.formula(paste(.y, "~", .x)),
                   data = ind_coop %>% filter(part == T) %>% ungroup()
              )
)

x_vars <- c( "Treatment + education_yr + BD_how_often + fishing_children  + Risk + Amb  + prop_ag")
out6 <-  map2(x_vars, y_vars,
              ~ lm(as.formula(paste(.y, "~", .x)),
                   data = ind_coop %>% filter(part == T) %>% ungroup()
              )
)

x_vars <- c( "Treatment + Place")
out7 <-  map2(x_vars, y_vars,
              ~ lm(as.formula(paste(.y, "~", .x)),
                   data = ind_coop %>% filter(part == T) %>% ungroup()
              )
)


x_vars <- c( "Treatment + education_yr + BD_how_often + fishing_children + fishing_future + Risk + group_fishing + sharing_art + Amb  + prop_ag  ")
out8 <-  map2(x_vars, y_vars,
              ~ lm(as.formula(paste(.y, "~", .x)),
                   data = ind_coop %>% filter(part == T) %>% ungroup()
              )
)

## Manual regressions to implement Caroline's suggestion of using part one as predictor of part 2.

## The final regression for the survey will contain a control variable for behaviour in the first part of the game
ind_coop <- ind_coop %>%
  select(-part) %>% ungroup() %>%
  left_join(ind_coop1 %>% select(-Place))

# history_rs?
y_vars <- c("mean_extraction", "mean_prop_extr", "med_coop", "variance", "var_extraction", "var_prop_extr", "coordination")
x_vars <- c("Treatment + Place + education_yr + BD_how_often + fishing_children + sharing_art + group_fishing + Risk + Amb  + prop_ag")
z_vars <- c("mean_extraction1", "mean_prop_extr1", "med_coop1", "variance1", "var_extraction1", "var_prop_extr1", "coordination1")

rhs <- map2(.x = x_vars, .y = z_vars,
            ~ paste(.x, .y, sep = " + "))

out <-  map2(rhs, y_vars,
             ~ lm_robust(as.formula(paste(.y, "~", .x)),
                         data = ind_coop  %>% ungroup(),
                         se_type = 'CR2', cluster = group)
)

## Note 191106: Stargazer does not currently recognize objects of the class lm_robust.
## to cirunvent that problem I create the same regression with lm and then manually
## modify coefficients, se, and p-values. See code below. The lm results are only used as 
## template, table results are replaced manually.
out_lm2 <-  map2(rhs, y_vars,
                 ~ lm(as.formula(paste(.y, "~", .x)),
                      data = ind_coop  %>% ungroup())
)


## without place
x_vars <- c("Treatment + education_yr + BD_how_often + fishing_children + sharing_art + group_fishing + Risk + Amb  + prop_ag")
rhs <- map2(.x = x_vars, .y = z_vars,
            ~ paste(.x, .y, sep = " + "))

out_noplace <-  map2(rhs, y_vars,
                     ~ lm_robust(as.formula(paste(.y, "~", .x)),
                                 data = ind_coop  %>% ungroup(),
                                 se_type = 'CR2', cluster = group)
)

out_lm_noplace <-  map2(rhs, y_vars,
                        ~ lm(as.formula(paste(.y, "~", .x)),
                             data = ind_coop  %>% ungroup())
)

## only place
x_vars <- c( "Treatment + Place")
rhs <- map2(.x = x_vars, .y = z_vars,
            ~ paste(.x, .y, sep = " + "))

out_onlyplace <-  map2(rhs, y_vars,
                       ~ lm_robust(as.formula(paste(.y, "~", .x)),
                                   data = ind_coop  %>% ungroup(),
                                   se_type = 'CR2', cluster = group)
)

out_lm_onlyplace <-  map2(rhs, y_vars,
                          ~ lm(as.formula(paste(.y, "~", .x)),
                               data = ind_coop  %>% ungroup())
)



df_rsqr <- tibble(
  original =   out %>% map(., summary) %>%  map(.,"adj.r.squared") %>% unlist(),
  no_place = out_noplace %>% map(., summary) %>%  map(.,"adj.r.squared") %>% unlist(),
  just_place = out_onlyplace %>% map(., summary) %>%  map(.,"adj.r.squared") %>% unlist(),
  vars = y_vars
)


# save.image(file = "Regressions_paper2_200527.RData", safe = TRUE)


#J200908: regression suggested by reviewer2:

edu <- lm_robust(
  prop_ag ~ Treatment + Place + education_yr + BD_how_often + fishing_children 
  + Risk + Amb,
  data = ind_coop %>% filter(part == T) %>% ungroup(),
  se_type = 'stata', cluster = group) 