## Analysis paper II
## Juan Rocha
## juan.rocha@su.se
## 
## Diff-in-diff

## Individual extraction
# clustered:
p4 <- plm(ind_extraction ~ Treatment + part +Treatment * part + as.numeric(Round) + StockSizeBegining , 
          data = pdata.frame(dat, index = c('ID_player' ,'Round', "group")),
          model = "random", random.method = "walhus",
          effect = "nested" )
# coeftest(p4, vcov.=function(x) vcovHC(x, method="white2", type="HC1", cluster = "group"))

## Alternatively with vcovNW with HC4 which is more strict
# coeftest(p4, vcov.=function(x) vcovNW(x,  type="HC4", cluster = "group"))


## Proportion of excraction:
## # clustered:
q4 <- plm(prop ~ Treatment + part +Treatment * part + as.numeric(Round) , 
          data = pdata.frame(dat, index = c('ID_player' ,'Round', "group")),
          model = "random", random.method = "walhus",
          effect = "nested" )
# coeftest(p4, vcov.=function(x) vcovHC(x, method="white2", type="HC1", cluster = "group"))

## Alternatively with vcovNW with HC4 which is more strict
# coeftest(p4, vcov.=function(x) vcovNW(x,  type="HC4", cluster = "group"))


## Cooperation:
# clustered:
c4 <- plm(cooperation2 ~ Treatment + part +Treatment * part + as.numeric(Round) , 
          data = pdata.frame(dat, index = c('ID_player' ,'Round', "group")),
          model = "random", random.method = "walhus",
          effect = "nested" )
# coeftest(c4, vcov.=function(x) vcovHC(x, method="white2", type="HC1", cluster = "group"))

## Alternatively with vcovNW with HC4 which is more strict
# coeftest(c4, vcov.=function(x) vcovNW(x,  type="HC4", cluster = "group"))


#### Calculating difference coefficients for graphic:
#### 
coeffs <- coef(p4)

diff_df <- data_frame(
  treatment = c("Baseline","Threshold","Risk","Uncertainty", "Threshold-Baseline", "Risk-Baseline", "Uncertainty-Baseline", "Threshold-counter", "Risk-counter", "Uncertainty-counter"),
  after = c(
    sum(coeffs[c(1,5)]), # mu and delta = intercept and partTRUE
    sum(coeffs[c(1,5,2,8)]),
    sum(coeffs[c(1,5,3,9)]),
    sum(coeffs[c(1,5,4,10)]),
    sum(coeffs[c(2,8)]),
    sum(coeffs[c(3,9)]),
    sum(coeffs[c(4,10)]),
    sum(coeffs[c(1,5,2)]),
    sum(coeffs[c(1,5,3)]),
    sum(coeffs[c(1,5,4)])
  ),
  before = c(
    coeffs[1],
    sum(coeffs[c(1,2)]),
    sum(coeffs[c(1,3)]),
    sum(coeffs[c(1,4)]),
    coeffs[2],
    coeffs[3],
    coeffs[4],
    sum(coeffs[c(1,2)]),
    sum(coeffs[c(1,3)]),
    sum(coeffs[c(1,4)])
  )
)

diff_df <- diff_df %>%
  mutate(difference = after - before)



## Visualization based on raw data
# dat %>% 
#   group_by(Treatment, part) %>% 
#   summarize(mean_t = mean(ind_extraction))  %>% 
#   ggplot(aes(x = part, y = mean_t)) + 
#   geom_point(aes(color = Treatment)) + 
#   geom_line(aes(group = Treatment, color = Treatment))

## Proportion of stock

coeffs <- coef(q4)

diff_df2 <- data_frame(
  treatment = c("Baseline","Threshold","Risk","Uncertainty", "Threshold-Baseline", "Risk-Baseline", "Uncertainty-Baseline","Threshold-counter", "Risk-counter", "Uncertainty-counter" ),
  after = c(
    sum(coeffs[c(1,5)]), # mu and delta = intercept and partTRUE
    sum(coeffs[c(1,5,2,7)]),
    sum(coeffs[c(1,5,3,8)]),
    sum(coeffs[c(1,5,4,9)]),
    sum(coeffs[c(2,7)]),
    sum(coeffs[c(3,8)]),
    sum(coeffs[c(4,9)]),
    sum(coeffs[c(1,5,2)]),
    sum(coeffs[c(1,5,3)]),
    sum(coeffs[c(1,5,4)])
  ),
  before = c(
    coeffs[1],
    sum(coeffs[c(1,2)]),
    sum(coeffs[c(1,3)]),
    sum(coeffs[c(1,4)]),
    coeffs[2],
    coeffs[3],
    coeffs[4],
    sum(coeffs[c(1,2)]),
    sum(coeffs[c(1,3)]),
    sum(coeffs[c(1,4)])
  )
)

diff_df2 <- diff_df2 %>%
  mutate(difference = after - before)


## Cooperation

coeffs <- coef(c4)

diff_df3 <- data_frame(
  treatment = c("Baseline","Threshold","Risk","Uncertainty", "Threshold-Baseline", "Risk-Baseline", "Uncertainty-Baseline","Threshold-counter", "Risk-counter", "Uncertainty-counter" ),
  after = c(
    sum(coeffs[c(1,5)]), # mu and delta = intercept and partTRUE
    sum(coeffs[c(1,5,2,7)]),
    sum(coeffs[c(1,5,3,8)]),
    sum(coeffs[c(1,5,4,9)]),
    sum(coeffs[c(2,7)]),
    sum(coeffs[c(3,8)]),
    sum(coeffs[c(4,9)]),
    sum(coeffs[c(1,5,2)]),
    sum(coeffs[c(1,5,3)]),
    sum(coeffs[c(1,5,4)])
  ),
  before = c(
    coeffs[1],
    sum(coeffs[c(1,2)]),
    sum(coeffs[c(1,3)]),
    sum(coeffs[c(1,4)]),
    coeffs[2],
    coeffs[3],
    coeffs[4],
    sum(coeffs[c(1,2)]),
    sum(coeffs[c(1,3)]),
    sum(coeffs[c(1,4)])
  )
)

diff_df3 <- diff_df3 %>%
  mutate(difference = after - before)

diff_df$response <- "individual extraction"
diff_df <- diff_df %>%
  mutate(type = ifelse(
    str_detect(treatment, "-Baseline"), "first_difference", 
    ifelse(str_detect(treatment, "-counter"), "counterfactual", "observed"))) %>%
  mutate(treatment = str_remove(treatment, "-Baseline"),
         treatment = str_remove(treatment, "-counter")) %>%
  gather(key = time, value = estimate, before, after) %>%
  mutate(time = as_factor(time), type = as_factor(type), treatment = as_factor(treatment)) %>%
  filter(type != "first_difference")

diff_df2$response <- "proportion stock"
diff_df2 <- diff_df2 %>%
  mutate(type = ifelse(
    str_detect(treatment, "-Baseline"), "first_difference", 
    ifelse(str_detect(treatment, "-counter"), "counterfactual", "observed"))) %>%
  mutate(treatment = str_remove(treatment, "-Baseline"),
         treatment = str_remove(treatment, "-counter")) %>%
  gather(key = time, value = estimate, before, after) %>%
  mutate(time = as_factor(time), type = as_factor(type), treatment = as_factor(treatment)) %>%
  filter(type != "first_difference")


diff_df3$response <- "cooperation"
diff_df3 <- diff_df3 %>%
  mutate(type = ifelse(
    str_detect(treatment, "-Baseline"), "first_difference", 
    ifelse(str_detect(treatment, "-counter"), "counterfactual", "observed"))) %>%
  mutate(treatment = str_remove(treatment, "-Baseline"),
         treatment = str_remove(treatment, "-counter")) %>%
  gather(key = time, value = estimate, before, after) %>%
  mutate(time = as_factor(time), type = as_factor(type), treatment = as_factor(treatment)) %>%
  filter(type != "first_difference")


diff_df4 <- bind_rows(
  diff_df, diff_df2, diff_df3) %>%
  mutate(response = as_factor(response),
         response = fct_relevel(response, "individual extraction", "proportion stock", "cooperation")) # j190929: this stopped working, the ordering of factors


## Linear hypotheses:
hyp_joint_baseline <- c("TreatmentThreshold:partTRUE + TreatmentThreshold = 0",
               "TreatmentRisk:partTRUE + TreatmentRisk = 0",
               "TreatmentUncertainty:partTRUE + TreatmentUncertainty= 0")

hyp_threshold <- "TreatmentThreshold:partTRUE + TreatmentThreshold = 0"
hyp_risk <- "TreatmentRisk:partTRUE + TreatmentRisk = 0"
hyp_uncertainty <- "TreatmentUncertainty:partTRUE + TreatmentUncertainty= 0"

# hyp <- list(hyp_joint_baseline, hyp_threshold, hyp_risk, hyp_uncertainty)
# models <- list(p4, q4, c4)
df_int <- interaction(c("p4", "q4", "c4"),
                      c("hyp_joint_baseline", "hyp_threshold", "hyp_risk", "hyp_uncertainty"),
                      sep = "-") %>%
  levels() %>%
  as_tibble() %>%
  separate(value, into = c("model", "hyp"), sep = "-", remove = TRUE)

##J191110: First time I'm doing this in parallel so woth a note of clarification. 
##I'm storing in a dataframe df_int the interactions I need for all F-tests. It is 
## all combinations of modles, and all combinations of hypotheses. However, I'm not 
## storing the object, only the name of the object as character. So to actually pass the object 
## without replicating it, I found out that one can do it with eval(sym(object)).
## It takes a character vector, make it into a "symbol" or its name, and then evaluate it inside
## the context of the function. Pretty neat trick -- not sure that's metaprogramming but 
## will save time and typing in the future.

hyp_out <- map2(.x = df_int$model , .y = df_int$hyp ,
  .f = function(x,y){
    x <- linearHypothesis(eval(sym(x)), eval(sym(y)), 
                          test = "F", vcov.=function(x) vcovNW(x,  type="HC4", cluster = "group") ) %>%
      tidy() %>% as.data.frame()
    return(x)
  })

df_int <- df_int %>%
  mutate(
    F_statistic = map_dbl(hyp_out, function(x){x[2,3]}),
    p_value = map_dbl(hyp_out, function(x) x[2,4])
  )

## change names to match the graphic:
df_int <- df_int %>%
  mutate(
    hyp = str_remove_all(hyp, "hyp_") %>%
      str_remove_all("joint_"),
    model = ifelse(
      model == "p4", "individual extraction",
      ifelse(model == "q4","proportion stock", "cooperation" ))
  ) %>%
  rename(response = model, treatment = hyp) %>%
  mutate(treatment = str_to_title(treatment)) %>%
  mutate(response = as_factor(response),
         response = fct_relevel(response, "individual extraction", "proportion stock", "cooperation"))

### Figure 1
### 
g_diff <- diff_df4  %>% 
  left_join(df_int) %>% 
  mutate(p.value = ifelse(
    p_value < 0.05, "< 0.05" ,
    ifelse(p_value < 0.1, "< 0.1", "> 0.1")
  )) %>%
  ggplot(aes(time, estimate, group = type)) +
  geom_point() +
  geom_line(aes(linetype = type, color = p.value)) + 
  facet_grid(response ~ treatment, scales = "free_y" ) +
  theme_light(base_size = 6) + theme(legend.position = "bottom") +
  labs()

# ggsave(g_diff, filename = "diff-in-diff.png", device = "png", width = 4, height = 4, units = "in", dpi = 600 )

g_diff
