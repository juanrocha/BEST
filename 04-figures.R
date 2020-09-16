## Script to reproduce the figures
## Juan Rocha
## juan.rocha@su.se
## 200625

## Run scripts 00-03 first, it will load libraries, read and clean data
## Figure 1 is also available on script 03
#### Figure 1new ##### requested by reviewers:
df_payoff <- tibble(
  baseline = c(rep(0,5),rep(5,14), rep(10,16), rep(5,10), rep(0,5)),
  treatments = c(rep(0,5),rep(1,22), rep(10,8), rep(5,10), rep(0,5)),
  stock_size = c(1:50)
)

g1 <- df_payoff %>%
  pivot_longer(cols = 1:2, names_to = "treatments", values_to = "rate") %>%
  ggplot(aes(stock_size, rate)) + 
  geom_col(aes(fill = treatments), position = "dodge", show.legend = FALSE) +
  facet_wrap(~treatments) + labs(x = "Stock size", y = "Reproduction rate") +
  scale_fill_brewer(palette = "Pastel1") +
  theme_light(base_size = 8)
# 
# g1 <- ggplot(
#   data = tibble(x = seq(0,50, by = 0.01)), aes(x)) +
#   stat_function(fun = function(x) {1*x*(1-(x/50))},
#                 color = "blue", size = 0.1) +
#   stat_function(fun = )
ggsave(g1, filename = "fig1_payoff.eps", device = "eps", width = 4, height = 2, units = "in", dpi = 800 )
#### Figure 2 ####
### 
g_diff <- diff_df4  %>% 
  left_join(df_int) %>% 
  mutate(p.value = ifelse(
    p_value < 0.05, "< 0.05" ,
    ifelse(p_value < 0.1, "< 0.1", "> 0.1")
  )) %>%
  ggplot(aes(time, estimate, group = type)) +
  geom_point(aes(color = p.value)) +
  geom_line(aes(linetype = type, color = p.value)) + 
  scale_color_manual(values = c("dodgerblue", "orange", "purple")) +
  facet_grid(response ~ treatment, scales = "free_y" ) +
  theme_light(base_size = 8) + 
  theme(legend.position = "bottom",
        legend.text = element_text(size = 6), 
        legend.title = element_text(size = 6)) +
  labs()

# ggsave(g_diff, filename = "diff-in-diff.eps", device = "eps", width = 4, height = 4, units = "in", dpi = 800 )

g_diff


#### Figure 4 ####

ind_coop <-  ind_coop %>% 
  # coordination_all is now the coordination score for all rounds, while coordination_2 is for second part
  rename(coordination_all = coordination) %>%  
  # step added to avoid using place names
  mutate(Place = fct_recode(Place, A = "Buenavista", B = "Las Flores", C = "Taganga", D = "Tasajera"))

names(ind_coop) <- str_remove_all(names(ind_coop), pattern = "2" )

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


g_reg2 <- out1 %>%
  map(tidy) %>%
  map2(., .y = y_vars , function(x,y) {x$model <- y; return(x)}) %>%
  bind_rows() %>% #pull(model) %>% unique()
  mutate( # correct the names of terms
    term = str_replace_all(string = term, pattern = "\\(Intercept\\)", replacement = "Intercept"),
    term = str_replace_all(string = term, pattern = "age", replacement = "Age [years]"),
    term = str_replace_all(string = term, pattern = "education_yr", replacement = "Education [years]"),
    term = str_replace_all(string = term, pattern = "BD_how_often",
                           replacement = "How often do you have bad days?\n [1:yearly - 4:daily]"),
    term = str_replace_all(term, "^Risk", "Risk aversion"),
    term = str_replace_all(term, "Amb", "Ambiguity aversion"),
    term = str_replace_all(term, "prop_ag", "% rounds with agreements"),
    term = str_replace_all(term, "fishing_children", "Do you expect your children to fish?\n[0:No, 1:Yes]")
    ## correct on data and regressions
  ) %>%
  mutate(
    model = str_replace_all(model, "mean_extraction", "mean extraction"),
    model = str_replace_all(model, "mean_prop_extr", "mean %\n extraction"),
    model = str_replace_all(model, "med_coop", "median\n cooperation"),
    model = str_replace_all(model, "variance", "variance\n cooperation"),
    model = str_replace_all(model, "var_extraction", "variance\n extraction"),
    model = str_replace_all(model, "var_prop_extr", "variance %\n extraction"),
    model = factor(model, levels =
                     c("mean extraction", "mean %\n extraction", "variance\n extraction", "variance %\n extraction",
                       "median\n cooperation", "variance\n cooperation", "coordination"))
  ) %>%
  mutate(
    var_type = ifelse(
      str_detect(term, "Treatment"), "Treatment",
      ifelse(
        str_detect(term, "Place"), "Place",
        ifelse(
          str_detect(term, "Intercept"), ".", "Socio-economic aspects")))
  ) %>% # pull(var_type) %>% unique()
  mutate(
    term = str_remove(term, "Treatment"),
    term = str_remove(term, "Place"),
    var_type = factor(var_type, levels = c("." ,"Treatment", "Place", "Socio-economic aspects") )
  ) %>%
  
  mutate(term = factor(term, levels = rev(unique(term)))) %>%
  mutate(conf.high = estimate + std.error,
         conf.low = estimate - std.error,) %>%
  mutate(p_value = ifelse(
    p.value < 0.05, "< 0.05" ,
    ifelse(p.value < 0.1, "< 0.1", "> 0.1")
  )) %>%
  ggplot(aes(x = estimate, y = term)) +
  geom_vline(xintercept = 0, color = "grey84", linetype = 2, size = 0.5) +
  geom_point(aes(shape = p_value, color = p_value), size = 2, show.legend = TRUE) +
  scale_shape_manual(name = "p value", values = c(19,7,1)) +
  geom_errorbarh(
    aes(xmin = conf.low, xmax = conf.high, height = .25, color = p_value), 
    size = 0.3) +
  scale_x_continuous(minor_breaks = NULL, breaks = scales::pretty_breaks(n=3)) +
  scale_color_manual(name = "p value",values = c("dodgerblue", "orange", "purple")) +
  theme_light(base_size = 7) +
  theme(legend.position = "bottom", axis.title.y = element_blank(), axis.text.x = element_text(size = 5) ) +
  facet_grid(var_type ~ model, scales = "free", switch = "y", space = "free_y")
#ggtitle("What does explain the behaviour of individuals?") #subtitle = "Robust estimation with standard errors HC1 (aka. Stata) and clustered around groups"


# ggsave(g_reg2, filename = "fig3_regression_cooperation.eps", device = "eps", width = 6, height = 4, units = "in", dpi = 600 )
g_reg2

# quartz(width = 6, height = 4, pointsize = 6)
# g_reg2
# quartz.save(file = "fig3_regression_cooperation.pdf", type = "pdf", width = 6, height = 4, pointsize = 6, dpi = 600, bg = "white")







#### Figure 3 ####
ind_coop <- ind_coop %>% select(-coordination) 
names(ind_coop) <- str_remove(names(ind_coop), "2")  
  
# g1 <- ind_coop %>% ungroup() %>%
#   filter(part == TRUE) %>%
#   dplyr::select(Treatment, med_coop, coordination) %>%
#   ggplot(aes(coordination)) +
#   geom_density(aes(color = Treatment, fill = Treatment),
#                alpha = 0.4,
#                show.legend = FALSE) +
#   geom_rug(aes(color = Treatment),
#            alpha = 0.4, sides = "r",
#            show.legend = FALSE,
#            length = unit(0.05, "npc")) +
#   xlab("") + ylab("") + scale_x_continuous(position = "top") +
#   coord_flip() + scale_y_reverse() + labs(tag = "A") +
#   theme_light(base_size = 8) + theme(axis.title.y = element_blank())

# g3 <- ind_coop %>% ungroup() %>%
#   filter(part == TRUE) %>%
#   dplyr::select(Treatment, med_coop, coordination) %>%
#   ggplot(aes(med_coop)) +
#   geom_density(aes(color = Treatment, fill = Treatment),
#                alpha = 0.4,
#                show.legend = FALSE) +
#   geom_rug(aes(color = Treatment),
#            alpha = 0.4, sides = "t",
#            show.legend = FALSE,
#            length = unit(0.05, "npc")) +
#   xlab("") + ylab("") + scale_x_continuous(position = "top") +
#   scale_y_reverse() +
#   theme_light(base_size = 8) + theme(axis.title.x = element_blank())

g2 <- ind_coop %>% ungroup() %>%
  filter(part == TRUE) %>%
  dplyr::select(Treatment, med_coop, coordination) %>%
  ggplot(aes( med_coop, coordination)) +
  # geom_rect(aes(xmin = 0, xmax = max(coordination)+0.1, ymin = 0, ymax = 1),
  #           fill = "skyblue", alpha = , color = "skyblue") +
  geom_vline(xintercept = 1, color = "orange") +
  geom_point(aes(color = Treatment), size = 1,
             alpha = 1,
             show.legend = FALSE) +
  geom_text(
    data = data.frame(
      x = c(0.5, 1.5),
      y = c(0.1, 0.1),
      text = c("cooperators", "defectors"),
      color = c("blue", "red")),
    aes(x = x,y = y,label = text),
    color = c("blue", "red"), size = 3,
    show.legend = FALSE) +
  scale_color_brewer(palette = "Spectral", direction = -1) +
  labs(x = "median cooperation", tag = "B") +
  theme_light(base_size = 8)

# library(ggpubr)
# 
# leg <- ind_coop %>% ungroup() %>%
#   filter(part == TRUE) %>%
#   dplyr::select(Treatment, med_coop, coordination) %>%
#   ggplot(aes(med_coop)) +
#   geom_density(aes(color = Treatment, fill = Treatment),
#                alpha = 0.4,
#                show.legend = TRUE) +
#   geom_rug(aes(color = Treatment),
#            alpha = 0.4, sides = "r",
#            show.legend = TRUE) +
#   coord_flip() + scale_y_reverse() +
#   theme_light(base_size = 8) +
#   theme(legend.position = "bottom")
# 
# legend <- leg %>%
#   get_legend() %>%
#   as_ggplot()

# g5 <- ind_coop %>% ungroup() %>%
#   filter(part == TRUE) %>%
#   dplyr::select(Treatment, mean_prop_extr) %>%
#   ggplot(aes(mean_prop_extr)) +
#   geom_density(aes(color = Treatment, fill = Treatment),
#                alpha = 0.4,
#                show.legend = FALSE) +
#   geom_rug(aes(color = Treatment),
#            alpha = 0.4, sides = "r",
#            show.legend = FALSE) +
#   xlab("") + ylab("") + scale_x_continuous(position = "top") + #xlab("mean % extraction") +
#   coord_flip() + scale_y_reverse() + labs(tag = "B") +
#   theme_light(base_size = 8) + theme(axis.title.y = element_blank())

# g6 <- ind_coop %>% ungroup() %>%
#   filter(part == TRUE) %>%
#   dplyr::select(Treatment, mean_extraction) %>%
#   ggplot(aes(mean_extraction)) +
#   geom_density(aes(color = Treatment, fill = Treatment),
#                alpha = 0.4,
#                show.legend = FALSE) +
#   geom_rug(aes(color = Treatment),
#            alpha = 0.4, sides = "t",
#            show.legend = FALSE) +
#   xlab("") + ylab("") + scale_x_continuous(position = "top") + #xlab("mean extraction") +
#   scale_y_reverse() +
#   theme_light(base_size = 8) + theme(axis.title.x = element_blank())

g1 <- ind_coop %>% ungroup() %>%
  filter(part == TRUE) %>%
  dplyr::select(Treatment, mean_extraction, mean_prop_extr) %>%
  ggplot(aes( mean_extraction, mean_prop_extr)) + 
  geom_point(aes(color = Treatment), size = 1, 
             alpha = 1, 
             show.legend = TRUE) + 
  scale_color_brewer(palette = "Spectral", direction = -1) +
  labs(x ="mean extraction", y ="mean % extraction", tag = "A") +
  theme_light(base_size = 7) #+
  # theme(legend.position = c(0.15, 0.85))

# g8 <- ind_coop %>% ungroup() %>%
#   filter(part == TRUE) %>%
#   dplyr::select(Treatment, var_extraction, var_prop_extr) %>%
#   ggplot(aes( var_extraction, var_prop_extr)) + 
#   geom_point(aes(color = Treatment), size = 1, 
#              alpha = 0.7, 
#              show.legend = FALSE) + 
#   xlab("variance extraction") + ylab("variance % extraction") +
#   theme_light(base_size = 7)
# 
# g9 <- ind_coop %>% ungroup() %>%
#   filter(part == TRUE) %>%
#   dplyr::select(Treatment, variance, coordination) %>%
#   ggplot(aes( variance, coordination)) + 
#   geom_point(aes(color = Treatment), size = 1, 
#              alpha = 0.7, 
#              show.legend = FALSE) + 
#   xlab("variance cooperation") + ylab("coordination") +
#   theme_light(base_size = 7)

# source('~/Dropbox/Code/multiplot.R')

#g <- list(g1, g2, g5, g7, legend, g3,  legend, g6, g_reg2)
# g <- list(g1, g2, g5, g7, g9, g3, g8, g6, legend)
# layout <- matrix(data = c(1,2,2,3,4,4,
#                           1,2,2,3,4,4,
#                           5,6,6,7,8,8,
#                           9,9,9,9,9,9),
#                  nrow = 4, ncol = 6, byrow = TRUE)

library(patchwork)
# g <- list(g1,g2)
# layout <- matrix(c(1,2), nrow = 1, ncol = 2, byrow = TRUE)

g1 +g2 + plot_layout(guides = 'collect', ncol = 2) &
  theme(legend.position = "bottom")

# quartz(width = 5, height = 2.5, pointsize = 8)
# multiplot(plotlist = g, layout = matrix(c(1:8, rep(9,8)), 4,4, byrow = TRUE))
# multiplot(plotlist = g, layout = layout)

# quartz.save(file="fig2_response_variables.tiff", type = "tiff", width = 7, height = 6, pointsize = 8, dpi = 600, bg= "white")

## I get erros with eps because it does not support semi transparencies (alpha values)
ggsave(
  filename = "fig2_descriptive_data.eps",
  plot =last_plot(),
  device = "eps", width = 5, height = 2.5, units = "in", dpi = 800 )


#### Fig S5 ####

pm <- GGally::ggpairs(
  data = ind_coop %>% filter(part == TRUE) %>%
    rename(
      `mean extraction` = mean_extraction,
      `mean % extraction` = mean_prop_extr,
      `variance extraction` = var_extraction,
      `variance % extraction` = var_prop_extr,
      `median cooperation` = med_coop
    ),
  columns = c(3:6,13,14),
  aes(color = Treatment, fill = Treatment),
  upper = list(continuous = wrap("cor", size = 2, alignPercent = 0.95, hjust = 0.1)),
  diag = list(continuous = wrap("densityDiag", size =0.4, alpha = 0.25)),
  lower = list(continuous = wrap("points", size = 0.5))
)

pm <- pm + theme_light(base_size = 6) + theme(legend.position = "bottom")

# ggsave(pm, filename = "correlations.tiff", device = "tiff", width = 5, height = 5, units = "in", dpi = 800 )

pm

# quartz(width = 5, height = 5, pointsize = 6)
# 
# quartz.save(file = "correlations.pdf", type = "pdf", width = 5, height = 5, pointsize = 6, dpi = 600)