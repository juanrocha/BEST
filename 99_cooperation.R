## trying out something with cooperation
## 
source("00_load_libraries.R")
source("01_read_data.R")

df_coop <- left_join(ind_coop1, ind_coop2)

df_coop %>% ggplot(aes(med_coop2, med_coop1, group = ID_player)) + 
  geom_point(aes(color = Treatment)) +
  geom_vline(aes(xintercept = 1), color = "grey50") + 
  geom_hline(aes(yintercept =1 ), color = "grey50") +
  annotate(geom = "text",
           x = c(0.5, 0.5, 1.5, 1.5), y = c(4, 0, 4, 0),
           label = (c("defector to cooperator", "uncodintional cooperator",
                     "unconditional defector", "cooperator to defector"))
           ) +
  geom_abline(slope = 1, intercept = 0) +
  labs(x = "median cooperation stage 2", y ="median cooperation stage 1") +
  #facet_wrap(~Treatment) +
  theme_light()


df_coop %>% ggplot(aes(med_coop2, med_coop1)) + 
  geom_point(aes(color = Treatment)) +
  geom_vline(aes(xintercept = 1), color = "grey50") + 
  geom_hline(aes(yintercept =1 ), color = "grey50") +
  geom_abline(slope = 1, intercept = 0) +
  # geom_smooth(aes(color = Treatment)) +
  labs(x = "median cooperation stage 2", y ="median cooperation stage 1") +
  facet_wrap(~Treatment) +
  theme_light()