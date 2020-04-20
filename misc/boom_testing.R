library(foreign)
library(tidyverse)
boom <- read.spss("Boom_data.sav", to.data.frame = T) %>%
  as.tibble() %>%
  janitor::clean_names() %>% 
  mutate(
    ssid = trimws(as.character(ssid), "both"),
    # ?? What's with this random line that is out of place?
    # vid_game = ifelse(ssid == "WPR052", "Wii Play (WP)", as.character(vid_game)),
    # condition = ifelse(ssid == "WPR052", "TG-RegControl", as.character(condition)),
    vid_game = factor(vid_game, levels = c("Mario Galaxy (MG)",
                                           "Wii Play (WP)",
                                           "Resident Evil (RE)")))

boom_proc <- boom %>% filter(complete.cases(.))
contrasts(boom_proc$vid_game)  <- contr.poly(3) # of levels, i.e. 3 
mod1 <- aov(headshot ~ vid_game, data = boom_proc)
mod2 <- aov(headshot ~ vid_game + (sex + gn_own_all + fire_gun + gun_att + vg_shoot + hits), data = boom_proc)

anova(mod1, mod2)

summary(mod2)
