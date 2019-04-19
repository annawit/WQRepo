wdi <- md2 %>%
  select(MLocID, `Sample Time`, DO, Site, `Station Description`) %>% 
  group_by(MLocID, Site, `Station Description`, month.p = floor_date(`Sample Time`, "month")) %>% 
  filter(Site %in% "22394-ORDEQ Nestucca River at first bridge ramp (upstream of Beaver)") %>% 
  summarize(min = min(DO))

p <- plot_ly(wdi,
        x = ~month.p,
        y = ~min,
        text = ~paste("Site:", ~`Station Description`),
        color = ~Site,
        # colors = coul,
        mode = "markers",
        type = "scatter",
        marker = list(size = 10,
                      line = list(color = "#000000",
                                  width = 0.2),
                      alpha = 0.8))
p  


summary(as.factor(md2))
est <- md2 %>% 
  distinct(Site, crit_Instant) %>% 
  arrange(crit_Instant)
as.vector(est$Site)
