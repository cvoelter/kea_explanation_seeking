

walk.away.data$condition<-fct_relevel(as.factor(walk.away.data$condition), "blocked", "teasing", "clumsy")
walk.away.data$condition2 <- jitter(as.numeric(as.factor(walk.away.data$condition), amount = .0001))

library(gghalves)


away_plot <- ggplot(data = walk.away.data, aes(x = condition, y= mean_prop_away_time, group=condition)) +
  geom_line(aes(x = condition2, group = subject), color = "darkgray", lty = 1, alpha = .3) +
  geom_point(data = walk.away.data %>% filter(condition == "blocked"), aes(x = condition2), color = "dodgerblue", size = 1.5, alpha = .5) 