require(lubridate)
ipea <-read_csv2("~/Downloads/ipeadata[16-08-2017-03-36].csv", col_names = c("date", "inflation"), skip = 1)

ipea <- separate(ipea, date, c("ano", "mes"), sep = 4)
ipea <- unite(ipea, "data", c("ano", "mes"), sep = " ")

ipea_ts <- ts(ipea$inflation, start = c(1944, 2), end = c(2017, 7), frequency = 12)

test1 <- as.data.frame(splink) %>%
  filter(yearcollected >= 1944) %>%
  mutate(monthcollected = as.numeric(monthcollected)) %>%
  filter(monthcollected %in% 1:12) %>%
  group_by(yearcollected, monthcollected) %>%
  count() %>%
  ungroup() %>%
  unite("data", c("yearcollected", "monthcollected"), sep = " ")

test1 <- test1[-1, ]

ggplot(ipea) +
  geom_line(aes(x = data, y = inflation, group = NA))

p_counts <- ggplot(test1, aes(x = data, y = n, group = NA)) + 
  geom_line() + 
  geom_smooth(span = 0.1) + 
  theme(axis.text.x = element_text(angle = 90, size = 5))

ggsave(p_counts, file = "~/Downloads/p_counts.png", width = 30, height = 10)

as.data.frame(splink) %>%
  group_by(yearcollected) %>%
  count() %>%
  ungroup() %>%
  ggplot() + 
  geom_line(aes(x = yearcollected, y = n)) + 
  annotate("text", x = 1950:2017, y = 50000, label = 1950:2017, size = 1.5)