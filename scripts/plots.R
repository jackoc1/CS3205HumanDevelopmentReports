hdi.pivot.df <- read.csv("../data/hdi_pivoted.csv")

country.names <- sort(unique(hdi.pivot.df$country_name))

min.hdi <- c()
max.hdi <- c()
for (country in country.names) {
  min.hdi <- c(min.hdi, min(hdi.pivot.df[hdi.pivot.df$country_name==country, "hdi"], na.rm=T))
  max.hdi <- c(max.hdi, max(hdi.pivot.df[hdi.pivot.df$country_name==country, "hdi"], na.rm=T))
}
min.max.hdi.df <- data.frame(country.names, min.hdi, max.hdi)

country_angles_1 <- -1 * c(country.names)[1:94] * 360 / 189 + 90
country_angles_2 <- -1 * c(country.names)[95:189] * 360 / 189 - 90

#

ggplot(min.max.hdi.df) +
  geom_bar(aes(x=country.names, y=max.hdi, group=country.names), alpha=0.7, fill="red", colour="red", width = 1, position="identity", stat = "identity") +
  geom_bar(aes(x=country.names, y=min.hdi, group=country.names), alpha=0.7, fill="blue", colour="blue", width = 1, position="identity", stat="identity") +
  geom_text(data=min.max.hdi.df[1:94,], aes(x=country.names, y=max.hdi, label=country.names, group=country.names), angle=country_angles_1, position="identity", hjust=-0.3, size=3) +
  geom_text(data=min.max.hdi.df[95:189,], aes(x=country.names, y=max.hdi, label=country.names, group=country.names), angle=country_angles_2, position="identity", hjust=1.1, size=3) +
  scale_y_continuous(breaks = 0:nlevels(country), expand = expand_scale(mult=0, add=0)) +
  theme_gray() +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.line = element_blank()) +
  coord_polar() +
  ggtitle("Max vs Min HDI over 1990-2019 period")
  theme(legend.position="none",
        plot.margin=margin(0, 0, 0, 0, "cm"),
        )

        