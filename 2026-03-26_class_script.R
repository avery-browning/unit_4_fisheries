# 2026-03-26 class script

library(tidyverse)

# https://www.ramlegacy.org/ 
load("data/RAMLDB v4.66/R Data/DBdata[asmt][v4.66].RData")

source("build_collapse_table.r")

glimpse(collapse)

# what characteristic makes it more likely for a fishery to collapse?

head(metadata)

# has a given stock ever collapsed?? is one stock more likely to collaspe than another
model_data = collapse %>%
  group_by(stockid, stocklong) %>%
  summarize(ever_collapsed = any(current_collapse)) %>%
  ungroup() %>%
  left_join(metadata, by=c("stockid", "stocklong")) %>% # stocklong will drop out if I don't group with it
  mutate(FisheryType = as.factor(FisheryType))

glimpse(model_data)

model_l = glm(data = model_data, ever_collapsed ~ FisheryType, family="binomial")
summary(model_l)

newdata = model_data %>% distinct(FisheryType) %>% arrange(FisheryType)
newdata
class(model_l)

model_l_predict = predict(model_l, newdata = newdata, type="response", se.fit=T)
collapse_fishery_type_predictions = cbind(newdata, model_l_predict)
collapse_fishery_type_predictions

# plot

ggplot(data = collapse_fishery_type_predictions) + 
  geom_bar(aes(x = FisheryType, y = fit, fill=FisheryType), stat = "identity", show.legend=F) +
  geom_errorbar(aes(x = FisheryType, ymin = fit - se.fit, ymax = fit + se.fit), width = 0.2) +
  coord_flip()


# poisson model - expects y to have a count distribution

# how many years has the stock been in a collapsed state?
# is the amount of time it's been collapsed a fxn or fishing pressure or biomass?

glimpse(tsmetrics)
tsmetrics %>% filter(tsshort == "BdivBmgtpref")
tsmetrics %>% filter(tsshort == "UdivUmsypref")

u_summary = timeseries_values_views %>%
  filter(!is.na(BdivBmgtpref),
          !is.na(UdivUmsypref)) %>%
  group_by(stockid, stocklong) %>%
  summarize(yrs_data = n()) ratio_yrs_overfished = sum((UdivUmsypref > 1/yrs_data),
  ratio_yrs_low_stock = sum((BdivBmgtpref < 1)/yrs_data)) %>%
select(-yrs_data) %>%
ungroup() %>%
left_join(metadata %>% select(stockid, FisheryType))

glimpse(u_summary)

collapse_summary = collapse %>%
  group_by(stockid, stocklong) %>%
  summarize(yrs_data = n(), yrs_collasped = sum(current_collapse)) %>%
  inner_join(u_summary, by=c("stockid", "stocklong"))

glimpse(collapse_summary)

hist(collapse_summary$yrs_collapsed)
table(collapse_summary$yrs_collapsed)
collapsed_summary_zero_trunc = collapse_summary %>% filter(yrs_collapsed > 0)
table(collapsed_summary_zero_trunc$yrs_collapsed)

model_p = glm(yrs_collapsed ~ FisheryType + ratio_yrs_overfished + ratio_yrs_low_stock,
          offset(log(yrs_data)),
          data = collapsed_summary_zero_trunc,
          family = "poisson")

summary(model_p)

# come back to video

install.packages("AER")
AER::dispersiontest(model_p)

# missing some code here


median_ratio_yrs_low_stock = median(collapsed_summary_zero_trunc$ratio_yrs_low_stock)
FisheryType = collapsed_summary_zero_trunc %>% distinct(FisheryType)

newdata = expand.grid()