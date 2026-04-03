# 2026-03-24 class script - missed

load("data/RAMLDB v4.66/R Data/DBdata[asmt][v4.66].RData")

head(tsmetrics)
head(timeseries)

timeseries_tsmetrics = left_join(timeseries, tsmetrics, by = c("tsid" = "tsunique"))
dim(timeseries)
dim(timeseries_tsmetrics)
head(timeseries_tsmetrics)

# come back to zoom recording at 10 minute mark 