# 2026-03-24 class script - missed

load("data/RAMLDB v4.66/R Data/DBdata[asmt][v4.66].RData")

head(tsmetrics)
head(timeseries)

timeseries_tsmetrics = left_join(timeseries, tsmetrics, by = c("tsid" = "tsunique"))
dim(timeseries)
dim(timeseries_tsmetrics)
head(timeseries_tsmetrics)

head(timeseries_values_views)

# creating a simple table called 'fish'
# what we're going to have to do is we're going to have to join time series values 
# views to stock.
# And then join that new double-wide table to taxonomy to get all the stuff that we want.
# Alright, so before we join with Taxonomy, let's join with stock.

# And we're going to join, as the metadata table just told us, we're going to join by stock ID.
# And then here's a little trick.

fish = timeseries_values_views %>%
  left_join(stock, by = c("stockid", "stocklong"))
glimpse(fish)

# that I learned after doing a lot of data joining, stock ID and stock long have a one-to-one 
# ratio. Stock ID is just a shorthand, and stock long goes ahead and spells it out for you. 
# If we join by both it's going to carry both alongside without creating new columns.
# So, we can do it really fast, and then I'll show you what I mean.
# Let's… let's glimpse it, because it's going to be long.
# And, let me do it once without joining with.
# Stock long.
# I want to dig into every detail, and then I'm also time-limited, and so that's tricky.
# I'm gonna super quickly create fish2

#fish2 = timeseries_values_views %>%
 # left_join(stock, by = c("stockid"))
#glimpse(fish2)

# which only joins by stock ID, and I'm gonna glimpse them both.
# And what I see is Fish 2, has created this stocklong.y column… And if I scroll up.
# There's a lot of columns. Here's stocklong.x. The problem is that those columns exist 
# in both of our data tables, the time series values, views, and the stock, and we didn't 
# tell it to join by stock long, so it doesn't try and merge them, it just keeps them separate, 
# which is annoying, and it's just repeating your data.
# So, in this case.
# We're gonna go… we're gonna get rid of Fish 2, and we're gonna join by both.

fish = timeseries_values_views %>%
  left_join(stock, by = c("stockid", "stocklong"))
glimpse(fish)

# And you'll see that there's gonna be only… one stock long.
# with no dot X or dot Y. So we've solved that little problem. It doesn't change the data, 
# but it just makes it a little bit more user-friendly. Now we've got all this stock data, 
# we can join with taxonomy.
# using the TSN taxonomic serial number.
# and then the scientific name, which are common in both stock and taxonomy. They don't exist 
# in time series values views, so without this intermediary
# Joining of tables, we wouldn't be able to add taxonomy to our stock data.


# So, we're gonna pipe to yet another left join.
# And we're gonna add, taxonomy.
# And just like before, I'm gonna join by the two columns that are in common between 
# taxonomy and stock.
# TSN… And scientific name. And what we're gonna create here is a monster.
# So take a look. Let's take the dimensions of it.
# 69 columns, 63,000 rows.

# So we're gonna do one more thing here in our creation of fish. 
# I'm gonna select just the columns that I care about.

fish = timeseries_values_views %>%
  left_join(stock, by = c("stockid", "stocklong")) %>%
  left_join(taxonomy, by = c("tsn", "scientificname")) %>%
  select(stockid, stocklong, year, TCbest, tsn, scientificname, commonname, region, FisheryType, taxGroup)

glimpse(fish)
dim(fish)

# Alright, so now we've got something reasonable to play with.
# We checked that the number of rows is the same.
# What we're really gonna dig into today is overfishing.
# And the state of a stock becoming collapsed.
# So when fishermen pull too many fish out of the water, sometimes the stock collapses, 
# and you go down to sort of a negligible amount of fish in the water. In our repo… Under, readings.
# I've got a famous paper, Science Paper 2006, by Boris Worm.
# Big Fisheries scientist. The paper is called Impacts of Biodiversity Loss on Ocean Ecosystem Services.
# And right here, I'm gonna read you this sentence.

# Globally, the rate of fisheries collapses, defined here as catches dropping below 10% of the 
# recorded maximum, has accelerated over time, with 29% of currently fished species considered 
# collapsed in 2003. This is the sentence that we're going to spend the rest of the day digging into.

# And we're going to take this definition to heart, and that… some of those,
# functions I showed you in the dplyr cheat sheet, the CUME MAX, those are functions we're going to use.
# So that we can apply Boris Worm's definition to our data. Boris worm defines a collapse as when 
# this year, your fishery is below 10% of the highest year in history.
# So if, at some point, any time in history, you were able to catch 100 metric tons of cod…
# The first year that it goes below 10 metric tons, that's the year that you declare the fishery is 
# in a state of collapse. 
# So we're going to apply that definition.

# So, first, what I want to do is just say, because I'm curious about this data that we've formed.
# how many fish are caught in general over time? Like, what stocks are… are providing really 
# big catches and quotas? I could do that… the first thing I want to do is just take advantage 
# of the console and explore. Just give me fish.
# And let's arrange that fish dataset in descending order by TC best. And we've done this before, 
# so it shouldn't look too scary, but essentially it's just re-sorting this data so that the 
# highest TC best is going to be printed first.

fish %>% arrange(desc(TCbest))

# And printing it out to the console was pretty bold here, but I see the highest catch in 
# the whole data set is from Atlantic bluefin tuna in the Western Atlantic in 1964, with…
# 18,600,000 metric tons of fish caught that year, in the year 1964.
# And bluefin tuna take the first 3 highest catches.
# And then after that, we've got a lot of Peruvian anchovita. Peruvian anchovita are considered 
# to be the biggest fishery in the world right now. So they're gonna dominate a lot of this data set. 
# And when we printed it to the console, it just printed the first 100 rows, which is why we weren't 
# completely overwhelmed.
# One thing we could do is throw a quick plot together.

ggplot() +
  geom_line(data = fish, aes(x = year, y = TCbest, color = stockid)) +
  theme(legend.position = "none") # keeps legend off the figure - the color bar doesn't get printed


# If I was curious about these few, kind of, key players in the stock world, I could subset 
# the data, and then if I subset it reasonably enough, I could go ahead and have the courage 
# to print the color bar. So I'll do that really quick for funsies.
# And I'm just gonna be lazy and do it right here in my ggplot call, and I'll say fish…
# filter, just give me fish whose TC best is greater than, and I…
# I looked around at what term would make this a reasonable plot, and I said 3 times 10 to 
# the 6.

ggplot() +
  geom_line(data = fish %>% filter(TCbest > 3e6), aes(x = year, y = TCbest, color = stocklong))

# So I plot that. I don't… I'm not gonna suppress my color bar anymore.
# Oh, I don't like that it's stock ID. I'm gonna go ahead and make it stock long so I 
# can actually see what I'm doing here.

# We only have 3 stocks that show up, and it's the two that we've seen before, plus Chilean jack 
# mackerel, which I have a picture in my 311 class that I teach of a per se net with just an 
# unimaginable amount of Chilean jack mackerel that are about to be hauled under this huge 
# commercial ship.
# So, they definitely get hauled in huge numbers. So, okay, that was our time series peak.
# Now, how can we use this to answer a fun question? Who has heard of the cod collapse in Canada 
# in the 1990s?
# It was a really big deal. A lot of people, in fact, I think I linked to an article in…
# 3,000 people lost their job, and it was a huge moment in the world of marine science, and 
# especially the world of fisheries, when people really understood that stuff in the ocean is 
# not limitless.
# It feels like the ocean is so vast, we can't possibly fish out all the fish, but it turns out 
# you can. So we're gonna dig into this moment that was really important for the history of 
# science, as well as politics and economics and people's livelihoods.

# And we're gonna figure out what happened with the COD collapse, and when did it happen, 
# using this dataset.
# First what we're gonna do is figure out what kind of COD data do we have available in our 
# time series values, in our new fish dataset, with our metadata added?


glimpse(fish)

# And I'm gonna just start with Fish, and before I save it to anything, I'm just gonna take a peek at
# Fish that have the scientific name Gadus morhua, which is cod.
# What I want to do is ask where are the stocks corresponding to this scientific name, which is COD?
# So I'm gonna ask… Tell me what distinct regions exist in the data. 

  fish %>%
  filter(scientificname == "Gadus morhua") %>%
  distinct(region)
  

# So we'll print that out to the console and see what we're working with.
# For specifically the species cod, There are 4 regions in the dataset.
# Canada East Coast, Europe, non-EU, the European Union, and the U.S. East Coast. 
# The real story for the COD collapse happens in Atlantic Canada.
# Although, U.S.'s stocks did not do great at that time.
# But to dig into the heart of this… this event in history, I want to just subset…
# stock assessments that occurred in the Canada East Coast region.
# And I'm gonna do that, and I'm gonna now create my own dataset called COD CAN, where CAN 
# is short for Canada.

cod_can = fish %>%
    filter(scientificname == "Gadus morhua", 
            region == "Canada East Coast", 
            !is.na(TCbest))
head(cod_can)

# In the Canada East Coast, for Atlantic cod, there was a total catch of 117,000 metric tons of fish.
# So let's see what that looks like over time.

ggplot(data = cod_can) +
  geom_line(aes(x = year, y = TCbest, color = stocklong)) +
  theme_bw() +
  ylab("Total catch in MT")

# Okay, this is pretty clear. I can see there's, I don't know, 10 different stocks that contribute. That means 
# 10 different management boundaries surrounding this one species, which is cod in Atlantic Canada. 
# And what I see here is we've got data from the 20s to almost current.
# And… in the 70s and a little bit in the 80s, there was this big boom of catching cod. They were just raking 
# in metric tons of cod, and then those numbers fell. Just from that quick check of Boris Form's paper and scanning 
# my eyeball here, to me, this looks like a collapse in the stock.
# But we can quantify it. And verify whether or not this would meet Boris' definition of the total catch, 
# say, in the year, you know, 1995, was at less than 10% of the total catch in any year previously in history.

