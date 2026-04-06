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

# We won't do it in the future, but just for COD, I'm gonna go ahead and add all of these stocks together.
# This is pretty high resolution, and I think there's good reason. Like Nova Scotia versus Labrador versus 
# Bay of Fundy, they're going to want to have different management units. But I'm going for the 30,000 foot 
# view, so I'm going to take all of the cod in eastern Canada.
# And just add those catches together, and turn all of these lines into one line.
# I'm going to create a new data frame to do it. 

# add all cod stocks in E CA together

# And my new data frame is going to be CODCANTOTAL. And we're gonna start with Cod Can.
# And we're gonna group by year. And say, for a given year, go ahead and add all of those TC bests 
# together. So, summarize Where total catch, it's this new column that I'm creating equals the sum of 
# all the TC best for that given year that it was grouped by in the line before.

cod_can_total = cod_can %>%
  group_by(year) %>%
  summarize(total_catch = sum(TCbest))
head(cod_can_total)

# Oh, and so this is very simple. We've lost all of our metadata, and that's what Summarize does. 
# Anything that either hasn't been grouped, or doesn't get a name in the summarize row gets dropped from the data.
# But for now, I feel comfortable with that. We already… we know that it's all cod from Eastern Canada.
# And so in the year 1917, there were 38,000 metric tons of cod caught on the East Coast. 
# So let's visualize that. Now that we've added them together. It's gonna look almost the same as this last plot.

ggplot(data = cod_can_total) +
  geom_line(aes(x = year, y = total_catch))

# Now we only have one stock. We've added them all together. It's a mega stock.

# play with the cumulative fxns in dplyr

dat = c(1,3,6,2,3,9,-1)
dat_max = cummax(dat)
dat_sum = cumsum(dat)
test_cum = data.frame(dat, dat_max, dat_sum)
test_cum

# Okay, so I just made them into different columns within a data frame. This is that original vector of numbers 
# that I just made up.
# And then I use the cummax function on that vector, and then the cumesum function on that vector.
# and stack them next to each other. So Max says, from this point in my data frame, examining all the rows 
# preceding, and that's what the word cumulative means. You're accumulating as you step through
# something. Step through a dimension. Could be space, could be time. In this case, it's stepping through rows.
# Stepping through elements of your vector.
# So, what is the max number I've encountered in my data so far? In row 1, the max number was 1.
# By row 2, that vector had… had gotten a 1 and a 3, so the biggest number out of 1 and a 3 is a 3.
# And then in row 3, the biggest number seen so far is a 6. 
# And then in row 4, the fourth element of that is 2.
# But the largest number so far, going sequentially through each element of that, the biggest number 
# was 6. So that's what this cumulative function is doing, is saying, so far, what's the max, the biggest 
# number we've encountered?

# cumsum, which we use to create the datsum column does kind of the same thing. It steps through 
# each row, and it adds things together. 
# So, for row 1, the sum of 1 and nothing else is just 1. 
# But for row 2, you're sending 1 and 3 for a cumulative sum of 4. 
# For row 3, you're sending 1, 3, and 6 for a cumulative sum of 10.

# Okay, who cares? Well, if your question is, has my stock collapsed? These tools are super helpful.
# What is the historical max that has occurred?
# And we're going to use some in a little bit, and we're going to use it in kind of a sneaky way. 
# But if we want to say, is the catch today of my stock of interest bigger than 10% of the historical 
# max catch, as Boris Worm defined. If it is, then okay. If it's not, then according to Boris, if you're 
# less than 10% of your historical max, you're in a state of collapse. So that's what we're going to use 
# this cummax function for.
# what is the historical maximum of fish caught for any given year? So in the year 1990, you're 
# only looking at years in the 80s, 70s, 60s. You're not looking at years in the 2000s.

# using Boris Worm (2006) stock collapse definition - has cod collapsed?

head(cod_can_total)

cod_collapse = cod_can_total %>%
  mutate(historical_max_catch = cummax(total_catch))

head(cod_collapse)

# So the cod catch was growing, and then it stopped growing here, and so that number, it didn't go backwards, 
# but it stayed the same to keep track of that historical max catch. So right now, I'm feeling good that this 
# function is doing what I thought it would do, but what is, for any given year, like this year, 19, is this 
# number less than 10 f the historical max catch?
# And now it's easy to do, because that data, they're both in the same row.
# And say, does this equal less than 0.1 times this? And if it does, the fishery is collapsed.
# So we've made it really manageable.
# So I'm gonna go ahead and use mutate to create a new variable.
# And my new variable is going to be collapse, and it's gonna be a true-false variable. And the question is, is total catch,
#  that second column for a given year, is it less than or equal to 0.1, 10%, times the historical max catch?

cod_collapse = cod_can_total %>%
  mutate(historical_max_catch = cummax(total_catch),
          collapse = total_catch <= 0.1 * historical_max_catch)

head(cod_collapse)
tail(cod_collapse)

# I want to do is I want to have an explicit number that says what year did it go from false to true? 
# And we're going to call that number COD Collapse Year, which makes sense.
# And we're going to pull it from this data that we just calculated ourselves, cod_collapse
# We're gonna filter this data, only give me years when collapse is true, and then give me the smallest year.

cod_collapse_yr = cod_collapse %>%
  filter(collapse == T) %>%
  summarize(year = min(year))
cod_collapse_yr # 1993

# Here's the weird thing, is if I want to use 1993 like a number by itself, which we're gonna do in a minute, because we're going to use,
# those tools that we use to draw, like, a ver… I want to draw a vertical line in my time series for the year that the COD 
# collapsed. Point the user, boom, this is the year it happened.
# And that requires a single number, the number 1993. And that looks like a number, but actually, if we ask 

class(cod_collapse_yr)

# It's not a number. It's a data frame. It's just a data frame with only one row and one column, and if we want just a number, 
# we need to turn this into a vector. And so I promised at least one of you that we were going to talk about this function 
# in this class.

# If we pipe and use the function pull, it pulls the thing that you care about out of the data frame and serves 
# it in a vectorized format.

cod_collapse_yr = cod_collapse %>%
  filter(collapse == T) %>%
  summarize(year = min(year)) %>%
  pull() # pull a vector out of the data.frame / tibble
cod_collapse_yr # 1993

# So now we're gonna plot this again, and we're gonna make it a little fancier now that we know exactly when cod collapsed.
# We could, if we wanted, add another aesthetic and say color equals collapse. Remember, collapse is just our column of TRUEs 
# and FALSEs. If we run that, now it draws the line red for false and blue for true.
# I want to add a vertical line that says here, this is the year when COD entered a collapsed state. And we've done 
# this in base R plotting before, but here's a trick to do it in ggplot. You can say geom_vline()
# Where V stands for vertical, And now you're just gonna say - where do I want my vertical line drawn? And, is it showing up?
# You can see in the little pop-up bar, X intercept as an option, and I'm going to use that to say where do I 
# want the vertical line to be drawn.
# So, X intercept equals this year that we painfully pulled out of our COD Collapse data frame, called COD Collapse Year. 
# If I try to give this X intercept parameter a table, R would freak out or throw an error. 
# It just wants a single number, so it knows where to draw that line.
# And even though that table that we saw before only had one number in it, it would see that it
# is in the form of a data frame, and it's just not set up to handle it that way.

ggplot(data = cod_collapse) +
  geom_line(aes(x = year, y = total_catch, color = collapse)) +
  geom_vline(xintercept = cod_collapse_yr) +
  theme_classic()

# we're going to take what we've done with COD, this nice insular example with a history to it. If you were 
# in Nova Scotia in the 90s, like, this would be a huge part of your life.
# And we're going to try and apply it to this humongous million row data set, which is where things get 
# scary. If you have big, scary data to work with, I strongly recommend you subset it and subset it again, 
# and play in your sandbox. A sandbox means, like, a virtual environment.
# Play in a small space until you are confident about what you're doing.

# apply collapse to full data set

# So I'm going to create a super data frame called collapse, not CodCollapse, it's collapse, 
# because it's going to be all of my fish.
# Remember Fish?
# I literally don't. So I have to take the head of it and take a peek. 

head(fish)

# We're going to ask the question, did this stock collapse this year relative to its historical max catch?
# We have to do a few things to make that play nicely. First, I want to get rid of all NAs.
# Cause they're just gonna haunt us later.
# So, is.na, flip it for TC best.
# Now, and this is key, we're going to group by stock.
# We don't want to compare the Acadian redfish catch in a given year to the historical max catch of cod. 
# Each stock has its own data that's unique to that stock. So we're grouping by stock.
# Now we can apply the functions that we've played with so far. Mutate. We want to create a new column 
# called Historical Max Catch.
# And it's going to be equal to the cumulative max from TC Best.
# Maybe we'll just stop there and see how things are going.
# Hmm, I'm gonna glimpse it.
# I didn't expect, really, to see much that was exciting, but the first few rows… and remember, we transposed a 
# glimpse, which is why it's not my favorite, but the few first few rows are Acadian redfish in the early 60s, 
# of which there was 33,000 metric tons, and then 20,000…
# And so, historical max catch keeps up with 33,000 until it goes up, and it's not going to go up in these 
# first X number of rows. But I see a column, or it is a column, but it looks like a row and glimpse. It seems 
# to be doing what I think it's doing.
# And importantly, it's doing it individually for each stock. That's what the grouping does. 
# The grouping is the magic of dplyr.
# Now I want to ask the question, for this given year, for this given stock, has the stock collapsed? 
# So, within Mutate, I'm going to create a new column called Current Collapse.
# And I'm gonna say the same question I said before. Is TC best for this stock, for this row number, 
# which is equivalent here, is it less than or equal to 10% of the Historical Max Catch.


collapse = fish %>%
  filter(!is.na(TCbest)) %>%
  group_by(stockid) %>%
  mutate(historical_max_catch = cummax(TCbest),
          current_collapse == TCbest <= 0.1 * historical_max_catch,
          collapsed_yet = cumsum(current_collapse) > 0)

glimpse(collapse)

# So I ran it…
# And at least for Acadian redfish, the first few rows are false, and it actually does flip 
# to true, even just in this quick glimpsed view. So something is happening there.
# And then… I'm gonna add one more column called collapsed yet, let me add it, and then I'll explain it, 
# which is the cumulative sum of current collapse. I told you we'd be doing something sneaky.
# Is the cumulative sum greater than zero? What does that do?
# When I first did this, I then came to realize that sometimes stocks collapse, and then, you know, big surprise, 
# honestly, they were well managed. And they rebounded. So sometimes, like, in the cod scenario, perhaps, 
# in 1993 you collapse, the 90s are going poorly, but the fishermen aren't allowed to fish anymore, 
# and then in 2005, let's say, enough cod were around, they rebounded, the fishermen were allowed to fish, 
# and they caught more than 10% of the historical max catch. So in 2005, in our little Gaduncan experiment, 
# cod are no longer in a state of collapse.
# So this question, could collapse in 2005, would become false again. They did collapse, but now they're okay.

# So I wanted to create one more variable that says, for this given year, 2005, I don't care what the 
# catch is this year, was there ever a period in the history of this stock that the fishery had collapsed?
# And so cumsum, remember what number false and true equate to? 
# What is a false, if you turn it into a number? Zero.
# And what is a true? 1.
# So, the cumulative sum of every collapse variable prior to this year… For a given stock.
# Hopefully, you're adding up all zeros, because it's never collapsed. But once it's collapsed for even 
# one year, it becomes one. And if it collapses for a second year, then the cumulative sum is going to be 2.
# And if it's collapsed for a third year, then it's going to be 3. 
# But as long as it's greater than zero for a given year, that means that some year in history, that stock was collapsed.

# So I'm gonna do one other thing, and this is just to prevent me from heartache in the future. I'm going to ungroup my data.

collapse = fish %>%
  filter(!is.na(TCbest)) %>%
  group_by(stockid) %>%
  mutate(historical_max_catch = cummax(TCbest),
          current_collapse == TCbest <= 0.1 * historical_max_catch,
          collapsed_yet = cumsum(current_collapse) > 0) %>%
  ungroup()

# It's been grouped by stock ID, and I want my collapsed data to forget that grouping.

# It's not going to change what the dataset looks like at all.
# Now I want… remember when we found out that COG collapsed in 1993? I want to find that out for every stock. 
# What is the year that any stock experienced a state of collapse? Because I want to see how bad we're doing as 
# humans harvesting stuff from the ocean.
# So, riffing off of my last data frame collapse, I'm going to create a new data frame called Collapse Year.

collapse_yr = collapse %>%
  group_by(stockid, stocklong, region) %>%
  filter(collapsed_yet == T) %>%
  summarize(first_collapse_yr = min(year)) %>%
  ungroup()

# error also video playback failure - come back to this 

# I'm gonna start with collapse.
# I'm gonna pipe…
# And now I'm going to group by… I want to group by stock again, just like I did before. 
# I'll go ahead and bring stock long along with it, because I just don't want to forget what Stocklong does, 
# it doesn't really change any of the math here.
# And I also want to group by region. That doesn't change anything either
# I'm going to do a summary, and once I summarize things, if they haven't made it into either a grouping 
# or a summary statistic, I'm going to lose them. And I don't want to lose region. So this is just carrying 
# it along with me.
# Now I pipe… I want to say, give me only the collapsed years. So, collapsed yet = true
# Give me just those years where our stocks had collapsed at some point in history. 
# And then just like we did for COD, what is the first year of the collapse? So, for that given stock, 
# for only the years where it had collapsed at some point in the past.
# What is the minimum year? What is the first year that that stock collapsed?
# And then, again, I'm going to do this ungroup thing, because I did a lot of grouping, and sometimes 
# when I go to plot or model things that have been grouped, I get some unexpected behavior.
# So I did some… some crazy stuff.
# Let's take a look at it. 