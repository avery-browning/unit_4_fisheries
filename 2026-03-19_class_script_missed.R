# 2026-03-19 class script - missed - 4.1 Joins and pivots

# RAM Legacy Database. RAM stands for Ransom A. Myers, which is a famous fisheries 
# biologist, and he started a database that has now been taken over by this group.
# But if you click on the link, or go to ramlegacy.org. and you go to Access the Database, 
# You say, download through Zenoto.
# And then what you want to download is this zip file, version 4.66 of the database.
# If you have cloned this repository it's stored in data, RAM LDB version 4.66. 
# We're going to use the RData version, which is a data file that's 
# literally called an RData file.

# And we'll talk for a second, again, about absolute. So this is an absolute path, it's 
# starting from root on my computer, but I don't want you to load data in, using an 
# absolute directory. I want you to make it relative to where you're at, and that serves 
# not just me if I'm checking your work, it also serves you if you bring your repo and 
# your code to another computer, then it's… you don't have to go and adjust the path names. 
# So this is where my stuff starts. In Unit 4 Fisheries, and then data after. So I'm going 
# to get rid of everything before that, including the opening slash, and now that load 
# function should work.

# So they, they chunk the data up, into groups that make sense.
# And then, if you want to get data from two different tables, you have to figure out how to 
# join them together. You can't just use a column bind, because the information in row 1 of 
# table 1 might not match with the information of row 1 of table 2.

# Side note, but a really important one. Here they have an, you know, this time series data, 
# that's the core of this data set, how fish changed over time. And they have asterisks next 
# to it, and it says, in the Excel format, because they served it as Excel and our data, the 
# time series table is separated into two.

# Here in this database structure metadata document, it's got this table called Table Linkages.
# So there's all these different tables that sometimes you want to join together, and this 
# is how you would link those tables. So here, if you wanted to join the stock table with the 
# taxonomy table the column, the variable name, TSN which exists in both of those tables. 
# And that's what we're gonna play with.
# Is assume that that identifier that allows you to match those databases correctly has the same 
# name in both tables.

# Join = taking two tables and join them together based off of some column with the same 
# data somewhere inside of it.
# wo kinds of joins. Mutating joins, it mutates or changes the table, which just means it adds 
# columns to it. You're making your table wider, you're providing more information, more variables.
# The second kind is a filtering join, and that's saying, take my first table and reduce it
# based off of data in the second table. 
# I don't really use filtering joins. I think it's easier to read my code if I just use 
# a filter function, but sometimes these can be useful, so we'll go over them briefly.
# And then these are the kinds of mutating joins, left join, right join, inner join, and full joins. 
# And then there's two kinds of filtering joins, semi-join and anti-join. 

library(tidyverse)

data1 = data.frame(ID = c(1,2), X1 = c("a1", "a2")) #Where ID equals a vector with just 1 
                                                    #and 2 in it.
                                                    # And X1 equals a vector with characters A1.
                                                    # and A2

data2 = data.frame(ID = c(2,3), X2 = c("b1", "b2"))

data1
data2

# So, data one is just a simple table.
# ID 1 and 2, X1 says A1 and A2. Data 2 is another simple table, ID 2 and 3.
# So if ID is that variable, it must be that variable that we're going to use to 
# join these two tables, I can see that they only have one row in common with ID2.
# Data 1 has an ID1, but data 2 doesn't. Data 2 has an ID3, and Data 1…
# But if I cared about X1, this variable in data 1, and X2, I need some way to bring those together. 

# And you know how you read from left to right? So data one is considered your left data set, 
# because it's what you're starting with. And then you join with data 2.

data12_left = left_join(data1, data2)

# it preserved data 1, my left data frame, reading from left to right. I think the naming is 
# kind of stupid, but it preserved that first data 1, and it just added information from data 2 
# where it was possible. So Data 1, the blue one on the left, does not have an ID3.
# Data 2 does, but it was not brought into the new table, because Data 1 is where you're 
# sitting. You're not going to add any new rows, theoretically.
# But you just want to add new column. So, it added information from X2 in data 2 
# where it was applicable, which, really, it's only applicable for ID2. 
# I'm confused just saying these things out loud, where ID2 is represented in data 2 with the 
# variable X2 has a value of B1, so that was brought over here.
# But there is no ID1 in data table 2, and thus it just filled it with NA. 
# And that's an important note. It recognized that there was no data there. It didn't remove
# the row from data 1 that has ID 1, it just said we couldn't provide any new information about this
# this new variable that we brought in, X2, so we're just going to fill it with an NA.
# So, a left join, your first data table that you're bringing in is sacred, and you're just 
# adding new information to it.

# when I code this, on my computer, I'm more likely to code it this way:

data12_left = data1 %>% 
  left_join(data2)
data12_left
# ^ has the same output

# Now let's do right join… This is the one I really recommend you never, ever, ever use.
# Because it will just confuse you.
# So, we are gonna start with data 1, and we're gonna right join with data 2.

data12_right = data1 %>%
  right_join(data2)
data12_right

# And it does exactly the opposite, where it takes the second data table, and it makes 
# that your sacred starting foundational table, and then it adds information from the first 
# table, as it's available. So it's just the opposite.
# And for me, that's confusing, so instead, if this is what I wanted to do, I would just 
# start a new line of code where I'm working with data 2, and then I would left-join data 1 
# to it. If I keep both in my head, then I'm gonna cross them.
# So I just keep left join in my head, and I always say, whatever I'm starting with, 
# that's the sacred table that I want to add information to, and then I left join a new data set.
# But, as we can see, that these first two columns, ID and X2, those come from data 2.
# And so the IDs are 2 and 3, whereas Data 1 had IDs 1 and 2.
# And then it inserted information from Data 1 where it could.
# which was, again, that common ID2. There's a value for X1, and there's no value
# for, for X1 corresponding to ID3 in data 1.

# Inner join
data12_inner = data1 %>%
  inner_join(data2)
data12_inner

# inner join only keeps rows where the ID exists in both data tables, data 1 and data 2.
# So, data 1 has ID 1 and 2, data 2 has IDs 2 and 3. The only thing in common in this very 
# simplistic data set is ID2, and so your final table only has ID2 in it, but it's still a 
# mutating join, so it has all… all the other columns came with it. The information from X1 and 
# the information from X2.
# I'm more likely to use a left join, and then filter out NAs, if that's what I wanted to do. 
# I would use a left join, and then filter out areas where, X2 and X1 had an NA in there.
# But whatever, you know, it's my fragile mind, whatever works for you is what you should do.

# full join

data12_full = data1 %>%
  full_join(data2)
data12_full

# Full join does what it sort of sounds like it does. It thinks everything in both tables 
# are useful. And this is… if I was going to use a second join, this is the one I might use 
# once in a while. If I really care about data too, and I want all of the information 
# possible there. Full join is helpful. And you can see that the left join stuff is 
# maintained, that's these first two rows, but then the right joined stuff is there too. 
# So IDs 1, 2, and 3 are represented, and where there wasn't information in either data 1 or 
# Data 2, it just filled it with an NA.
# Which join you want to use really depends on what it is you're doing. 

# Filtering joins - semi- and anti-

# semi join

data12_semi = data1 %>%
  semi_join(data2)
data12_semi

# In a filtering join, whatever data set you start with first, which in our case, 
# we've been starting with data 1, no new columns are going to be added, but that data set 
# will be filtered by the information available in Data 2. So in this case, it said, here's 
# data 1, just get rid of any rows where the ID does not exist in Data 2.
# And what I should have been showing you this whole time is you can…
# explicitly say what column you're using to join the two datasets together. And I haven't, 
# I've been lazy about it because both of those data sets, data 1 and Data 2, have a column called ID.

# So…
# The semi-join function is intelligent enough that it can see that they have one and only one column 
# in column in common, one column in common, and it's going to use that to execute the join. But let's do that.
# And see that when you explicitly say by equals ID, the result is the same.

data12_semi = semi_join(data1, data2, by = "ID")
data12_semi

# And that result is, we're just eliminating ID 1, because ID1 does not exist in the second data table.
# So you're filtering Data Table 1 based off of the information available in Data Table 2.

# anti

data12_anti = anti_join(data1, data2, by = "ID")
data12_anti

# I kind of like anti-join. That's the one that sometimes I'll bring into my code, and I use it to test stuff.
# Anti-join says, give me all the stuff that's not available in Data 2.
# So we know that the ID in common between the two data tables is 2, and so what it returned was the ID 
# in the first table that does not exist in the second table.
# And you think, well, that sounds kind of stupid, but actually, when you're at that beginning, 
# like, designing your project phase, it can be really important.
# let's say you're… you care about the time series of all these different kinds of fish, but you're 
# only going to be able to do your project if you have all these extra parameters for them. Like, what is 
# the typical total length of a cod? You know, and what genus does it live in? Like, the genus one is pretty 
# easy, but there's these other metrics, like…
# How long does it usually survive?
# Maybe you've got a column that's like, what is its most common predator? What is its most common prey item? 
# And if you need that ancillary data from data 2, to conduct the project that you want to do, 
# the first thing I would say is, given the data I have in data one, what is missing for me to do 
# my project? And if it turns out, like, the key variables that I care about are missing,

# re-initializing data1 and data2 for the next part
# joins sound simple but it can really mess with your data if you are not very careful, which can
# happen if you accidentally duplicate a row

# Let's do a quick row duplication. I'm gonna go up here, and Data 2, I'm gonna add…
# two copies of ID, And, I'm gonna add 2 copies of ID 2.
# And then just some more data for X2. 

data1 = data.frame(ID = c(1,2), X1 = c("a1", "a2")) 
data2 = data.frame(ID = c(2,2,3), X2 = c("b1", "b2", "b3"))

# And then let's do my favorite join, my lefty join.

data12_lefty = left_join(data1, data2)
data12_lefty

# Okay, this seems pretty innocent, right? And let's compare it to the original left join.
# Alright, in the original left join, before I added a second row with ID2 in data 2, this is what I got. 
# I got 2 rows, which is the same number of rows in data 1. now that I added that second…
# that additional row in data, too, with ID2, what it did was it wanted to respect that there's 
# two different pieces of information in X2, and bring them both over, which sounds really kind.
# Thank you for not dropping information. However, like, Data1 was our sacred database. We left 
# joined, like Erin said. And just adding rows willy-nilly, depending on what your next step is, could 
# really mess with your analysis.

# At a bare minimum, always check the dimensions of your data frame after and before you conduct one 
# of these joins. You want to know what columns did it add, but that's not as scary as what rows did it add.
# You have to know what rows it added, in case data were duplicated.
# So live in fear, y'all, and use that dim()

dim(data12_lefty)
dim(data1)

# data pivots

# Quadrat ID 
# A quadrat,  I bet most of you know, but it's like a square, often made of PVC, that you could set on 
# the environment, and then you count all the things within your quadrat, or you do some kind of s
# ampling within the boundaries of that square. So, let's say you were throwing some quadrats in the 
# intertidal and counting all the invertebrates that you saw within your square. 
# So first, I'm going to do it a few times, so each quadrat needs a unique identifier. 
# We're going to call that quadrat ID, and we're going to pretend that they were 101, 102, 103 and 104.
# Now, how many… Invertebrates, did I count in each of those quadrats?

survey = data.frame(quadrat_id = c(101,102,103,104),
                    barnacle = c(2,11,8,27),
                    chiton = c(1,0,0,2),
                    mussel = c(0,1,1,4))
survey

# this is what I call the wide format, and we're gonna transform it into the long format, 
# and then we're gonna transform it back to the wide format.

# plotting really likes long data. ggplot does magic with long data, and it does not do magic with wide data.

# So let's pivot our data and make it long, where we've got a quadrat ID for one column, the IDs are 101, 2, 
# 3, and 4, and then we have counts for another column. How many beasties did I find? 
# And my third column is species. Which species did I count?
# That's the long format.


# Calls is like, which columns do you want to take and lengthen? And we want to lengthen… We can name them explicitly.
# Then, it says… When you turn all of these distinct columns into a single column, what do you want to call it?
# And this, again, is not, for me, intuitive, but I want to call the new column names2 is the parameter. 
# What do you want to call the new column that holds the words Barnacle, chitin, and muscle? I want to call it, probably, taxa.

# It's these three characters that are going to form the data within the taxa column.
# Barnacle, chitin, and mussel. But we also have the data within the cells of all those variables. Those are called our values. 
# So now those values need to be in their own distinct column.
# And what should that column be called? And I think it should be called counts, or N, or something like that. 
# So we'll say values to equals counts.

long = survey %>%
  pivot_longer(cols = c("barnacle", "chiton", "mussel"), names_to = "taxa", values_to = "count")
long

# So this is the long format. You take multiple columns, and you turn them into two columns.
# One column that contains the name of all the columns that you subsumed into your long pivot, and the 
# other column contains the values, what was in the cells under all of those old columns.

# we'll do a plot together, too, to show why that's useful.

# Okay, sometimes you get served this data.
# Often I've been served data that looks like this, and that's great, but if I wanted to run a linear 
# model, like, how are barnacles impacted by chitinase in the same area?
# This is not gonna work.
# So I would have to widen it. So we're gonna go backwards… And we'll call it wide.
# And so we're going to start with long, and now we're going to use pivot wider.

wide = long %>%
  pivot_wider(names_from = taxa, values_from = count)
wide
survey

# exercise 1.2
# plot long data
ggplot(data = long) +
  geom_point(aes(x = quadrat_id, y = count, color = taxa))

ggplot(data = wide) +
  geom_point(aes(x = quadrat_id, y = barnacle), color = "violet") +
  geom_point(aes(x = quadrat_id, y = chiton), color = "cyan") +
  geom_point(aes(x = quadrat_id, y = mussel), color = "yellowgreen")
