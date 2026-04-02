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
