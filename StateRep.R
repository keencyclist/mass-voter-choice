install.packages("tidyverse")
library(tidyverse)


# read in complete 1970-2016 state election data
elstats <- read.csv('elstats.csv', na.strings="NULL")

# CLEANING - First Hampden and Fifth Middlesex 1999 Special Elections were erroneously coded as State Senate, should be State Rep.
elstats <- elstats %>%
  mutate(office_name = replace(office_name, contest_id %in% c(15950, 15951, 15949, 15952, 15953, 15954), 'State Representative'))
  
# create a dataframe that has only state senate election from 1996 and later
house1996 <- elstats %>%
  filter(office_name == 'State Representative' & year>1995)


# --------------ERROR CHECKING
# would be better to separate display_name first into 
# suffix (based on ",") and last name (right side to first space)

# check for duplicate candidate IDs.
# create list of unique candidate name / candidate_id combos
unique_id <- house1996 %>%
  group_by(candidate_id,display_name) %>%
  summarise(n = n())

# find candidates with more than one candidate_id
duplicate_id <- unique_id %>%
  group_by(display_name) %>%
  summarise(n = n()) %>%
  filter(n>1)
rm(unique_id,duplicate_id)
# -----------------------------

# Install and load the car package for recode
install.packages("car")
library(car)
# CLEANING recode candidates that should have the same ID:
house1996$candidate_id <- recode(house1996$candidate_id, "15871=216; 8737=755; 15508=10132;
                                 10919=157; 9544=9236; 9741=973; 9388=9542; 9823=108; 9730=955;
                                 9758=673; 15804=9874; 10424=829; 11427=829; 11463=883;
                                 9896=738; 949=948; 7980=554; 11985=263; 15468=11910; 9746=880;
                                 15434=12069; 10034=125; 12051=15398")

#216 Bradley H. Jones, Jr 15871 Bradley H. Jones, Jr
#8737 Anne M. Paulsen 755 Anne Marie Paulsen
#15508 Antonio d. F. Cabral 10132 Antonio F.D. Cabral
#157 Brian M. Ashe 10919 Brian Michael Ashe
#9236 Carolyn A. Grenier 9544 Carolyn Serra Grenier
#9741 Daniel J. Mackenzic 973 Daniel J. MacKenzie
#9542 Daniel P. Dowd 9388 Daniel Paul Dowd
#108 Harriett L. Stanley 9823 Harriett Lari Stanley
#9730 Israel Reyas 955 Israel Reyes
#673 James B. Leary 9758 James Brendan Leary
#15804 Jeffrey SÃ¯Â¿Â½nchez 9874 Jeffrey Sanchez
#10424 Jerry J. Jachimczyk 829 Jerzy J. Jachimczyk 11427 Jerzy J. Jachimezyk
#11463 John T. Dohahue 883 John T. Donahue
#9896 John W. Lambert 738 John Warren Lambert
#949 Kerry Murphy-Healy 948 kerry Murphy Healcy
#554 Lida E. Harkins 7980 Lida Eisenstadt Harkins
#11985 Mark J. Cusack 263 Mark James Cusack
#15468 Paul Schmid, A. Lll 11910 Paul Schmid, III
#9746 Richard A. Yampell 880 Richard Alan Yampell
#15434 Rufus J. Faulk 12069 Rufus Jackson Faulk
#10034 Steven M. Walsh 125 Steven Myles Walsh
#12051 Susannah M. Whipps Lee  15398 Susannah Whipps M. Lee
#

# CORRECTIONS -- Contest 8096 is an erroneous duplicate, the correct entry is 15986
#             -- Contest 15985 is an erroneous duplicate (13th Suffolk General Special--1997-03-11, that was the primary date)
#             -- Add missing candidate_party
house1996 <- house1996[!house1996$contest_id==15985,]
house1996 <- house1996[!house1996$contest_id==8096,]
house1996 <- house1996 %>%
  mutate(candidate_party = replace(candidate_party, candidate_id %in% c(442, 620, 628), 'Democratic')) %>%
  mutate(candidate_party = replace(candidate_party, candidate_id == 834, 'Republican'))

# --------------ERROR CHECKING
# check for null values
null_house <- house1996 %>%
  filter (is.na(candidate_id))
# conclusion: -- placeholder data for primary elections with no candidates (mostly Republ and minor party)
# remove unneeded data frames
remove(null_house)

# find the unique district name for each redistricting year
districts1996 <- house1996 %>%
    filter(year==1996)  %>%
  distinct(district_name)

districts <- house1996 %>%
  filter(is.na(is_special)) %>%
  group_by(year) %>%  
  distinct(district_name) 

district_count <- districts %>%
  group_by(district_name) %>%
  summarise(n=n())

# --------districts added and created --
# in 2002, the 13th Hampshire and 38th and 39th Middlesex districts were deleted
# the 5th Barnstable, 18th Essex, 18th Worcester districts were added
# in the 2012 redistricting, no district names changed

# 13th Hampden: incumbent Gale Conderas went to 12th Hampden; 
#                 12th Hampden incumbent Benjamin Swan went to 11th Hampden, which had been Paul E. Caron's:
# "When he [Caron] lost the mayoral race, there were whispers of retaliation in a political culture where such threats cannot 
# be taken lightly. Ultimately his legislative district was torn asunder in redistricting in a way that would have 
# forced him to run against another incumbent had he sought re-election. Caron probably would have won that contest, 
# but it would have been a divisive and even ugly campaign." -  http://tommydevine.blogspot.com/2008/07/paul-caron.html
#  13-12, 12-11
#
# 38th Middlesex: incumbent Paul J. Donato, J. sought and won 35th Middlesex in 2002; 
#                 35th incumbent Michael Festa ran for 32nd Middlesex, which had been Rachel Kaprielian's district, 
#                 Kaprilian moved to 29th Middlesex, with was Tim Toomey's who moved to 26th Middlesex, which had
#                 been Anne Paulsen's, she went to 24th Middlesex, which was William G. Greene's, who went to 22nd,
#                 which was Brian Cresta's, who resigned from the House of Representatives to accept an appointment by 
#                 President George W. Bush at the United States Department of Health and Human Services
# 38-35, 35-32, 32-29, 29-26, 26-24,24-22

# 39th Middlesex: Colleen Garry went to 36th, Christopher G. Fallon went to 33rd, Carol A. Donovan went to 30th,
#                 Jehlen went to 27th, Alice Wolf went to 25th, Marzilli went to 23rd, Murphy went to 21st, 
#                 Bradley Jones to 20th, Miceli went to 19th, Kevin J. Murphy went to 18th, Nangle to 17th,
#                 Thomas A. Golden, Jr. to 16th, which had been Carol C. Cleven's district:
#                 "But it was a disagreement with Democratic Speaker Thomas Finneran that ended her career. 
#                 A Finneran-led redistricting changed Cleven's Chelmsford-Carlisle based district, carving the 
#                 community into four legislative districts." She retired instead of running against another incumbent.
#                 http://www.lowellsun.com/breakingnews/ci_27697072/beloved-former-state-rep-carol-cleven-passes
# 36-33, 33-30, 30-27, 27-25, 25-23, 23-21, 21-20, 20-19, 19-18, 18-17, 17-16

#  - note: redistricting details above were used in coding the DISTNUM in house_districts.csv to be consistent 
#          across the 2002 redistricting (same numbers were used for 2012 districts as for 2002)
#-----------------------------------


# find the election winner - max of candidate_votes - winner is r=1
house1996 <- house1996 %>%
   group_by(contest_id) %>%
  mutate(r = min_rank(desc(candidate_votes)))

# correction - in the 2010 Sixth Worcester general election, the EL data has Durant ahead by 1 vote, 
# but other sources says it was an exact tie, forcing special election in May 2011. Incumbent held the seat until 
# then, so Alicea is given r=1 and Durant is given r=2
house1996 <- house1996 %>%
  mutate(r = replace(r, contest_id == 15465 & candidate_id == 360, 1)) %>%
  mutate(r = replace(r, contest_id == 15465 & candidate_id == 361, 2))

# recode to assign the applicable districting year 
house1996$district_year <- recode(house1996$year,"1993:2001=1993; 2002:2016=2002")

# convert date into date type
library(lubridate)
house1996$election_date <- ymd(house1996$date)

# CORRECTIONS: Regular Elections in 1998 are missing the date
# assign Nov 3, 1998 to  1998 state general elections and Sept 15, 1998 for primaries
house1996 <- house1996 %>%
  mutate(election_date = replace(election_date, year==1998 & is.na(is_special) & contest_stage == 'General', '1998-11-03')) %>%
  mutate(election_date = replace(election_date, year==1998 & is.na(is_special) & contest_stage != 'General', '1998-09-15'))

# CORRECTIONS: Missing election date for a few general elections, and all primaries miscoded, in 1996
# pretty sure the primary date is correct, but could not find official source
house1996 <- house1996 %>%
  mutate(election_date = replace(election_date, year==1996 & is.na(is_special) & contest_stage == 'General', '1996-11-05')) %>%
  mutate(election_date = replace(election_date, year==1996 & is.na(is_special) & contest_stage != 'General', '1996-09-10'))


#-------ERROR CHECK
# check for special elections in 2002
house_special_2002 <- house1996 %>%
  filter(is_special==1 & year == 2002)
rm(house_special_2002)
#-----------------

# just one Special Election in 2002 - use 1993 districts
house1996 <- house1996 %>%
  mutate(district_year = replace(district_year, election_date == "2002-03-12", 1993))

# Assign consistent district names for 1993 to 2002 (no change in district names in 2012 redistricting)
house_districts <- read.csv('house_districts.csv', na.strings="NULL")

# join elections data to senate district # info, under new dataframe name
house <- house1996 %>%
  left_join(house_districts, by = c("district_year" = "Year", "district_name"))

# --------------CHECKING
# create new dataframe with each general election winner
reps <- house %>%
  filter(r == 1, contest_stage == 'General') %>%
  arrange(DISTNUM, election_date)
# check the number of elections for each senate district (some are special)
table(reps$DISTNUM)
# make sure the races for the 3 new districts in 2002 had no incumbents running
house_new_2002 <- house %>%
  filter(year == 2002 & DISTNUM %in% c(5,42,160)) 
# conclusion: looks good
#-------------------------------

# replace date of December special primary to match year of January special general
house <- house %>%
  mutate(year = replace(year, contest_id == 34652, 2014))


# create dataframe of elections - first with general election data only
house_elections <- house %>%
  filter(contest_stage == 'General') %>%
  group_by(DISTNUM,election_date, contest_id) %>%
  summarize(district = first(district_name), g_candidates = n(), is_special = first(is_special), g_votes = first(total_votes), 
            year=first(year), winner_id = candidate_id[r == 1], winner = display_name[r == 1], 
            winner_party = candidate_party[r == 1], winner_pct = pct_candidate_votes[r == 1], 
            write_in = is_write_in[r == 1])

# order the general election contests (for each district) and identify the incumbent
# note: for "switched party", exclude cases where incumbent switched parties (e.g., from Unenrolled to Democratic) 
house_elections <- house_elections %>%
  group_by(DISTNUM) %>%
  mutate(election_order = min_rank(election_date)) %>%
  mutate(incumbent_id = lag(winner_id, order_by = election_order)) %>%
  mutate(incumbent_won = ifelse(winner_id == incumbent_id, 1, 0)) %>%
  mutate(incumbent_party = lag(winner_party, order_by = election_order)) %>%
  mutate(switched_party = ifelse(winner_party != incumbent_party & winner_id != incumbent_id & 
                                   incumbent_party != 'Unenrolled' & winner_party != 'Unenrolled', 1, 0)) %>%
  arrange(DISTNUM, election_order)

# Note - check situation with write-ins  -- there were a few cases where candidate was written in in multiple
# primaries, and this can mess up the matching with primary election data


# add the relevant primary elections - those of the incumbent's party
house_summary <- house_elections %>%
  filter(year > 1996) %>%
  left_join(house, by = c("DISTNUM", "year","is_special","incumbent_party" = "contest_stage"))

# determine if the incumbent is participating in the primary - will sum to 1 if one of the participants is an incumbent, else to 0.
# this will work unless the incumbent is running as an unenrolled candidate (or switched parties, which is very unlikely)
house_summary <- house_summary %>%
  group_by(DISTNUM,election_date.x) %>%
  mutate(incumbent_ran = ifelse(candidate_id == incumbent_id, 1, 0))


# NOTE: Error in the original file: Prior to 2002, some or all of the primary elections have the general election (November) date 
#       in place of the actual (September) date. - not fixed.

# summarize the primary data  
house_summary <- house_summary %>%
  filter(election_date.x > "1996-12-31") %>%
  group_by(DISTNUM,election_date.x) %>%
  summarize(contest_id = first(contest_id.x), district = first(district), g_candidates = first(g_candidates),
            is_special = first(is_special), g_votes = first(g_votes), year=first(year), 
            winner_id = first(winner_id), winner = first(winner),winner_party = first(winner_party), g_write_in = first(write_in),
            winner_pct = first(winner_pct), election_order = first(election_order), incumbent_id = first(incumbent_id),
            incumbent_won = first(incumbent_won), switched_party = first(switched_party), p_candidates = n(), 
            p_votes=first(total_votes),incumbent_ran = sum(incumbent_ran),p_winner_pct = pct_candidate_votes[r == 1],
            p_write_in = is_write_in[r == 1])

# add values for winning candidates not nominated by a primary (unenrolled), or 
# primary election info missing (Corbitt, 2nd Plymouth, 1996 Special election - primary was in 1995)
# includes 4 elections where incumbent was originally elected as Unenrolled, then Dem. in next election (13185,13248,14433,15359)
# note: in 2012 17th Essex election, incumbent Republican did not run and no other Republican competed in the primary
house_summary <- house_summary %>%
  mutate(p_winner_pct = replace(p_winner_pct, contest_id %in% c(9214,13185,13248,14433,15359,22563,13248,16007,14320.20175,21694,7492,8273), 1)) %>%
  mutate(incumbent_ran = replace(incumbent_ran, contest_id %in% c(9214,22563,13248,16007,14320,20175,21694,7492,8273), 0)) %>%
  mutate(incumbent_ran = replace(incumbent_ran, contest_id %in% c(13185,13248,14433,15359), 1))

# If no incumbent, then no primary info -- but should add in winner's primary info, and then make sure coded as no incumbent
# add values for 3 new districts created in 2002
house_summary <- house_summary %>%
  mutate(p_winner_pct = replace(p_winner_pct, contest_id %in% c(20062, 20013,20073), 1)) %>%
  mutate(incumbent_ran = replace(incumbent_ran, contest_id %in% c(20062, 20013,20073), 0))

# determine cases where incumbent was running but lost
house_summary <- house_summary %>%
  filter(year > 1996) %>%
  mutate(upset = ifelse(incumbent_ran == 1 & incumbent_won != 1, 1, 0))

# recodes
house_summary <- house_summary %>%
  mutate(p_plurality = ifelse(p_winner_pct < .50, 1, 0)) %>%
  mutate(g_plurality = ifelse(winner_pct < .50, 1, 0)) %>%
  mutate(plurality_either = ifelse(g_plurality == 1 | p_plurality == 1, 1, 0)) %>%
  mutate(p_non_comp = ifelse(p_winner_pct > .60, 1, 0)) %>%
  mutate(non_comp = ifelse(winner_pct > .60, 1, 0)) %>%
  mutate(p_unopposed = ifelse(p_candidates == 1, 1, 0)) %>%
  mutate(g_unopposed = ifelse(g_candidates == 1, 1, 0)) %>%
  mutate(unopposed = ifelse(g_candidates == 1 & p_candidates == 1, 1, 0)) %>%
  mutate(non_comp_both = ifelse(p_non_comp == 1 & non_comp == 1, 1, 0)) %>%
  mutate(primary_general = (p_votes / g_votes))



# Error checking
house_check <- house_summary %>%
  filter(is.na(incumbent_won))

# --------------CROSSTABS
install.packages("janitor")
library(janitor)

# Frequency count
# note: format is column, row
crosstab1 <- house_summary %>%
  filter(year>1996) %>%
  crosstab(incumbent_ran, is_special, percent = "none") 

crosstab2 <- house_summary %>%
  filter(year>1996) %>%
  crosstab(incumbent_ran, p_plurality, percent = "none") 

crosstab3 <- house_summary %>%
  filter(year>1996) %>%
  crosstab(incumbent_ran, g_plurality, percent = "none")

crosstab4 <- house_summary %>%
  filter(year>1996) %>%
  crosstab(incumbent_ran, non_comp, percent = "none")

crosstab5 <- house_summary %>%
  filter(year>1996) %>%
  crosstab(incumbent_ran, p_non_comp, percent = "none")

crosstab6 <- house_summary %>%
  filter(year>1996) %>%
  crosstab(incumbent_ran, g_unopposed, percent = "none")

crosstab7 <- house_summary %>%
  filter(year>1996) %>%
  crosstab(incumbent_ran, p_unopposed, percent = "none")

crosstab8 <- house_summary %>%
  filter(year>1996) %>%
  crosstab(incumbent_ran, unopposed, percent = "none")

# NOTE: 3 cases where incumbent_won is NA are the 3 new districts created in 2002
crosstab9 <- house_summary %>%
  filter(year>1996) %>%
  crosstab(incumbent_ran, incumbent_won, percent = "none")

crosstab10 <- house_summary %>%
  filter(year>1996) %>%
  crosstab(incumbent_ran, switched_party, percent = "none")

crosstab11 <- house_summary %>%
  filter(year>1996) %>%
  crosstab(incumbent_ran, non_comp_both, percent = "none")

crosstab12 <- house_summary %>%
  filter(year>1996) %>%
  crosstab(incumbent_ran, plurality_either, percent = "none")

# get list of plurality winners
plurality <- house_summary %>%
  filter(plurality_either == 1) %>%
  arrange(p_winner_pct, winner_pct)

# get list of current members, as of 2016 election
current_members <- house_summary %>%
  filter(year == 2016 & is.na(is_special))

# get current members who were plurality members
current_plurality <- current_members %>%
  inner_join(plurality, by = "winner_id")

write.csv(current_plurality, file='current_plurality.csv')

write_ins <- house %>%
  filter(is_write_in == 1) 

incumbent_defeated <- house_summary %>%
  filter(upset == 1)
  

write.csv(incumbent_defeated, file='incumbent_defeated.csv')

crosstab13 <- incumbent_defeated %>%
  filter(year>1996) %>%
  crosstab(g_plurality, p_plurality, percent = "none")

crosstab14 <- incumbent_defeated %>%
  filter(year>1996) %>%
  crosstab(g_plurality, switched_party, percent = "none")

# compare open seat primary, general, special votes for open seat elections
house_special_turnout <- house_summary %>%
  filter(incumbent_ran != 1 & p_non_comp == 0) %>%
  group_by(is_special)  %>%
  summarise_at(vars(g_votes, p_votes), funs(mean, min, max), na.rm=TRUE)

crosstab15 <- house_summary %>%
  filter(year>1996 & incumbent_ran != 1 & is.na(is_special)) %>%
  crosstab(non_comp, p_non_comp)

write.csv(house_summary, file='house_summary.csv')

house_summary$is_special[is.na(house_summary$is_special)] <- 0

results1 <- lm(g_votes ~ g_unopposed + is_special + year + incumbent_ran, data = house_summary)
summary(results1)

results2 <- lm(p_votes ~ p_unopposed + is_special + year + incumbent_ran, data = house_summary)
summary(results2)

# find 2016 general winners who were also elected in 1996.
winners_1996 <- house_elections %>%
  filter(year == 1996 & is.na(is_special))

winners_2016 <- house_elections %>%
  filter(year == 2016 & is.na(is_special))

before1996 <- winners_2016 %>%
  inner_join(winners_1996, by = "winner_id")
# wow, 24 who were first elected in 1996 or earlier

longtime_incombents <- elstats %>%
  inner_join(before1996, by = c("candidate_id" = "winner_id"))

write.csv(longtime_incombents, file='longtime_incumbents.csv')
