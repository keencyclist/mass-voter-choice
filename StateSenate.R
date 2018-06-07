install.packages("tidyverse")
library(tidyverse)

# read in complete 1970-2016 state election data
elstats <- read.csv('elstats.csv', na.strings="NULL")

# CLEANING - First Hampden and Fifth Middlesex 1999 Special Elections were erroneously coded as State Senate, should be State Rep.
elstats <- elstats %>%
  mutate(office_name = replace(office_name, contest_id %in% c(15950, 15951, 15949, 15952, 15953, 15954), 'State Representative'))
  
# create a dataframe that has only state senate election from 1996 and later
senate1996 <- elstats %>%
  filter(office_name == 'State Senate' & year>1995)

# --------------ERROR CHECKING
# check for duplicate candidate IDs.
# create list of unique candidate name / candidate_id combos
table(senate1996$candidate_id)

unique_id <- senate1996 %>%
  group_by(candidate_id,display_name) %>%
  summarise(n = n())

# find candidates with more than one candidate_id
duplicate_id <- unique_id %>%
  group_by(display_name) %>%
  summarise(n = n()) %>%
  filter(n>1)
# -----------------------------

# Install & load the car package for recodes
install.packages("car")
library(car)
# CLEANING recode candidates that should have the same ID:
senate1996$candidate_id <- recode(senate1996$candidate_id,"10375=1187; 10377=1190; 10412=1018; 11414=680; 10413=1019;11416=11415; 
      11742=434; 15547=10930; 15805=10930; 15438=10930; 10396=588; 10081=456; 11989=439; 15849=163; 10922=158; 15864=15420; 10123=912") 
 
# 1187 Klaus Kubierschky 10375 Klaus Kublerschky
# 1190 Lois G. Pines 10377 Lois G. Pins
# 10412 Michael P. Froimowits 1018 Michael P. Froimowitz
# 11414 Pam Resor 680 Pamela P. Resor
# 1019 Peter C. Schoaff 10413 Peter C. Sehoaff
# 11415 Richard G. Youkus 11416 Richard G. Yurkus
# 11742 Sandi Martinez 434 Sandra B. Martinez
# 15547 Sonia Rosa Chang-D?az 15805 Sonia Rosa Chang-DÃ¯Â¿Â½az 15438 Sonia Rosa Chang-DÃaz 10930 Sonia Rosa Chang-DÃƒÂ�az
# 588 Steven C. Panagiotakos 10396 Steven C. Panaglotakos
# 456 Thomas F. Keyes 10081 Thomas Francis Keyes
# 11989 Cynthia Stone Creem 439 Cynthia Stone Creem
# 15849 Donald F. Humason, Jr 163 Donald F. Humason, Jr
# 10922 Enrico John Villamaino, III 158 Enrico John Villamaino, III
# 15864 Eric P. Lesser 15420 Eric Philip Lesser
# 10123 Cheryl Anne Jacques --> 912


# --------------ERROR CHECKING
# check out the 621 null values
null_senate <- senate1996 %>%
  filter (candidate_id == "NULL")
table(null_senate$contest_stage, null_senate$year)      
# conclusion: none are general election, most are Republican and minor party primaries

# check if contest_id refers to a unique combination of office_name, district_name, contest_stage, is_special, year 
unique_contest <- senate1996 %>%
  group_by(contest_id,office_name, district_name, contest_stage, is_special, year) %>%
  summarise(n = n())

# find candidates with more than one candidate_id
duplicate_contest <- unique_contest %>%
  group_by(contest_id) %>%
  summarise(n = n()) %>%
  filter(n > 1)
# result: empty data frame, so contest_id is correct

# remove unneeded data frames
remove(unique_contest, unique_id)
remove(duplicate_contest, duplicate_id)
remove(senate)
remove(null_senate)
# -----------------------------------------------------


# find the election winner - max of candidate_votes - winner is r=1
senate1996 <- senate1996 %>%
   group_by(contest_id) %>%
  mutate(r = min_rank(desc(candidate_votes)))

# recode to assign the applicable districting year 
senate1996$district_year <- recode(senate1996$year,"1993:2001=1993; 2002:2011=2002; 2012:2016=2012")

# convert date into date type
library(lubridate)
senate1996$election_date <- ymd(senate1996$date)

# the special elections in early 2002 use the pre-2002 district names
# Note: there was a special election in Jan 2012, but the old and new district names were the same, so no need to correct this
senate1996 <- senate1996 %>%
  mutate(district_year = replace(district_year, election_date > "2001-12-31" & election_date < "2002-09-01", 1993))

# Assign consistent district names based on reading in data from MassGIS
senate_districts <- read.csv('senate_districts.csv', na.strings="NULL")

# join elections data to senate district # info, under new dataframe name
senate <- senate1996 %>%
  left_join(senate_districts, by = c("district_year" = "Year", "district_name" = "DISTNAME"))

# drop unneeded new columns (all except DISTNUM)
senate <-  senate %>%
  select(-FIRST,-LAST, -PARTY, -NAME)


# CLEANING 1998 is missing the date, but there were no special state senate elections that year [except Jan 06, see below]
# assign Nov 3, 1998 to all 1998 state senate elections
senate <- senate %>%
  mutate(election_date = replace(election_date, year==1998, '1998-11-03'))

# CLEANING there was a Jan 6, 1998 special election, but the year was coded as 1997 because primary was in 1997
senate <- senate %>%
  ungroup %>%
  mutate(year = replace(year, election_date == '1998-01-06', 1998))

# CLEANING for Distnum 11, General election in 2000 failed to list candidate party
senate <- senate %>%
  mutate(candidate_party = replace(candidate_party, DISTNUM == 11 & contest_stage == 'General' & 
                                   election_date == '2000-11-07', 'Democratic'))


#--------Error Checking--
# create new dataframe with each general election winner
senators <- senate %>%
  filter(r == 1, contest_stage == 'General') %>%
  arrange(DISTNUM, election_date)
# check the number of elections for each senate district (some are special)
table(senators$DISTNUM)
#------------------------

# create dataframe of elections - first with general election data only
senate_elections <- senate %>%
  filter(contest_stage == 'General') %>%
  group_by(DISTNUM,election_date, contest_id) %>%
  summarize(district = first(district_name), g_candidates = n(), is_special = first(is_special), g_votes = first(total_votes), 
            year=first(year), winner_id = candidate_id[r == 1], winner = display_name[r == 1], 
            winner_party = candidate_party[r == 1], winner_pct = pct_candidate_votes[r == 1])

# order the general election contests (for each district) and identify the incumbent
senate_elections <- senate_elections %>%
  group_by(DISTNUM) %>%
  mutate(election_order = min_rank(election_date)) %>%
  mutate(incumbent_id = lag(winner_id, order_by = election_order)) %>%
  mutate(incumbent_won = ifelse(winner_id == incumbent_id, 1, 0)) %>%
  mutate(incumbent_party = lag(winner_party, order_by = election_order)) %>%
  mutate(switched_party = ifelse(winner_party != incumbent_party & winner_id != incumbent_id & 
                                   incumbent_party != 'Unenrolled' & winner_party != 'Unenrolled', 1, 0)) %>%
  arrange(DISTNUM, election_order)

# CORRECTION. In 1998, Charles Shannon, the 2nd Middlesex incumbent Republican, ran (uncontested) in the Democratic party
#             and thereafter was elected as a Democrat.
senate_elections <- senate_elections %>%
  mutate(incumbent_party = replace(incumbent_party, contest_id == '7997', 'Democratic'))
  
# CORRECTION. The primary for the 1998 Special Election in the Suffolk & Norfolk district occured in Dec 1997
#             The primary for the 2012 Special Election in the Second Suffolk and Middlesex district occured in Dec 2011
senate <- senate %>%
  mutate(year = replace(year, contest_id == '15977', 1998)) %>%
  mutate(year = replace(year, contest_id == '34632', 2012))

# add the relevant primary elections  -  those of the incumbent's party
senate_summary <- senate_elections %>%
  filter(year > 1996) %>%
  left_join(senate, by = c("DISTNUM", "year","is_special","incumbent_party" = "contest_stage"))

# determine if the incumbent entered the general election winner's primary - will sum to 1 if one of the participants is an incumbent, else to 0.
senate_summary <- senate_summary %>%
  group_by(DISTNUM,election_date.x) %>%
  mutate(incumbent_ran = ifelse(candidate_id == incumbent_id, 1, 0))
         
# summarize the primary data -- # of p_candidates, p_votes, p_winner_pct 
# NOTE: Error in the original file: Prior to 2002, some or all of the primary elections have the general election (November) date 
#       in place of the actual (September) date.
senate_summary <- senate_summary %>%
  group_by(DISTNUM,election_date.x) %>%
  summarize(contest_id = first(contest_id.x), district = first(district), g_candidates = first(g_candidates),
            is_special = first(is_special), g_votes = first(g_votes), year=first(year), 
            winner_id = first(winner_id), winner = first(winner),winner_party = first(winner_party), 
            winner_pct = first(winner_pct), election_order = first(election_order), incumbent_id = first(incumbent_id),
            incumbent_won = first(incumbent_won), p_candidates = n(), switched_party = first(switched_party), p_votes=first(total_votes), 
            p_winner_pct = pct_candidate_votes[r == 1], incumbent_ran = sum(incumbent_ran))

# CORRECTION - need to code Boncore (First Suffolk and Middlesex District) in Sept & Nov 2016 elections as incumbent.
# because only the special primary election of earlier in 2016 is in the data, not the special general. But it is on the website.
senate_summary <- senate_summary %>%
  mutate(incumbent_ran = replace(incumbent_ran, contest_id == 40172, 1)) %>%
  mutate(incumbent_won = replace(incumbent_won, contest_id == 40172, 1))

# CORRECTION - State Senator Jim Mazilli withdrew from the 2008 election (following his arrest), but his name was still on the ballot.
#              State Senator Rauschenbach withdrew for another job after primary but before general in 2000
senate_summary <- senate_summary %>%
  mutate(incumbent_ran = replace(incumbent_ran, contest_id %in% c(8894, 14327), 0))

# determine cases where incumbent was running but lost
senate_summary <- senate_summary %>%
  filter(year > 1996) %>%
  mutate(upset = ifelse(incumbent_ran == 1 & incumbent_won != 1, 1, 0))

# recode plurality_win - p_winner_pct < .50
senate_summary <- senate_summary %>%
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

# Error checking ---------
senate_check <- senate_summary %>%
  filter(year > 1996 & is.na(incumbent_ran)) 
# ------------------------

# --------------CROSSTABS
install.packages("janitor")
library(janitor)

# Frequency count
# note: format is column, row
crosstab1 <- senate_summary %>%
  filter(year>1996) %>%
  crosstab(incumbent_ran, is_special, percent = "none") 

crosstab2 <- senate_summary %>%
  filter(year>1996) %>%
  crosstab(incumbent_ran, p_plurality, percent = "none") 

crosstab3 <- senate_summary %>%
  filter(year>1996) %>%
  crosstab(incumbent_ran, g_plurality, percent = "none")

crosstab4 <- senate_summary %>%
  filter(year>1996) %>%
  crosstab(incumbent_ran, non_comp, percent = "none")

crosstab5 <- senate_summary %>%
  filter(year>1996) %>%
  crosstab(incumbent_ran, p_non_comp, percent = "none")

crosstab6 <- senate_summary %>%
  filter(year>1996) %>%
  crosstab(incumbent_ran, g_unopposed, percent = "none")

crosstab7 <- senate_summary %>%
  filter(year>1996) %>%
  crosstab(incumbent_ran, p_unopposed, percent = "none")

crosstab8 <- senate_summary %>%
  filter(year>1996) %>%
  crosstab(incumbent_ran, unopposed, percent = "none")

crosstab9 <- senate_summary %>%
  filter(year>1996) %>%
  crosstab(incumbent_ran, incumbent_won, percent = "none")

crosstab10 <- senate_summary %>%
  filter(year>1996) %>%
  crosstab(incumbent_ran, switched_party, percent = "none")

crosstab11 <- senate_summary %>%
  filter(year>1996) %>%
  crosstab(incumbent_ran, non_comp_both, percent = "none")

crosstab12 <- senate_summary %>%
  filter(year>1996) %>%
  crosstab(incumbent_ran, plurality_either, percent = "none")

# get list of plurality winners
plurality <- senate_summary %>%
  filter(plurality_either == 1) %>%
  arrange(p_winner_pct, winner_pct)

incumbent_defeated <- senate_summary %>%
  filter(upset == 1)
write.csv(incumbent_defeated, file='incumbent_defeated.csv')


write_in_victory <- senate %>%
  filter(is_write_in == 1 & r == 1) 

# create output file for data visualizations
write.csv(senate_summary, file='senate_summary.csv')

