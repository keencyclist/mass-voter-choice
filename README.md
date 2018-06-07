# mass-voter-choice
Analysis of Massachusetts elections and the need for Ranked Choice Voting

Uses data from Massachusetts elections - http://electionstats.state.ma.us/

Separate code for Mass. House and Senate. Both are for 1998 - 2016 elections.

The original data does not identify the incumbent. The district definitions changed over this period, so boundary files were examined for redistricting years to determine the incumbent after redistricting, since in many cases the districts remained very similar but their numbering changed.

The code includes a substantial amount of data cleaning. For example, candidates were not always given a single unique ID. It was  necessary to fix this problem to identify the incumbent (winner of previous regular or special election).

The code calculates summary statistics per election cycle -- including data about both primaries and generals -- including:
* winner's share of vote
* total votes in primary and in general
* number of candidates in primary and general
* flag for open seat election; plurality winner in primary, general, or both; unopposed in primary and general; non-competitive in primary and general
