# NOAA Storm Data Analysis

## Note for grader

The complete submission is in index.PDF.

-----

This analysis is in fulfillment of the Course 5, Week 4 assignment for the John Hopkins Data Science in R series of Coursera courses. 

Assignment

The basic goal of this assignment is to explore the NOAA Storm Database and answer some basic questions about severe weather events. You must use the database to answer the questions below and show the code for your entire analysis. Your analysis can consist of tables, figures, or other summaries. You may use any R package you want to support your analysis.

Questions

Your data analysis must address the following questions:

1.	Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful with respect to population health?

2.	Across the United States, which types of events have the greatest economic consequences?
Consider writing your report as if it were to be read by a government or municipal manager who might be responsible for preparing for severe weather events and will need to prioritize resources for different types of events. However, there is no need to make any specific recommendations in your report.

Review criteria
1.	Has either a (1) valid RPubs URL pointing to a data analysis document for this assignment been submitted; or (2) a complete PDF file presenting the data analysis been uploaded?
2.	Is the document written in English?
3.	Does the analysis include description and justification for any data transformations?
4.	Does the document have a title that briefly summarizes the data analysis?
5.	Does the document have a synopsis that describes and summarizes the data analysis in less than 10 sentences?

Sypnosis

We analyze the storm data available from NOAA for the years 1950-2011. The data as loaded requires filtering for columns we will not use and several transformations. In our first transformation, we select the year for each observation out of a string, and later perform an inflation adjustment for the financial costs by year using the website [https://www.usinflationcalculator.com/]. We perform transformations into new columns that capture the sum of fatalities and injuries, and the sum of financial damage to property and crops. To get to the sum of financial damage to property and crops, we fist change the exponent for property (PROPDMGEXP) and crop (CROPDMGEXP) damage from a letter, any other character, or blank to a number. We use this number as the product to multiply the respective base (PROPDMG, CROPDMG) costs by. . We perform an outlier analysis of the fatalities and the sum of financial damage to property and crops, and make corrections to the data for the latter. We recode the event type (EVTYPE) to eight categories from hundreds of descriptions found in the data. Finally, we sum the data for each year and each category and plot the result for fatalities and injuries, and the sum of financial damage to property and crops. 

6.	Is there a section titled "Data Processing" that describes how the data were loaded into R and processed for analysis?
7.	Is there a section titled "Results" where the main results are presented?
8.	Is there at least one figure in the document that contains a plot?
9.	Are there at most 3 figures in this document?

Figures:

![Property Costs](final-data-plot-property-1.png)

![People Costs](final-data-plot-people-1.png)

![Property Costs since 1993](final-data-plot-property-since1993-1.png)

10.	Does the analysis start from the raw data file (i.e. the original .csv.bz2 file)?
11.	Does the analysis address the question of which types of events are most harmful to population health?

Results

The eight categories of natural disaster in our analysis are:

- DROUGHT, EXCESSIVE HEAT
- HEAVY RAIN, FLOODING, MUDSLIDES, LANDSLIDES
- HURRICANE SEASON
- THUNDERSTORM
- TORNADO, HAIL, HIGH WIND
- WILDFIRE
- WINTER WEATHER
- OTHER (e.g.: Tsunamis, rip currents, fog)

Knowning that the categories are not comparable means that all we can say about the data when we consider all of the data are the total costs to property and crops and the total fatalities and injuries. 

With the caveat of incompletely reported data, for what has been reported between 1950 and 2011, "TORNADO, HAIL, HIGH WIND" events have cost	101,337 fatalities and injuries, or 65.1 percent of all reported fatalities and injuries in this dataset. All of the other categories represent much smaller fractions of this total, yet the data is unevenly reported.

For fatalities and injuries, though, the TORNADO, HAIL, HIGH WIND, at 28,883 has cost more than twice the total number of fatalities and injuries than the nearly equally costly three next categories: THUNDERSTORM at 12734 fatalities and injuries, DROUGHT, EXCESSIVE HEAT at 12174 fatalities and injuries, and HEAVY RAIN, FLOODING, MUDSLIDES, LANDSLIDES at 11566 fatalities and injuries.

12.	Does the analysis address the question of which types of events have the greatest economic consequences?

Results

The eight categories of natural disaster in our analysis are:

- DROUGHT, EXCESSIVE HEAT
- HEAVY RAIN, FLOODING, MUDSLIDES, LANDSLIDES
- HURRICANE SEASON
- THUNDERSTORM
- TORNADO, HAIL, HIGH WIND
- WILDFIRE
- WINTER WEATHER
- OTHER (e.g.: Tsunamis, rip currents, fog)

The years reported for the eight categories of natural disasters vary for all effects (property damage, crop damage, fatalities, and injurues). The TORNADO, HAIL, HIGH WIND category has data for the complete time period of this analysis (1950-2011). Fatalities and Injuries are reported for 1983-2011 for the THUNDERSTORM category. For the other six categories, and for the the THUNDERSTORM category for property damage and crop damage. 

The total costs for property and crop damage, knowing that all categories are not fully reported for the 1950-2011 time period, after inflation adjustment and recoding of a handul of the largest outliers for property and crop damage is $495.7 billion dollars. The total number of fatalities and injuries for this time period is 155,673, with 15,145 (9.7 percent) consisting fatalities, and 140,528 (90.3 percent) consisting of injuries.

Knowning that the categories are not comparable means that all we can say about the data when we consider all of the data are the total costs to property and crops and the total fatalities and injuries. 

With this caveat, for what has been reported between 1950 and 2011, "TORNADO, HAIL, HIGH WIND" events have cost	$172.9 billion dollars in property and crop damage, followed by 
"HEAVY RAIN, FLOODING, MUDSLIDES, LANDSLIDES"	property and crop damage costs at $139.6 billion dollars, and "HURRICANE SEASON"	property and crop damage costs at $119.6 billion dollars. The next categories accounted for relatively lower costs ($20 billion dollars and lower) each.

The comparable data range for the data in this analysis is the time period 1993-2011. The totals for this time range are shown below. The top two categories for property and crop damage costs are close: HEAVY RAIN, FLOODING, MUDSLIDES, LANDSLIDES at	$139.6 billion dollars, and 
HURRICANE SEASON at	$119.6 billion dollars. The third category has also been financially costly, TORNADO, HAIL, HIGH WIND, at $62.2 billion dollars. 


13.	Do all the results of the analysis (i.e. figures, tables, numerical summaries) appear to be reproducible?
14.	Do the figure(s) have descriptive captions (i.e. there is a description near the figure of what is happening in the figure)?
15.	As far as you can determine, does it appear that the work submitted for this project is the work of the student who submitted it?

