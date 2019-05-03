# Research Methods Project

## Guiding Questions

For this study: I plan to run the following analyses   

1. ANOVA on the interaction between knowledge, participation, and 3-category Party ID on each of the 4 feeling thermometers  
2. Correlation of knowledge, as a continuous variable, on feelings towards opposite political party/ideology (i.e. divide data among democrats or republicans/liberals or conservatives and see if, among these groups, they become warmer to their own party/colder to the opposite party/ideology as they increase with knowledge)  
3. Correlation like above but with participation.

## Data Source

The root file for the analyses come from the ANES 2016 Time Series Data which can be accessed for free online at the following links:

- [ICPSR](https://www.icpsr.umich.edu/icpsrweb/ICPSR/studies/36824)
- [ANES Data Center](https://electionstudies.org/project/2016-time-series-study/)

The data used for the analyses are a condensed version of the main file. The `Management.R` script file cleans the data for analysis and extracts only the necessary variables for the analysis.

The resulting CSV file is located in the repository as `RMProjData.csv` and can be used to replicate the analyses in SAS, SPSS and Stata. The R Code for converting this file to formats compatible in these other analyses software is available in the `DataConvert.R` file.

## Core Variables

The code for the variable cleaning process are all located in the `Knowledge/` folder in this repository.

### Political Participation 

|ANES Variable Code| Variable Name| Notes|
|:-------------|:-----------:|:----------|
|V162010| talk| Talk to people about voting|
|V162011| meeting| Attend a political meeting|
|V162012| wear| Wear Campaign Button|
|V162013| work|Do any other work for party|
|V162014| campaigndon| Donate money to a campaign|
|V162016| partydon| Donate money to a political party|
|V162018A| attend| Attend a rally or protest|
|V162018B| petition| Signed a petition|
|V162018E| post| Post on social media about politics|
|V162019| contact| Contact a US Representative or Senator|
|V162031X| vote16| Voted in the 2016 Presidential Election|

### Political Knowledge

|ANES Variable Code| Variable Name| Notes|
|:-------------|:-----------:|:----------|
|V162072| biden| Joe Biden - Vice President of the United States|
|V162073A| ryan| Paul Ryan - Speaker of the House|
|V162074B| merkel| Angela Merkel - Chancellor of Germany|
|V162075A| putin| Valdmir Putin - President of Russia|
|V162076B| roberts| John Roberts - Chief Justice of the Supreme Court|

### Feeling Thermometers

|ANES Variable Code| Variable Name| Notes|
|:-------------|:-----------:|:----------|
|V161095| feeldem| Feelings towards Democrats --> 0 - 100|
|V161096| feelrep| Feelings towards Republicans --> 0 - 100|
|V162097| feellib| Feelings towards Liberals --> 0 - 100|
|V162101| feelcons| Feelings towards Conservatives --> 0 - 100|

### Participant Variables

|ANES Variable Code| Variable Name| Notes|
|:-------------|:-----------:|:----------|
|V161158X| pid7| Party ID 7 category, 1 = Strong Democrat, 7 = Strong Republican|
|V161155| pid3| Party ID 3 category, 1 = Democrat, 2 = Republican, 3 = Independent|
|V162171| ideo7| Ideology 7-point, 1 = Extremely Liberal, 7 = Extremely Conservative|
|V162171A| ideo3| Ideology 3-point, 1 = Liberal, 2 = Conservative, 3 = Moderate| 

## Code Files

The following files address the questions of interest proposed for this project. 

`CorrAnalysis.R`: Run correlations for the knowledge and participation variables. Code answers the following questions:

- Does more knowledge lead to more polarized feelings towards the opposite party?
- Does more participation lead to more polarized feelings towards the opposite party?

`AOVAnalysis.R`: Run ANOVA to understand the interaction between knowledge and participation on feelings towards Democrats, Republicans, Liberals, and Conservatives.

## Additional Code Files

The following code files were generated after running the above files. The results prompted further curiosity and motivation to run analyses to answer the following questions.

`Know-PartAOV.R`: Run the following ANOVAs (`aov()` is the R notation for ANOVA)

- AOV Party x Knowledge on Feelings towards Democrats
- AOV Party x Knowledge on Feelings towards Republicans
- AOV Party x Participation on Feelings towards Democrats
- AOV Party x Participation on Feelings towards Republicans
- AOV Ideology x Knowledge on Feelings towards Liberals
- AOV Ideology x Knowledge on Feelings towards Conservatives
- AOV Ideology x Participation on Feelings towards Liberals
- AOV Ideology x Participation on Feelings towards Conservatives

`PartIdeoAOV.R`: Run the following ANOVAs

- AOV Party x Ideology on Feelings towards Democrats
- AOV Party x Ideology on Feelings towards Republicans
- AOV Party x Ideology on Feelings towards Liberals
- AOV Party x Ideology on Feelings towards Conservatives

## Graphics

This folder contains the graphs generated in each of the R codes.


