# German Willkommenskultur - Where and Why? An analysis of German refugee initiatives

by Christopher Cosler and Lisa Schmid

<a href="https://rawgit.com/ChristopherCosler/CSSR_FinalProject/master/Final_Presentation.html" target="_blank"> Presentation of our project </a>


Any feedback, comments, and ideas are encouraged. Please send them to <a href="mailto:c.cosler@mpp.hertie-school.org">Christopher Cosler</a> or <a href="mailto:lisa.schmid@mpp.hertie-school.org">Lisa Schmid</a> or submit a pull request.

This is not the final paper. All the work included in this repository is work in progress and should be seen as such!

The shiny app for this project can be found here: <a href="https://christophercosler.shinyapps.io/RefugeeApp" target="_blank">here on the shinyapps.io servers</a>.

## Instructions and background information on our repository

This repository contains the files of our data collection and analysis part for the course <a href="https://github.com/HertieDataScience/SyllabusAndLectures" target="_blank">MPP-E1180: Introduction to Collaborative Social Science Data Analysis</a> taught by <a href="https://github.com/christophergandrud
" target="_blank">@ChristopherGandrud</a> at the <a href="https://hertie-school.berlin" target="_blank">Hertie School of Governance</a>  in Fall 2015.

In addition to the Readme file, our repository contains two BibTeX files, an RMD file, a PDF file, a Shiny app and the data necessary to reproduce the study. The PDF file was created using the knit function of R Studio, the app by R Shiny. The referenced literature can be found in the BibTeX file "Literature.bib". The R Packages used for the paper are cited in the BibTeX file "Rpackages.bib". As the BibTeX files and the RMD file are dynamically linked, it is important to save all three files in the same folder when trying to run the RMD file. The Shiny app can be found [here](https://christophercosler.shinyapps.io/RefugeeApp).

The focus of our research is the identification of the determinants of organizations assisting refugees (refugee initiatives). 

We used data from the <a href="https://www-genesis.destatis.de/genesis/online">German Statistical Office</a>, the <a href="https://www.regionalstatistik.de/genesis/online">German Regional Statistical Offices</a>, and the <a href="http://www.proasyl.de/de/ueber-uns/foerderverein/mitmachen/">customized Google Map maintained by ProAsyl</a> as well as data from <a href="http://www.geodatenzentrum.de/geodaten/gdz_rahmen.gdz_div?gdz_spr=deu&gdz_akt_zeile=5&gdz_anz_zeile=1&gdz_unt_zeile=19&gdz_user_id=0">German Federal Agency for Cartography and Geodesy</a>.

No authorization is required to access any of these sources. Some of the data is retrieved automatically when running the R code, other had to be downloaded manually and is now stored in a subfolder of this project (Data). If you want to download the data yourself please follow the instructions in the paper accompanying this project.

## Structure of our paper

Our paper follows the following strucutre:

1. Introduction
2. Short Summary of Hypotheses
3. Description of Data Sources
    1. Dependent Variable
    2. Independent Variables
4. Analysis
    1. Descriptive Statistics
    2. Inferential Statistics
5. Outlook
6. Bibliography

## Task for the assignment

- Gather web based data from at least two sources
- Merge the data sets
- Conduct basic descriptive and inferential statistics on the data to address a relevant research question 
- Briefly describe the results including with dynamically generated tables and figures

Deadline for submission:  13th November 2015

## Folder and file structure

Please note that we have three R scripts in our repository: Gather.R imports all data sets, CleanandMerge.R cleans and merges all those data sets imported (and is linked to Gather.R). In Analysis.R we compute all descriptive and inferential statistics. It is also linked to CleanandMerge.R so that running Analysis.R should suffice without having to run the two other scripts. 

    .
    ├── German Willkommenskultur Output.rmd # Research proposal rmd file used to produce pdf, html and Word document
    ├── German Willkommenskultur Output.pdf # Research proposal as pdf
    ├── Literature.bib                      # Contains literature cited in the proposal.
    ├── RPackages.bib                       # Contains packages used for the analysis.
    ├── README.md                           # Readme file
    ├── Analysis                            # Folder containing the anaylsis code
    │   ├── Analysis.R                       # Analysis R code
    ├── Data                                # Folder containing data, the Gather code and the Clean and Merge code
    │   ├── Gather.R                        # Gather R code - import of all data sets
    │   ├── CleanandMerge.R                 # Clean and Merge R code - cleaning and merging of data sets
    │   ├── Shapefiles                      # Folder containing the shapefiles
    │   ├── Indenpendent variables          # Folder containing the independent variables
    |       ├── 252-01-4.csv                # Voter turnout
    |       ├── 659-71-4.csv                # Unemployment
    |       ├── 661-31-4.csv                # Refugees   
    |       ├── 12411-0017.csv              # Gender, age, and population numbers
    |       ├── AI002-1.csv                 # Population density
    |       ├── AI003-2.csv                 # Abitur/Education
    |       ├── AI-N-10.csv                 # GDP per capita
    ├── App                                 # Folder containing code and files for the Shiny app
    │   ├── app.R                           # Shiny app R code
    │   ├── Shapefiles                       # Folder containing the shapefiles
    

## Annex 
R version 3.1.3 (2015-03-09) -- "Smooth Sidewalk"
Copyright (C) 2015 The R Foundation for Statistical Computing
Platform: x86_64-w64-mingw32/x64 (64-bit)

R Studio Version 0.99.484 – © 2009-2015 RStudio, Inc.

R Core Team (2015). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. URL http://www.R-project.org/.

Style guide: We use Google's Style Guide from https://google-styleguide.googlecode.com/svn/trunk/Rguide.xml
