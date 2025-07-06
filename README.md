## PIAAC_Variable_Finder
The PIAAC Variable Finder: An interactive Shiny app for cleaning, interpreting and analyzing Programme for the International Assessment of Adult Competencies data

Nate Breznau, breznau.nate@gmail.com
German Institute for Adult Education - Leibniz Centre for Lifelong Learning

### App Deployment Options

#### Permanent deployment at shinyapps.io

[Web-based](https://nate-breznau.shinyapps.io/PIAAC_Variable_Finder/)

#### Run locally

Download and unzip folder and run from R Studio (or IDE of choice)

[Local app, zip folder](https://github.com/nbreznau/PIAAC_Variable_Finder/blob/main/Shiny.zip)

### Workflow

#### PUFs by country as csv

##### 1. Gain access to PUFs https://www.oecd.org/en/about/programmes/piaac/piaac-data.html

##### 2. Download all and put into folders
2.1. Data/piaac/cycle1csvs - Cycle 1 files
2.2. Data/piaac/cycle2csvs - Cycle 2 files

##### 3. Run /Code/prep_piaac_data.R

Note that for whatever reason my Cycle 1 files are "," separated but Cycle 2 are ";" separated. You might have to adjust this in the routine that combines the files /Data/piaac/piaac_prep.R and /Data/piaac/piaac_prep2.R

##### 4. Run /Code/prep_shiny.R

This parses the questionnaires and merges all the relevant meta data.

It also checks for non-missings across Cycle and the two countries.

Note that here the user could add more countries to the app by adding more questionnaires and parsing them into the relevant meta data

##### 5. Deploy Shiny app