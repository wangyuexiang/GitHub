1. Files
2. Data Structure
3. Running

###########
1. Files
  1.1 Reference file
	    *Ref_sens.csv*: table Entr-Sor to SensEntr-SensSor
	1.2 Input files
	  - *App.csv*: history of real users
		-	*VIP.csv*: history of beta-testers
	1.3 Rscript
	  - *DataPreparation.R*: prepare the data from input file to usable R object
		-	*Functions.R*: all the functions
		-	*main.R*: main script for the project
		-	*mainWithoutArgs.R*: pour tourner dans RStudio
2. Data Structure
  1.1 Input file:
	  - EVA
	  - Ste (5 digits): eg. 25004
		- Badge(7 digits) : eg. 9033577
		- Porteur(5 digits) : eg. 3 (which means 00003)
		- All the other columns: Entr, Sor, Date, DOW, WOY, TimeEntr, TimeSor, Voie

3. Running
  3.1 Parameters for main.R:
	  - InputFileName
		- OutputFileName
		- Start Date
		- End Date
		- (number of lines to read )
	3.2 Command example
		*rscript main.r App.csv Output.csv 2015-5-1 2015-8-31 *
		*rscript main.r App.csv Output.csv 2015-5-1 2015-8-31 5000 *
