## Index
###1. Files
###2. Data Structure
###3. Parameters
###4. Running


## Details
###1. Files in 4 Repositories
	-Input
		- *App.csv*: history of real users
		- *VIP.csv*: history of beta-testers
	- Output
	- Parameters (in details in Section 3)
		- *Param_common.csv*: limits of filter for results
		- *Param_algo1.csv*: parameteres for algo1
		- *Param_makeGrid.csv*: paramteres for make Grid system
	- Reference
		- *Ref_gares.csv*: 
		- *Ref_sens.csv*: table Entr-Sor to SensEntr-SensSor
		- *Ref_VIPn.csv*
		- *Ref_GridLimit.csv*
		- *Ref_ODtoGrid.csv*
	- Rscript
		- *Algo.R*: main script for Algo, combine Algo1, Algo2
		- *Algo1_DataPreparation.R*: prepare the data from input file to usable R object
		- *Algo1_Functions.R*: all the functions for Algo1
		- *Algo2_Functions.R*: all the functions for Algo2 (connecting Grid to make Zone)
		- *MakeGrid.R*: create a Grid system according to *Param_makeGrid.csv*
###2. Data Structure
	- Input file:
	  - EVA
	  - Ste (5 digits): eg. 25004
		- Badge(7 digits) : eg. 9033577
		- Porteur(5 digits) : eg. 3 (which means 00003)
		- All the other columns: Entr, Sor, Date, DOW, WOY, TimeEntr, TimeSor, Voie
	- Output file: (name is in the format: VIP_vYYYYMMDD_HHMM.csv)
		- Algo1_TS: ID, OD, Tmin, Tmax
		- Algo2_ZW
			- "Zone-Grid" reference
			- Zone during the "Day" period
			- Zone during the "Night" period
		- Algo_Summary: summary of result from Algo1 & Algo2
###3. Parameters
	- For general: (got in the script) (used for *Algo.R*)
		- filename.Input: input filename (with .csv)
		- ParamRepo: repository of parameters
	- *ParamRepo/Param_common.csv* (used for *Algo.R*)
		- limit.Algo1.noPsg : under which the result of Algo1 is ignored, by default = 5
		- limit.Algo2.GridPer : under whcih Grid is not considered as frequently visited, by default = 0.5
		- limit.Algo2.ActiveDay : under which Zone is not considered as frequently visited, by default = 5
		- limit.Algo2.Day: under which Zone is not considered as frequently visited in the "Day" period, by default = 5
		- limit.Algo2.Night: under which Zone is not considered as frequently visited in the "Night" period, by default = 3
		- Algo2.DayStart: start Hour of "Day" period, by default = 6
		- Algo2.DayEnd: end Hour of "Day" period, by default = 22
		- day.start: start date of period in question
		- day.end: end date of period in question
		- filter: decide whether or not ID with less trx will be removed
		- logFile: decide whether intermediate result will be saved (in .log files)
	- *ParamRepo/Param_makeGrid.csv* (used for *MakeGrid.R*)
		- gridStep: unit in degree, width & height of each grid rectangular, by default = 0.25 (about 35km)
		- zoneStep: unit in degree, extension for Entr or Sor with no Lng&Lat, ex. Systeme Ouvert, by default = 0.1
###4. Running Command example: *rscript Algo.r*
