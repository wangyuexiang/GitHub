1. Files
2. Data Structure
3. Parameters
4. Running
###########
1. Files in 4 Repositories
  -Input
	  - *App.csv*: history of real users
		- *VIP.csv*: history of beta-testers
	- Output
	- Parameters
		- *Param_Algo.csv*
		- *Param_Algo1.csv*
		- *Param_Algo2.csv*
		- *Param_Algo2_makeGrid.csv*
	- Reference
		- *Ref_gares.csv*: 
	  - *Ref_sens.csv*: table Entr-Sor to SensEntr-SensSor
		- *Ref_VIP.csv*
		- *Ref_GridLimit.csv*
		- *Ref_ODtoGrid.csv*
	- Rscript
	  - *Algo1_DataPreparation.R*: prepare the data from input file to usable R object
		-	*Algo1_Functions.R*: all the functions
		-	*Algo1.R*: main script for Algo1_TimeSpace
		-	*mainWithoutArgs.R*: pour tourner dans RStudio
2. Data Structure
  - Input file:
	  - EVA
	  - Ste (5 digits): eg. 25004
		- Badge(7 digits) : eg. 9033577
		- Porteur(5 digits) : eg. 3 (which means 00003)
		- All the other columns: Entr, Sor, Date, DOW, WOY, TimeEntr, TimeSor, Voie
	- Output file: (name is in the format: VIP_vYYYYMMDD_HHMM.csv)
		- Algo1_TimeSpace
		- Algo2_ZoneWindow
			- Zone frequently visited by each ID
			- Window with high active visits
3. Parameters
  - For Algo1: *Param_Algo1.csv*
	  - InputFileName
		- Start Date
		- End Date
	- For Algo2: *Param_Algo2.csv*
		- InputFileName
		- Start Date
		- End Date 
		- limit.ActiveDay
		- limit.Zone
	- For Algo2_makeGrid: *Param_Algo2_makeGrid.csv*
		- gridStep
		- zoneStep
4. Running Command example
	*rscript Algo1.r*
	*rscript Algo2.r*
