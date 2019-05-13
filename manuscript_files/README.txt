Project: VARYING DATASET RESOLUTION ALTERS PREDICTIVE ACCURACY OF SPATIALLY EXPLICIT ENSEMBLE MODELS FOR AVIAN SPECIES DISTRIBUTION
Accepted in Ecology and Evolution 12-Oct-2018.
Versioning visible at: https://github.com/baeolophus/ou-grassland-bird-survey

Please contact Claire M. Curry (cmcurry@ou.edu or curryclairem@gmail.com) about the *.R files
or Jeremy Ross (jdross@ou.edu) about the point count and transect data.

Description for each file or group of files:
	pointcount.csv: Missing data as "NA".  7079 rows, 13 columns.
		Date = date in month/day/year.  ONE OF FOUR FIELDS USED AS PRIMARY KEY to merge with pointcount_metadata.csv
		Observer = name or initials of observer.   ONE OF FOUR FIELDS USED AS PRIMARY KEY to merge with pointcount_metadata.csv
		Location = original written name for location.  ONE OF FOUR FIELDS USED AS PRIMARY KEY to merge with pointcount_metadata.csv
		Point = Name of point within location.  ONE OF FOUR FIELDS USED AS PRIMARY KEY to merge with pointcount_metadata.csv
		Species = 4-letter AOS banding code for species
		whattodowiththissighting = categorical used to filter out duplicated or otherwise bad records.
		Quantity = number of individuals seen
		Heard.Seen = S = seen, H = heard
		Angle..deg. = angle of birds from observer, in degrees.
		Distance..m. = distance of bird from observer, estimated in meters
		Breeding.Code = notes on birds' breeding status recorded opportunistically (i.e. blank does not mean absence of evidence)
		Flyover = categorical, was the bird flying over the point?
		Notes = any notes written by observer.
	
	pointcount_metadata.csv: Missing data as "NA".  625 rows, 17 columns.
		Date = date in month/day/year.  ONE OF FOUR FIELDS USED AS PRIMARY KEY to merge with pointcount.csv
		Observer = name or initials of observer.  ONE OF FOUR FIELDS USED AS PRIMARY KEY to merge with pointcount.csv
		Location = original written name for location.  ONE OF FOUR FIELDS USED AS PRIMARY KEY to merge with pointcount.csv
		Point = Name of point within location.  ONE OF FOUR FIELDS USED AS PRIMARY KEY to merge with pointcount.csv
		newspotnames = names for GPS points.
		index = alphanumeric field used in confirming gps locations to point names.  not used in analysis.
		cmc_comments  = questions from Claire Curry to Jeremy Ross during data cleaning/analysis
		answers  = comments from survey designer and participant Jeremy Ross to data cleaning/analysis done by Claire Curry
		whattodowiththisrecord  = categorical used to filter out duplicated or otherwise bad records.
		Start.Time..24h. = start time of point count in 24 hour time.
		Latitude = latitude in decimal degrees
		Longitude = longitude in decimal degrees
		Cloud = estimate of cloud cover in percentages
		Wind = wind estimates in miles per hour, ordinal with 5 mph bins
		Notes = any notes written by observer relating to the whole point count period
		Species.before = species seen before the point count began
		Species.After = species seen after the point count ended

	transect.csv: Missing data as "NA".  9724 rows, 14 columns.
		Date = date in month/day/year.  ONE OF FOUR FIELDS USED AS PRIMARY KEY to merge with transect_metadata.csv
		Observer = name or initials of observer.  ONE OF FOUR FIELDS USED AS PRIMARY KEY to merge with transect_metadata.csv
		Location = original written name for location.  ONE OF FOUR FIELDS USED AS PRIMARY KEY to merge with transect_metadata.csv
		Transect = transect name within location.  ONE OF FOUR FIELDS USED AS PRIMARY KEY to merge with transect_metadata.csv
		Time..24hr. = time of individual observation in 24 hour time.
		Possible.Species = 4-letter AOS banding code for species observed at this time.
		whattodowiththissighting  = categorical used to filter out duplicated or otherwise bad records.
		Quantity = number of individual birds for a given species
		Heard.Seen = categorical, S = seen, H = heard
		Angle..deg. = angle of birds from observer, in degrees.
		Distance..m. = distance of bird from observer, estimated in meters
		Breed.Code  = notes on birds' breeding status recorded opportunistically  (i.e. blank does not mean absence of evidence)
		Flying = categorical, was the bird flying over the point?
		Notes = any notes written by observer.
	
	transect_metadata.csv: Missing data as "NA".  168 rows, 23 columns.
		Date = date in month/day/year.  ONE OF FOUR FIELDS USED AS PRIMARY KEY to merge with transect.csv
		Observer = name or initials of observer.  ONE OF FOUR FIELDS USED AS PRIMARY KEY to merge with transect.csv
		Location = original written name for location.  ONE OF FOUR FIELDS USED AS PRIMARY KEY to merge with transect.csv
		Transect = transect name within location.  ONE OF FOUR FIELDS USED AS PRIMARY KEY to merge with transect.csv
		names = alphanumeric field used in confirming gps locations to transect names.  not used in analysis.
		START.newspotnames  = names of GPS points for end of transect
		END.newspotnames = names of GPS points for end of transect
		cmc_comments  = questions from Claire Curry to Jeremy Ross during data cleaning/analysis
		answers  = comments from survey designer and participant Jeremy Ross to data cleaning/analysis done by Claire Curry
		whattodowiththisrecord = categorical used to filter out duplicated or otherwise bad records.
		Start.Time..24h. = transect start time in 24 hour time.
		Start.LAT = latitude in decimal degrees of the starting point for the transect
		Start.LON = longitude in decimal degrees of the starting point for the transect
		Start.Cloud = percentage estimate of cloud cover at start time of transect
		Start.Wind = category estimate of wind speed in miles per hour at start time of transect, ordinal with 5 mph bins
		End.Time..24h. = transect end time in 24 hour time.
		End.LAT  = latitude in decimal degrees of the ending point for the transect
		End.LON = longitude in decimal degrees of the ending point for the transect
		End.Cloud  = percentage estimate of cloud cover at end time of transect
		End.Wind = category estimate of wind speed in miles per hour at start time of transect
		Notes = any notes written by observer relating to the whole transect period
		Species.before = species seen before the transect began
		Species.After = species seen after the transect ended

	SEEM_code.zip
		Contains the code required to process data files (pointcount.csv, transect.csv, pointcount_metadata.csv, transect_metdata.csv, and ebird data) into the format used in the SEEM_model.zip code.
		No de-identification was necessary on the data and all original variables were retained in the dataset files, though not all were included in analyses.
		Code files are prefixed with a number showing the order in which they need to be run.
		Files with the same prefix can be run in any order within their number (all 0_*.R files can be run in any order before a 1_*.R file, for example).
		
		The following files prepare the some of the predictor rasters and prepare the response data.
			0_preparation_downscale_models_aggregating_all_predictor_rasters_to_lowres.R
			0_preparation_ebird_data_import.R
			0_preparation_support_set_pixel_coverage_tests.R - generates Fig. 2
			1_preparation_data_manipulation.R
		2_run_from_here.R calls others files named source_*.R to run the spatially explicit ensemble model.
		source_*.R files contain code snippets or functions called from the other numbered *.R files.
		3_results_*.R and 4_results_*.R files generate Figs. 1, 3-8, and S1-S7.

Associated datasets elsewhere:
	eBird 2014 reference dataset available on request from ebird.org
	Full citation is Munson, M.A., Webb, K., Sheldon, D., Fink, D., Hochachka, W.M., Iliff, M., Riedewald, M., Sorokina, D., Sullivan, B., Wood, C. & Kelling, S.(2014) The eBird Reference Dataset, Version 2014.Cornell Lab of Ornithology, Ithaca, NY.

