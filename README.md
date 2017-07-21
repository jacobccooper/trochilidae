# trochilidae
Global Hummingbird Niche Models and Associated Code
Jacob C. Cooper, The University of Chicago & The Field Museum
20 July 2017

This repository contains the data necessary for completing the analyses in Cooper & Soberon's 
hummingbird stacking project. Data available here are for the 95% confidence threshold of the data. 
Due to size, data for the 90% and 99% thresholds are available by request (they are currently on 
a private server at The University of Chicago). If there are any questions/comments about these data, please
contact Jacob C. Cooper at <black.hawk.birder AT gmail.com>.

Folders and associated content:

NOTE: Oversize folders are available via UChicago Box via their associated link.

IOC_hummers.csv: a complete list of hummingbirds from the IOC list.
Cooper&Soberon-codes.R: a near complete file of annotated R codes used for this project.

1. bioclim. Available via https://uchicago.box.com/s/7gn3627ei81is1xys1gxjcm03itbj7k5
	1. Clipped
		Bioclim data clipped to the extent of the M hypotheses for each species. 
	2. Raw
		Raw bioclim data that were used to train the models.
2. Extracts
	A list of coordinate files for the expanded localities used to estimate the number of species.
	1. Results
		Each sup-repository has an extracted list of species for each locality.
		The files directly in this folder are the summary files for each scenario.
3. M
	The M hypotheses for each species.
4. occurrences
	Several repositories of points used to create the models. 
	locs_rem refers to "testing localities removed" as is the dataset used for the paper.
5. output
	MAXENT outputs for each species and scenario. Available via https://uchicago.box.com/s/7gn3627ei81is1xys1gxjcm03itbj7k5
6. raw_eBird
	Raw eBird data used for this project. Available via https://uchicago.box.com/s/7gn3627ei81is1xys1gxjcm03itbj7k5
7. raw_GBIF
	Raw GBIF data used for this project.
8. results
	The thresholded and reprojected MAXENT results are located in subrepositories herein. Available via https://uchicago.box.com/s/7gn3627ei81is1xys1gxjcm03itbj7k5
