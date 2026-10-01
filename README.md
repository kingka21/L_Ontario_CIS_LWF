This repository provides the supporting code and data used to compare historical and contemporary spawning habitat of cisco and lake whitefish in Lake Ontario.
The code supports the manuscript K. King, Flood, P., Brant, C., and Alofs, K. under review. Comparing historical and contemporary spawning habitat to inform conservation and restoration of cisco and lake whitefish in Lake Ontario. Canadian Journal of Fisheries and Aquatic Sciences. 

**The 'scripts' folder includes the following:**  
01_data_wrangling that includes the raster data manipulations 
02_cis_hist_model that includes methods for running the MaxEnt model for historical cisco data, pulling threshold information, and the Boyce Index     
03_cis_contemp_model that includes methods for running the MaxEnt model for contemporary cisco data, pulling threshold information, and the Boyce Index     
04_lwf_hist_model that includes methods for running the MaxEnt model for historical lake whitefish data, pulling threshold information, and the Boyce Index      
05_lwf_contemp_model that includes methods for running the MaxEnt model for contemporary lake whitefish data, pulling threshold information, and the Boyce Index  
06_marginal_plots that includes methods used for creating the marginal effects plots  

**The 'data' folder includes the following:**

points_cis_hisotrical that includes the historical cisco data and their sources
points_cis_conemp that includes the contemporary cisco data and their sources
points_lwf_historical that includes the historical lake whitefish data and their sources
points_lwf_contemp that includes the contemporary lake whitefish data and their sources
ont_cis_marginal_hist that includes all of the historical cisco Maxent model marginal data  
ont_cis_marginal_contemp that includes all of the contemporary cisco Maxent model marginal data  
ont_lwf_marginal_hist that includes all of the historical lake whitefish Maxent model marginal data  
ont_lwf_marginal_contemp that includes all of the contemporary lake whitefish Maxent model marginal data  
