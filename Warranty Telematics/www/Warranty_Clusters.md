
#### Word Cloud: 
This tab generates the most used words in the warranty data description. 
To remove a word from the word cloud, type the word in the text input panel __(Enter Stopwords separated by comma)__ and click on __Submit__ button. In order to remove more than one words, use __comma (,)__ between the words. 

#### Topics: 
This tab enables the user to form clusters from the related words identified in the word cloud above. 
User can set the number of clusters to be formed, between 0 and 20, using the Slidebar in the __"No. of Clusters"__ panel. Default Number of clusters is set to 13. 

Further, user can select the cluster number in the __"Select cluster"__ panel to view __Replacement Rate__, __Warranty Hours__ and __Reliability Curve__ for a particular cluster. 
* __Replacement Rate:__ Gives the rate at which replacements have been done for the different parts across topics. The replacement rate is calculated as below:
	* Replacement Rate for a Part = Number of Replacements of the part/Number of claims for the part
	
![check.](../www/Replacement_Rate.png)


* __Warranty Hours:__ The plot for warranty hours provides the average warranty hours and standard deviation of the warranty hours across the topics. 
	* The typical warranty hours at time of replacement for parts inside topics gives an visual understanding of when the machine can be expected to fail.

![check.](../www/WarrantyHours.png)


* __Reliability Curve:__ Provide the __Topic-wise Distribution__ of warranty claims. It measures the reliability of the machine at various time periods.
	* The reliability of the machine follows a Weibull distribution. The reliability at any given point can be viewed by hovering above the point of the curve for a time period.
	
![check.](../www/Reliability_Curve_All.png)


