# MLB Pitchers Friend

*"Helping hitters become batters...and then sitting them back down"*

The **MLB Pitchers Friend** uses data analytics techniques to visualize hitting characteristics for specific Major League Baseball (MLB) players.  Future releases will perform regression modeling and principal component analysis (PCA) to recommend specific pitching techniques to defeat them.

**MLB Pitchers Friend** uses a cloud-based, microservice architecture.  The major components are described below.

* [Queen Ant](https://github.com/fire-ants/mlb-queen-ant) - orchestrates the aggregation (Crawler Ant) and analysis (Analytics Ant) microservices.  
* [Crawler Ant](https://github.com/fire-ants/mlb-crawler-ant) - connects to MLB Game Day data mart and extracts observed variables on every pitch thrown and At Bat events for the targeted analysis window.  Writes information to the database (Database Ant) via the API (API Ant).  Written in Java.
* [Analytics Ant](https://github.com/fire-ants/mlb-analytics-ant) - analyzes pitches experienced by hitters and the outcomes.  Applies the **Fire Ant** proprietary **Hitter Val (HVAL) qualitative and quantitative scoring algorithm** on a per pitch basis to determine expected outcome per pitch.  Creates a comparison visualization between traditional Heat Map and HVAL and stores it in the **Virtustream Object Storage** for the Mobile Application.  Written in R.
* [API Ant](https://github.com/fire-ants/mlb-api) - provides a front-end to the Database Ant and facilitates bi-directly communication (read / write).
* [Database Ant](https://github.com/fire-ants/mlb-database-ant) - stores all raw data provided by the Crawler Ant.  Currently hosted on MongoDB.
* [Mobile App](https://github.com/fire-ants/mlb-mobile-app) - presents visualizations and per-pitch analysis to the user.  Written in React Native to allow for Apple and Android compatibility.

# MLB Analytics Ant
The **MLB Analytics Ant** is the Data Analytics component of the **MLB Pitchers Friend**, developed by the **Fire Ants**.

The Analytics-Ant receives a single variable, Major League Baseball Identification Number (**mlbID**), from the Queen-Ant control station.   The Analytics Ant then connects to the API-Ant to query for pitches on specific batters that are maintained in the Mongo database.

![](https://github.com/fire-ants/mlb-admin/blob/master/admin/mlb-pf-appflow-lg.png)
## Application Workflow
These R subroutines will run within the same container.

###01-ObtainData.R

1. Receive 1 variable (mlbID) from the docker run command used to activate the container
2. Query the MLB-API-Ant for information pertaining to that specific **mlbID**
3. Generate aggregate data frame with all pitches and atbat events for the referenced hitter...

###02-TidyData-R

1. Apply data transformations to the aggregate data frame
    - Apply Hitter Value per-pitch scoring algorithms (QuantScore & QualScore) to pitches
    - Transform similar pitch types

2. Subset the transformed aggregate data frame for specific batter

###03-GenerateImages.R

1. Generate a traditional Hitter Heat Map for season to date information.
2. Generate a Hitter Value Heat Map.
3. Storage these images in Virtustream Storage Cloud.

####04-Regressison Analysis.R *(To Be Developed)*

1. Fit a prediction model to the subset hitter data frame
2. Create confidence intervals and recommendations on how to defeat hitters.
