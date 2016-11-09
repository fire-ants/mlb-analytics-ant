# MLB Pitchers Friend

*"Helping hitters become batters...and then sitting them back down"*

### [Full Project Description](https://github.com/fire-ants/mlb-admin/blob/master/README.md)

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
