# Name of App *(Give your app a short and informative title. Please adhere to our convention of Title Case without hyphens (e.g. My New App))*

NestFind

Github repository: https://github.com/Vogelwarte/NestTool

## Description
The NestFind app is a tool to identify the most likely nest location for birds during the current field season and thus aid in field data collection. 

## Documentation
The NestFind app takes current tracking data (up to the last 4 weeks), and identifies the most likely nest location for every individual based on recursive movement patterns of each individual.
The main requirement is a reasonably high temporal resolution of tracking data (ideally 1 location per hour, but it can work with 5-10 locations per day) with sufficient spatial precision (error of up to 20 m).
The user needs to specify the Movebank Study from which the data will be sourced, a time horizon (the last 1, 2, 3, or 4 weeks), and an error radius around the nest that will be very species specific.
The error radius will be used to calculate the departure and return times from/to the 'nest' site, and a sensible value is needed because this radius will determine how long an animal remained within that radius or how long it spent outside this radius. Users should consider device accuracy and general movement behaviour of the species when specifying the radius (we suggest a default of 50 m for medium raptors nesting in open landscape with good GPS coverage).
For each individual the app will calculate the recursion times and frequencies for each GPS location, and will then select the GPS location where the individual spent the most time and to which the individual returned most frequently.
Locations are sorted by the time an individual spent within the user-specified radius, and the location with the greatest number of nearest neighbours is selected as the likely nest location.
This app does **NOT** estimate any probability of this location being a nest - it will only return the most likely location of a nest if a breeding individual has been tracked and data are inspected for the correct time of the year.



### Application scope
#### Generality of App usability
This app was developed for bird species that are tracked with GPS transmitters that stream the data into Movebank in near real-time.
The purpose of the app is to use the tracking data to find nests in the field.
We have tested this app for a range of diurnal bird species (red kite, Egyptian vulture, Golden eagle, nutcracker) with different data resolutions.
This app will **NOT** indicate whether a bird is nesting, it will only return the most likely location of a nest if a breeding individual has been tracked and data are inspected for the correct time of the year.
For retrospectively identifying nest locations and the probability to breed, we recommend the package (NestTool)[https://github.com/Vogelwarte/NestTool].
This App was developed to identify nest sites, but can probably also be used to identify any kind of location clusters like feeding sites or watering holes.

#### Required data properties
This App is only applicable to data that reflect behavior by breeding birds during the breeding season. 
The data should have a fix rate of at least 1 location per 60 minutes over the time period specified by the user (current day - X weeks). 
The App should work for any kind of high-resolution location data with spatial errors << the error radius specified by the user.

### Input type
*Indicate which type of input data the App requires.*

*Example*: `move2::move2_loc`

### Output type
The app primarily returns a Shiny user interface to indicate the most likely nest location per individual on a map.
There is an option to export a 'gpkg' geo-package file that can be used for field navigation by handheld electronic devices with GPS functionality.


### Artefacts
*If the App creates artefacts (e.g. csv, pdf, jpeg, shapefiles, etc), please list them here and describe each.*

`rest_overview.csv`: csv-file with Table of all rest site properties
`rest_overview.gpkg`: csv-file with Table of all rest site properties

### Settings 

`Timeframe`: The number of weeks prior to the current date over which data will be downloaded. Unit: `weeks`.
`Error radius`: Radius around a nest site in which GPS locations can be recorded when the individual is attending the nest site. This is a function of both geolocation error and the movement behaviour of the species. Unit: `metres`.

*Always include the "Store settings" setting as it will appear automatically in all shiny apps*
`Store settings`: click to store the current settings of the App for future Workflow runs. 

### Changes in output data
*Specify here how and if the App modifies the input data. Describe clearly what e.g. each additional column means.*

*Examples:*

The App adds to the input data the columns `Max_dist` and `Avg_dist`. They contain the maximum distance to the provided focal location and the average distance to it over all locations. 

The App filterers the input data as selected by the user. 

The output data is the outcome of the model applied to the input data. 

The input data remains unchanged.

### Most common errors
*Please describe shortly what most common errors of the App can be, how they occur and best ways of solving them.*

### Null or error handling
*Please indicate for each setting as well as the input data which behaviour the App is supposed to show in case of errors or NULL values/input. Please also add notes of possible errors that can happen if settings/parameters are improperly set and any other important information that you find the user should be aware of.*

*Example:* **Setting `radius`:** If no radius AND no duration are given, the input data set is returned with a warning. If no radius is given (NULL), but a duration is defined then a default radius of 1000m = 1km is set. 
