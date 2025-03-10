# Olympic Data Analysis (1896–2016)

This is a Shiny web application that provides an interactive dashboard to analyze Olympic data from 1896 to 2016. The app allows users to explore medal counts, athlete statistics, and trends across different sports, regions, and years.

## Features

- **Main Dashboard**:  
  - View overall data with filters for season, gender, sport, region, and year.  
  - Interactive **Medal Map** to explore medal distributions globally.  
  - Summary of total medals (Gold, Silver, Bronze).  
  - Trends in athlete participation by gender.  
  - Top 20 medalists visualization.  
  - Total and top 10 sport events analysis.  

- **Athlete Data Overview**:  
  - Distribution of age, height, and weight for athletes.  
  - Gender distribution pie chart.  
  - Filters for athlete type (all or medalists only).  

- **Olympic Results by Sport**:  
  - Detailed table with medal counts per sport.  

- **Olympic Results by Medalist**:  
  - Detailed table with medal counts for individual medalists.  

## Data Sources

- `athlete_events.csv`: Contains information on athletes, events, medals, etc.  
- `noc_regions.csv`: Contains NOC regions for mapping.  
- `rnaturalearth` package: Used for country coordinates.  

## Technologies Used

- **R Programming** with the following libraries:
  - `shiny` and `shinydashboard` for web application framework.  
  - `leaflet` and `leaflet.extras` for interactive maps.  
  - `plotly` and `ggplot2` for data visualization.  
  - `sf` for handling spatial data.  
  - `dplyr` for data manipulation.  
  - `DT` for interactive data tables.  

## Installation

To run this app locally, ensure you have the following R packages installed:

```R
install.packages(c("shiny", "shinydashboard", "leaflet", "leaflet.extras", 
                   "ggplot2", "ggiraph", "plotly", "countrycode", 
                   "rnaturalearth", "rnaturalearthdata", "sf", "dplyr", "DT"))
```

Clone this repository and run the app:

```R
library(shiny)
runApp("path_to_your_app_directory")
```

## Screenshots

*(Add screenshots of the main dashboard, athlete data overview, and other key features here.)*

## Project Structure
```
Olympic-Data-Analysis/
├── Data/
│   ├── athlete_events.csv
│   ├── noc_regions.csv
├── app.R
├── README.md
```

- `Data/`: Contains the dataset files.
- `app.R`: Main Shiny app script.
- `README.md`: Documentation file.

## Custom Styling

The app includes custom CSS for a dark theme with orange accents to enhance readability and visual appeal.

## Future Improvements

- Add more interactive plots and filters.  
- Integrate predictive analysis for medal trends.  
- Enhance mobile responsiveness.  

## Contributing

Feel free to fork this repository, make changes, and create a pull request. Contributions are welcome!

## License

This project is open-source and available under the MIT License.

## Acknowledgments

- [Shiny](https://shiny.posit.co/)  
- [rnaturalearth](https://cran.r-project.org/web/packages/rnaturalearth/)  