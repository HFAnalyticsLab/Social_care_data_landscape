# Mapping the landscape of open data on adult social care in England

#### Project Status: completed March 2020

## Project Description

Before the COVID-19 outbreak, the Health Foundation collaborated with researchers from City, University of London to map and visualise sources of open data across the adult social care pathway. This project aimed to collate information on the availability, timeliness, coverage and quality of public data sources, to match these against stakeholder groups and information requirements. The aim was to create informative visualisations on data availability and gaps to investigate the following in order to raise the profile of adult social care statistics to inform action:

1. What is the extent of the data that are publically available with regards to Adult Social Care
in England?
2. How much can the data tell us about funding and provision of services, the workforce and
unpaid carers who deliver services, and ultimately the users of the services?
3. Do those data sufficiently inform us about the needs, the care system performance and the
outcomes?
4. What can we say about the quality of available data?

## Data source

This study focused on the meta data of available data, not the underlying values of that data itself. We included publicly available data and measured related to adult social care published at regular intervals. 

Data sources were identified through the report listed in the References section and by searching the data archives of organisations including NHS England, NHS Digital, the Office for National Statistics, Public Health England, Skills for Care, the Care Quality Commission, the Department for Work and Pensions, the Ministry of Housing, Communities & Local Government and the NIH National Institute of Ageing.

## Visualisations 

This interactive, exploratory [visualisation](https://hfanalyticslab.github.io/Social_care_data_landscape/site/scdl_vis.html) shows our attempt to visualise the complex nature of available measures on adult social care. Individual measures are mapped against the adult social care pathway on three levels with increasing granularity. At level 1, this consists of the main service and stakeholder groups including users, unpaid carers, workforce, the provided service, providers and funders. Vertical lines show how measures can be used to explore relationships between different parts of the social care pathway. Measures are grouped according to whether they relate to demand, supply, operations or outcome and the opacity is based on a simple scoring algorithm for measure strength based on population or survey size, time lag and geographical resolution. The number of measures related to each path of the pathway is encoded in the grey background and areas with better data coverage are shown with a lighter background. The visualistion is interactive and allows the user to zoom and filter to explore groups in more detail and hovering over individual items will bring up a description of the data source. The horizontal sort order of the vertical measure lines can be changed from measure strength to a bitmap of demand|supply|operations|outcome, with the latter pushing demand-related measures to the left and outcome-related measures to the right hand side. 

Screenshot:

<a href="https://hfanalyticslab.github.io/Social_care_data_landscape/site/scdl_vis.html">
  <img src="site/vis_screenshot.png" width="800">
</a>

## How does it work?

- The [data](data) folder contains three csv files containing the pathway (domain model), the publicly available measures, and the mappings between them.
- The [data model](data_model) folder contains a view of the underlying data structure and attributes, including the [plantuml](https://plantuml.com/) script which creates the image.
- The [src](src) folder contains:
  - A python notebook to preprocess the csv files into a single joined form, suitable for the visualisation. The joined file is written back to the [data](data) directory.
  - A markdown file containing elm code to generate a [Vega-Lite](https://vega.github.io/vega-lite/) visualisation specification. The markdown file is dependent on [litvis](https://github.com/gicentre/litvis) and [elm-vegalite](https://github.com/gicentre/elm-vegalite) from the [giCentre](https://github.com/gicentre) at City, University of London. 
- The [site](site) directory contains the resulting [Vega-Lite](https://vega.github.io/vega-lite/) specification and an html page to render the visualisation with [Vega Embed](https://github.com/vega/vega-embed).

## Authors

* **Toby Rubenstein** - on [GitHub](https://github.com/trubens71)
* **Fiona Grimm** - on [Twitter](https://twitter.com/fiona_grimm) or [GitHub](https://github.com/fiona-grimm)

## License

To be added.

## References

* Office for Statistics Regulation, [Report on Adult Social Care statistics in England](https://osr.statisticsauthority.gov.uk/publication/report-on-adult-social-care-statistics-in-england), January 2020
* The Kings Fund, [Adult Social Care 360](https://www.kingsfund.org.uk/sites/default/files/2019-05/social-care-360-pdf.pdf?utm_source=website&utm_medium=social&utm_term=thekingsfund&utm_content=pdfreport&utm_campaign=socialcare360), April 2019
* Care Quality Commission, [State of Care](https://www.cqc.org.uk/publications/major-report/state-care), October 2019

## Related work 

Future Care Capital have been publishing regular updates of their [Social Care Data Finder Project](https://futurecarecapital.org.uk/research/social-care-data-finder/), which provides links to social care datasets that have been openly published in the UK <ins>since January 1 2020</ins>. In addition to a large number of national data sources, the Social Care Data Finder includes information on availability and accessibility of local datasets on adult social care from 155 councils.

