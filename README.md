# CCSample

This Shiny app allows you to create a sample from Common Crawl Data, selecting sites with a specific language (in the [demo](https://actionable-analytics.shinyapps.io/CC-Sample/), it is German). In order to use the app, go through the following steps:

* Download the ranks data from [Common Crawl](http://commoncrawl.org).
* Use the create_data.Rmd notebook to create the data (you need a Spark cluster for this, it can be a local one)
* change the paths in app.R
