# Ice Saints

R script to check wheter the Ice Saints are a good predictor of frost free nights. 

## Rationale 

Every year we start pre-growing some plants from seed indoors and eagerly wait for the moment we can finally transplant them to the outdoors. However, we want to make sure, that our precious seedlings are safe from frosty nights. As we recently moved to a new place (Germany), we checked with the locals what the best time for transplanting seedlings would be and heard about an old weather proverb (*Bauernregel* in German) which says that you should wait until the day of the Cold Sophia has passed. That day is the last of the so-called [Ice Saints](https://en.wikipedia.org/wiki/Ice_Saints) (or *Eisheiligen* in German), which are usually three to five (depending on where you live and whom you ask) name days of Saints in mid-May. The folklore has it, that once these days have passed, the nights will be guaranteed to be free from frost for the remaining  growing season.

But since I don't want to leave the fate of my precious seedlings to the musings of long-gone Saints alone, let's employ the power of data and R to check if the Saints were right all along!

## What's in the script?

For that, I will 

- download publicly available, historic weather data from the German Meteorological Service ([Deutscher Wetterdienst, DWD](https://www.dwd.de/)),
- then import these into [R](https://www.r-project.org/),
- fit a [logit model](https://en.wikipedia.org/wiki/Logistic_regression) to the data using [nested data frames](https://cran.r-project.org/web/packages/tidyr/vignettes/nest.html),
- and finally make some predictions based on the model results about when it would be safe to plant the seedlings outside.

Find more information about this script in my blog article [*When is it safe to plant your seedlings outside? Or: Should you consult the Saints or trust the data?*](https://www.janknappe.com//blog/r-when-save-plant-seedlings-outside-ice-saints/).
