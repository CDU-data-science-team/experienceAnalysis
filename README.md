# experienceAnalysis

Package `{experienceAnalysis}` contains a suite of functions for performing 
_text mining_ such as sentiment analysis, analysis of word counts, TF-IDFs and 
_n_-grams etc. The package was  developed as a helper package for use with other 
packages/repos developed by
the [CDU Data Science Team](https://github.com/CDU-data-science-team), but the 
functions are generic and thus suitable for broader use. The largest focus is 
on calculating sentiment indicators and word counts/frequencies for labeled or unlabeled text,
and plotting the outcomes to easily detect potentially important information in
the text. However, there are a few "spin-off" functions for assessing the 
performance of a classification model, e.g. calculating accuracy per class, making
and plotting confusion matrices etc.

For an example of how the package is used in practice, see the 
[source code](https://github.com/CDU-data-science-team/pxtextminingdashboard) 
for [this  dashboard](https://involve.nottshc.nhs.uk:8443/text_mining_dashboard/).

The package makes extensive use of [{tidytext}](https://www.tidytextmining.com/index.html) 
(Silge & Robinson, 2017) but also employs `Python` libraries for sentiment 
analysis (e.g. [TextBlob](https://textblob.readthedocs.io/en/dev/)) with the use 
of [{reticulate}](https://rstudio.github.io/reticulate/).

In line with the broader work of the CDU Data Science Team, all function names
have prefixes that give users a hint of what type of operations they perform 
(e.g. `prep_*` and `plot_*` for preparing and plotting data respectively). 
See [Naming guidelines for functions](#naming-guidelines-for-functions).

## Naming guidelines for functions

### Data manipulations

-   `get_*()`: Get data, e.g. from a database or a file;
-   `tidy_*()`: Tidy data, e.g. renaming
    variables, removing duplicates, creating factors, "wide" to "long"
    format etc.;
-   `collect_*()`: Collect data of specific cases, mainly wrapper
    functions for specific filter commands;
-   `prep_*()`: Prepare data for further use, e.g. to create tables,
    sorting vectors etc.;

### Data analyses

-   `calc_*()`: Calculations or analyses, e.g. counting data, regression
    analyses etc.;
-   `summary_*()`: Summarise results of calculations (there might be
    some overlap with `prep_*()`);

## Visualisations

-   `plot_*()`: Create a plot;

## References
Silge J. & Robinson D. (2017). Text Mining with R: A Tidy Approach. Sebastopol, CA: 
O’Reilly Media. ISBN 978-1-491-98165-8.
