# Visualising p-value distributions
A shiny app to visualise p-value distributions. Intended to facilitate the discussion of how to interpret p-values.

The basic setup assumes an experiment with measurements taken pre- and post-treatment. The interface allows to tweak
various aspects of the outcome. 

* The difference in pre/post means due to the treatment
* Pre/post difference independent of the treatment
* Variance of pre and post measures can be specified separately

Output is displayed for the case where there is no treatment effect (but pre and post means may differ due
to the presence of a confounder) as well as for the case where the specified treatment effect is present.
For both cases a sample of up to 50 individual p-values is displayed as well as a histogram of all generated
p-values.
