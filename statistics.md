
# Statistics

Statistics is about organizing data in a way that can allow further meaning and inferences.

One of the means for organizing data is [frequency distribution](#orgce70972)

When using pictograms, beware of the possibility of giving a false impression with the data.

When describing the data, it is good to have the [measures of central tendency](#org5cf38d9) and [measures of dispersion](#org2e4c99f)


# Statistical hypothesis testing

Null hypothesis - general statement that nothing is happening.


# Errors

Type 1 - False positive. The mistaken rejection of an actually true null hypothesis. "An innocent person is convicted."

Type 2 - False negative. The mistaken acceptance of an actually false null hypothesis. "A guilty person is not convicted."

A crossover error rate (CER) is the point at which both types are equal. A lower CER means something is more accurate.


<a id="orgce70972"></a>

# Frequency distribution

Frequency distribution is where the you take each possible value of data and enumerate the total number of times that data has appeared (the *frequency*)

Where the number of different of data points becomes difficult (eg the income of every individual in Canada), consider [grouping data](#org797ee13).

Frequency distribution lends itself well to [statistics](statistics.md)


<a id="org797ee13"></a>

# Grouping data

When making a [frequency distribution](#orgce70972), it can sometimes make the information clearer by grouping data. An example for this is "income per individual." Instead of having thousands of individual points, you can group them like so:

| Income    | Frequency |
| 120k-159k | 45        |
| 90-119k   | 345       |
| 60-89k    | 44453     |
| 30-59k    | 3345      |
| 0-29k     | 600       |

Rules of thumb when creating classes:

- Keep number of classes reasonable to what you're trying to convey (8-15)
- Classes should be of the same size
- Classes should be easy to handle

One way to handle this is to take the range from highest to lowest, then divide by the number of classes. While you *can* do something like "120k+" plus in the above table, you lose the ability to do arithmetic on the data. It might also hide outliers and anomalies.


# Law of Large Numbers

Don't mistake probability for reality


<a id="org5cf38d9"></a>

# Measures of central tendency

MCT can be best thought of as averages. A useful tool in [Statistics](statistics.md)

| Mean   | weighted center, summing the values then dividing by size of the set |
| Median | the numerical center, halving the set length                         |
| Mode   | the most frequent value in a set                                     |

In a set of [1, 2, 2, 4, 6], the mean is 3, the mode is 2, the median is 2.

If the number of values in a set is even, the median of the set is the average of the two middle-most numbers.

range = highest value - lowest value (obvious, but worth stating)


<a id="org2e4c99f"></a>

# Measures of dispersion

Dispersion (also called variability, scatter, or spread) is the extent to which a distribution is stretched or squeezed

On symmetrical distributions, you'll want to find the standard deviation (how close numbers are to the mean).

Where distributions are asymmetric or have outlier values, consider using the semi-interquartile range: Q = 1/2(Q3 - Q1)


# Percentile

The percentile of a set is the number of n percent of the set.


# Bar graphs

Bar graphs are a way of visualizing data that can make numbers more obvious, for example showing [frequency distribution](#orgce70972).

A histogram is similar to a bar graph, but the variables are broken into intervals.
