# YPInterimTesting

Provide monitoring boundaries for interim testing using the adaptively weighted log-rank test developed by Yang and Prentice (2010). The package use a re-sampling method to obtain stopping boundaries in sequential designs. The asymptotic distribution of the test statistics of the adaptively weighted log-rank test at the interim looks is examined in Yang (2017, pre-print).

## Installation
``` r
install.packages("YPInterimTesting")
```


## Example

```
library(YPInterimTesting)
data(virtual_data)
spenfun <- c(1.3E-5, 4.4E-4, 0.003, 0.008)

result <- ypinterim(time=virtual$time, event=virtual$event, group=virtual$group, spenfun=spenfun)
result
```
The data "virtual" is a virtual data set created to show how to utilize the package. Any inference cannot be drawn from this data set. The object `result` can be formatted to a table using the function `summary`.

```
summary(result)
```

## Reference
Yang, S. and Prentice, R. (2010), Improved Weights. Biometrics, 66: 30–38.

Yang, S . Interim monitoring using the adaptively weighted log-rank test in clinical trials for survival outcomes. 2017. Pre-print.
