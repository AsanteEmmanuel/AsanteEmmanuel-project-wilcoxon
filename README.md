
<!-- README.md is generated from README.Rmd. Please edit that file -->

# project summary

The goal of project is to implement and investigate the behaviour of the
Wilcoxon Rank Sum test statistic on pairs of sample drawn from the same
population as against pairs of samples drawn from different population
distributions. The project is aimed to study the variation of this test
statistic with the two cases and to examine the strength of the test as
a dependant on the underlying sample.

I will also examine the behaviour of the test statistic if the two
samples are from a symmetric distribution or not.


# Wilcoxon One Table Function

### Arguments

  * **table_length** : This is the number of rows you want the simulated dataframe to have
  * **sample_from** : This is the probability distribution that we want to take the two samples from
  * **alpha** : The significance level. We compare the p-value to this. The value of this goes into the the `Reject` column on the output dataframe called one_table
  
  
### Values(Output)
  * **type_one_error_rate** A numeric scalar which represents the proportion of wilcoxon test p-values that resulted in a rejection of the Null Hypothesis.
  * **one_table** An object of type dataframe that contains the samples drawn, the p-value, and the Rejection decision (which is a logical vector).

