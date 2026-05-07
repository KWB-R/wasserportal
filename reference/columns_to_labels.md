# Create Text Labels from Data Frame Columns

Create Text Labels from Data Frame Columns

## Usage

``` r
columns_to_labels(data, columns, fmt = "%s: %s", sep = ", ")
```

## Arguments

- data:

  data frame

- columns:

  names of columns from which to create labels

- fmt:

  format string passed to
  [`sprintf`](https://rdrr.io/r/base/sprintf.html)

- sep:

  separator (default: ", ")

## Value

vector of character with as many elements as there are rows in data

## Examples

``` r
data <- data.frame(number = 1:2, name = c("adam", "eva"), value = 3:4)
columns <- c("name", "value")
columns_to_labels(data, columns)
#> [1] "name: adam, value: 3" "name: eva, value: 4" 
columns_to_labels(data, columns, fmt = "<p>%s: %s</p>", sep = "")
#> [1] "<p>name: adam</p><p>value: 3</p>" "<p>name: eva</p><p>value: 4</p>" 
```
