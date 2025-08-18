# flyingmonkey: Helping your surveys get off the ground

flyingmonkey, initially created to facilitate working with SurveyMonkey data, is an easy way to prepare CSV data for analysis. While CSV data is ubiquitous, it has several
fundamental limitations. A universal shortcoming is that it cannot easily store order information for ordered factors (ex. Small, Medium, Large) which are one of the most common types of 
survey data. When it comes specifically to survey data, CSVs are insufficient for handling the fact that what the survey taker sees (ex. "I am very satisfied with my choice") is often 
much more cumbersome than what the survey analyst wants to work with (ex. "very satisfied").

The predictable way to handle these issues is to rename, reorder, and recast data in the code itself, but the standard functions 
make the process long, awkward, and error prone. Drawing on the very successful idea of infrastructure-as-code tools like terraform, flyingmonkey makes it easy to read and write
configuration files ("monkeyfiles") designed specifically for these transformations.

## Usage:
### write_monkeyfile
The process starts with creating the template for the transformations. This function will read your data, and create a file populated with all of the relevant values and variables.
Then you go into that file, and make the changes that you want to see in your data. You're still reordering, recasting, and renaming -- but you're doing it all in one place, 
in an easy to read format, that doesn't clutter up your code, and without any of the boilerplate you would be typing otherwise.

**Ex:**
```
write_monkeyfile(data, "transformations") # => A preliminary, pre-populated 'transformations.txt' file

# Finalized transformations.txt:
OLD_NAME:
"What is your opinion of the governor's policy?"
NEW_NAME:
"policy"
TYPE:
"ordered"
OLD_VALUES:
"I strongly support his policy"
"I somewhat support his policy"
"I do not support his policy"
NEW_VALUES:
"strong"
"somewhat"
"oppose"
- - - - -
OLD_NAME:
"Do you have any other thoughts about the policy?"
NEW_NAME:
"thoughts"
TYPE:
"character"
- - - - -
OLD_NAME:
"What is your age?"
NEW_NAME:
"age"
TYPE:
"numeric"
- - - - -
```

### read_monkeyfile
Then when your file is complete, use it to transform your data, and you're ready for analysis!

**Ex:**
```
transformed_data <- read_monkeyfile(original_dataframe, "transformations.txt")
```
In this early form, the biggest issues with flymonkey are with error handling. There should be better warnings and instructions around misformatted monkeyfiles, but it's hard to
anticipate every possible issue. If something isn't working, check carefully for how your file's format might differ from the documentation.

### plot_dataframe
The base R function 'plot.data.frame' is only meant for numeric data. This function gives you a one-line way to see appropriate plots of all your data, whether numeric or categorical.
(And the factors actually make sense, because you've ordered them the way you want them in your monkeyfile!)
A great way to get an instant high-level view of your entire dataset.

**Ex:**
```
plot_dataframe(data, makepdf=T, "plots")
```

### sideways_plot
Finally, this function provides several plot transformations that I'm using constantly. Maybe you will too!
For single CATEGORICAL variables, sideways_plot flips the chart sideways so that the value names are easy (possible!) to read. 

**Ex:**
```
sideways_plot(health)
```
![alt text](https://github.com/amloewi/flyingmonkey/blob/main/images/sideways_1.png?width=200&height=150)


For a categorical variable AND A BOOLEAN variable ('logical' in R), sideways_plot shows two distributions: the values that are, vs the values that are not, indicted by the boolean.
These are both normalized to make them comparable, and colored in transparent blue (bool=TRUE) and transparent red (bool=FALSE) to make them easy to distinguish, even when they overlap. Purple means they're overlapping!
```
sideways_plot(health, received_treatment)
```
![alt text](https://github.com/amloewi/flyingmonkey/blob/main/images/sideways_2.png?width=200&height=150)


