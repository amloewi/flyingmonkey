# Flying Monkey
## Helping your surveys get off the ground

flyingmonkey, initially created to facilitate working with SurveyMonkey data, is an easy way to prepare CSV data for analysis. While CSV data is ubiquitous, it has several
fundamental limitations. A universal shortcoming is that it cannot easily store order information for ordered factors (ex. Small, Medium, Large) which are one of the most common types of 
survey data. When it comes specifically to survey data, CSVs are insufficient for handling the fact that what the survey taker sees (ex. "I am very satisfied with my choice") is often 
much more cumbersome than what the survey analyst wants to work with (ex. "very satisfied").

The predictable way to handle these issues is to rename, reorder, and recast data in the code itself, but the functions available are not designed with these transformations in mind, 
making the process long, awkward, and error prone. Drawing on the very successful idea of infrastructure-as-code tools like terraform, flyingmonkey makes it easy to read and write
configuration files ("monkeyfiles") designed specifically for these data transformations.

## Example:
OLD_NAME:
"What is your opinion of the governor's policy?"
NEW_NAME:
"policy"
TYPE:
'ordered'
OLD_VALUES:
"I strongly support his policy"
"I somewhat support his policy"
"I do not support his policy"
NEW_VALUES:
"strong"
"somewhat"
"oppose"
- - - - - 
