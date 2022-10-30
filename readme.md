---
title: "User guide for the PeRseus project"
author: "Jiri Munich"
date: "January 26, 2020"
output:
  pdf_document: default
  html_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

This is a user guide for the immediate use of the future **PeRseus** package. 

## Dependencies

Except for a color scheme imported in the plotting functions, the whole project is written in base R, so no extra package installations should be necessary.

```{r}
## Load packages for plotting (optional)
library("RColorBrewer")
library("plotrix")
```

## Setup

To initiate the project, source the following files in prescribed order:

```{r}
run <- c("Basic/Basic_functions.R", "Basic/Dominate.R", 
         "Robots/Naive.R", "Robots/Dominate-K.R", 
         "Basic/K-1.R", "Robots/K-level.R", 
         "Robots/Nash.R", "Robots/Optimist.R",
         "Robots/Pessimist.R", "Robots/Altruist.R",
         "Basic/Get_data_functions.R", "Basic/Plot_functions.R")

invisible(lapply(run, source))
```

To proceed with the replication, continue going through **model_jags_implementation.R**. What follows is a desription of functions written for this project.

Sorry about the uncivilized state. Don't hesitate to contact me with more questions.

# Folder structure

Right now, the project script is divided into two folders. Folder **Basic** contains some elementary functions used throughout the project. Folder  **Robots** contains implementations of used decision rules. In the following two sections, I will explain the content of these two folders. Five files can be found in the **Basic** folder. **Basic_functions.R**, **Dominate.R** and **K-1.R** contain functions used for algorithms in the **Robots** folder. **Get_data_functions.R** contains some functions used for the retrieval of data and **Plot_functions.R** contains functions for the visualization of generated data. The folder **Robots** contains implementation of the decision rule algorithms.

The intended order of use is following:  **Basic_functions.R**, **Dominate.R** and **K-1.R** are used to create instruments for constructing the robots. Then, the **Robots** folder is used to produce the robots and finally, **Get_data_functions.R** are used to retrieve data using the robots. **Plot_functions.R** can be used to produce visualizations.

The files will be explained in the afforementioned order.

## Basic_functions, Dominate and K-1

### Basic_functions

File **Basic_functions.R** contains elementary operations used within decision rules.

#### my_max(); my_min

Find the index of a maximum/minimum of a vector of numeric values. If there are multiple maxima/minima, the functions select on at chance. The functions return a list containing the indices and a flag, noting whether there were multiple identified maxima/minima.

```{r}
value_vec <- c(1,2,3,2,1)
my_max(value_vec)
my_min(value_vec)
```
#### generate_game()

Generates a game with random payoffs with p1 choices available to player 1, p2 choices available to player 2 and payoff values coming from vector y. Player 1 choices are represented on the horizontal axis. Player 1 payoffs are represented by the even columns. The payoff matrix can be seen in the plot with inverted vertical axis.

```{r}
# Generate a 3x3 game with payoffs ranging from 0 to 9
game <- generate_game(3,3,0:9)
print(game)
```

```{r}
e_o <- search_k.level(game,2)
e_o$eye_movement <- list(c(10,10),c(20,20))
plot_eye(e_o)
```

#### game_own(); game_other()

Splits the game into a matrix of own/other player's payoffs.

```{r}
print(game)
# Even columns represent own payoffs
game_own(game)
game_other(game)
```

#### transpose_game()

Transposes the game, so it is seen from the other player's perspective

```{r}
## Look at the game from the perspective of player 2
t_game <- transpose_game(game)
print(t_game)
## See the game as player 1 again by transposing the transposition
t_t_game <- transpose_game(t_game)
print(t_t_game)
## Transposed transposition equals the original game
all.equal(game, t_t_game)
```

#### transpose_eye()

Takes eye_movement coordinates and transposes the coordinates, so they can be mapped on a transposed game

```{r}
## In this example, e_o is an eye movement object, containing the original game payoff
## structure and a list of coordinates containing eye movement.
## e_o can be plotted

e_o <- search_k.level(game,2)
plot_eye(e_o)

## Transposing the game matrix and the eye movement coordinates,
## plotting e_o produces the same plot on a transposed game

e_o$eye_movement <- transpose_eye(e_o$eye_movement, e_o$game)$eye_data
e_o$game <- transpose_game(game)
plot_eye(e_o)
```

#### search_maximum()

Given a set of coordinates and payoff values in the coordinates, the function reproduces search for the highest value in the set by looking up payoff values, storing them in memory and droping dominated values.

```{r}
meta_memory <- c(2,4,6,8,10)
loc_meta_memory <- list(c(1,1),c(2,2),c(3,3),c(4,4),c(4,5))

search_maximum(meta_memory = meta_memory, loc_meta_memory = loc_meta_memory)
```


### Dominate and K-1

#### domination(); search_k.1()

Perform one itteration of removing dominated choices/performing k-level reasoning. Operate as the robots and will be explained in the **Robots** section.

## Robots

Functions in this folder are the decision rule algorithms. All start with a prefix search_ and continue with the given algorithm. They use a **game** as an input and return an **eye-object**. K-level and domination are itterative decision rules and therefore contain a **k** parameter indicating the level of reasoning.

* **game** is a matrix of payoff values as described for generate_game() in the previous section.

* **eye-object** is a list consisting of:
  * **an_choice**: is an index of the choice selected by the algorithm, using a quasianalytical solution. Besides performing an information search through the game matrix, a simpler implementation of each decision rule was used, applying simple functions. The purpose of **an_choice** is to validate the decision rule implementation. Each choice resulting from an information search can be compared to the choice done by simple matrix operations applying the same rule. If **flag** is TRUE, the **an_choice** and **eye_choice** are allowed to differ, as will be explained later.
  * **eye_choice** is an index of the choice selected by the algorithm after performing its information search.
  * **eye_movement** is an ordered list of coordinates recording all locations visited by the algorithm.
  * **flag** is used to monitor situations where more solutions were available. In such case, the algorithms decides randomly between equivalently valued choices (and in such situation, **an_choice** and **eye_choice** may differ). Flag can be also used to find games that do not offer multiple solutions given a rule.
  * **game** is the original game used as an input. It is stored in the **eye_object** for future reference.


```{r}
## In this example, an altruist information search is performed, returning an eye_object (printed)
alt_eye_object<- search_altruist(game)
print(alt_eye_object)
```

## Get_data_functions

Having produced eye_objects, data can be classified into (meta-)transition types and used in further analysis.

### get_transitions()

Can be provided either with **eye_movement** type data (a list of vectors with two dimensional coordinates), or an entire **eye_object** (type of data must be specified in the function parameters). Uses parameter **n** indicating the order of meta-transitions (defaults at 1). 

Returns a list containing:
* **transitions** a character vector with simple transition types.
* **meta-transitions** a character vector containing the requested type of meta-transitions.

```{r}
## Get transitions from eye movement coordinates
get_transitions(eye_data = alt_eye_object$eye_movement)

## Get transitions from an eye object
get_transitions(eye_object = alt_eye_object, is.object = TRUE)

## Get meta-transitions of the second order
get_transitions(eye_object = alt_eye_object, is.object = TRUE, n = 2)
```

### initiate_meta_transitions()

A function preparing a data frame for the collection of observations, intended for use by other functions. Contains one parameter, indicating the meta-transition order

```{r}
init <- initiate_meta_transitions(n = 1)
```

### write_eye_data()

Turns a list of lists of eye coordinates (**eye_movement** object) into a data frame with frequencies of meta-transitions of the requested level.

```{r}
eye_list <- list(
  alt_eye_object$eye_movement,
  alt_eye_object$eye_movement
)

write_eye_data(eye_list, n=1)
```

### simulate_search()

Produces a number of searches for a number of games, returning transition and meta-transition data.

Parameters are:
* **games** the number of games to be randomly generated and used
* **repetitions** indicating the number of times each game is played by every algorithm
* **n** indicating the order of meta-transitions

Returns a list with:
* **transition_data** a data frame with transition counts
* **meta_transition_data** a data frame with meta-transition counts of the requested order
* **games** a list with all games used getting the data

```{r}
searches <- simulate_search(games = 2 , repetitions = 2, n = 1)
head(searches$meta_transition_data)
```

### impossible_meta_transitions(); add_postion()

**impossible_meta_transitions()** returns a character vector of impossible meta-transitions of the requested order by performing every possible meta-transition of the requested order. **add_postion()** is a function written with a typo in its name. It is used to create all possible steps from a given list of position coordinates.

```{r}
impossible_meta_transitions(3,3,1)
```

#### chance_meta_transition_probabilities()

Returns a list o probabilities of any level meta-transition in a matrix of a given size occuring by chance. The chance is computed from the proportion of n-long walks of a given type to all possible n-long walks.

```{r}
chance_meta_transition_probabilities(p1 = 3, p2 = 3, n = 1)
```

#### simulate_search_get_data()

Is used for aggregating simulated data in order to determine meta-transition typical for decision rules.

### Plot_functions

#### plot_eye()

Uses an **eye_object** to plot information search. Some jitter can be added to the coordinates to distinguish overlapping transitions. When used empty, the function produces a plot with a random game.

```{r}
plot_eye()
plot_eye(alt_eye_object)
```

#### animate_search() and animate_search_gif()

**animate_search()** produces an animation by plotting information search of an **eye_object** in itterations. The gif version instead saves frames into a folder. The images can be used to produce a gif animation.
