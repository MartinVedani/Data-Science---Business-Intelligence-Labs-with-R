# Trees 1, 2 y 3. Por Martin Vedani.

# This is a development to read line by line and go running order by order
# Manually, interactively.

# All text to the right of the symbol "#" are comments from the author. It is irrelevant
# to R if you highlight or not with the mouse before you click on "run"
# since R simply execute all the to the left of # on each line and ignores
# whatever is to the right of # (in each line, of course). In the text editor
# (Script editor) from my current version of RStudio,  the text R ignores when running
# a command is displayed in green.

######################################################################################
######################################################################################

# First of all, we gather our tools.
# We install the R package with tree algorithms (from the closest mirror of CRAN)
# for the download to be as quick as possible. Any mirror of CRAN
# will work obviously because they are all equal. RStudio has its own CRAN mirror
# as well.

install.packages("rpart") #Jsut do this once, it will remain installed going forward

# Load the package into the active R session

library(rpart) #You have to load the package every time you open a new sesion of R.

# Now we import the files into the active sesson creating new variables.
# You need to have the raw data of each separate tree prepared in advance in
# 3 different files and saved as *.CSV file (from Excel in our case).
# We will be using the fastest method for uplaoding files into variables.
# There are many methods and additional formats that can be used that are
# very well explained in a huge number of forums on the web and YouTube.

tree1 <- read.csv("C:\\Users\\Martins\\Google Drive\\myWork\\myRwork\\Swirl - Decision Trees\\Tree1CSV.csv", header = T) 
tree2 <- read.csv("C:\\Users\\Martins\\Google Drive\\myWork\\myRwork\\Swirl - Decision Trees\\Tree2CSV.csv", header = T)
tree3 <- read.csv("C:\\Users\\Martins\\Google Drive\\myWork\\myRwork\\Swirl - Decision Trees\\Tree3CSV.csv", header = T)

# Let see and understand the class structure for each of the newly created variables

str(tree1) #data frame of 10k observations and 7 variables
str(tree2) #data frame of 20k observations and 7 variables
str(tree3) #data frame of 40k observations and 7 variables


###################################################################################
##################################### Exercise 1 ##################################
###################################################################################

# Generate a decision tree for the data in the table tree1.
# Will use the functions and algorithms within the rpart package.

# The rpart () function requires arguments. Let's see its documentation to understand
# the details

?rpart

# Ok, the documentation tells us that we will need at least 3 arguments.
# Formula: The dependent variable (y), in our case, "Result"
# Depending on ("~") all independent variables or predictors (x)
# In our case attributes # 1:6 (attributes 1 through 6). Mathematically we write
# Y = f (x1, x2, x3, x4, x5, x6)
# Data: the data set you want to use for the construction of the decision tree (predictive model)
# Method: type of prediction to be achieved.

# Well, now that we understand the arguments, we can build a new variable with our
# very first tree.


fit.tree1 <- rpart(formula = Result ~ Atribute.1 + Atribute.2 + Atribute.3
              + Atribute.4 + Atribute.5 + Atribute.6, data = tree1, method = "class")

# LEt's check the results to know what's in our first tree

printcp(fit.tree1)
summary(fit.tree1)
rsq.rpart(fit.tree1)

# That's a lot of information. Let's see if we can understand things a little
# better with some graphs

plot(fit.tree1)
text(fit.tree1)

# Hmm, this is either too basic, or super advanced, to understand it well.
# Let's see if we can plot something more useful for our modest introductory level.

par(mfrow = c(1,2)) #divide graph device into 1x2 array
plotcp(fit.tree1)
plot(fit.tree1, uniform = TRUE, main = "tree1")
text(fit.tree1, use.n = T, all = T, cex = .7) #add text to previous graph
par(mfrow = c(1,1)) #reset graph device to 1x1 array

# These graphics are giving me a headache. And the error message R gave us
# is not very clear about what it means.

# Fortunately R has additional packages that help us achieve graphics
# that are far more interesting

install.packages(c("rattle", "rpart.plot", "RColorBrewer"))
library(rattle) #load each package
library(rpart.plot) 
library(RColorBrewer)

# Let us study the documentation of the new graphics function will use .
# (Understanding R's documentation takes practice and experience)

?fancyRpartPlot

# OK, beyond title and subtitle, both optional, the only thing we really have to do
# is tell fancyRpartPlot() the name of the variable that contains our tree (our 
# predictive model) which should be an object of rpart() - NO PROBLEM, our tree was
# grown (created) with the rpart() function!!

par(mfrow = c(1,2))
plot(fit.tree1, uniform = TRUE, main = "tree1") #Ugly graph #1
text(fit.tree1, use.n = T, all = T, cex = .7) #add text to ugly graph
fancyRpartPlot(fit.tree1) #Graph 2
par(mfrow = c(1,1))

# Graphic # 2 is much more clear and useful.
# And Free => Thanks to the R Community who develops this type of useful packages!
# Special thanks to:

?citation
citation("rpart")
citation("rattle")
citation("rpart.plot")
citation("RColorBrewer")

# Let's enlarge graph 2 and look at it carefully to appreciate the detail and
# Understand the information it contains

fancyRpartPlot(fit.tree1)

# We can see that the order of predictive importance (predictive power) for our
# attributes is: 1 -> 6 -> 2 -> 5.
# It reads:
# Atribute.1 <0.5 "yes" (tends to "0" or "FALSE"),
# Atribute.1 <0.5 "no" (tends to "1" or "TRUE"),
# And so on with attributes 6, 2, and 5.

# The other attributes (3 and 4) would not help us with predictions. These two are,
# according to rpart(), irrelevant.

# One moment though, we had 7 variables in our raw data didn´t we?
# Yes, we did, but variable 7 is the "Result" y ~ x1:6. And it can either be
# dependent (y) or Independent Predictor (x). Not Both!!


###################################################################################
##################################### Exercise 2 ##################################
###################################################################################

# Select a set of random data from the Tree2 table and build a Decision Tree
# out of that set.
# Do the same on the unused data and compare.

# We will split Tree2 in half. One half will be our training variable and
# the other half will be our testing variable

tree2.train <- tree2[1:10000,]
tree2.test <- tree2[10001:20000,]

# Let'r review the structures

str(tree2.train) #data frame of 10K observations and 7 variables
str(tree2.test) #data frame of 10K observations and 7 variables

# Generate a decision tree off the data in the data fram tree2.train

fit.tree2.train <- rpart(formula = Result ~ Atribute.1 + Atribute.2 + Atribute.3
                    + Atribute.4 + Atribute.5 + Atribute.6, 
                    data = tree2.train, method = "class")

# Generate a decision tree off the data in the data fram tree2.test

fit.tree2.test <- rpart(formula = Result ~ Atribute.1 + Atribute.2 + Atribute.3
                         + Atribute.4 + Atribute.5 + Atribute.6, 
                          data = tree2.test, method = "class")

# Is it true that "All mothers love their children equally"?
# Suppose that, yes, rpart() is completely objective.

# Compare both learning variables just created and we test our confidence in rpart().
# A TRUE = identical results would show that we are right, and
# FALSE = No identical results would show that our trust in rpart() has been 
# misplaced, right?
# We want to test (on tree2.test) what we
# learned (with tree2.train) on two different bases and assess whether
# predictions work or not.

identical(fit.tree2.train, fit.tree2.test)

# FALSE, which means that our models learned different things,
# But ... based on the same or different data?

identical(tree2.train, tree2.test)

# FALSE

# Ok, so far we know that rpart () taught us different things about two different
# halves of raw data bases. Does this mean that rpart () has a favorite child then?

# Let's check visually, to see if both different learnings lead us to Rome

par(mfrow = c(1,2))
fancyRpartPlot(fit.tree2.train)
fancyRpartPlot(fit.tree2.test)
par(mfrow = c(1,1))

# We see that the importance of our predictive attributes keeps the same order.
# The rpart() function is objective and obviously knows how to adjust its teaching
# methods (tree construction) to different children (different set of data)
# to give them equal opportunity to both get to Rome, regardless of the way or road.
# Excellent !! We are on track. rpart() is a great teacher and no one needs
# therapy.


###################################################################################
##################################### Exercise 3 ##################################
###################################################################################

# Select three sets of random data (a, b, c) from the table tree3.
# Build a decision tree from a subset.
# Make pruning using the subset b.
# Check that predictive power of each node using the subset c.

tree3.train <- tree3[1:15000,] #base for decision tree
tree3.prune <- tree3[15001:30000,] # base for prunning
tree3.test <- tree3[30001:40000,] #base for testing

str(tree3.train)
str(tree3.prune)
str(tree3.test)

identical(tree3.train, tree3.prune)
identical(tree3.prune, tree3.test)
identical(tree3.train, tree3.test)

# Great, all databases are different.
# We know that rpart() creates different models depending on the database
# used to build them but shuold achieve the same predictions.

# Knowing this, we do not need three predictive models. Building fit.tree3.prune and
# fit.tree3.test is not necessary so we will not do it. The enthusiastic student
# is more than welcomed to do so on their own, the practice of copy / paste / edit
# makes the analyst and is a key requirement for every aspiring developer

# Let us then go ahead and grow (fabricate, learn) our predictive tree.

fit.tree3.train <- rpart(formula = Result ~ Atribute.1 + Atribute.2 + Atribute.3
                          + Atribute.4 + Atribute.5 + Atribute.6, 
                          data = tree3.train, method = "class")

# And now that we have it let's use to predict something, shall we?

# Lets study the new function that we will use

?predict

# OK ... with time we will understand more and more out of these documents as
# we accumulate experience

prediction.tree3.test <- predict(fit.tree3.train, tree3.test, type = "class")

# That seems to have done something interesting at very high speed
# What is inside this new variable "prediction.tree3.test" that is useful?

# When calling fit.tree3.train, we used the tree grown
# (learnt/manufactured) using database tree3.train

# Remember the formula for it

printcp(fit.tree3.train)

# formula = Resul ~ Atribute.1 + Atribute.2 + Atribute.3 + Atribute.4 + Atribute.5 
# + Atribute.6, data = tree3.train, method = "class")

# When defining arbol3.test to create our variable "prediction.tree3.test"
# We ask predict () a prediction of the variable "Result" for each of
# 10,000 observations (lines) using attributes 1:6 inside data frame tree3.test
# And we ask predict() to predict these "Result"
# in the way we learned while growing or developing the tree called "fit.tree3.train"
# Using rpart (). Ok, everything makes sense.

# Let's see what results have then.

summary(prediction.tree3.test)
str(prediction.tree3.test)

# 9,004 0s (or FALSE) y 996 1s (or TRUE)

# OK. Let graph:

fancyRpartPlot(prediction.tree3.test)

# It does not work, gives out an error message. This function is powerful but not
# to be used for a vector of 1 factor with 2 levels (1s and 0s / TRUE and FALSE).
# We will have to try something more basic

plot(prediction.tree3.test)

# The same information that we obtained with the summary function ()
# Nothing new = Frustrating ... that's how life is soemthing for a Data Analyst and
# Forecaster.

scatter.smooth(prediction.tree3.test)

# Worse. A chart to tell us we have 2 levels in 10 000 observations, no help.
# Good thing we studied the str() function and we know we have 0s and 1s with nothing
# else.
# Are we on a losing streak? A little light turbulence won't break us.
# I think that we are over complicating ourselves, we do not yet realize what we have
# in our hands.

# Consider the first few lines of the raw results

head(prediction.tree3.test)

# Hmm, ¿1s y 0s for numbered observations?
# LEt's see a few more, the first 50 lines of resutls

head(prediction.tree3.test, 50)

# Ok, it seems that yes, there is an ascending order of observations with results 1s
# and 0s. When we run the str() function, there were 10,000 lines, so,
# Let's see what the last few lines of resutls look like.

tail(prediction.tree3.test, 50)

# Aja !! we built tree3.test database with the latest observations (30.001 to 40.000)
# of our original and complete database called tree3.

# So we definitely now have 10 000 predictions of 0s and 1s for "Result" based on
# Attributes 1:6 of the database "tree3.test" (NOT based on attributes from
# the test3.train database, different from "tree3.test", which were only used to 
# learn and build our predictive model "fit.arbol3.train").

# All this variables and names are becoming too many!
# It is essential to keep pace with all our variables, we used
# long and impractical names for logistical purposes and to illustrate this point,
# since variables can take many forms:
# I) raw data: tree1, tree2, tree3;
# Ii) subdivisions of raw data: tree3.train, tree3.prune, tree3.test;
# Iii) predictive models: fit.tree3.train;
# And now
# Iv) results of predictions: prediction.tree3.test

# Great. We have the right amount of predictions, based on the correct attributes
# for the correct original observations (lines) numbered 30.001 to 40.000
# Thanks to a well-learned or taught predictive model created with rpart ()

# How do we present these predictions?
# Using amounts and proportions would be a good idea.

table(prediction.tree3.test)
prop.table(table(prediction.tree3.test))

# We are anticipating 9,004 0s or FALSE (90.04%) and 996 1s or TRUE (9.96%).

# And since we know the actual results when looking at the variable (column) "Result"
# of our subdivisions of raw data: arbol3.test ...

# ... How good is our prediction? What it is the result of this test on
# Arbol.3.test data base? What is the purpose of this whole exercise?

# We did it to compare apples to apples

table(tree3.test$Result)
prop.table(table(tree3.test$Result))

# Guau, the difference between predictions and actuals is small. How myuch is it?

9004-8982; ((9004-8982)/8982)*100; 1-abs(((9004-8982)/8982))

# For 0s or FALSE we have 22 units in approximately 10 thousand or a 0.24% delta.
# This means that our predictive model (tree) has a 99.75% certainty
# for 0s or FALSE at the aggregate level.

996-1018; ((996-1018)/1018)*100; 1-abs(((996-1018)/1018))

# For 1s or TRUE. We also have 22 units on approximately 1000 or a 2.2% delta
# This means that our predictive model (tree) has a
# 97.8% certainty for 1s or TRUE at the aggregate level.

# What is the atomic precision, line by line, observation by observation?
# In other words, how many originally FALSE, were also presidcted as FALSE?

sum(tree3.test$Result==prediction.tree3.test)/length(prediction.tree3.test)

# 98.06%.

# Excellent !! If I knew better, I would say that the data given to us is totally
# fictitious (given the high level of certainty that, experience shows, is impossible
# to achieve in reality )... Anyway, we are happy!

# Time to export and send these predictions to interested users.
# LEt's say, via email in a file that can be easily used.
# Let's be proactive and add a column called "Observations" to make life even
# easier for our internal clients / bosses/ Users

# We will also create a new variable so that we do not mess with the prediction that 
# we have made and not ruin everything if we make a mistake.

prediction.tree3.share <- data.frame(Observation = 30001:40000, 
                                    Result = prediction.tree3.test)

write.csv(prediction.tree3.share, file = "prediction.tree3.csv", 
          row.names = FALSE)

# To know where R has recorded our new file and go looking for it,
# We need to know the directory of your workspace (work directory).

getwd()

# Now remember the first exercise 1 where the attributes 3 and 4
# Seemed potentially irrelevant.

# Could it be that our predictive model (our tree) has branches
# That can be pruned? Let's see if it does and how it would impact on the results of
# Predicting WITHOUT pruning we just did.

# The purpose behind pruning our tree is to avoid overfitting the data.
# Usually we select a size that minimizes the tree's error
# Number of cross-validation, xerror column printed by printcp ().

printcp (fit.tree3.train)

# Examining the results of cross-validation error, you must select the
# parameter complexity (cp) associated with the slightest mistake,
# And place it in the function prune(). In this case it is easy to identify, is the 
# cp number 5.
# Alternatively, when the data is far more extensive, we can use the
# following code for prune() to automatically look for the smallest cp:
# $ Cptable fit [which.min ($ cptable fit ["xerror"]), "CP"]

prune.tree3 <- prune(fit.tree3.train, cp = 0.01000000)

prune.tree3.AutoCp <- prune(fit.tree3.train, cp = fit.tree3.train$cptable
                            [which.min(fit.tree3.train$cptable[,"xerror"]),"CP"])

# Do we trust this automatic coding?

identical(prune.tree3, prune.tree3.AutoCp)

# TRUE. Well, they are the same! The source is of high quality and in the future we
# will be happy to return to it for more help. Thanks HSAUR for the idea
# (http://www.statmethods.net/about/books.html)
# I'm going to remove one of the two identical prune variables to keep
# my desk as clean and tidy as possible.

rm(prune.tree3.AutoCp)

# We will plot the pruned tree and compare it with the its version prior to pruning

par(mfrow = c(1,2))
fancyRpartPlot(fit.tree3.train)
fancyRpartPlot(prune.tree3)
par(mfrow = c(1,1))

# Visually, they are both the same

identical(fit.tree3.train, prune.tree3)

# TRUE, identical () agrees, there is no difference. In reality, this could be a
# an exception that might also occur. Rpart() is very skilled and, unknowingly,
# we realized early on that the attributes 3 and 4 were ignored (trimmed)
# by rpart().

# We had an additional subset of data created in the variable tree3.prune
# The challenge for you now is to re-do this exercise 3 completely from scratch
# Using tree3.prune for "training" the predictive model and tree3.test
# for testing.


###################################################################################
################################## Random Forest ##################################
###################################################################################

# In simple terms, the technique Random Forest focuses
# Iin the overfitting problem in decision trees. When does overfitting occur?
# It Happens when a decision tree (a predictive model)
# learn "too much", when it is adjusted too closely to the training database
# and its ability to make useful predictions about other databases
# decreases. For example, in the last exercise we saw that 
# rpart () ignored some attributes (3 and 4) to generate the predictive tree
# And therefore we assumed this omissiong by rpart() was the reason why pruning 
# was not manually required.
# This is not entirely correct. Attributes 3 and 4 could be irrelevant with
# nothing to do with overfitting.

# Random Forest is form of pruning, more advanced than rpart() and prune().
# It involves creating multiple trees with different levels of fit to their
# training database. At the time of the prediction, for each line or observation, 
# each manufactured tree (trained) generates their own
# prediction and has as its "vote". For example, if we have 3 trees,
# 2 of which predict 0 or FALSE, and the third one predicts 1 or TRUE,
# the observation will be predicted as 0 or FALSE by "majority of votes".
# This training technique on trees with different adjustment reaches the
# Final decision of classification (prediction) in a democratic manner and avoids 
# overfitting with such a process.

# The first thing we must ensure is that our databases have no values
# missing.

# In our database, there are no missing values. In practice, it is most common
# to have missing values in large quantities.
# The solutions are varied. You can eliminate the observation(s) with
# Missing data, you can complete the missing values with mean values or
# median values calculated of out the values of other observations for that same 
# variable, and, surprisingly, you can predict the missing values with trees and
# fill in the blanks

# Once we have no data points missing, we can start with the first
# Step required for the creation of a tree, which would be to divide the
# Data.frame in parts, i.e. data.frame.train and data.frame.test

?randomForest2Rules

# To run the randomForest() function, we need to install the package ramdomForest

install.packages("randomForest")
library("randomForest")
?randomForest

# randomForest() is very similar to rpart(). The first argument is the formula, same
# format as rpart(). With no argument method, we have to convert
# the dependent variable "Y" into a factor in the argument "formula":

# formula = as.factor(Result) ~ Atribute.1 + Atribute.2 + Atribute.3
# +                                         +Atribute.4 + Atribute.5 + Atribute.6

# One way to reduce the amount of typing in R, is to use a synonym for "ALL
# ATTRIBUTES. And that's the point ". "
# It would then be typed easier and shorter, as follows

# formula = as.factor(Result) ~ . 

# It reads: Result (in "factor" format) based on all variables (columns)
# Existing (not including the "Result" column obviously). the
# Problem with this short way to include all, is that it does not allow us to 
# alter the order in which variables are feed into the formula. 
# We have never altered
# The order of the variables in our exercises so far, but we might
# Want to do it in practice. For example, we may want to enter the variable "Sex"
# And "Age" first if in our database these are in columns 3 and 8.

# The data argument is the same as in rpart().

# New argument "importance = TRUE" will allow us to see the importance of each
# variables invovled predicting the forest (model) created by randomForest ().

# The ntrees argument limits the number of trees generated by randomForest.
# This can help with the management of computing resources when they are
# Limited.

# The argument set.seed (123) ensures that R's random numbers generator
# that RandomForest () uses starts always from the same sequences or "place"
# so that you can get the same results over and over again. Otherwise, the
# the randomizer would always from a different random seed everytime.
# and it will never be possible to reproduce exactly the same results.
# This is really useful to walking through a prediction, if necessary, with
# students or managers who want to see exactly how and where the numbers
# presented in PowerPoint, or PDF report, etc. etc. etc, come from.

# Once the model is generated by randomForest (with an amount of ntrees
# Within it), you can use the function predict() as we have been doing.

# Let's re-use our existing databases to see an example and compare.

str(tree3.train)
str(tree3.test)

# We will set the seed for the randomizer to replicate the same results
# in the future and build our model

set.seed(123); fit.forest.t3.train <- randomForest(as.factor(Result) ~ ., 
                                          data = tree3.train, importance = TRUE,
                                              ntree = 1000)

# For entertainment, let's see if there is any difference between our predictive
# models fit.forest.a3.train and fit.tree3.train

identical(fit.forest.t3.train, fit.tree3.train)

# FALSE. To be expected, 1 tree versus 1000 trees. Let's go deeper.

printcp (fit.tree3.train)
printcp (fit.forest.t3.train)

# Not useful, fit.bosque is not an object of rpart (), is an object of randomForest ()

# Let's start by analyzing the important variables of our randomForest

varImpPlot(fit.forest.t3.train)

# The graph on the left shows us how the accuracy or predictive quality
# of our forest would decay if we removed each variable individually.
# The chart on the right shows the Gini coefficient which tells us the importance
# Of each indepent variable in the model.
# Two different ways of saying similar things, yet not exactly the same.
# The chart on the left speaks of the fall in overall performance for the entire 
# model as each variable is removed. The Gini coefficient tells us the individual
# contribution of each variable independent from the others.
# In our case, the descending order of importance is the same in both graphs,
# but this is not always the case. Two variables, A and B in order of importance 
# 2 and 3 respectively the Gini coef graph on the right, could very well have an 
# opposite order in the graph on the left.
# In both graphs, the larger the displacement of the value for each
# variable to the right, the greater the importance of this variable.
# within or for the predictive model. The order of importance is 1 -> 6 -> 2 -> 5
# 3 and 4 approximately zero.

# Remember fit.tree3.train:

fancyRpartPlot(fit.tree3.train)

# Exactly the same with rpart(): 1 -> 6 -> 2 -> 5

# Let's try to do a graphical comparison

par(mfrow = c(1,2))
fancyRpartPlot(fit.tree3.train)
fancyRpartPlot(fit.forest.t3.train) # Ups, error
plot(fit.forest.t3.train) # This one works
par(mfrow = c(1,1))

# Different graphs, trying to compare rpart () to randomForest () in this way 
# is like comparing apples to oranges. Not good.

# Nevertheless, the second graph does tell us something usefull:

# 1000 ntrees in randomForest was a good argument since the error
# variance flatens out after tree 550 aproximately. Nice!!

# OK, the ability of each player can be seen in the field - what really matters 
# are the results.

# Let's do predictions using our test data base trees3.test

pred.forest.t3.test <- predict(fit.forest.t3.train, tree3.test)

#Let's compare predictions

identical(prediction.tree3.test, pred.forest.t3.test)

# FALSE. OK, interesting

head(prediction.tree3.test, 15)
head(pred.forest.t3.test, 15)

tail(prediction.tree3.test, 15)
tail(pred.forest.t3.test, 15)

# OK, they both seem to start and end in the same manner, with the same predicted
# values.

# Let's use the tables to see if the totals and aggregated proportions look
# alike

table(prediction.tree3.test)
table(pred.forest.t3.test)

# OK, ramdomForest () predicts more 0s or FALSE and less 1s or TRUE than rpart()
# We have the ("real") REsults in our original data base tree3.test to compare our
# predictions versus reality. Lets see how it looks.

# Who gets the benefit of the predictions, 0s or FALSE or 1s or TRUE?

table(tree3.test$Result)
table(prediction.tree3.test)
table(pred.forest.t3.test)

9004-8982; ((9004-8982)/8982)*100; 1-abs(((9004-8982)/8982))
9060-8982; ((9060-8982)/8982)*100; 1-abs(((9060-8982)/8982))

# For 0s or FALSE we have 22 units in approximately 10 thousand or 0.24% delta
# With rpart() and 78 units or 0.87% delta with randomForest().
# This means that rpart() has a 99.75% certainty for 0s or FALSE and
# RandomForest() has 99.13%.
# Rpart() was more successful for 0s or FALSE that randomForest() at the "aggregate"
# level.

996-1018; ((996-1018)/1018)*100; 1-abs(((996-1018)/1018))
940-1018; ((940-1018)/1018)*100; 1-abs(((940-1018)/1018))

# For 1s or TRUE, rpart() predicted 22 units less, or 2.2% delta, giving it
# 97.8% certainty for 1s or TRUE at the "aggregate" level. 
# randomForest() predicts 78 units less, or 7.7% delta, giving us a 92.3% certainty 
# for 1s or TRUE at the "aggregate" level.

# Hmm, in aggregate amounts rpart() clearly outperforms randomForest(). Is this
# comparison right, sure, the math works. Is it fair, It is really NOT!!

# At the atomic level, observation by observation, who is better?

# We knew rpart() had an accuracy by comparing observation by observation
# (ie line 35667 predicted vs. 35667 actual) at the atomic level, of 98.06%.
# Remember:

sum(tree3.test$Result==prediction.tree3.test)/length(prediction.tree3.test)

# Let's calculate it for randomForest()

sum(tree3.test$Result==pred.forest.t3.test)/length(pred.forest.t3.test)

# 98.16%. 

# Wow, randomForest() predicted each individual observation better than rpart() .
# This makes, randomForest() the most accurate at atomic level.

98.16-98.06

# Democracy between 1000 voting trees within randomForest()
# generated an improvement of 0.10%.

# Small difference undoubtedly, yet of relatively importance!! Each case in reality,
# with real data, will dictate the true importance of such small improvements.

# It remains for us now to create a data.frame with 2 columns to share with our users

# Let's create a new variable to avoid disturbing the variable that contains our
# predictions

pred.forest.t3 <- data.frame(Observation = 30001:40000, 
                                  Result = pred.forest.t3.test)

# Export to a file compatible with other programs

write.csv(pred.forest.t3, file = "pred.forest.tree3.csv", row.names = FALSE)

# Done!!!

# We had an additional subset of data created in the variable tree3.prune
# The challenge is for you to re-do to this exercise of randomForest completely 
# from scratch using tree3.prune for "training" the predictive model and tree3.test
# for testing.