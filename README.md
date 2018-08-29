**Rapid Return to Teacher:**  
**Report Builder**


The purpose of this program is to take results from a testing management system and produce a score report to be delivered to teachers.
Eventually, the repository will also host an Excel template for the reports.
However, the template will be stored in unzipped form so that git can track it.

### Large Scale Changes
#### Store test setup info in hidden tabs
* Create a format for storing information in the hidden tabs.
* There should be something that indicates what the information is, like row and column headers
* Have the hidden tabs reference the information in the user interface tabs.
* Have the program pull the information from the hidden tabs
#### Split the test set up out of the topic alignment tab
* Do this after completing the **Store test setup info in hidden tabs** modifications
* Add a field to the test setup to indicate the values that should be used when scoring questions (as opposed to the values to be used when setting up a test)
* Add a field to the topic alignment to indicate “Discussable”
	* Programmatically add high scoring and low scoring topics to narrative from discussable list.
	* Retain information about all topics
* Add a field to indicate the response set for MC questions
	* Default would be Alpha (indicating the first n letters of the alphabet)
	* Another option would be Number1 (the n natural numbers)
	* Number1 could be generalized to NumberX, where x is a number, indicating the first n whole numbers beginning with X
#### Split the Item Comparison out of the Overall Comparison tab
* This will allow for extensions to the general test information
#### Handling Dropped Items
* Do this after completing the Split the test set up out of the topic alignment tab modifications
* This is actually more complicated than I had at first imagined.  The problem is that, when an item is dropped, all responses for it are set to NA.  That is not what we want.  
* The item setup should have something that indicates whether an item, if dropped, should be included in the analysis (but not the scores).  This will account for two separate situations:
	* When the item was not supposed to be on the test, so students were instructed to all bubble in the same answer, there is no information in those responses, and so the response pattern should not be interpreted.
	* When the item was dropped after the fact (e.g. it was too hard of a question), the pattern of responses might still be meaningful.
* When the item is dropped for only some students:
	* The item is not taken into consideration in the overall score for the student
	* When the item analysis narrative is generated, it is done based only on the responses such that the student was responsible for that question. 
* When the item is dropped for all students:
	* The item is not taken into consideration in the overall score for any student
	* The row of the breakdown tab with that item is italicized and greyed out
	* The narrative mentions that the item was dropped
	* The narrative does not use that item in any item analysis
* This may require duplicating the ItemInfo table.  There could be an ItemInfoRaw table, and then another one that replaces 

### Small Scale Priority Changes
#### Handouts
* When handouts are set to show topics, there should be nothing that shows up for the Answer or Response for topics.
#### Add special scoring methods
* Mutford scoring
* Decrement scoring (Zalucki style)
#### Add GoogleForms as a TMS
* How does this work with the TAB?
* Does there need to be an RGoogleForms project to keep track of tests?
* Need to develop tables of student ID’s and email addresses
* Need to talk to Dalton about collecting email addresses automatically
#### Add ASAP as a TMS
* Need to develop table - student ID’s x Exam with enrollment as entry
* Need to create a program that converts ASAP export files to item response CSVs
#### Item Types
* To make the item frequency counts robust to changes and new types, there should be a different kind of setup
* The ItemTypeCategories member of the REPORT class should be a data frame.  
	* The first column holds the name of the item type.  
	* Other columns should include info on how to handle that item type, such as whether to treat responses as scores.
	* One factor to consider is how to generate the response set for a particular item.  Right now, MC is a vector of sequential uppercase letters, ER is a sequence from 0 to the value, and all of the gridded are grouped together and a unique set of actual responses are collected.  There should be a column that holds a code that specifies which method to use for a particular item type.
	* There should be a Grouping column.  For item types whose response sets are determined empirically, sometimes we want them to be grouped and sometimes we don’t.  The Grouping column could contain a number that indicates a sort of superordinate item type category.  The response set for all items with the same Grouping value would be generated together.
* Once the ItemTypeCategories is fleshed out, changes need to be made wherever specific item types are named in the code.
	* setItemInfo_REPORT.R
	* setItemSummary_REPORT.R
	* addResponseFrequencies_REPORT.R
	* setItemResponseScores_RESULT.R

### Frills
* rrttCore
	* Create a package called rrttCore
	* Remove some functions from rrttReportBuilder and put them in rrttCore
	* Make rrttReportBuilder depend on rrttCore
	* Change the function calls in rrttReportBuilder to reference rrttCore
	* Make rrttExportConverter depend on rrttCore
* Testing the program
	* Assemble a bunch of test cases so that, when updates are made, we can quickly check to make sure everything still works
		* Multiple TMS’s
		* Multiple item types
		* Multiple special scoring situations
		* Multiple topic situations
		* Multiple comparison situations
		* Anonymize the data so it can be included in the distribution of the package
		* Are these called unit tests?  Look into how this stuff is supposed to be included in a package.
	* Add a function that generates a test setup file given some set of info
	* Add a function that generates random test data given a setup file
* Remove the upload tab from the score report, since it’s unnecessary
	* Remove from the template
* Directly load data into the Scores tab instead of having it reference a hidden tab
	* Change the report template to remove the formulas there
	* Change the way the report is created to directly load the info on the scores tab
* Check to make sure that no class sizes are too large for the reporting space
	* 4 sections of max 39 students
	* Rest are max 25 students
	* Throw an error or something
* If desired, export upload files for specific topics
	* This should only be implemented after the Item Setup and Topic Alignment tabs are separated
	* Maybe add a column to the topic alignment that holds an upload indicator
	* If the indicator is true, then upload CSV’s should be generated for that topic
* messageLevel
	* Add messageLevel parameter to all function calls, set as messageLevel-1
	* Add messages throughout that print the current status of the program as long as messageLevel>=1
* Dashboard tab
	* overall stats
	* Settings
	* Narrative
* Tab showing topic scores by student, broken down by section
	* This can be done with the existing Topic Scores tab, since nothing references it
	* The tab will need to be reformatted, but not much
	* When the data is loaded, the location of loading will need to be managed based on the number of students in the section
	* When the data is loaded, styles will need to be added
* For curves that allow scores to go over 100, adjust the boxplots and score chart
* Add ZipGrade as a TMS
* Update parameter for generateReport
	* Add a parameter to generateReport
	* Default it to FALSE
	* When a report on a test was recently created but hasn’t yet been sent, call generateReport with the new parameter set to TRUE
	* If the ScoreUpdate.txt file exists, don’t replace it.  Just add extra rows.
	* If the functionality to indicate “more updated” “even more updated” etc already exists, this parameter could tell it whether to increment. 
* After the Item Setup is split from the Topic Alignment, change the way items are named
	* Instead of just a “Question #:” row, there should be three rows: 
		* Item name
		* Answer Form item name - if blank, defaults to Item Name
		* Reporting item name - if blank, defaults to Item Name
* Also for item setup, allow items to be marked as Required.  Then, if a student is missing a score for that item, throw an error.
* Checking the answer key - if a question has a very low score, then even if the correlation is high, it should be marked as a Check Key item
* Verifying setup - Have something that extracts setup from the TMS and compares it to the setup file to make sure the keys and item names and item types and points values are the same
