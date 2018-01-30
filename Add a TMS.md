---
title: "Adding a new TMS"
author: Paul de Barros
date: January 20, 2018
output: html_document
---

##Introduction

There are several things that need to be done in order to add support for a new testing management system.  Below are instructions for the four files that may need modification.  Before you start modifing, you need to know the following:  

1.  Is it possible that some of the export CSV's will contain no data?  This could happen if the TMS generates CSV's for every assigned class section, regardless of whether that section has data for that assessment.    
2.  Where are the section names?  Sometimes they are contained in the files.  Sometimes they are the entire file name (except the .csv part).  Sometimes they are constructed from the file name (e.g. replacing underscores with spaces).    
3.  What is the structure of export CSV's?  Are there extraneous columns or rows that need to be ignored?     
4.  Are student names stored as one field or two?    
5.  Does the export CSV come with columns that include the score or the total points for each student?    

##setTestName_Report.R
Add a new else-if condition with the name of the new TMS.  
If it is possible that the new TMS will have export CSV's with no data, build the new code based on the ScantronAS section.  Otherwise, build it based on the ASAP section.  


##setResults_REPORT.R
Add a new else-if condition with the name of the new TMS.  
If the section names will be contained within the file, build the new code based on the LinkIt section.  
If the section names will be the exact filenames of the CSV's, build the new code based on the ASAP section.  
If the section names will be constructed from the filenames of the CSV's, build the new code based on the ScantronAS section.  
If it is possible that the new TMS will have export CSV's with no data, base the for loop code on the ScantronAS section (to use the hasData variable).  


##setItemResponses_RESULT.R
Add a new else-if condition with the name of the new TMS.   
If it is possible that the new TMS will have export CSV's with no data, include the if(nrow(ItemResponses)) code from the ScantronAS section.   
Use the structure of the export files to determine how to read them.   
If the exports will have full names rather than first and last names, create the first and last names.   
If the exports lack TotalPoints, TestDate, or score, add them and set them as NA.   
At the end, the ItemResponses data.frame should have columns named score, StudentID, LastName, FirstName, TestDate, and TotalPoints, followed by columns named using the item names.   


##setItemResponseScores_RESULT.R
If the new TMS does not include TotalPoints in the export CSV's, it needs to be added.  Add the name of the new TMS to the if(TMS %in% ) line.  
