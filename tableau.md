---
layout: default
title: Tableau Hands-On
---

<span>&#129311;</span> Cevi Herdian -> [itsmecevi.github.io](https://itsmecevi.github.io/) 

<span>&#128202;</span> About Tableau Software -> [Click Here](https://www.tableau.com/about)

<span>&#128201;</span> Technical Spesifications-> [Click Here](https://github.com/itsmecevi/Tableau-Technical-Specifications/blob/master/Tableau-Technical%20Specifications.pdf)

<span>&#xf108;</span> [Install Tableau free 14-day trial](https://www.tableau.com/en-gb/products/trial)

<span>&#xf108;</span> [Sign up Tableau Public account](https://public.tableau.com/s/)


## 1-The Power of Data Visualization

* [Hans Rosling](https://www.youtube.com/watch?v=jbkSRLYSojo)
* [Gapminder](https://www.gapminder.org/)
* Goals : 
     * Data Exploration — find the unknown (trends, outliers, patterns)
     * Data Analysis — check hypotheses
     * Presentation — communicate and disseminate (share function-> data journalism)
* The Five-Step Model :
![5stepdesign](https://user-images.githubusercontent.com/27078712/72888440-266b7800-3d40-11ea-8668-db66477a76fb.png)
Picture link: [https://bit.ly/3byKYeD](https://bit.ly/3byKYeD)
* [Visual References Summary](https://github.com/itsmecevi/visualreferences/blob/master/VisualReferences-SQLBI.pdf)
* [Data Storytelling: Edward Tufte Rules](https://docs.google.com/presentation/d/1zBK1Lny6N_8-8UhYsvbdbUpiBV2K_6-KUx-e6JkmQuw/edit?usp=sharing)

## 2-Business Intelligence 101
 
* [Click Here: Hands-On Tableau BI: Tableau vs Power BI](https://docs.google.com/presentation/d/1a1-lRjjwSYBv4IUw5aLWeRr7x9JESrR8qkHR-Ug1u_4/edit?usp=sharing)
* [Quiz](https://forms.gle/zQpJANaynKiVL8bz8)


## 3-Decision Support System (DSS):= "BI"

* [Click Here: DSS](https://docs.google.com/presentation/d/1_Gp2_J6BrfddSLpv5bSxnpqudwgWZA3UklM3IGG6pMI/edit?usp=sharing)
* Demo : [Global Superstore](https://github.com/itsmecevi/global-superstore-new)
    * Excel vs Tableau: Pivot Table vs Marks
    * Machine Learning in Tableau: Forecasting & Clustering

## 4-Tableau & Big Data

* [Click Here: SWOT Analysis](https://docs.google.com/presentation/d/1_HzgnLBup74XiDjfenyEayHecj6sT7uNll8BspKSd4Q/edit?usp=sharing)
* Tableau GUI Demo

## 5-Connecting to data

* Live connection :
     * When you have a fast database
     * When you need up-to-the minute data (real-time)
* Extract connection (in memory) :
     * When your database is too slow for interactive analytics
     * When you need to take load off a transactional database (size of the data)
     * When you need to be offline (without internet connections)
     
___________


## 6-Data Preparation

* Data connections :
   * Union (vertical connection) : [amazn_stock.pdf](https://public.tableau.com/s/sites/default/files/media/amzn_stock.pdf)
   * Joins / Blend / Inner Join (horizontal connection) : [Global Superstore](https://github.com/itsmecevi/global-superstore-new)
   * [Left Join, Right Join, Full Outer Join]=> database/datawarehouse
* Data Interpreter : [amazn_stock.pdf](https://public.tableau.com/s/sites/default/files/media/amzn_stock.pdf), [CO2 API](https://github.com/itsmecevi/World-Data-CO2-API/blob/master/API_EN.ATM.CO2E.PC_DS2_en_excel_v2_247989.xls)
* Data Pivoting (Unpivoting) : [CO2 API](https://github.com/itsmecevi/World-Data-CO2-API/blob/master/API_EN.ATM.CO2E.PC_DS2_en_excel_v2_247989.xls)
* Splitting : [CO2 API](https://github.com/itsmecevi/World-Data-CO2-API/blob/master/API_EN.ATM.CO2E.PC_DS2_en_excel_v2_247989.xls) (Indicator Name-Header)
* Manage data properties :
   * Rename a data field:
   * Assign an alias to a data value
   * Assign a geographic role to a data field
   * Change data type for a data field (number, date, string, boolean, etc.)
   
   
## 7-Understanding Tableau Concepts

Data: [Global-Superstore](https://github.com/itsmecevi/global-superstore-new/blob/master/Global%20Superstore.xls)

* Simple Tableau Workflow 
  * Workflow type 1 (we don't have data): outcome->design->data searching->Tableau
  * Workflow type 2 (we have data): data connection->data preparation->sheet->[dashboard]->story
* Show Me Tool Bar
* Dimensions and measures
* Column & Rows Shelf
* Marks Cards
* Discrete and Continuous Fields :
  * Blue color: discret, qualitative data (string, geographics, date, date & time, boolean). If added qualitative data, then they are separate the graphs.
  * Green color: continuous, quantitative data, number, aggregation (sum, avg, etc).-> If added quantitative data, then they aren’t separate the graphs.
* Tableau Hands-On Chart Basic: [Demo File + Datasource](https://github.com/itsmecevi/hands-on-tableau)
  * 1a-Word maps: profit vs sales by country (country, profit, sales)
  * 1b-Bar chart: negative profit by country (country, profit)
  * 2a-Line chart: profit by category
  * 2b-Line chart: profit by market
  * 2c-Line chart: profit trendline full
  * 2d-Line chart: profit forecasting
  * 3a-Bar chart: profit, profit by category and subcategory
  * 3a-Stacked Bar chart: profit by market and ship mode
  * 4-Bubble chart: profit by city
  * 5a-Tree maps: profit by region and subcategory, profit clustering, profit by product
  * 5b-Tree maps: profit by product
  * 5c-Tree maps: profit by region and subcategory clustering
  * 6-Funnel chart: profit by segment
  * 7-Waterfall chart: profit by sub-category
  * 8-Pie chart: shipping cost by region
  * 9-Maps: profit by state
  * 10-Lollipop chart: sales by subcategory
  
* Aggregation / Calculation: When to use calculations,
  * To segment data
  * To convert the data type of a field, such as converting a string to a date.
  * To aggregate data
  * To filter results
  * To calculate ratios
  * Hands-on calculation: [Calculation](https://github.com/itsmecevi/calculation-tableau)
  
        1. totals
        2. percentages
        3. profit ratio-1
        4. profit ratio-2
        5. diskon ratio
        6. cost
        7. aov (average order value)
        8. profit status-if
        9. profit level-elseif
        
* Machine Learning: Quick Clusterisation & Forecasting
* Sheet
* Dashboard 
* Story


## 8-Use Case Various Dataset and Industry: 

* Customized for client

## 9-Tableau Wikipedia All Tutorial: 

* Link: [Click Here](https://trello.com/b/IR0xqNMM/tableau-wiki-cevi)