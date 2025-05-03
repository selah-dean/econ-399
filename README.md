# MLB Payroll Distribution and Team Success (2015–2024)

This repository contains all the code and data used in the analysis for the paper:  
**“Impact of Payroll on Major League Baseball Team Success”**

## Overview

This project investigates the relationship between MLB team payroll structures and team performance across a ten-season period (2015–2024). The analysis considers whether teams with smaller payrolls or different payroll strategies can achieve similar success and consistency as high-spending teams. Players are grouped by contract type—**pre-arbitration**, **arbitration-eligible**, and **veteran**—to examine how the allocation of payroll impacts team wins and year-to-year variation.

## Course Context

This project was completed as part of an **Economics Independent Study** course. The research combines economic theory with statistical analysis to explore how financial strategies and labor market structures impact competitive balance in Major League Baseball.

## Repository Structure

    ├── archive                     # Archived code (original/uncleaned scripts and early versions)
    ├── datasets                    # All data used in the project 
    │   ├──                         # Original datasets from Baseball Reference and Spotrac
    │   └── cleaned                 # Datasets after cleaning
    ├── figures                     # Figures included in the paper and additional plots 
    ├── web_scraping.ipynb          # Jupyter notebook use to scrape and assemble payroll and roster data
    ├── data_cleaning.R             # R script to clean and format scraped datasets
    ├── analysis.Rmd                # RMarkdown file containing the full analysis     

## Key Questions Addressed

- Can small-market teams achieve the same level of success as large-market teams despite payroll disparities?
- How does the distribution of payroll across pre-arb, arb-eligible, and veteran players affect a team's performance?
- Does consistent investment in certain player categories lead to more stable performance year over year?

## Tools & Languages

- **Python (Jupyter Notebook):** Data scraping and acquisition
- **R:** Data cleaning and wrangling
- **RMarkdown:** Statistical analysis, visualization, and reporting
