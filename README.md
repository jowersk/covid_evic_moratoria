# 2020 COVID-19 Eviction Moratoria and Utility Disconnection Policies 

## Overview

This repository contains data and code related to a comprehensive study of eviction moratoria and utility disconnection policies implemented in response to the COVID-19 pandemic and other emergency conditions, including extreme weather. The goal of the study is to provide a detailed understanding of housing stabilization policies across the United States, expanding beyond COVID-specific measures to include broader eviction prevention and utility disconnection protections.

## Files

1. jowers_state_evic_policies.xlsx
This file contains the full dataset on state-level eviction moratoria, including citations and explanations for any differences between our dates and those used in Benfer et al.'s data. For more information on why we make this comparison, see the Methodology notes below.  

2. new_evic_policy_data.csv
This file presents the eviction moratoria data in a long format, focusing on the dates associated with each state's moratoria. It provides a simplified view of the policies without citations or explanatory notes.

3. wide_data.csv
This file offers a wide format version of the eviction moratoria data, consolidating key dates for each state into a more compact representation. It is useful for cross-state comparisons or more streamlined analyses.

## Methodology

This project catalogs COVID-19 eviction and utility disconnection moratoria across the U.S. from March to November 2020. While building on the foundational work of Benfer and Koehler (2022) in documenting actions by governors, legislators, and local authorities, this dataset provides specific citations for each action and expands its scope to include non-COVID-19 measures, such as cold-weather policies and distinctions between utility disconnection protections for private and public utilities. We developed this independent dataset after identifying discrepancies in earlier versions of Benfer et al.'s data and because those data did not differentiate between private and public utilities. Our focus was on capturing any action that resulted in the suspension of evictions or utility disconnections, regardless of its connection to the pandemic.

In addition to eviction moratoria, we examined utility disconnection policies, including seasonal protections during extreme temperatures. These measures, which vary by state, are typically enforced during certain months to safeguard vulnerable populations. For more details, see the government’s compilation of state seasonal disconnection policies.

We also considered whether utility commissions had the authority to regulate both private and public utilities in each area. Utility commissions often have limited jurisdiction, typically overseeing only private utilities, while public utilities—such as those run by municipalities or cooperatives—may operate independently. Whether protections applied in a given county often depended on whether it was primarily served by a private utility, which fell under the commission's oversight, or a public utility, which did not. This distinction was critical in determining whether moratoria or other consumer protections were enforced in a region.

Our process involved:

- Reviewing each state's policies in-depth (e.g., COVID-related, cold weather bans, extreme weather protections).
- Referencing orginal data sources (e.g., the orders, legislation, etc.) to create specific citations to them to document our decisions and verifying data accuracy.
- Accounting for whether utility disconnection policies applied to private or public utilities (or other limitations on the scope of a specific order on utility disconnections). 

The primary purpose of our database is to create a more comprehensive picture of eviction and utility disconnection moratoria across all U.S. states, covering measures related to both the COVID-19 pandemic and other emergency conditions.

## R Code

The R scripts used to clean and process the eviction moratoria (and utility disconnection data) are also available in this repository. The code includes data transformation steps for both long and wide formats, ensuring traceability of the cleaning process.

## Citations to Other Work

- Benfer, Emily, and Koehler, Robert. Eviction Moratoria & Housing Policy: Federal, State, Commonwealth, and Territory: 2023.02.01 Moratoria  Supportive Measures Datasets.xlsx. Ann Arbor, MI: Inter-university Consortium for Political and Social Research [distributor], 2023-02-06. https://doi.org/10.3886/E157201V2-139166. 

## Contact

For questions or further information, please contact Kay Jowers at kay.jowers@duke.edu.
