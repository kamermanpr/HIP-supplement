# HIV Pain Intervention Project (HIP) - supplementary files

## Description

This repository contains supplementary files for the HIV Pain Intervention Project (HIP). These files include: 

- The study protocol ([protocol.pdf](protocol.pdf)) 
- The data cleaning and data analysis R/RMarkdown scripts ([scripts/](scripts/))  
- The ouputs of the RMarkdown scripts ([outputs/](outputs/))  

**Note:** Participant consent did not provide for the publication of their data, and hence neither the original nor cleaned data have been made available. However, we do not wish to bar access to the data unnecessarily and we will judge requests to access the data on a case-by-case basis. Examples of potential use cases include independent assessments of our analyses, and secondary data analyses. Please contact Prof Romy Parker ([romy.parker@uct.ac.za](mailto:romy.parker@uct.ac.za)), Dr Antonia Wadley ([antonia.wadley@wits.ac.za](mailto:antonia.wadley@wits.ac.za)), or open an [_issue_](https://github.com/kamermanpr/HIP-supplement/issues) on this repo.

If your request for access is successful and you wish to reproduce the analyses, we recommend that you save the _demographics.xlsx_ and _amalgamated_data.xlsx_ files we will supply you with into the _original_data_ directory and then run the _build.R_ script located in the root directory of this repo. Doing so, will extract and clean the data from the two Excel files into five R data objects (_demographics.rds_, _bpi.rds_, _bdi.rds_, _eq5d.rds_, and _se6.rds_) located in the _data_ directory. These new data files will then be used to render the RMarkdown scripts in the _scripts_ directory, overwriting the _\*.md_ files in the _outputs_ directory.

## Bibliometric information

#### Repository citation
Kamerman P, Madden V, Wadley A, Parker R, Cameron S, Devan D, Jackson K, Reardon C. Multimodal intervention for pain in HIV/AIDS: depression predicts a remarkably high loss to follow-up. _Figshare_ , 2018. DOI: [10.6084/m9.figshare.6148394](https://doi.org/10.6084/m9.figshare.6148394)

#### Manuscript citation 
Parker R, Madden VL, Devan D, Cameron S, Jackson K, Kamerman PR, Reardon C, Wadley A. Multimodal intervention for pain in HIV/AIDS: depression predicts a remarkably high loss to follow-up.

#### Manuscript abstract
Pain continues to affect over half of people living with HIV/AIDS and pharmacological treatment has limited efficacy. However, preliminary evidence supports non-pharmacological interventions.  We previously piloted a multimodal intervention in amaXhosa women living with HIV and chronic pain in South Africa. Improvements were seen in all outcomes, in both intervention and control groups. We attempted a multicentre randomised controlled trial with 160 participants to determine whether the piloted multimodal peer-led intervention reduced pain in different populations of both male and female South Africans living with HIV/AIDS. We were unable to assess the efficacy of the intervention due to a 58% loss to follow up (LTFU). Here we describe our experience and the barriers to retention. We also discuss the limitations of traditional solutions aimed at improving feasibility of studies and interventions in this context. We found that sex, education, employment status or income stability were not associated with LTFU. The different sociocultural context in South Africa may warrant a different approach to interventions for pain in HIV compared to resource-rich countries including a concurrent strategy to address barriers to health care service delivery. We found that the one factor associated with LTFU was greater severity of depressive symptoms (p=0.01). We suggest that assessment of pain and depression need to occur simultaneously in those with pain in HIV. In PLWHA and pain who are retained in studies, that participation alone appears sufficient to improve pain and depression. We suggest investigation of the effect of social inclusion on pain and depression. 
