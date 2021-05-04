# PhonologicalSystematicity 
# Updated 4th May 2021

Data and code for the project 'Phonological Systematicity in Early Phonological Development' (Laing, in prep).

This analysis draws on two corpora:

Providence (US English); 5 infants (3 females) from Demuth et al, 2006
Lyon (French); 5 infants (3 females) from Demuth & Tremblay, 2008

Demuth, Katherine, Jennifer Culbertson, & Jennifer Alter. 2006. Word-minimality, Epenthesis, and Coda Licensing in the Acquisition of English. Language & Speech, 49, 137-174.
Demuth, K. & A. Tremblay (2008). Prosodically-conditioned variability in children's production of French determiners. Journal of Child Language, 35, 99-127.

The following scripts are included for both corpora:

1. *Start with data_cleaning.R*

This file takes the output files from Phon and joins them together in one big file, then prepares them for analysis.

2. *Then distance_scoring.R*

This file takes each consonant of each word (target and actual forms) and quantifies it according to distinctive features.

3. *matrix_script_globalnetwork.R* 

Separate scripts for Providence, Lyon, and a separate one for Naima due to the size of her dataset. These scripts take target/actual forms and compare the first production of each word with the first production of each other word, to create a global network for each data type.

4. *thresholds.R*

This script prepares the data for analysis from each of the global distance matrices. It calculates the degree for each word pair across a set of thresholds from E=0.1-1 in relation to age, vocab size, and age of production (global network). It then runs correlations between degree and AOP.

5. *network_graphs_globalnetworks.R*

This script generates network graph data for the global network across WPs (5, 25, 50 and 100), as well as Watts Strogatz and Erdos Renyi models for comparison. It then runs models that compare clustering coefficient (global and average) and average path length for target and actual data in relation to both Watts Strogatz and Erdos-Renyi models.
 
6. *powerlaws.R*

This script uses code from Fourtasst et al to generate statistics for the powerlaws analysis