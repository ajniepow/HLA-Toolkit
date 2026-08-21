# HLA-Toolkit
A small collection of analysis tools built for use in the HLA laboratory

DSA:

	Description:
		An interactive tool for assessing DSA using a patient's fusion data against a manually entered donor typing or a donor typing from Histotrac.

	R Packages needed: 
		odbc, immunogenetr, stringr, tibble, dplyr, ggplot2, tidyr, gt

	Usage: 
		Call the function with 'DSA()'. The script will prompt you for: patient MRN, UNOS donor ID, manual or auto typing. If manual is selected, it will prompt you for 2 HLA types at each locus. If auto is selected, the donor must be in Histotrac with typing entered, and the UNOS ID field populated, or the script will not find the typing.

	Results:
		A graph of the patient's antibody MFIs throughout their HLA Fusion history, filtered down to donor antigens. A corresponding html file with raw MFI values will open in your web broswer.





DSA2.0:
	Description:
		An interactive tool for assessing DSA using a patient's fusion data against a manually entered donor typing, donor typing from Histotrac, or a DD typing fetched through the UNOS API.

	R Packages needed: 
		odbc, immunogenetr, stringr, tibble, dplyr, ggplot2, tidyr, gt, httr2, openxlsx, openssl

	Usage: 
		Call the function with 'DSA2()'. The script will prompt you for: patient MRN, UNOS donor ID, manual (m), HistoTrac (h), or UNOS (u) typing. If manual is selected, it will prompt you for 2 HLA types at each locus, can accept blanks (for homozygous, null, no allele at drb345, etc.). If auto is selected, the donor must be in Histotrac with typing entered, and the UNOS ID field populated, or the script will not find the typing. If UNOS is selected, a browser window will open with a UNOS login page for user authorization of the API. Enter your credentials and complete 2FA. A 'page cannot be displayed' page will open, but this is actually a succesful login. Copy the URL from this page and paste into the prompt in R.

	Results:
		A graph of the patient's antibody MFIs throughout their HLA Fusion history, filtered down to donor antigens. A corresponding html file with raw MFI values will open in your web broswer.

	Note:
		Donor typings in UNOS utilize a lookup system. The typing pulled from the API does not always correspond directly to the antigen, but rather contains a lookup code. The script handles this for you and converts them, but it depends on the lookups in '\\mehealth.org\data\Protected\Nordx HLA\R Scripts\UNOS_Lookups' being up-to-date. See the section on 'download_UNOS_lookups' for more detail.





download_UNOS_lookups:
	Description:
		Downloads HLA antigen lookup tables from UNOS. UNOS maintains these lookups, and they are what maps an 'Id' in a donor typing to an antigen/allele. These may be periodically updated and will therefore need to be re-downloaded. This script automates the download process and drops the files into: '\\mehealth.org\data\Protected\Nordx HLA\R Scripts\UNOS_Lookups'. Utilizes the UNOS API with an authroization flow that does NOT require user credentials, only API keys, which are built into the script.

	R Packages needed: 
		httr, openxlsx

	Usage: 
		Call the function with 'download_UNOS_lookups()'. No prompts are required.

	Results:
		Lookups for all loci should be populated/overwritten in: '\\mehealth.org\data\Protected\Nordx HLA\R Scripts\UNOS_Lookups'
		




ept_analysis:
	Description:
		An analysis tool that returns possible eplet reactivity for a given patient sample. Calls eplets positive if an allele with the given eplet is above the user-specified cutoff. Then filters out any eplets that are present on beads that are below the cutoff (negative). Final list is ONLY eplets that are present ONLY on positive beads. E.g. if a threshold of 1000 MFI is set by the use, and a 138K eplet is on one bead at 1500 MFI and one bead at 500 MFI, it will not be in the final eplet assignement list. However, if it is one one bead that is at 1500 MFI and one that is at 1250 MFI, it will be listed since BOTH beads are above the threshold.

	R Packages needed: 
		odbc, stringr, tibble, dplyr, tidyr, gt, openxlsx

	Usage: 
		Call the function with 'ept_analysis()'. User will be prompetd for patient MRN, sample date of interest, HLA Class ('I' or 'II'), and negative cutoff in MFI.

	Results:
		An html file will open in your browser with all 'positive' eplets. See description section for detail on what is considered positive.

	Note:
		This script utilizes an excel file downloaded from the HLA eplet registry (https://www.epregistry.com.br) that correlates eplets to HLA alleles. See the section on 'Load_Eplets' for more detail.





Load_Eplets:
	Description:
		A lightly modified version of an R script by lcreteig: (https://github.com/lcreteig/hlapro/blob/main/R/eplet_registry.R). Downloads an up-to-date version of the eplet registry in a tabulated excel format from: (https://www.epregistry.com.br). Should be re-run periodically if any updates are made to the registry for accurate reporting from the 'ept_analysis' tool.

R Packages needed: 
		rlang, rvest, stringr, utils, purrr, tidyr, dplyr, openxlsx

	Usage: 
		Call the function with 'load_eplet_registry()'. User will be prompted for confirmation of download (1 for yes, 2 for no).

	Results:
		eplets.xlsx file should be populated/overwritten in: '\\mehealth.org\data\Protected\Nordx HLA\R Scripts'



		
