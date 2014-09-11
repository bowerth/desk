
## Summary

Calculate STAN indicators


### Calculation {#calculation}

The calculation of indicators involves several steps

__Indicator formula__ The user-selected indicator is looked up in a table with calculation formulas (see table below)

__Formula parsing__ The calculation formula is stripped from all non-alphanumeric characters, e.g. calculation characters. Underscores identify specific years or industries, mainly for denominators

__Data selection__ Variable codes remaining in the parsed formula are looked up from the STAN data set for all industries. All possible combinations of selected variables and valid denominator years or industries are added to the data frame

__Indicator calculation__ One or more calculation formulas are applied to the data set and indicator values added to the set of results


### List of indicators
	
|indic |formula                                                                                                                   |
|:-----|:-------------------------------------------------------------------------------------------------------------------------|
|VSHT  |VALU / VALU\_DTOTAL                                                                                                        |
|VSHM  |VALU / VALU\_D10T33                                                                                                        |
|ESHT  |EMPN / EMPN\_DTOTAL                                                                                                        |
|ESHM  |EMPN / EMPN\_D10T33                                                                                                        |
|LBNT  |LABR / LABR\_DTOTAL                                                                                                        |
|LBNM  |LABR / LABR\_D10T33                                                                                                        |
|LBET  |LABR / EMPE\_DTOTAL                                                                                                        |
|LBEM  |LABR / EMPE\_D10T33                                                                                                        |
|LBVA  |LABR / VALU                                                                                                               |
|IPYE  |VALK / EMPN / (VALK\_2005 / EMPN\_2005)                                                                                     |
|IPYH  |VALK / HRSN / (VALK\_2005 / HRSN\_2005)                                                                                     |
|IULC  |EMPN / EMPE * LABR / VALK / (EMPN\_2005 / EMPE\_2005 * LABR\_2005 / VALK\_2005)                                               |
|LULC  |EMPN / EMPE * LABR / VALK                                                                                                 |
|AVHW  |HRSN / EMPN                                                                                                               |
|VAPR  |VALU / PROD                                                                                                               |
|INPR  |INTI / PROD                                                                                                               |
|INVV  |GFCF / VALU                                                                                                               |
|INVT  |GFCF / GFCF\_DTOTAL                                                                                                        |
|INVM  |GFCF / GFCF\_D10T33                                                                                                        |
|RDST  |RDNC / RDNC\_DTOTAL                                                                                                        |
|RDSM  |RDNC / RDNC\_D10T33                                                                                                        |
|RDIV  |RDNC / VALU                                                                                                               |
|RDIP  |RDNC / PROD                                                                                                               |
|CMTB  |((EXPO - IMPO) - (EXPO\_D10T33 - IMPO\_D10T33) * (EXPO + IMPO) / (EXPO\_D10T33 + IMPO\_D10T33)) / (EXPO\_D10T33 + IMPO\_D10T33) |
|EXIM  |EXPO / IMPO                                                                                                               |
|TBAL  |EXPO - IMPO                                                                                                               |
|XSHT  |EXPO / EXPO\_DTOTAL                                                                                                        |
|XSHM  |EXPO / EXPO\_D10T33                                                                                                        |
|MSHT  |IMPO / IMPO\_DTOTAL                                                                                                        |
|MSHM  |IMPO / IMPO\_D10T33                                                                                                        |
|XSHP  |EXPO / PROD                                                                                                               |
|MPEN  |IMPO / (PROD - EXPO + IMPO)                                                                                               |


&copy; OECD (2014)
