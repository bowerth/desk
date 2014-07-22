### Dimensions

__Country__ 3-digit ISO code

__Variable__ STAN variable code

__Industry__ STAN ISIC Rev. 3 industry code(s)

Following controls vary across tabs

#### Types of Tables:

__Data__ Pivoted table of values for selected combination of country,
variable and industry dimensions by source with years in columns.

__Calculation__ If more than one variable or industry is selected, the
ratio between the two variables or industries is calculated by
default. Alternatively, the difference can be calculated.

#### Types of DataTables:

__Sources__ Selected sources for estimation as stored in array object.

__Hierarchy__ Hierarchy of industry lists

#### Types of Plots:

__Lines__ Line plot: multiple sources, one industry

__Bars__ Bar plot: one source, multiple industries

### Modification of Data Sources

#### Package Operations
  - Add source to `packageData.R` section with corresponding ISIC
  - Update R data object in `STANNAi3.rda` using `packageData()`
    function with required dimension members (idem ISIC Rev. 4 sources)
  - Compile and install stanData package

#### Script Modification

  - Add variables and countries to source array

&copy; OECD (2014)
