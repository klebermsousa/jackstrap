## Notes
###Seventh submission

The package was submitted for seventh time with repairs indicated by CRAN R staff.

>Please only capitalize names, sentence beginnings and
>abbreviations/acronyms in the description text of your DESCRIPTION file.
>e.g. --> jackstrap
>If you are referring to the package here, please put it in single quotes
>--> 'jackstrap'

The DESCRIPTION file was changed to fix these mistakes indicated.

>All your examples are wrapped in \donttest{} and therefore do not get
>tested.
>Please unwrap the examples if that is feasible and if they can be
>executed in < 5 sec for each Rd file or create additionally small toy
>examples to allow automatic testing.
>When creating the examples please keep in mind that the structure
>would be desirable:
>examples{
>    examples for users and checks:
>    executable in < 5 sec
>    \dontshow{
>        examples for checks:
>        executable in < 5 sec together with the examples above
>        not shown to users
>    }
>    \donttest{
>        further examples for users; not used for checks
>        (f.i. data loading examples, or time consuming functions )
>    }
>    \dontrun{
>     not used by checks, not used by example()
>     adds the comment ("# Not run:") as a warning for the user.
>     cannot run (e.g. because of missing additional software,
>     missing API keys, ...)
>    }
>    if(interactive()){
>        functions not intended for use in scripts,
>        or are supposed to only run interactively
>        (f.i. shiny)
>    }
>}
>Your users will appreciate to see how the function is supposed to be
>called for tiny examples.

I created a toy example to test in  \dontshow{} for every Rd file.

>Possibly mis-spelled words in DESCRIPTION:
>    Stosic (17:16)
>    heaviside (16:45)
>    jackstrap (15:64)

These words are correct.

###Sixth submission

The package was submitted for sixth time with repairs indicated by R CRAN staff.

>Thanks, please write the DOI as <doi:10.1007/s11123-005-4702-4>.

The DOI was fixed.

> Possibly mis-spelled words in DESCRIPTION:
>     definy (16:28)
>     heaviside (16:45)

The first word was fixed. The second word was checked, but it is correct because the authors of paper defined the criterion with this exact word in the base paper.

###Fifth submission

The package was submitted for fifth time with repairs indicated by R CRAN staff.

>Please double quote publication titles in the Description field of the
>DESCRIPTION file.

I put double quote in publication titles like indicated.

>The Description field of the DESCRIPTION file is intended to be a (one
?paragraph) description of what the package does and why it may be
>useful. Please elaborate.

I built a description with what the package does.

>Please add small executable examples in your Rd-files to illustrate the
>use of the exported functions but also enable automatic testing.

I inserted the executable examples in each Rd-file following the structure indicated by R CRAN staff.

>You write information messages to the console that cannot be easily
>suppressed.
>It is more R like to generate objects that can be used to extract the
>information a user is interested in, and then print() that object.
>Instead of print()/cat() rather use message()/warning()  or
>if(verbose)cat(..) if you really have to write text to the console.
>(except for print, summary, interactive functions)

The informations write in console by print() and message() functions were either removed or changed to warning, when necessary.

>You are setting options(warn=-1) in your function. This is not allowed.

The settings with options(warn=-1) were removed from every files.

###Fourth submission

>* checking CRAN incoming feasibility ... NOTE
> Maintainer: 'Kleber Morais de Sousa <kleberfinancas@gmail.com>'

The package was submitted by maintainer.

>Possibly mis-spelled words in DESCRIPTION:
>  Stosic (4:17, 15:48)

The name is correct because it is the author's name.

> * checking DESCRIPTION meta-information ... NOTE
> Author field differs from that derived from Authors@R
>   [...]
> Maintainer field differs from that derived from Authors@R
>   [...]

The name was fixed.

>The Title field should be in title case. Current version is:
>'Correcting Nonparametric Frontier Measurements For Outliers by Sousa & Stosic (2005)'
>In title case that is:
>'Correcting Nonparametric Frontier Measurements for Outliers by Sousa & Stosic (2005)'

The title of package was changed to 'Correcting Nonparametric Frontier Measurements for Outliers'.


###Third submission

> The Title field starts with the package name.

The title field was changed and it has new title.

> The Description field should not start with the package name,'This package' or similar.

The description field was changed and it has new description.  

>Possibly mis-spelled words in DESCRIPTION:Jackstrap (3:8, 7:14), Stosic (3:26, 7:74)

I checked and the words is correct.

> * checking CRAN incoming feasibility ... NOTE
> Maintainer: 'Kleber Morais de Sousa <kleberfinancas@gmail.com>'

The informations about authors were write with new format and details. 

>* checking top-level files ... NOTE
>Non-standard file/directory found at top level:
>  'cran-comments.md'

It was inserted exception in .Rbuildignore.

>* checking package dependencies ... NOTE
>Depends: includes the non-default packages:
>  'fBasics', 'Benchmarking', 'dplyr', 'ggplot2', 'foreach',
>  'doParallel', 'reshape', 'tidyr', 'scales'

The dependencies was changed and the packages were included like imports.

>* checking dependencies in R code ... NOTE
>Packages in Depends field not imported from:
>  'Benchmarking' 'doParallel' 'dplyr' 'fBasics' 'foreach' 'ggplot2'
>  'reshape' 'scales' 'tidyr'
>  These packages need to be imported from (in the NAMESPACE file)
>  for when this namespace is loaded but not attached.

The dependencies was changed and the packages were included like imports.

> Check: R code for possible problems, Result: NOTE
>   hist_jack_ks: no visible global function definition for 'ggplot' and others

The functions and variables were inserted like imports in each files.

## R CMD check results

After that, there were no ERRORs, WARNINGs or NOTEs.

###Second submission

The package was submitted with some fixes about description in this files.

## Test environments

* local OS X, R 3.6.2
* local Windows, R 3.6.2
* local ubuntu 16.04, R 3.6.2
* win-builder

## R CMD check results

There were no ERRORs or WARNINGs.

There was 4 NOTEs.

New submission

## Reverse dependencies

None
