# moppNord
This package provides templates for a master thesis and other mandatory assignments at Nord University. All templates include a title page and formatted to comply with official requirements.
## Installation
To get access to the templates, you need to have library `devtools` installed. Then run

`devtools::install_github("ev-ant/moppNord")`

It is better to restart **R** in order to see *Monography MOPP*, *Article-based MOPP*, and *Assignment at Nord* in the list of \.Rmd templates.

### For MacOS users
These templates rely on Calibri font that is native for the Windows. To install Calibri font on MacOS, follow [this instructures from Apple StackExchange](https://apple.stackexchange.com/questions/128091/where-can-i-find-default-microsoft-fonts-calibri-cambria) (use the first answer).

## Shared structure
When chosing a template and setting its name, you will get a folder with chosen name that contains three types of files:

1. *<chosen_name>.Rmd* - here you will write the work itself
2. *.bib file - here you need to store your references in the BibTex format.
3.  pictures `nordlogoen` and `nordlogono`, which contain Nord University's logo in English and Norwegian.

*.Rmd* file also contains some basic tips on what should be written there. The main "rules" of writting in Rmarkdown is given here: [Rmarkdown from RStudio](https://rmarkdown.rstudio.com/lesson-1.html).

**NB**. Citation also should be done in Rmarkdown-friendly format:

- `@citation-key` to get citation in form *Author (year)*
- `[@citation-key]` to get citation in form *(Author, year)*
- `-@citation-key` to get citation in form *(year)*
- `[@citation-key1; @citation-key2]` to get citation in form *(Author1, year; Author2, year)*.

You should not delete the code provided in the *.Rmd* - to have the best looking pdf.

## Template *Monography MOPP*
This template is made for a monography-based master thesis. To get a complete thesis:

1. Edit *<chosen_name>.Rmd*, opened first (with the name you have specified), by fill in the **title** for your thesis and your **candidate number**.
2. Write all the bits of the master thesis in this *.Rmd* document.
3. Insert your \.bib file with a bibliography for the master thesis into *library.bib*.
4. Compile the master thesis with the button *Knit*.


## Template *Article-based MOPP*
This template is made for an article-based master thesis. To get a complete thesis:

1. Edit *<chosen_name>.Rmd*, opened first (with the name you have specified), by fill in the **title** for your thesis and your **candidate number**.
2. In the *chapter1.bib* you have to insert the bibliography for the kappa/ introduction. Include only those citations that are used - this format could not filter out citations that are not referenced. 
3.  In the *chapter2.bib* you have to insert the bibliography for the article. Include only those citations that are used - this format could not filter out citations that are not referenced.
4. Compile the master thesis with the button *Knit*.

   
   * \.bib file can be exported from your citation managers, like [*Mendeley*](https://blog.mendeley.com/2011/10/25/howto-use-mendeley-to-create-citations-using-latex-and-bibtex/), [*Zotero*](https://unimelb.libguides.com/c.php?g=565734&p=3897111), [*EndNote*](https://www.rhizobia.co.nz/latex/convert) (instructions are under the links); or [prepare it on your own](https://en.wikibooks.org/wiki/LaTeX/Bibliography_Management#BibTeX) (better not to if you are not experienced with BibTeX); [this link](https://truben.no/latex/bibtex/#) can be helpful.

## Template *Assignment at Nord*
This is a template for the assignment at Nord. To get a pdf with your work:

1. Edit *<chosen_name>.Rmd*, opened first (with the name you have specified), by fill in the **title**, **course**  and your **candidate number**.
2. Write all the bits of the master thesis in this *.Rmd* document.
3. Insert your \.bib file with a bibliography for the master thesis into *library.bib*.
4. Compile the master thesis with the button *Knit*.


Feedback is welcomed.
