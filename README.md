# moppNord
Templates for a master thesis and other mandatory assignments at Nord University. Both templates for MOPP include a title page and formatted to comply with official requirements.
## Installation
To get access to the templates, run

`devtools::install_github("ev-ant/moppNord")`

It is better to restart **R** in order to see *MOPP - Nord University* and *MOPP_mono - Nord University* in the list of \.Rmd templates.

## Template *MOPP - Nord University*
This template is made for an article-based master thesis. To get a complete thesis:

1. Edit *<your_file_name>.Rmd*, opened first (with the name you have specified), by fill in the **title** for your thesis and your **candidate number**.
2. In the *kappe.Rmd* you have to write your introductory part for the thesis. This document can be rendered also separately.
3. In the *article.Rmd* you have to write your article. You can also render an article separately.

    * **OBS** If you want to use article separately, do not forget to fill in title and author fields in the header.
    
4. Insert your \.bib file with a bibliography for the master thesis into *library.bib*.

To get the master thesis that includes both parts, run the command `moppNord::create_mopp("<your_file_name>.Rmd")`.

## Template *MOPP_mono - Nord University*
This template is made for a monography-based master thesis. To get a complete thesis:

1. Edit *<your_file_name>.Rmd*, opened first (with the name you have specified), by fill in the **title** for your thesis and your **candidate number**.
2. Write all the bits of the master thesis in this *.Rmd* document.
3. Insert your \.bib file with a bibliography for the master thesis into *library.bib*.
4. Compile the master thesis with the button *Knit*.

More practical notes are given in the template itself - read carefully.

**OBS**: Use **only** RMarkdown-friendly citation!

Feedback is welcomed.
