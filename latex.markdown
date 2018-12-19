# LaTeX
adding quotes
-------------

	\begin{quote}
	...
	\end{quote}

Cheatsheet
----------

	\documentclass{book}
	\begin{document}
	\maketitle
	\end{document}

bibtex
------

	% Add to the preamble:
	% Load biblatex package
	\usepackage{biblatex}
	% Load bibTeX file
	\addbibresource{file.bib}
	% Add a citation somewhere in your file
	Blah blah \autocite{<key>}
	% In the backmatter),
	\printbibliography


document classes
----------------


* book
* article
* report
* letter
* slides


ePub
----

* <https://www.tug.org/TUGboat/tb32-3/tb102rishi.pdf>
* <http://pandoc.org/>
* <https://tex2ebook.wordpress.com/>


installing on Fedora
--------------------


* texlive
* pdflatex
* texlive-xetex
* texlive-cm
* texlive-hyphen-base
* texlive-mfware


