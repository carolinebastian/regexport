# regexport
R package to export nicely formatted regressions

This is an R function to create nicely formatted output for regressions, suitable for both analysis and publication. 
Unlike many similar packages, it splits the work of output into two parts: deciphering the output from a regression
function (like lm, glm, etc.) and outputting it. This means that if you are working with a new type of regression,
you just need to write an extension that puts it into a format that the output functions can work with. Similarly, if
you have a new type of output that you want to create, you can write an output function that works with a standard input.

This is very much in the alpha stage of development. Most of the functions should work, and if they don't, please drop
me a note that helps me to replicate the problem and I'll try and fix them (as I have time). Also feel free to make
suggestions. So far it works with lm, glm, and a handful of other methods that I use from time to time. I'd like to
expand that. Also so far, it outputs to Excel and the terminal (which looks great on Windows and needs work on Linux).

Please let me know if you like the package. I'm happy to receive all feedback! cbastian at stern.nyu.edu.

Note: this is released under an MIT license.

**Caroline R. Bastian**, Research Scholar <cbastian@@stern.nyu.edu>
Center for the Future of Management\cr
Leonard N. Stern School of Business\cr
New York University
 
Citations are appreciated! Please use:
 
Caroline R. Bastian (2022). regexport: Regression Table Output for R. R package version 0.20.