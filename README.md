# scheme2hask

improved parser in several ways relative to the reference.
1. most importantly, we can handle arbitrary white space after open paren & before close paren, for all types of lists.  this was not supported in the reference case.
2. we also added the ability to handle semicolon comments.  I would prefer to have done this as some kind of pre-processing step, but we implemented it into the actual parser.  the one advantage of this in principle is that theoretically we could use comments in some semantic way.  but obviously that goes against the common sense of what comments typically mean.
