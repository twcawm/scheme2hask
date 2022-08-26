# scheme2hask

note: even though we improved handling of comments, the way we did it means we can currently only handle 1 comment per line/expression.  
if we encounter multiple comments in a row, the rest of the file is not parsed.  
this could be something to understand/fix later.
I actually would prefer to handle comments as a pre-processing step.  currently we are not doing that.
currently we are trying to handle comments during actual parsing of expressions, which I do not prefer.  but the way this parser was designed is convoluted enough that it is difficult to understand where preprocessing should go.
