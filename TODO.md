# Wayes to improve rb-cc
## TODOS
- move Parser2 to Libs
- Remove Parser1 
- Add interpriator for Parser2
- Add semantic to calculate simple function from Parser2
- Add semantic substitution to interpriator
- Add semantic calculation to interpriator



## Error handling

### TODO print error like ghci


```
app/Error.hs:19:1: warning: [-Wincomplete-patterns]
    Pattern match(es) are non-exhaustive
    In an equation for ‘printError’:
        Patterns of type ‘String’, ‘Error’ not matched: _ (ErrorCode _)
   |
19 | printError input (ErrorToken t text) = do
   | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^...

```
1. print line number in left-margin part
2. print a few line of text around error
3. highlit error construction

### TODO system to test errors


