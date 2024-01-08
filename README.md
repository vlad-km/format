#  Common Lisp function FORMAT

Rewrited FORMAT function from repo  https://github.com/public-domain/cmucl

Not included in the standard JSCL distribution

Only for Windows/Electron platform. How to use JSCL on the electron, see https://github.com/vlad-km/how-to/blob/main/jscl-electron.txt

___

# Status

### Pretty Print - Development

### format - first release
### format directives included in the release:
-  _`~A`_  _`~S`_ _`~C`_
-   `~W`
-   `~D`  `~B`  `~O`  `~X`  `~R`
-  `~P` `~F`
-  `~%` `~&` `~~`  `~#\newline`
-  `~*` `~?`
-  `~{` `~;` `~^` `~{`
-  `~/`
___
 
# Compilation

## development

___

```lisp
  (load "./repo/format/src/streams.lisp")
  (setq *out (open "./repo/format/rep.txt" 'write))
  (load "./repo/format/src/format.lisp")
  (setq *sot *standard-output*)
  (setq  *standard-output* *out)
  (load "./repo/format/src/format-test.lisp")
  (close *out)
```
___

## production

```lisp
(require "./format.js")
(jscl::fset 'format (fdefinition '%das!format))
```

___

# License

Public domain

___


### @vlad-km
   

