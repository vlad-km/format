#  Common Lisp function FORMAT

Rewrited FORMAT function from repo https://github.com/public-domain/cmucl

Not included in the standard `JSCL` distribution

Only for Windows/Electron platform. How to use `JSCL` on the `Electron`, see https://github.com/vlad-km/how-to/blob/main/jscl-electron.txt

___

# Status

### Pretty Print - Development

### FORMAT - first release, version 1.0
-  `FORMATTER function`* is also included in the release


#### FORMAT directives, included in the release:
-  _`~A`_  _`~S`_ _`~C`_
-   `~W`
-   `~D`  `~B`  `~O`  `~X`  `~R` `~F`
-  `~P`
-  `~%` `~&` `~~`  `~#\newline`
-  `~*` `~?`
-  `~{` `~;` `~^` `~[`
-  `~/`

#### See some implementation limitations in the report:
- file `./rep.txt'
___
 
# Compilation

## development


```lisp
  (load "./repo/format/src/format.lisp")
  ;; or
  (require "./format.js")
  (load "./repo/format/src/pp/format-pp.lisp")
  (load "./repo/format/src/pp/pp.lisp")
```
___

## production

```lisp
   (require "./format.js")
   (jscl::fset 'format (fdefinition '%das!format))
   ;; after the command, `DAS!FORMAT' function is called as `FORMAT'
   ;;`JSCL FORMAT' now is available for use as `JSCL::!FORMAT'
   ;; (format t "~5&Hello~5%")      - ANSII FORMAT
   ;; (jscl::!format t "~&Hello~&") - JSCL FORMAT
   (setq *f1 (formatter "~5&Hello ~a~%"))
   (format t *f1 :|any name|)
```

___

# License

Public domain

___


### @vlad-km
   

