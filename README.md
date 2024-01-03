#  format


rewrited FORMAT function from repo  https://github.com/public-domain/cmucl

not included in the standard JSCL distribution

for Windows/Electron platform only

# status

development

 
# compilation

## development

```lisp
;;; compile 'format', save "format.js"
(load "./repo/format/src/format.lisp" :hook (let ((x #()) x) :output "./format.js")
;;; compile 'format-test.file', run test-cases
(load "./repo/format/src/format-test.lisp")
```
## production

```lisp
(require "./format.js")
(jscl::fset 'format (fdefinition '%das!format))
```

# License

Public domain


### @vlad-km
   

