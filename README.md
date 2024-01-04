#  Common Lisp function FORMAT


Rewrited FORMAT function from repo  https://github.com/public-domain/cmucl

Not included in the standard JSCL distribution

Only for Windows/Electron platform. How to use JSCL on the electron, see https://github.com/vlad-km/how-to/blob/main/jscl-electron.txt

# Status

Development

 
# Compilation

## development

```lisp
  (load "./repo/format/src/streams.lisp")
  (setq *out (open "./repo/format/rep.txt" 'write))
  (load "./repo/format/src/format.lisp")
  (setq *sot *standard-output*)
  (setq  *standard-output* *out)
  (load "./repo/format/src/format-test.lisp")
  (close *out)
```
## production

```lisp
(require "./format.js")
(jscl::fset 'format (fdefinition '%das!format))
```

# License

Public domain


### @vlad-km
   

