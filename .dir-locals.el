((haskell-mode
  . (
     (haskell-process-args-stack-ghci . ("--ghci-options=-ferror-spans" "--no-build" "--no-load"
                                         "aeson-typescript:lib"
                                         "aeson-typescript:aeson-typescript-tests"
                                         ))
     )))
