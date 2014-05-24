I'm on OS X Mavericks 10.9.2, running Emacs 24.4.50.1 GNU Emacs
24.4.50.1 (x86_64-apple-darwin13.1.0, NS apple-appkit-1265.19)
[compiled via homebrew][brew]. I was trying to get back into Emacs
package authoring with the help of [rejeep's][] excellent [Cask][] and
[ert-runner][] tools, but I was getting an error that didn't pop up on
Google very well:

    bash-3.2$ cask exec emacs -batch -l test/grunt-test.el
    Cannot open load file: subst-ksc

Searching for `subst-ksc` on google pops up results from other OS X
users from two or three years ago and no obvious fixes. I tried a
couple different things and ended up putting a fake `subst-ksc.el`
file in my load path with `(provide 'subst-ksc)` in it just to avoid
the error, but that just gave more of the same:

    bash-3.2$ cask exec emacs -batch -l test/grunt-test.el
    Cannot open load file: subst-gb2312

Eventually, I realized via `$ emacs --version` that I recently
switched to a new computer where I didn't symlink/replace the default
`/usr/bin/emacs` binary with my newer homebrew'd version. I tried the
new method [recommended in the comments][mu4e] of replacing that file
with a short shell script to invoke the proper emacs:

    #!/bin/sh
    /Applications/Emacs.app/Contents/MacOS/Emacs "$@"

and all became well.

[brew]: http://blog.danielgempesaw.com/post/81019534028/emacs-24-use-homebrew-instead-of-emacsformacosx
[rejeep's]: https://github.com/rejeep
[ert-runner]: https://github.com/rejeep/ert-runner.el
[Cask]: https://cask.github.io
[m4ue]: http://blog.danielgempesaw.com/post/43467552978/installing-mu-and-mu4e-with-homebrew-with-emacs-from#comment-1388528799
