I started seeing the following timer error in [mooz's `js2-mode`][1],
the actively maintained fork of Steve Yegge's `js2-mode`. This would
happen whenever I changed any javascript code, and the mode wouldn't
rehighlight the new code. Invoking `js2-mode` interactively would
properly apply the syntax highlighting, but then the next change to
the file would cause the error again.

     timer-set-function: Wrong type argument: timerp, [t nil nil nil nil nil nil nil]

There is also a fork of this fork, [`js3-mode`][j], which was working
fine for me, but unfortunately [`skewer-mode`][s] is only compatible
with the AST generated from `js2-mode`. I'd guess that making
`skewer-mode` compatible with `js3-mode` might be just a change to a
variable name or something, but I wasn't too interested in digging
into that on my own.

I'm unfortunately not sure what caused the issue for me. According to
the solution, it seems to have been updating to Emacs 24.3 instead of
24.2, but I hadn't recently updated my Emacs. Either way, I was
fortunate enough to find a solution in an [issue on Github][2]. It
seems that the culprit is stale bytecode compiled by 24.2 that is
invalid on 24.3 So, as [described in the issue][3], recompiling js2-mode
and restarting Emacs fixed it for me as well:

    (byte-recompile-directory
      (expand-file-name "~/.emacs.d/elpa/js2-mode-20130307.2012/") 0 t)

You'll have to replace the js2-mode version string with the date of
your actual js2-mode folder. `byte-recompile-directory` also works
recursively, so it can be applied to the `~/.emacs.d/elpa` folder to
recompile all of the elpa packages, or the `~/.emacs.d/` folder to do
your entire init folder.

[1]: https://github.com/mooz/js2-mode
[2]: https://github.com/mooz/js2-mode/issues/72
[3]: https://github.com/mooz/js2-mode/issues/72#issuecomment-15176816
[j]: https://github.com/thomblake/js3-mode
[s]: https://github.com/skeeto/skewer-mode
