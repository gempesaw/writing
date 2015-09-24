I use Emacs compilation mode to execute most of my scripts - it has a
lot of nice built in things, including error highlighting with
automatic navigation. But, I've noticed that with really long lines,
the compliation buffer gets so slow that it makes Emacs unresponsive
to input, and I end up having to frantically <kbd>Ctrl+G</kbd> and
hope that I can stop the compilation before I lose Emacs and have to
Force Quit it.

One particular instance is when I'm using Webdriver to take
screenshots - Webdriver takes the screenshot (in binary format?) and
base64 encodes it, so that means I've got a very long string to work
with, and if I accidentally print it out, Emacs gets quite overwhelmed.

As usual, I'm not the only one to run into this issue, and there's a
[gmane.emacs.bugs thread][thread] ([cache][])
about disabling the Maven regular expression in the
`compilation-error-mode-alist`:

    (setq compilation-error-regexp-alist
          (delete 'maven compilation-error-regexp-alist))

I had thought it had to do with colorization, or perhaps the rainbow
delimiters that I use, but [that simple line][line] of disabling the
Maven regex made a huge difference. Hooray for correcting
long-standing annoyances.

[thread]: http://comments.gmane.org/gmane.emacs.bugs/28783
[cache]: http://webcache.googleusercontent.com/search?q=cache%3Acomments.gmane.org%2Fgmane.emacs.bugs%2F28783&oq=cache%3Acomments.gmane.org%2Fgmane.emacs.bugs%2F28783&aqs=chrome..69i57j69i58.734j0j4&sourceid=chrome&es_sm=91&ie=UTF-8
[line]: https://github.com/gempesaw/dotemacs/commit/a8bf86d3c4148fcbf630b5f2a1c8ae6c9d981237
