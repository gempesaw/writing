One of my favorite parts about my frontend JS workflow is using Live
Reload with Grunt/Gulp. Saving a few keystrokes a couple times a
minute is pretty great, especially if I can stay on the home
keys. Grunt and Gulp have a task that watches the files in your
project and then you kick can off certain tasks when they change, like
refreshing your display and re-running all the unit tests. In Scala,
you can get a similar thing by invoking your `sbt` command prefixed
with a `~`, but I didn't have anything for my Perl projects (not to
mention a general solution!).

At first, I looked for a similar tool that would watch files for
changes and let me hook into that action. I found
[App::Prove::Watch](https://metacpan.org/pod/App::Prove::Watch) which
was a Perl-only solution. It worked swimmingly after a quick PR, but
it only solved the problem for Perl projects. It wasn't until a few
days later that I realized I could leverage Emacs to get a reasonably
general solution, only depending on use of the `*compilation*` buffer
(no idea why it didn't occur to me earlier!).

Of course, Emacs knows exactly when my files change, as we always
eventually invoke `(save-buffer)` - even if it's advised with with
whitespace cleanup, or if it's invoked via some sort of
autosave. Instead of watching files on disk for changes, we can just
use Emacs to have the `(save-command)` trigger our tasks - aka
re-compiling the `*compilation*` buffer.

I used a global variable `ar-auto-recompile` to turn autocompilation
on and off, and made a function to handle the toggling for me:

    (setq ar-auto-recompile nil)
    (defun ar-auto-re-compile ()
      (interactive)
      (setq ar-auto-recompile (not ar-auto-recompile))
      (message (format "Auto recompile is now %s"
                       (if ar-auto-recompile "ON" "OFF"))))

It just toggles the variable and messages us about it. Next, our
advice to save-buffer should check whether the user wants
auto-recompilation. I also decided I only wanted it to re-compile if
the `*compilation*` buffer was in a visible window, which
`(get-buffer-window)` happily tells us.

    (defadvice save-buffer (after ar-auto-recompile activate)
      (when (and ar-auto-recompile
                 (get-buffer-window "*compilation*"))
        (set-buffer compilation-last-buffer)
        (revert-buffer t t)))

This solution isn't _quite_ the same as the watch behavior of the
other tools, as they smartly only respond to changes in files in their
own project. When turned on, this would blindly recompile any time a
buffer is saved and the `*compilation*` buffer is open. But, I don't
really mind - a few extra compilations probably won't hurt anything
unless the compilation wasn't safe to repeat in the first place.

So, my workflow is now to open up my test file, toggle on the
recompilation with `(ar-auto-re-compile)`, run a test to bring up the
`*compilation*` buffer. I keep that buffer visible and then do edits
and such as usual, [saving with `M-s`][save], which automatically
re-runs the test. And, if I need a more complicated compilation
command, it's just a `C-u M-x compile` away, and then that can get
repeated for me. As an extra bonus, this works perfectly with
[projectile][]'s compilation and test running commands (by default on
<kbd>C-c p c</kbd> and <kbd>C-c p P</kbd>), as they utilize the
`*compilation*` buffer as well. I also made a key-chord to switch the
current buffer over to the compilation one, as I often accidentally
close it:

    (key-chord-define-global "vv" (lambda () (interactive)
                                    (switch-to-buffer "*compilation*")))

Finally, if you don't want to use advice, it's [simple enough][] to
use a wrapper around `(save-buffer)` and update your save keybinding
to use your own function. And as usual, the whole snippet is in my
github [somewhere][].

[save]:
https://github.com/gempesaw/dotemacs/blob/6ca8a3995d558ac924e576bd60c516dbd1c450e2/dg-elisp/dg-kbd.el#L135-L140
[projectile]: https://github.com/bbatsov/projectile
[simple enough]: https://github.com/gempesaw/dotemacs/blob/8e7cb17e72339069d5e318fd7eb44d4718faa36c/dg-elisp/dg-defun.el#L461-L466
[somewhere]: https://github.com/gempesaw/dotemacs/blob/6ca8a3995d558ac924e576bd60c516dbd1c450e2/dg-elisp/dg-defun.el#L459-L470
