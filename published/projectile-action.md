For the longest time, I've wanted to be able to bookmark the
[`magit`][magit] buffers of my various git projects. It's really
useful to be able to jump to the top level view and get a `git status`
summary of what I was in the middle of. However, I wasn't able to use
Emacs's built in bookmarks for this, because those require a file
reference to jump to, and the magit buffers were not file based. Enter
Projectile :)

[Projectile][projectile] (*Project* *I*nteraction *L*ibrary for
*E*macs) by [Bozhidar Batsov][bb] is a really nifty tool for managing
different projects in Emacs. For me it primarily smooths out finding
files in projects, and switching between project directories. In
particular, I'v got `s-p` bound to `projectile-switch-project`:

    (define-key projectile-mode-map [?\s-p] 'projectile-switch-project)

Projectile keeps a cache of the different git directories I've been
to, and provides it as a list of choices. You can customize the action
that Projectile does after you select a project, and there are a few
[suggestions][readme] like `projectile-find-file`, which would prompt
you for a file to visit, and `projectile-dired`, which immediately
opens up the new project's directory in a `dired` buffer. Both useful,
and for a while I settled for opening the project in a directory and
invoking `magit-status` myself.

However, my new favorite is

    (setq projectile-switch-project-action 'projectile-vc)

which tells projectile to open up the vc buffer for the new project,
which would be the magit buffer for git projects. Itch scratched!

[bb]: http://emacsredux.com/
[projectile]: https://github.com/bbatsov/projectile
[magit]: https://github.com/magit/magit
[readme]: https://github.com/bbatsov/projectile#switching-projects
