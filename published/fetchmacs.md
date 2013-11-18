[Fetchnotes](http://www.fetchnotes.com) is yet another todo-list/note taking app. I like their service because everything is just plain text, so it's very straightforward from a usage standpoint. I don't have to worry about formatting or saving a url or just part of a page or anything that I'm not concerned about; it's just text! Additionally, they use tags to organize your notes; I have an affinity for tagging/labelling things excessively with the assumption that future me will be able to think of at least one of the tags past me used.

They've got a web interface and a nice responsive mobile view, and they also have an iOS app, an iPad app, and an Android app, which means that I can CRUD notes from any device I've got; since it's just text, it's easy on every device. As is the Emacs way, a thing worth doing is worth doing from inside Emacs. I've written [fetchmacs](https://github.com/gempesaw/fetchmacs.git), an Emacs major-mode to interact with their API from inside of Emacs. I've been using for myself for a few weeks.

It's pretty straightforward - the biggest issue that I always have with Elisp is figuring out how to dereference the data structures. Otherwise, it's just basic auth stuff and CRUD, with some keybindings to smooth things along. I haven't gotten around to MELPA/Marmalade integration, although that's my next goal. For the time being I've just been `load-file`ing it:

    (defvar fetchmacs-user-email "user@n.ame")
    (defvar fetchmacs-user-pass "password")
    (load-file 'fetchmacs.el)

Put your user and pass in `fetchmacs-user-email` and `fetchmacs-user-pass` and you should be good to go. Invoke `fetchmacs-view-notes` to get to the notes dashboard, using `fetchmacs-view-notes-mode`. I've got it bound as `C-x f`, because I never use `set-fill-column`:

    (global-unset-key (kbd "C-x f"))
    (global-set-key (kbd "C-x f") 'fetchmacs-view-notes)

I really enjoy using [magit](http://philjackson.github.com/magit/), so whenever I possible I tried using the same or similar keys.

* `RET` or `e` or `o` : `fetchmacs-view-edit-note-at-point`
* `c` : `fetchmacs-create-new-note`
* `D` or `k` : `fetchmacs-delete-note-at-point`
* `/` : `fetchmacs-search`
* `g` : `fetchmacs-refresh`
* `n` : `fetchmacs-goto-next-note`
* `p` : `fetchmacs-goto-previous-note`
* `t` : `fetchmacs-filter-by-tag`

Outside of `fetchmacs-view-notes-mode` there's also an editing mode which pops up in another window, just like editing `magit` commits. In there, the the following keybindings are available:

* `C-c C-c` : `fetchmacs-save-note`
* `C-c C-k` : `fetchmacs-cancel-edit`

So just like `magit`, you can save or cancel from in there.

And that's about it! On the off chance that a) there's actually anyone reading this, b) you're an Emacs user, c) _and_ you're a Fetchnotes user - a pretty unlikely cross section - please give it a try! :)
