Bookmarks in Emacs are very, very useful for getting to places you frequently go. If you don't use them to help you navigate, you should consider it! For whatever reason, bookmarks are saved in an external file, not in an Emacs variable. I can't count the number of times I added a bookmark, closed Emacs, and inadvertently deleted it. Even with version control, I still kept losing bookmarks because I kept forgetting to save.

    (setq bookmarks-default-file "~/.emacs.d/bookmarks")
    (defadvice bookmark-set (after save-bookmarks-automatically activate)
      (bookmark-save))

Let Emacs know where you want to save the bookmarks, and then use some advice! This little advice saves you all that woe: any time you set a bookmark, Emacs now saves all of them for you automatically. Presto :)