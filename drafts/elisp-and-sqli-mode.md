# Using Elisp to programmatically connect to a MySQL Server

Emacs has a nifty built-in function `sql-mysql` that prompts you for
your username, password, database, and server. After collecting that
information, it opens up an interactive SQL session exactly the same
as what you'd get by invoking `mysql` on the command line. I happened
across a useful helpfile in tandem with a lucky google search and I
figured out how to invoke sql-mysql from elisp and prevent it from
prompting me interactively. I find this to be particularly useful, as
I'm usually connecting to the same two or three DBs again and again;
encapsulating that functionality in a defun and binding it to a key
saves me tons of time looking up the creds each time I want to connect.


    (require 'noflet)

    (setq db-lookup '((local . ((user "test")
                                (password "pass")
                                (server "localhost")
                                (database "testdb")))))

    (defun non-interactive-sql-mysql (server)
      (let* ((server (assoc server db-lookup))
             (sql-user (cadr (assoc 'user server)))
             (sql-password (cadr (assoc 'password server)))
             (sql-server (cadr (assoc 'server server)))
             (sql-database (cadr (assoc 'database server))))
        (noflet ((sql-get-login (&rest args)))
          (with-temp-buffer
            ;; (cd "/ssh:remote:/home/daniel")
            (sql-mysql (concat sql-user "@"
                               sql-database "."
                               sql-server))))))

    (non-interactive-sql-mysql 'local)


* `(let ...`: First things first, set up the DSN information.

* `(with-temp-buffer (cd ...`: In case the sql-user you're using has
restrictive privs that dictate which IP you must use to connect to the
server, you can leverage `TRAMP` and an appropriate entry in your
`~/.ssh/config` file to open an ssh connection to that remote box
before attempting to connect to the sql server. `cd` changes the
`default-directory` in the current buffer; since we don't want to
change our current buffer, we use a temp-buffer to change it and
discard it afterwards.

* `(nofet ((sql-get-login ...`: This is the part that turns off the
interactive prompting for the user/pass/db/server. Since `flet` has
been deprecated, we're using Nic Ferrier's [`noflet`][noflet] to convince
`sql-get-login` not to prompt us in the minibuffer.

* `(sql-mysql ...`: And now to open the sql connection without
interactively prompting you for anything! Wonderful - `sql-mysql`
accepts as its single argument the name of the buffer to be created,
which we can specify based on the user, db, and server in question.

I also came across an interesting looking [elisp package][edbi] that uses
Perl's `DBI` to provide an interactive MySQL session (like queries
that update as you change them?!) but I haven't looked into it much
further than skimming the README on Github. It looks really powerful,
though, and it would completely obsolete the above snippet.

[noflet]: https://github.com/nicferrier/emacs-noflet
[edbi]: https://github.com/kiwanami/emacs-edbi/
