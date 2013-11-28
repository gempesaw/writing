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


    (defun non-interactive-sql-mysql ()
      (let ((sql-user "username")
            (sql-password "secret-password")
            (sql-server "some.server.address")
            (sql-database "testdb")
            (sql-interactive-get-login
             (symbol-function 'sql-get-login)))
        (with-temp-buffer
          ;; (cd "/ssh:remote:/home/daniel")
          (fset 'sql-get-login 'ignore)
          (sql-mysql (concat "\*SQL: " sql-user "@" sql-database "." sql-server "\*"))
          (fset 'sql-get-login
                (symbol-function 'sql-interactive-get-login)))))


* `(let...`: First things first, set up all the credentials and
addresses, and hold on to the function definition of `sql-get-login`.

* `(with-temp-buffer (cd...`: In case the sql-user you're using has
restrictive privs that dictate which IP you must use to connect to the
server, you can leverage `TRAMP` and an appropriate entry in your
`~/.ssh/config` file to open an ssh connection to that remote box
before attempting to connect to the sql server. `cd` changes the
`default-directory` in the current buffer; since we don't want to
change our current buffer, we use a temp-buffer to change it and
discard it afterwards.

* `(fset 'sql-get-login 'ignore)`: This is the part that turns off the
interactive prompting for the user/pass/db/server. But, we need to
restore the interactive behavior of `sql-get-login` for later in case
we invoke `M-x sql-mysql` some time later. That's why we've clumsily
stored it in `sql-interactive-get-login` in the let-binding. The
cleaner way to do this would've been to use `flet`, but that was
deprecated in recent versions of Emacs. There are replacement
libraries available - I know of at least one by Nic Ferrier, and I'm
speculating that `mocker.el` should be able to provide similar
functionality. But, for the time being, it's just `fset` before and
after :P.

* `(sql-mysql)`: And now to open the sql connection without
interactively prompting you for anything! Wonderful - `sql-mysql`
accepts as its single argument the name of the buffer to be created,
which we can specify based on the user, db, and server in question.

I also came across an interesting looking elisp package that uses
Perl's `DBI` to provide an interactive MySQL session (like queries
that update as you change them?!) but I haven't looked into it much
further than skimming the README on Github.

If you're interested in using SQLi-mode for a MySQL db, like [Bozhidar
mentions for Postgres][bb] - editing your SQL queries in
a separate `.sql` file and sending them over to the MySQL buffer, I
had to set the product `M-x sql-set-product RET mysql RET` in the
`.sql` file buffer before `sql-set-sqli-buffer` recognized the
`*SQL*` buffer as a viable selection.

[bb]: d
