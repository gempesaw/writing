Working with JIRA, there are a bunch of instances where I see a JIRA
ticket inside of Emacs and I want to open it in Chrome - they come up
often in jabber chatrooms, there's tons of them in emails, etc. No
reason for me to copy it manually or search for it again on JIRA -
just let Emacs do the work. :D

    (defun open-jira-ticket-at-point ()
      (interactive)
      (let ((ticket (thing-at-point 'sexp)))
        (if (eq nil (string-match-p "^[A-z]+-[0-9]+$" ticket))
            (setq ticket (read-from-minibuffer
                          "Not sure if this is a ticket: "
                          ticket)))
        (browse-url
         (concat
          "http://arnoldmedia.jira.com/browse/" ticket))))

`(thing-at-point THING-TYPE)` is a useful function that "returns the
_THING_ at point". It accepts a couple different options of things
that it tries to look for - in our case, `sexp` seems to be reasonable
at picking out JIRA tickets from different contexts. Sometimes, it
picks up some extra stuff and we need to manually fix the input. So,
if the thing-at-point doesn't match the regexp `"^[A-z]+-[0-9]+$"`,
then ask the user to fix it a little bit, and then go ahead and
`browse-url` the ticket in Chrome. Easy peasy.

The better thing to do is probably go into `thingatpt.el` and figure
out how to make my own `thing` that it recognizes, but I couldn't be
bothered at the time.
