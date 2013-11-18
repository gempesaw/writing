    (display-time-mode 1)
    (setq display-time-day-and-date t)

Whenever I have to find out the current month and date (number), I
have to go to the top right corner on OS X or the bottom right corner
in Windows and open up the OS's time module. Maybe there's a way to
customize it to show the current date number, and there's definitely
programs I could install to do it, but to me it's just yet another
thing that I can put in Emacs.

`display-time-mode` puts the time in the modeline, and setting
`display-time-day-and-date` to `t` means that the current short month,
day name, and date number is also shown there. My modeline is getting
kind of long, but `diminish.el` is great for dealing with that.
