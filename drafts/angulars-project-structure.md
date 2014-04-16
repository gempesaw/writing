A few months ago, the AngularJS team published a [document][gd]
outlining a new best-practices project structure. There's also a
(huge) Github [issue discussing its pros and cons][gh] in finer
detail, and it was mentioned in the `generator-angular` [2014 Q1
roadmap issue][gh2]

The old recommended structure, which is what `yo angular` generates,
groups files together by function: controllers are with other
controllers, directives are with other directives, etc. In some cases,
they're all even in the same file.

    app/
        css/
            app.css
            bootstrap.css
        scripts/
            app.js
            controllers/
                main.js
                sidemenu.js
            services/
                backend.js
            directives/
                datepicker.js
        templates/
            datepicker.html
            main.html
        index.html
    test/
        spec/
           controllers/
               mainSpec.js
           services/
               backendSpec.js
           directives/
               datepickerSpec.js


That was easy for me to grasp initially and due to the strong tooling
built around that structure, it was easy to add new directives and
providers because the tools knew how to navigate the folder structure
and set up the boilerplate. But, I really like the new proposed
structure due to some shortcomings of the old one:

* It doesn't scale very well. In a large project, the controllers,
  directives, and services folder will each have many files, and the
  majority of them will be unrelated to each other.

* It's hard to find things if you don't know where to look, or if
  you've just forgotten where you put it. Given a front end component
  that in which you found a bug, it's not immediately evident where
  you would look to find and edit that functionality.

* When editing a file, it's not immediately evident from the filename
  what kind of file you're looking at, as the 'type-of-thing'
  classification is done in the folder name.

The new structure is grouped by components and might look like

    app/
        app.js
        index.html
        components/
            datepicker/
                datepicker-directive.js
                datepicker-directive_test.js
                datepicker.html
            backend/
                backend-service.js
                backend-service_test.js
        main/
            main-controller.js
            main-controller_test.js
            main.html

The components are grouped by functionality and house services,
filters, directives, and related files (templates, test files,
etc). Meanwhile, the sub sections like `main` would only have
controllers, css, and templates. I really enjoy using the new layout
and it makes it super easy to switch between the test and the
implementation, since they're right next to each other.

When I converted my project to this structure, I had to account for
the new file structure in the Gruntfile and in the karma
configuration. Both of the config files have a useful `**/*.js` glob
option that recursively includes all child files that match, but the
`bower_components` folder also gets picked up, and then I end up
linting someone else's files.

My current project only has one sub-section, `editor`, so the end of
the [karma.conf.js files section looks like][mykarma]:

     // our scripts
     'app/app.js',
     'app/editor/**/*.js',
     'app/components/**/*.js'

The gruntfile was a bit more of a pain, since the js files are
referenced in more places. But, the `watch:livereload` task after the conversion
looked like:

    livereload: {
        options: {
            livereload: '<%= connect.options.livereload %>'
        },
        files: [
            '<%= yeoman.app %>/{components,editor}/**/*.{js,html,css}',
            '<%= yeoman.app %>/index.html',
        ]
    }

It's a bit unwieldy - the `a/{b,c}/e` construct matches `a/b/e` and
`a/c/e`, and the `**/*` does a recursive descent into folders, so that
one line catches everything in components and in my only subsection,
and skips the `bower_components` folder.

[gd]:
https://docs.google.com/document/d/1XXMvReO8-Awi1EZXAXS4PzDzdNvV6pGcuaF4Q9821Es/pub
[gh]: https://github.com/yeoman/generator-angular/issues/109
[gh2]: https://github.com/yeoman/generator-angular/issues/553
[mykarma]: https://github.com/gempesaw/honeydew-ng/blob/18045cfd0d50fe48fcb8df9381f80354e7bec32e/karma.conf.js#L40-L45
