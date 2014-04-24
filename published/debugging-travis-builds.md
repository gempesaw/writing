### 1. motivation

There's been a curious `dump_config` build error in my Dist::Zilla
Travis-CI builds as of late. I'm using the [TravisYML] Dist::Zilla
plugin to generate my `.travis.yml` file, and up until now it's been
working great. But, there seems to be some dependency conflict that I
don't have on my local machine - at some point, the travis build box
can no longer run any `dzil` commands, as it bombs out with:

    The method 'dump_config' was not found in the inheritance hierarchy
    for Dist::Zilla::App::CommandHelper::ChainSmoking at [...]

Which is then followed by a completely unhelpful (to me) Moose
stacktrace.

### 2. debugging travis builds?

I wasn't aware of a good method to debug travis builds - previously, I
had been just committing minor changes, pushing 'em to Github, and
then waiting for the Travis build to kick off. That feedback loop
could take up to 15 minutes, and wasn't really conducive to any sort
of debugging method.

I emailed support AT travis-ci.com inquiring about getting access to
one of their build boxes, since the outdated vagrant box method was
[now obsolete][vagrant] and one of their support staff got back to me
within minutes with a cloud box I could SSH into for 24 hours. Really
pleased with their turnaround time and support, considering it's a
free service!

[vagrant]: http://stackoverflow.com/questions/16677232/where-can-i-download-the-64-bit-travis-ci-vm-images/17133843#17133843

### 3. perlbrew on travis debug boxes

The box I got had a `~/perl5` folder with a bunch of perls already
installed. But, it didn't have the `perlbrew` binary in the path. I
updated the path

    $ export PATH=~/perl5/perlbrew/bin:$PATH

but the commands like `perlbrew use` and `perlbrew switch` weren't
having any impact on `perl -v`. It turns out I needed to

    $ perlbrew init
    $ source ~/perl5/perlbrew/etc/bashrc

in order to get `perl -v` to recognize I didn't want to use the system
perl anymore. Had I been using more shells, I'd probably want to put
all of that in the ~/.bash_profile, but I only needed the one,
especially since `perlbrew lib` lets you manage completely separate
`local::lib`s.

So, I switched to 5.19 and spun up a new local::lib and started
playing around with dependencies. If I got too far and broke
everything again, a new local::lib can reset all the installed deps
and get me into pristine condition.

    $ perlbrew switch 5.19
    $ perlbrew lib create debug\_dist\_zilla
    $ perlbrew use 5.19@debug\_dist\_zilla

### 4. getting to the heart of the issue

The generated `.travis.yml` file does a couple of things - it exports
some global variables, configures a bit of necessary git settings, and
then gets to the business of handling author and project dependencies.

    $ cpanm --quiet --notest --skip-satisfied Dist::Zilla
    $ dzil authordeps | grep -vP '[^\\w:]' | \
        xargs -n 5 -P 10 cpanm --quiet --notest --skip-satisfied"
    $ dzil listdeps   | grep -vP '[^\\w:]' | cpanm --verbose --skip-satisfied"

The travis build boxes come with 5.006 installed, so the first line
actually doesn't do anything. As reported in the [github issue][gh],
the `dump_config` bug occurs at the `dzil listdeps` step. So, one of
the authordeps must be causing it ... probably.

    $ dzil authordeps --missing | head -n 1 | cpanm --skip-satisfied && dzil

It just steps through each of the author dependencies one at a time,
installing it with cpanm, and then verifying that I can still `dzil`
afterwards.

[gh]: https://github.com/SineSwiper/Dist-Zilla-TravisCI/issues/16#issuecomment-40705662

### 5. solutions?

In not-so stunning turn of events that seems incredibly obvious now,
the problem seems to arise after installing
[Dist::Zilla::TravisCI][pod]. In the generated travis script, the
author deps are installed with `cpanm --notest`, so it's not caught
until the next time we attempt to `dzil listdeps`. Stepping through
the tests one by one, the `::TravisCI` install actually fails its own
tests with the same `dump_config` error that we see in the travis logs
after `--notest` installing it.

So, in theory, commenting out the `[TravisYML]` from dist.ini should
remove it from `dzil authordeps` and we should be good to go. I'm
going to go try that now...

[pod]: https://metacpan.org/pod/Dist::Zilla::TravisCI
