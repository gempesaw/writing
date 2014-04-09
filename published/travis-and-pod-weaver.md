I've taken the plunge with the excellent [Dist::Zilla][dz] recently
for authoring my Perl distributions. Since picking up the maintainer
role of [S::R::D][srd], D::Z fit my needs perfectly: speed up
distribution and alleviate all the repetitive parts of the process.

I recently started work on Perl bindings to the
[Browsermob Proxy][bmp] REST API called, unimaginatively,
[Browsermob::Proxy][bp]. As usual, we want it to run its tests on
travis, and there's the lovely `[TravisYML]` plugin for that:

    # dist.ini
    [TravisYML]

    [GatherDir]
    include_dotfiles = 1

    [PruneCruft]
    except = \.travis.yml

    [PodWeaver]

    [ReadmeAnyFromPod]
    type = markdown
    filename = README.md
    location = root

and we also want to use Pod::Weaver for more of that DRY
goodness. Since we're writing all the documentation in the main module
anyway, `[ReadmeAnyFromPod]` generates a markdown readme for us that
Github likes. The last bit is getting the Travis build status badge
into that readme. Surprisingly, Travis even have a suggested form for
POD badges:

    =for HTML <a href="https://travis-ci.org/gempesaw/Browsermob...

But unfortunately that won't work in our case, since we're outputting
to markdown. Via trial and error, I found that using the markdown form
of the build badge in a `=for markdown` POD section works with
`[ReadmeAnyFromPod]` with `type = markdown`:

    =for markdown [![Build Status](https://travis-ci.org/gempesaw...


in the POD does get it into the generated readme, but since it's not
in a dedicated section, `[Pod::Weaver]` just lumped it into the
`[Leftovers]` section, which by default is near the end of the
document. So, I [moved leftovers up to the top][gh-commit] and we're
[in business][bpgh]!

    # weaver.ini
    [@CorePrep]

    [-SingleEncoding]

    [Name]
    [Leftovers]
    [Version]
    ...

(This may be troublesome if other things fall into the Leftovers
section, so we'll have to keep an eye out...)

[dz]: https://metacpan.org/pod/Dist::Zilla
[srd]: https://metacpan.org/pod/Selenium::Remote::Driver
[bmp]: http://bmp.lightbody.net/
[bp]: https://metacpan.org/pod/Browsermob::Proxy
[bpgh]: https://github.com/gempesaw/Browsermob-Proxy
[gh-commit]: https://github.com/gempesaw/Browsermob-Proxy/commit/7e6e14cb9d8a1dc42bf9882af743176d692269c2
