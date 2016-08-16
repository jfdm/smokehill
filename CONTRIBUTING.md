# Contributing to Smokehill

I welcome pull requests, bug reporting, and bug squashing!  However, I
cannot do it all ourselves, and want to make it as easy as possible to
contribute changes to get things working.  Here are a few guidelines
that I would like contributors to follow so that we can have a chance
of keeping on top of things.

## Getting Started

1. Make sure you are familiar with [Git](http://git-scm.com/book).
1. Make sure you have a [GitHub account](https://github.com/signup/free).
1. Make sure you are familiar with: Haskell
1. Make sure you have installed [Idris](https://www.idris-lang.org).

## Issue Reporting

Before you report an issue, or wish to add cool functionality please
try and check to see if there are existing
[issues](https://github.com/jfdm/smokehill/issues) and
[pull requests](https://github.com/jfdm/smokehill/pulls).  We do not
want you wasting your time, duplicating somebody's work!

## The Campsite Rule

We try to follow the **campsite rule**: leave the code base in better
condition than you found it.  Please clean up any messes that you
find, and don't leave behind new messes for the next contributor.

## Making Changes

Idris developers and hackers try to adhere to something similar to the
[successful git branching model](http://nvie.com/posts/a-successful-git-branching-model/).
The steps are described below.

### New contributors

For those new to the project:

1. Fork our
   [main development repository](https://github.com/jfdm/smokehill)
   `smokehill` on github e.g.
2. Clone your fork to your local machine:

```sh
$ git clone git@github.com/<your github user name>/smokehill.git
```

3. Add `idris-lang/smokehill` as a remote upstream

```sh
$ git remote add upstream git@github.com:jfdm/smokehill.git
```

### Existing Contributors

For those already contributing to the project:

1. Ensure your existing clone is up-to-date with current `HEAD` e.g.

```sh
$ git fetch upstream
$ git merge upstream/master
```

### Remaining Steps

The remaining steps are the same for both new and existing contributors:

1. Create, and checkout onto, a topic branch on which to base you work.
  * This is typically the master branch.
  * Please avoid working on the `master` branch.

```sh
$ git branch fix/master/my_contrib master
$ git checkout fix/master/my_contrib
```

1. Make commits of logical units.
1. Check for unnecessary whitespace with

```sh
$ git diff --check
```

1. Make sure your commit messages are along the lines of:

> Short (50 chars or less) summary of changes
>
> More detailed explanatory text, if necessary.  Wrap it to about 72
> characters or so.  In some contexts, the first line is treated as the
> subject of an email and the rest of the text as the body.  The blank
> line separating the summary from the body is critical (unless you omit
> the body entirely); tools like rebase can get confused if you run the
> two together.
>
> Further paragraphs come after blank lines.
>
> - Bullet points are okay, too
>
> - Typically a hyphen or asterisk is used for the bullet, preceded by a
>   single space, with blank lines in between, but conventions vary here

1. Make sure you have added any necessary tests for your changes.
1. Run all the tests to ensure nothing else was accidentally broken.

```
$ make test
```

1. Push your changes to a topic branch in your fork of the repository.

```sh
$ git push origin fix/master/my_contrib
```

1. Go to GitHub and submit a pull request to `smokehill`

From there you will have to wait on one of the `smokehill` developers
to respond to the request.  This response might be an accept or some
changes/improvements/alternatives will be suggest.  We do not
guarantee that all requests will be accepted.

## Increasing chances of acceptance.

To help increase the chance of your pull request being accepted:

1. Run the tests, and make sure the package manager works..
1. Update the documentation, the surrounding code, examples elsewhere,
   guides, whatever is affected by your contribution
1. Use appropriate code formatting for both Idris and Haskell.



Adapted from the most excellent contributing files from the [Puppet project](https://github.com/puppetlabs/puppet) and [Factory Girl Rails](https://github.com/thoughtbot/factory_girl_rails/blob/master/CONTRIBUTING.md) and [Idris](https://github.com/idris-lang/Idris-dev).
