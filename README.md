# Smokehill

> Functionality First; Efficiency Next.

Smokehill is an simplistic package utility for Idris. It helps me with
the installation of the packages that I have authored or have
used. This has evolved from an initial `bash -> shelly -> bash` script
i have used over the years. I have decided to clean it up, slap some
monads on, and release it to the world.

> I need a corporal. You're it, until you're dead or I find someone better.

The engineering quality and design and this code is not my greatest
and needs improvement. Haskell is not my primary FP language, Idris
is, with that said my Haskell knowledge and ability is not as good as
my Idris foo. So forgive the Haskell quality.

If you don't like anything, PRs are welcome. I do have plans to
improve various aspects of the coding and functionality, but do not
necessarily have the time at the moment.

Questions? please ask.

> Why not write this in Idris?

Fact: I like Idris and dependent types.

This project is in Haskell rather than Idris is for two main reasons:

1. Haskell package eco-system is more developed and, when I search
   Idris' libraries the functionality that provides for system
   programming and interaction are just not there yet. It will be but
   for the time being some things are quicker to achieve using
   pre-built libraries than building them all myself...

2. I thought it would be good to learn Haskell for the Greater Good,
   and for the CV!

> Why is the package database bundled with the code and not an
> external git repo?

The package database is currently bundled with the application to make
development slightly simpler. There are plans to provide an external
source of package lists (*the chipshop*) but as with the first point:
*Functionality First; Efficient Next*.

Once smokehill has been installed, if you want to add a package to the
database you can add an audited ipkg file to the ipkg package
directory specified in:

```sh
smokehill paths
```

Package files can be *audited* using:

```sh
smokehill audit <pkg location>
```

> Son, you're on your own!

Unlike the preacher in 'Blazing Saddles' you are not on your own.  Any
issues please add to the issue tracker, and if you want have a go at
fixing them yourself. List of commands can be found out using:

```sh

smokehill --help
```
