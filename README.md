# Periodic Commit Minor Mode

This minor mode will periodically commit all changes to a git repository when
any of its member files are saved (if the mode is active in the buffer being
saved). This mode is **not intended** to be used with software development
projects. Rather, it allows you to take advantage of git to preserve file
history automatically if corruption or user error are concerns.

## Why?

I wrote this mode to protect myself from losing data in notes files that I sync
using Dropbox. Because Dropbox sends all files everywhere, if I were to
mistakenly destroy content and save the file, and then if I were to lose my undo
history as well, Dropbox would happily sync the destroyed files everywhere.

As my notes have grown in length and importance, I can't risk major data
loss. The obvious solution is version control, but I also don't want to change
my notes and then also describe my changes in commit messages, nor do I want to
be slowed down by remembering to commit in the first place.

This minor mode solves that problem for me, and hopefully it will be of use to
someone else out there.

## What about Foo/Bar/Quux Mode?

This minor mode differs from `git-auto-commit-mode` and `vc-auto-commit` in the
following ways:

* Commits only _periodically_, with configurable minimum interval
* Can commit only tracked or _all_ changes, which is also configurable

## Why is this not in MELPA?

I wrote this quickly using `magit`'s functions, but it can be made more
universal by calling the built-in `vc` functions instead. Once that is done, I
may release this through MELPA. For now, use it if you wish, but know that there
is no guarantee it will work at all.
