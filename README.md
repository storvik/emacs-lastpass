# Emacs LastPass

[![MELPA](https://melpa.org/packages/lastpass-badge.svg)](https://melpa.org/#/lastpass)

A lastpass command wrapper for Emacs.
Includes an interactive LastPass mode for managing accounts and some useful functions which can be used to include LastPass in your settings and configurations.

## Installation

To use this package the [LastPass CLI](https://github.com/lastpass/lastpass-cli) must be installed.
The easiest way to install and configure emacs lastpass is to include this in your init.

``` emacs-lisp
(use-package lastpass
  :config
  (setq lastpass-user "foobar@foobar.com"))
```

# LastPass manager

Interactive lpass manager can be invoked with `M-x lastpass-list-all`.
Actions in `lastpass-list-all`:
- `n` next line
- `p` previod line
- `r` reload accounts
- `a` add or generate password
- `s` show password
- `w` add password to kill ring
- `m` move account to group
- `d` delete account
- `q` quit

# Interactive functions

Functions that can be run interactively by the `M-x` interface.

#### lastpass-login

Runs lpass login asynchronously and asks user for password.
Note that since this is an asynchronous process it will NOT wait for user input to continue.

#### lastpass-logout

Logs out of lpass using the --force option.
Good practice to do this whenever lpass functions aren't needed.

#### lastpass-status

Check if `lastpass-user` is logged in and prints message to minibuffer.

#### lastpass-getpass (account)

Display password for given account.
`account` can be either account id or account name.

#### lastpass-addpass (account user password url group)

Add account to LastPass.
Account name, user and password must be specified, but url and group can be set to `nil`.
When run interactively user is prompted for input.
If password is set to `nil`, or empty string when run interactive, it will be generated.
Default length is set in `lastpass-pass-length` and no symbols can be turned on with `lastpass-no-symbols`.

## Other functions

Functions that can't be run invteractively.

#### lastpass-runcmd (cmd &rest args)

Run lpass command `cmd` with arguments `args`.
Returns a list containing return code and return string, (returncode, returnstring).
Can be used to run custom lpass commmand not implementet in `lastpass.el`.

#### lastpass-pipe-to-cmd (cmd prepend &rest args)

Pipe `prepend` to lpass command `cmd` with arguments `args`.
Returns a list containing return code and return string, (returncode, returnstring).
The prepend string must be formatted to correspond with lpass notation, see `man lpass`.
Can for example be used to add account to LastPass:

``` emacs-lisp
(lastpass-pipe-to-cmd "add" "Username: Foo\nPassword: bar" "FooBarAccount")
```

This corresponds to the following shell command:

``` shell
printf "Username: Foo\nPassword: bar" | \
    lpass add FooBarAccount --non-interactive
```

#### lastpass-logged-in-p

Check if `lpass-user` is logged in.
Returns `nil` if user not logged in.
Example usage below.

## Example usage

Check if logged in to LastPass before running mu4e update.
Continues with update if user is logged in and asks user to log in if not.

``` emacs-lisp
(defun lastpass-mu4e-update-mail-and-index (update-function &rest r)
    (if (lastpass-logged-in)
        (apply update-function r)
      (progn
        (message "LastPass: Not logged in, log in and retry")
        (lastpass-login))))

(advice-add 'mu4e-update-mail-and-index :around #'lastpass-mu4e-update-mail-and-index)
```
