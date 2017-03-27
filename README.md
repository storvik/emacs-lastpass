# Emacs LastPass

[![MELPA](https://melpa.org/packages/lastpass-badge.svg)](https://melpa.org/#/lastpass)
[![Build Status](https://travis-ci.org/storvik/emacs-lastpass.svg?branch=master)](https://travis-ci.org/storvik/emacs-lastpass)

A lastpass command wrapper for Emacs.
Includes an interactive LastPass mode for managing accounts and some useful functions which can be used to include LastPass in your settings and configurations.
Also featuring a custom `auth-source` backend allowing LastPass integration.

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-generate-toc again -->
**Table of Contents**

- [Installation](#installation)
    - [`lastpass-list-all-delimiter`](#lastpass-list-all-delimiter)
    - [Multifactor authentication](#multifactor-authentication)
- [Auth-source backend](#auth-source-backend)
- [LastPass manager](#lastpass-manager)
- [Function lists](#function-lists)
    - [Interactive functions](#interactive-functions)
        - [`lastpass-login`](#lastpass-login)
        - [`lastpass-logout`](#lastpass-logout)
        - [`lastpass-status`](#lastpass-status)
        - [`lastpass-getpass (account)`](#lastpass-getpass-account)
        - [`lastpass-visit-url (account)`](#lastpass-visit-url-account)
        - [`lastpass-addpass (account user password url group)`](#lastpass-addpass-account-user-password-url-group)
        - [`lastpass-auth-source-enable`](#lastpass-auth-source-enable)
    - [Other functions](#other-functions)
        - [`lastpass-runcmd (cmd &rest args)`](#lastpass-runcmd-cmd-rest-args)
        - [`lastpass-pipe-to-cmd (cmd prepend &rest args)`](#lastpass-pipe-to-cmd-cmd-prepend-rest-args)
        - [`lastpass-logged-in-p`](#lastpass-logged-in-p)
        - [`lastpass-getid (account)`](#lastpass-getid-account)
- [Example usage](#example-usage)
    - [Mu4e pre-compose check and offlineimap with LastPass](#mu4e-pre-compose-check-and-offlineimap-with-lastpass)
    - [Auth-source configuration for gmail](#auth-source-configuration-for-gmail)

<!-- markdown-toc end -->

# Installation

To use this package the [LastPass CLI](https://github.com/lastpass/lastpass-cli) must be installed.
The easiest way to install and configure emacs lastpass is to include this in your init.

``` emacs-lisp
(use-package lastpass
  :config
  ;; Set lastpass user
  (setq lastpass-user "foobar@foobar.com")
  ;; Enable lastpass custom auth-source
  (lastpass-auth-source-enable))
```

## `lastpass-list-all-delimiter`

This variable can be used to customize how emacs-lastpass interacts with the `lpass` command utility `ls` function.
Should be set to a character that is not to be found in any of the following fields:
- `id`
- `account name`
- `group`
- `user name`

## Multifactor authentication

When using multifactor authentication the variable `lastpass-multifactor-use-passcode` must reflect wether the user should be prompted for a passcode or not.
By default this variable is set to `nil`, hence only authentication without passcode is supported.
To enable authentication with passcode, add the following to your init (or in `:config` in the configuration example above);

``` emacs-lisp
(setq lastpass-multifactor-use-passcode t)
```

# Auth-source backend

LastPass auth-source backend can be enabled with the function `(lastpass-auth-source-enable)`.
Host, in emacs, must match the LastPass account name to make this work.
Recommended way of achieving this is to keep a seperate group, for example auth-source, containing all accounts that should be used together with the auth-source backend.
Configuration example can be seen in the [Example usage](#example-usage) section.
To implement this an advice is used:

``` emacs-lisp
(advice-add 'auth-source-backend-parse :before-until #'lastpass-auth-source-backend-parse)
```

Thanks to DamienCassou and his [auth-password-store](https://github.com/DamienCassou/auth-password-store) for help and guidance.

# LastPass manager

Interactive lastpass manager can be invoked with `M-x lastpass-list-all`.
Actions in `lastpass-list-all`:
- `enter` open URL in browser
- `n` next line
- `p` previod line
- `r` reload accounts
- `a` add or generate password
- `s` show password
- `w` add password to kill ring
- `m` move account to group
- `d` delete account
- `q` quit

# Function lists

List of functions in this package.

## Interactive functions

Functions that can be run interactively by the `M-x` interface.

### `lastpass-login`

Runs lpass login asynchronously and asks user for password.
Note that since this is an asynchronous process it will NOT wait for user input to continue.

### `lastpass-logout`

Logs out of lpass using the --force option.
Good practice to do this whenever lpass functions aren't needed.

### `lastpass-status`

Check if `lastpass-user` is logged in and prints message to minibuffer.

### `lastpass-getpass (account)`

Display password for given account.
`account` can be either account id or account name.

### `lastpass-visit-url (account)`

Open URL in web browser.
If run interactively it prompts for account, which can be either account name or unique ID.

### `lastpass-addpass (account user password url group)`

Add account to LastPass.
Account name, user and password must be specified, but url and group can be set to `nil`.
When run interactively user is prompted for input.
If password is set to `nil`, or empty string when run interactive, it will be generated.
Default length is set in `lastpass-pass-length` and no symbols can be turned on with `lastpass-no-symbols`.

### `lastpass-auth-source-enable`

Enable LastPass `auth-source` backend.

## Other functions

Functions that can't be run invteractively.

### `lastpass-runcmd (cmd &rest args)`

Run lpass command `cmd` with arguments `args`.
Returns a list containing return code and return string, (returncode, returnstring).
Can be used to run custom lpass commmand not implementet in `lastpass.el`.

### `lastpass-pipe-to-cmd (cmd prepend &rest args)`

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

### `lastpass-logged-in-p`

Check if `lastpass-user` is logged in.
Returns `nil` if user not logged in.
Example usage below.

### `lastpass-getid (account)`

Get LastPass id for account.
Returns nil if no match for account.

# Example usage

## Mu4e pre-compose check and offlineimap with LastPass

Check if logged in to LastPass before running mu4e update.
Continues with update if user is logged in and asks user to log in if not.

``` emacs-lisp
(defun lastpass-mu4e-update-mail-and-index (update-function &rest r)
  "Check if user is logged in and run UPDATE-FUNCTION with arguments R."
  (unless (lastpass-logged-in-p)
    (lastpass-login)
    (error "LastPass: Not logged in, log in and retry"))
  (apply update-function r))

(advice-add 'mu4e-update-mail-and-index :around #'lastpass-mu4e-update-mail-and-index)
```

This snippet can be used together with offlineimaps `pythonfile` and use LastPass when fetching mail.
`offlineimap.rc` should contain the follwing:

```
[general]
pythonfile = ~/offlineimap.py

[Repository Remote]
type = IMAP
remotehost = imap.gmail.com
remoteuser = foobar@gmail.com
remotepasseval = getLpass()
```

The python script `offlineimap.py` should look like:

``` python
#!/usr/bin/env python2
from subprocess import check_output

def getLpass():
    return check_output("lpass show --password AccountName", shell=True).strip("\n")
```

## Auth-source configuration for gmail

Use LastPass auth-source when sending mail.
This will replace the `.authinfo` file containing account and password information.
For this to work, `lastpass-auth-source-enable` must be run.
The following mail configuration can be used:

``` emacs-lisp
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-stream-type 'starttls)
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-user "foobar@gmail.com"
      smtpmail-smtp-service 587)
```

For this to work the lastpass account name must be `smtp.gmail.com`, i.e.

```
LastPass Vault
      └── auth-source
            └── smtp.gmail.com
```
