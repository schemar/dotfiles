# Mail dotfiles

dotfiles for macOS for managing email.

## Installation

You can use the `install.sh` helper.

Requirements:
* [Homebrew](https://docs.brew.sh/Installation) (on macOS, Linux, or WSL)
* [Homebrew's requirements](https://docs.brew.sh/Installation)

## Usage

You can use `mbsync -l <channel or group>` to list the incoming folders/labels.

You can use `echo "Test" | msmtp -a <account> "receiver@example.com"` to test sending.
(Likely to end up in spam)

Use `./poll-mbsync.sh` to poll IMAP every 10 minutes (`mbsync`).

## Setup

After you synced everything with `mbsync`, you can use `./fill-lbdb.sh` to fill little brother database
with information from the existing mails.

## Troubleshooting

If you **really** want to delete mail, you need to delete the mail from `~/.mail/...` **and** `~/.mbsync`.

To delete a mail in Gmail, do not delete it.
Instead, move it to `[Gmail]/Trash`.

