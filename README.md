## COMMENT-TAGS.el

An emacs package to highlight and list comment tags such as 'TODO', 'FIXME', 'BUG', 'XXX', 'HACK', 'KLUDGE'.

## Customizations
Ability to customize highlight colors and keywords list.

  * `comment-tags-keywords` to alter the list of highlighted/searched words.
  * `comment-tags-require-colon` to require a matching colon (default t).
  * `comment-tags-comment-start-only` to only match tags at the beginning of a comment (default nil).
  * `comment-tags-keymap-prefix` to alter the default prefix (default `C-c t`).
  * `comment-tags-case-sensitive` to set case-sensitivity (default t).
  * `comment-tags-faces` to set faces for keywords.
  * `comment-tags-lighter` to set modeline text (default nil).
  * `comment-tags-show-faces` to show colors/faces in comment-tags buffer/search (default t).

## Usage
The prefix for commands is `C-c t`.

Commands include:
  * `b` to list tags in current buffer (`comment-tags-list-tags-buffer`).
  * `a` to list tags in all buffers (`comment-tags-list-tags-buffers`).
  * `s` to jump to tag in current buffer by a word or phrase using reading-completion (`comment-tags-find-tags-buffer`).
  * `n` to jump to next tag from point (`comment-tags-next-tag`).
  * `p` to jump to previous tag from point (`comment-tags-previous-tag`).

## TODO:
  * Issues here: https://github.com/vincekd/comment-tags/issues


## Screenshots

List tags in buffer and select to jump to point.
![screenshot](screenshots/buffer-list.png)

Search tags in buffer (with search of line text) and jump to point.
![screenshot](screenshots/buffer-search.png)
