## COMMENT-TAGS.el

An emacs package to highlight and list comment tags such as 'TODO', 'FIXME', 'BUG', 'XXX', 'HACK', 'KLUDGE'.

## Customizations
Ability to customize highlight colors and keywords list.

  * `comment-tags-keywords` to alter the list of highlighted/searched words.
  * `comment-tags-require-colon` to require a matching colon (default t).
  * `comment-tags-comment-start-only` to only match tags at the beginning of a comment (default nil).
  * `comment-tags-keymap-prefix` to alter the default prefix (default `C-c c`.
  * `comment-tags-case-sensitive` to toggle case-sensitivity (default t).
  * `comment-tags-foreground-color` to set default color for keywords.
  * `comment-tags-background-color` to set default background color for keywords.
  * `comment-tags-keyword-colors` to alter colors for keywords.

## Usage
The prefix for commands is `C-c c`.

Commands include:
  * `C-c c b` to list tags in current buffer
  * `C-c c a` to list tags in all buffers
  * `C-c c s` to filter to tags in current buffer by a word or phrase
  * `C-c c r` to filter to tags in all buffers by a word or phrase

## TODO:
  * Ability to customize faces/colors for different tags.
  * Filter tags with search of comment text.

