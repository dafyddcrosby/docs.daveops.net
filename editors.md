
# Editors

# Emacs

- C-x - Character eXtend. Followed by one character.
- M-x - Execute Extended Command (Named command eXtend). Followed by a long name
- M-S-x - Execute Extended Command for Buffer - show/run commands relevant to current buffer (Emacs 28+)

| keystrokes | desc         |
|---------- |------------ |
| C-x C-c    | quit         |
| C-g        | stop command |


## files

| keys    | M-x               | desc                                |
|------- |----------------- |----------------------------------- |
| C-x C-f |                   | find file                           |
| C-x C-s |                   | save file                           |
| C-x s   |                   | save buffers to files               |
|         | recover-this-file | Recover file from an auto-save file |


## buffers

| keys    | desc           |
|------- |-------------- |
| C-x b   | switch buffers |
| C-x C-b | list buffers   |


## tabs

| keys    | desc                     |
|------- |------------------------ |
| C-x t b | open new tab with buffer |
| C-x t 2 | new tab                  |


## windows

| keys    | desc                          |
|------- |----------------------------- |
| C-x 0   | delete active window          |
| C-x 1   | one window, kill all others   |
| C-x 2   | split window below            |
| C-x 3   | split window right            |
| C-x 4 b | change buffer in other window |


## frames

| keys    | desc                         |
|------- |---------------------------- |
| C-x 5 2 | Create new frame             |
| C-x 5 b | Switch buffer in other frame |
| C-x 5 0 | Delete active frame          |
| C-x 5 1 | Delete other frames          |


## modes

| M-x             | desc                                            |
|--------------- |----------------------------------------------- |
| fundmental-mode | Fundamental mode (apostrophe is word separator) |
| text-mode       | Text mode (intended for human language)         |


## misc text

| keys      | M-x            | desc                                    |
|--------- |-------------- |--------------------------------------- |
|           | auto-fill-mode | split text                              |
| C-x f     |                | Set fill mode margin (default 70 chars) |
| C-x 8 e i | emoj-insert    |                                         |
| C-x 8 e s | emoji-search   |                                         |


## bookmarks

bookmarks are persistent across sessions

| keys    | desc             |
|------- |---------------- |
| C-x r m | Set bookmark     |
| C-x r l | List bookmarks   |
| C-x r b | Jump to bookmark |


## shells

| M-x       | desc                   |
|--------- |---------------------- |
| shell     | comint-driven shell    |
| ansi-term | ANSI terminal emulator |


## help

Append C-h to a prefix key for combination documentation

`M-x customize` to customize settings, `M-x customize-browse` for tree mode

`C-h i m` opens up the builtin manuals `C-h R` to open a specific manual


### info reader

M-x info / C-h i

| Key | Purpose                                   |
|--- |----------------------------------------- |
| [   | previous node                             |
| ]   | next node                                 |
| l   | back in history                           |
| r   | forward in history                        |
| n   | next sibling node                         |
| p   | previous sibling node                     |
| u   | one level to a parent node                |
| SPC | scroll one screen at a time               |
| TAB | cycle through cross-references and links  |
| RET | opens the active link                     |
| m   | prompts for a menu item name and opens it |
| q   | closes the info browser                   |


### apropos

| key   | M-x                   | desc                  |
|----- |--------------------- |--------------------- |
|       | apropos               | search **everything** |
| C-h a | apropos-command       | search commands       |
| C-h d | apropos-documentation | search documentation  |


### describe

| key   | M-x               | desc                      |
|----- |----------------- |------------------------- |
| C-h m | describe-mode     | major mode documentation  |
| C-h x | describe-command  | command documentation     |
| C-h f | describe-function | function documentation    |
| C-h v | describe-variable | variable documentation    |
| C-h k | describe-key      | key binding documentation |

| key | desc                |
|--- |------------------- |
| i   | open info manual    |
| s   | go to source        |
| c   | customize interface |


## Bug Reference Mode

Built-in in version 28.2

```elisp
((nil . ((bug-reference-url-format . "https://example.org/browse/%s")
         (bug-reference-bug-regexp . "\\(\\[\\([A-Z]+-[0-9]+\\)\\]\\)"))))
```


## File local variables

Header

```
-*- mode: mode-name; variable: value -*-
```

Footer

```
Local Variables:
mode: mode-name
variable: value
End:
```


# VSCode


## Keyboard shortcuts

| shortcut      | desc                   |
|------------- |---------------------- |
| ctrl-k ctrl-s | keyboard shortcut menu |
| ctrl-\\       | split pane             |


# Obsidian


## Shortcuts

| Cmd    | desc            |
|------ |--------------- |
| Ctrl-O | open file       |
| Ctrl-P | command palette |


## File formats

- Markdown files: md;
- Image files: png, jpg, jpeg, gif, bmp, svg;
- Audio files: mp3, webm, wav, m4a, ogg, 3gp, flac;
- Video files: mp4, webm, ogv;
- PDF files: pdf.

<https://help.obsidian.md/Advanced+topics/Accepted+file+formats>


# Mindforger

<https://github.com/dvorka/mindforger/>
