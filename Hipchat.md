---
title: Hipchat
==============

# Hipchat

Message commands
----------------

| command          | description         |
|------------------|---------------------|
| /part            | leave room          |
| /available <msg> | green bubble        |
| /away <msg>      | yellow bubble       |
| /dnd <msg>       | red bubble          |
| /topic <msg>     | set room topic      |
| /quote <msg>     | monospace font      |
| /code <msg>      | syntax highlighting |
| /me <msg>        | emote command       |


Keyboard shortcuts
------------------

| shortcut    | action                         |
|-------------|--------------------------------|
| cmd+f       | search                         |
| cmd+i       | invite                         |
| cmd+t       | join room                      |
| cmd+shift+n | create room                    |
| cmd+w       | close room                     |
| cmd+n       | search for room, user          |
| option+up   | put last line in your text box |


Modify last line
----------------


s/search/replace/

Links
-----


* [Emoticons list](https://www.hipchat.com/emoticons)
* <https://community.atlassian.com/t5/Hipchat-articles/A-Simple-Hack-Sorting-rooms-and-people/ba-p/650964>




# Server

## XMPP

```bash
# See if XMPP enabled
hipchat network --show

# Enable XMPP
hipchat network --enable-xmpp-ports
```

## Export

```bash
# Run an export job
hipchat export --export -p PASSPHRASE # passphrase decrypts the package

# Check status of the export job
hipchat export --check
```

* https://confluence.atlassian.com/hc/exporting-and-importing-your-hipchat-data-688882302.html

## Links
[Admin Guide](https://confluence.atlassian.com/hc/administering-hipchat-server-622985653.html)
