# Web Browsers


# Firefox


## list of about: pages

```
about:about
```

<https://developer.mozilla.org/en-US/Firefox/The_about_protocol>


## DNS-over-HTTP

- <https://support.mozilla.org/en-US/kb/firefox-dns-over-https>
- <https://support.mozilla.org/en-US/kb/configuring-networks-disable-dns-over-https>
- <https://support.mozilla.org/en-US/kb/canary-domain-use-application-dnsnet>


## Debug logging

<https://developer.mozilla.org/en-US/docs/Mozilla/Debugging/HTTP_logging>

```shell
export NSPR_LOG_MODULES=timestamp,nsHttp:5,nsSocketTransport:5,nsStreamPump:5,nsHostResolver:5
export NSPR_LOG_FILE=~/Desktop/log.txt
./firefox
```


## Disable screenshots

extensions.screenshots.disabled -> true


## Disable new tab page

browser.newtabpage.enabled -> false


## Container tabs

- <https://addons.mozilla.org/en-US/firefox/addon/multi-account-containers/>


# Chromium

SHIFT-ESC - see which tabs are chewing up the most CPU/memory


# Google


## Don't redirect based on country

<https://google.com/ncr>


# HyperCard

- <https://blogs.loc.gov/thesignal/2021/07/all-hyped-up-for-hypercard-further-adventures-with-an-apple-legacy-format/>
