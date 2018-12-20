---
layout: default
---

<ul class="post-list">
  {%- for post in site.posts -%}
  <li>
    {%- assign date_format = site.minima.date_format | default: "%b %-d, %Y" -%}
    <span class="post-meta">{{ post.date | date: date_format }}</span>
    <h3>
      <a class="post-link" href="{{ post.url | relative_url }}">
        {{ post.title | escape }}
      </a>
    </h3>
  </li>
  {%- endfor -%}
</ul>

This is daveops.net, my little repo of code snippets and chunks of wisdom
that's in no particular organized state.

The formatting of some pages are likely wrong, as many of the pages were ripped
out of Tiddlywiki (which was no longer up to the task), then ReStructuredText,
then Zim, and now Markdown (for a few different engines).

Pull requests are appreciated, but added at my discretion.
