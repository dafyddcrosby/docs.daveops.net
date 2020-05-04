---
layout: default
---

<ul class="post-list">
  {%- for page in site.pages -%}
  <li>
    <a class="post-link" href="{{ page.url | relative_url }}">
      {{ page.title | escape }}
    </a>
  </li>
  {%- endfor -%}
</ul>

This is daveops.net, my little repo of code snippets and chunks of wisdom
that's in no particular organized state.

The formatting of some pages are likely wrong, as many of the pages were ripped
out of Tiddlywiki (which was no longer up to the task), then [ReStructuredText](restructuredtext.md),
then Zim, and now Markdown (for a few different engines).

Pull requests are appreciated, but added at my discretion.
