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

{% include_relative README.md %}
