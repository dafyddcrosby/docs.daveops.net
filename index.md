---
layout: default
---

<ul class="post-list">
  {%- for page in site.pages -%}
  {%- if page.title != "" -%}
  <li>
    <a class="post-link" href="{{ page.url | relative_url }}">
      {{ page.title | escape }}
    </a>
  </li>
  {%- endif -%}
  {%- endfor -%}
</ul>

{% include_relative README.md %}
