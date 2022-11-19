<div class="html" id="org36b238a">
<p>
&lt;ul class="post-list"&gt;
</p>

</div>

{%- for page in site.pages -%}

<div class="html" id="org154db49">
<p>
&lt;li&gt;
</p>

</div>

{{ page.title | escape }}

<div class="html" id="org9c2fa80">
<p>
&lt;/li&gt;
</p>

</div>

{%- endfor -%}

<div class="html" id="org0dedb35">
<p>
&lt;/ul&gt;
</p>

</div>

{% include<sub>relative</sub> README.md %}